open Base
open Stdio

type t =
  { main : App.t
  ; name : string
  }

let create main name = { main; name }
let filename = "/proj.cbt"

let set_printer_longest proj =
  let longest =
    List.fold
      ~init:!Printer.longest_module
      ~f:(fun acc app ->
        let len = String.length app.name in
        match Int.compare acc len with
        | -1 -> len
        | _ -> acc)
      proj.main.modules
  in
  Printer.longest_module := longest
;;

let build_cmd proj =
  let packages =
    match List.is_empty proj.main.libs with
    | true -> None
    | false ->
      let prefix = Cmd.Fsingle "-linkpkg" in
      let packages =
        List.map
          ~f:(fun l ->
            match l with
            | "threads" -> Cmd.Fsingle "-thread"
            | x -> Cmd.Fdouble ("-package", x))
          proj.main.libs
      in
      Some (prefix :: packages)
  in
  let output_file = proj.name ^ ".exe" in
  let output = Some (Cmd.Fdouble ("-o", output_file)) in
  let modules_sorted = List.sort ~compare:App.compare proj.main.modules in
  let modules =
    List.fold ~init:"" ~f:(fun a m -> a ^ m.name ^ ".cmx" ^ " ") modules_sorted
  in
  Cmd.(empty
  |> add_flags packages
  |> add_flag output
  |> add_flag (Some (Fsingle (modules ^ proj.main.name ^ ".cmx")))
  |> to_string)
;;

let compile ?(force = false) ?(show_cmd = false) proj =
  set_printer_longest proj;
  let compiled_modules =
    App.compile ~force ~show_cmd proj.main ~already_compiled:[]
  in
  let output_file_exists =
    match Unix.access proj.name [ Unix.F_OK ] with
    | () -> true
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
  in
  match List.length compiled_modules, output_file_exists with
  | 0, true -> Printer.ok "project is already up to date"
  | len, _ ->
    if (not output_file_exists && Int.(len = 0))
    then
      Printer.info
        "everything is already up to date but the executable file is missing";
    let cmd = build_cmd proj in
    Printer.project_compiling ~show_cmd proj.name cmd;
    let c = Unix.open_process_in cmd in
    match Unix.close_process_in c with
    | Unix.WEXITED 0 ->
      Unix.rename (proj.name ^ ".exe") proj.name;
      Printer.ok "done"
    | Unix.WEXITED code ->
      Printer.error
      @@ "couldn't link project, subprocess returned "
      ^ Int.to_string code
      ^ " (probably missing some dependencies?)"
    | _ | (exception _) ->
      Printer.error "something went wrong during compilation..."
;;

let from_file path =
  let app = App.from_file path |> List.hd_exn in
  { main = app; name = app.name }
;;

let is_valid_lib name =
  let cmd = "ocamlfind list | grep -P \"^" ^ name ^ "\\h\"" in
  let c = Unix.open_process_in cmd in
  match Unix.close_process_in c with
  | Unix.WEXITED 1 -> false
  | _ -> true
;;

let restore ?channel () =
  let main_regex = Re.(compile @@ seq [ bol; str "let () =" ]) in
  let namespace_regex =
    Re.(
      compile
      @@ alt
           [ seq [ str "open "; upper; rep lower ]
           ; seq [ upper; rep lower; char '.' ]
           ])
  in
  let strip_extension filename =
    let len = String.length filename in
    assert (len > 3);
    String.sub ~pos:0 ~len:(len - 3) filename
  in
  let prepare_deps_list lst =
    match lst with
    | [] -> "_"
    | _ -> String.concat ~sep:", " lst
  in
  let cwd = Unix.getcwd () in
  let rec readdir acc handle =
    match Unix.readdir handle with
    | filename -> readdir (filename :: acc) handle
    | exception End_of_file -> acc
  in
  let dir_handle = Unix.opendir cwd in
  let ml_files =
    readdir [] dir_handle
    |> List.filter ~f:(fun file ->
      match String.length file with
      | x when x > 3 ->
        (match String.substr_index ~pos:(x - 3) ~pattern:".ml" file with
         | Some _ -> true
         | None -> false)
      | _ -> false)
  in
  match ml_files with
  | [] -> raise @@ Invalid_argument "no .ml files in current directory"
  | _ ->
    ();
    let main_file =
      List.find
        ~f:(fun path ->
          let contents = In_channel.read_all path in
          match Re.matches main_regex contents with
          | [] -> false
          | _ -> true)
        ml_files
    in
    let main_file =
      match main_file with
      | Some x -> x
      | None -> raise @@ Invalid_argument "couldn't detect main file"
    in
    let module_files =
      List.filter ~f:(fun x -> not (String.equal main_file x)) ml_files
    in
    let modules =
      List.map
        ~f:(fun s -> String.sub ~pos:0 ~len:(String.length s - 3) s)
        module_files
    in
    let extract_all_data path =
      let contents = In_channel.read_all path in
      let libs_raw = Re.matches namespace_regex contents in
      let all_libs =
        Set.to_list
        @@ List.fold
             ~init:(Set.empty (module String))
             ~f:(fun acc x ->
               let len = String.length x in
               match String.index x ' ' with
               | Some i ->
                 Set.add acc @@ String.sub ~pos:(i + 1) ~len:(len - i - 1) x
               | None -> Set.add acc @@ String.sub ~pos:0 ~len:(len - 1) x)
             libs_raw
      in
      let local_modules =
        List.filter_map
          ~f:(fun dep ->
            let dep = String.map ~f:Char.lowercase dep in
            match List.find ~f:(String.equal dep) modules with
            | Some x -> Some (String.map ~f:Char.lowercase x)
            | None -> None)
          all_libs
      in
      let external_libs =
        List.filter_map
          ~f:(fun dep ->
            match String.map ~f:Char.lowercase dep with
            | "stdlib" -> None
            | lowercase_dep when is_valid_lib lowercase_dep ->
              Some lowercase_dep
            | _ -> None)
          all_libs
      in
      strip_extension path, local_modules, external_libs
    in
    let submodules_data = module_files |> List.map ~f:extract_all_data in
    let main_data = extract_all_data main_file in
    let all_libs =
      Set.to_list
      @@ List.fold
           ~init:(Set.empty (module String))
           ~f:(fun acc (_, _, libs) -> List.fold ~init:acc ~f:Set.add libs)
           (main_data :: submodules_data)
    in
    let header =
      String.concat
        ~sep:" ; "
        [ strip_extension main_file
        ; prepare_deps_list modules
        ; prepare_deps_list all_libs
        ]
    in
    let submodules =
      List.map
        ~f:(fun (name, subs, libs) ->
          String.concat
            ~sep:" ; "
            [ name; prepare_deps_list subs; prepare_deps_list libs ])
        submodules_data
    in
    let proj_data = String.concat ~sep:"\n" (header :: submodules) in
    (match channel with
     | Some c ->
       Printer.info "printing generated \"proj.cbt\" file";
       Out_channel.output_line c proj_data;
       Out_channel.flush c
     | None ->
       Out_channel.write_all "proj.cbt" ~data:proj_data;
       Unix.mkdir "./_build" 0o777)
;;

let init name =
  let open Printf in
  let open Unix in
  let info = sprintf "creating project at %s/%s" (getcwd ()) name in
  Printer.info info;
  let perms = 0o777 in
  mkdir name perms;
  mkdir (name ^ "/_build") perms;
  mkdir (name ^ "/_test") perms;
  let cbt_contents =
    sprintf
      "# module ; relies on modules ; uses external libs\n%s ; _ ; _"
      name
  in
  let file_contents = "let () = print_endline \"Hello, World!\"" in
  let open Out_channel in
  let proj_channel = create (name ^ filename) in
  output_string proj_channel cbt_contents;
  close proj_channel;
  let file_channel = create (name ^ "/" ^ name ^ ".ml") in
  output_string file_channel file_contents
;;

let drop_merlin proj =
  Printer.info "creating .merlin file...";
  let packages = String.concat ~sep:" " @@ App.get_packages proj.main in
  let contents = "S .\nS _test\nB _build\n\nPKG " ^ packages in
  Out_channel.write_all ".merlin" ~data:contents
;;

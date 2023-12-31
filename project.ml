open Base
open Stdio

type t =
  { main : App.t
  ; name : string
  }

let create main name = { main; name }
let filename = "/proj.cbt"

let[@warning "-16"] compile proj ?(force = false) =
  let output = Some ("-o", proj.name ^ ".exe") in
  let packages =
    match List.length proj.main.libs with
    | 0 -> None
    | _ ->
      let prefix = "-linkpkg", "" in
      let packages = List.map ~f:(fun l -> "-package", l) proj.main.libs in
      Some (prefix :: packages)
  in
  let modules =
    List.fold
      ~init:""
      ~f:(fun a m -> a ^ m.name ^ ".cmx" ^ " ")
      proj.main.modules
  in
  match App.compile ~force proj.main with
  | 0 -> printf "had to compile nothing, lmao\n"
  | _ ->
    ();
    let cmd =
      Cmd.empty
      |> Cmd.add_flags packages
      |> Cmd.add_flag output
      |> Cmd.add_flag (Some ("", modules ^ proj.main.name ^ ".cmx"))
      |> Cmd.to_string
    in
    printf "compiling project...\n";
    let c = Unix.open_process_in cmd in
    (match Unix.close_process_in c with
     | Unix.WEXITED _ -> Unix.rename (proj.name ^ ".exe") proj.name
     | _ | (exception _) -> printf "something went wrong during compiling...\n")
;;

let from_file path =
  let app = App.from_file path |> List.hd_exn in
  { main = app; name = app.name }
;;

let init name =
  printf "creating project at %s/%s\n" (Unix.getcwd ()) name;
  let perms = 0o777 in
  Unix.mkdir name perms;
  Unix.mkdir (name ^ "/_build") perms;
  let cbt_contents =
    Printf.sprintf
      "# module ; relies on modules ; uses external libs\n%s ; _ ; _"
      name
  in
  let file_contents = "let () = print_endline \"eat ass\\n\"" in
  let proj_channel = Out_channel.create (name ^ filename) in
  Out_channel.output_string proj_channel cbt_contents;
  Out_channel.close proj_channel;
  let file_channel = Out_channel.create (name ^ "/" ^ name ^ ".ml") in
  Out_channel.output_string file_channel file_contents
;;

let drop_merlin proj =
  printf "creating .merlin file...\n";
  let packages = String.concat ~sep:" " @@ App.get_packages proj.main in
  let contents = "S .\nB _build\n\nPKG " ^ packages in
  Out_channel.write_all ".merlin" ~data:contents
;;

open Base
open Stdio

exception CompilationError

type t =
  { name : string
  ; modules : t list
  ; libs : string list
  }

let empty = { name = ""; modules = []; libs = [] }
let set_name name a = { a with name }
let add_lib l a = { a with libs = l :: a.libs }
let add_module m a = { a with modules = m :: a.modules }

let compare app1 app2 =
  match
    List.find ~f:(fun m -> String.equal app1.name m.name) app2.modules,
    List.find ~f:(fun m -> String.equal app2.name m.name) app1.modules
  with
  | Some _, Some _ | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
;;

let source_updated app already_compiled =
  let ml_modification = (Unix.stat (app.name ^ ".ml")).st_mtime in
  let cmx_modification =
    match Unix.stat ("_build/" ^ app.name ^ ".cmx") with
    | s -> s.st_mtime
    | exception _ -> 0.0
  in
  Float.is_positive (ml_modification -. cmx_modification)
;;

let build_cmd app =
    let packages =
      match app.libs with
      | [] -> None
      | _ -> Some (List.map ~f:(fun l -> Cmd.Fdouble ("-package", l)) app.libs)
    in
    let into_module = Some (Cmd.Fdouble ("-c", app.name ^ ".ml")) in
    let output = Some (Cmd.Fdouble ("-o", "_build/" ^ app.name ^ ".cmx")) in
    Cmd.empty
    |> Cmd.add_flags packages
    |> Cmd.add_flag into_module
    |> Cmd.add_flag output
    |> Cmd.to_string
;;

let rec compile ?(force = false) ?(show_cmd = false) ?(already_compiled = []) a =
  let is_source_file_updated = source_updated a already_compiled in
  let submodules_compiled =
    List.fold ~init:already_compiled
              ~f:(fun acc sub -> compile sub ~force ~show_cmd ~already_compiled:acc)
              a.modules
  in
  let was_any_submodule_compiled =
    List.fold ~init:false
              ~f:(fun acc subm ->
                acc || match List.find ~f:(String.equal subm.name) submodules_compiled with
                | Some _ -> true
                | None -> false
              ) a.modules
  in
  let is_already_compiled =
    match List.find ~f:(fun name -> String.equal name a.name) already_compiled with
    | Some _ -> true
    | None -> false
  in
  match (is_already_compiled), (is_source_file_updated || was_any_submodule_compiled || force) with
  | true, _ ->
    submodules_compiled
  | false, true ->
    let cmd = build_cmd a in
    Printer.module_compiling ~show_cmd a.name cmd;
    let process = Unix.open_process_in cmd in
    let new_submodules_compiled =
      match Unix.close_process_in process with
      | Unix.WEXITED 2 ->
        Printer.error ("couldn't compile " ^ a.name);
        Stdlib.exit 1
      | Unix.WEXITED x ->
        a.name :: submodules_compiled
      | _ ->
        Printer.error "something went wrong...";
        Stdlib.exit 1
    in
    new_submodules_compiled
  | false, false ->
    Printer.module_skip a.name;
    submodules_compiled
;;

let remove_spaces = String.filter ~f:(fun c -> not (Char.equal c ' '))

let rec get_packages app =
  let raw =
    app.libs @ List.fold ~init:[] ~f:(fun acc sm -> get_packages sm) app.modules
  in
  let rec filter acc l =
    match l with
    | hd :: tl ->
      filter (hd :: acc) (List.filter ~f:(fun s -> not (String.equal hd s)) tl)
    | [] -> List.rev acc
  in
  filter [] raw
;;

let from_file path =
  let parse_line l =
    let[@warning "-8"] [ name; modules_raw; libs_raw ] =
      String.split ~on:';' l
    in
    let libs =
      String.split ~on:',' libs_raw
      |> List.map ~f:remove_spaces
      |> List.filter ~f:(fun str -> not (String.equal str "_"))
    in
    let modules =
      String.split ~on:',' modules_raw |> List.map ~f:remove_spaces
    in
    { name = remove_spaces name; modules = []; libs }, modules
  in
  let lines =
    In_channel.read_lines path
    |> List.filter ~f:(fun str -> not (String.is_prefix ~prefix:"#" str))
  in
  let app_n_ms = List.map ~f:parse_line lines in
  let rec populate (app, ms) =
    match ms with
    | [] | [ "_" ] -> app
    | hd :: tl ->
      let m, sm =
        List.find ~f:(fun (a, _) -> String.equal a.name hd) app_n_ms
        |> Option.value_exn
      in
      populate ({ app with modules = populate (m, sm) :: app.modules }, tl)
  in
  List.map ~f:populate app_n_ms
;;

open Base
open Stdio

let temp_filename = "./.temp_file.ml"

let test_app ?(show = false) app =
  let open App in
  let open Printf in
  let test_filename = sprintf "test_%s.ml" app.name in
  match Unix.access test_filename [ Unix.F_OK ] with
  | () -> begin
    Printer.info ("testing " ^ app.name);
    let header = "#use \"topfind\";;\n" in
    let with_libs =
      List.fold ~init:header ~f:(fun acc lib ->
        acc ^ (sprintf "#require \"%s\";;\n" lib)
      ) app.libs
    in
    let with_submoduless =
      List.fold ~init:with_libs ~f:(fun acc a ->
        acc ^ (sprintf "#mod_use \"%s.ml\";;\n" a.name)
      ) app.modules
    in
    let final_header = with_submoduless ^ (sprintf "#mod_use \"%s.ml\";;\n" app.name) in
    let file_data = In_channel.read_all test_filename in
    Out_channel.write_all temp_filename ~data:(final_header ^ file_data);
    let proc = Unix.open_process_in ("ocaml " ^ temp_filename) in
    match Unix.close_process_in proc with
    | Unix.WEXITED 0 ->
      Printer.ok "all tests have been completed"
    | Unix.WEXITED code ->
      Printer.error (sprintf "something went wrong, exited with code %d" code)
    | _ -> ()
  end
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
    if show then
      Printer.info (sprintf "%s doesn't have a test file. %s not found." app.name test_filename)
;;

let test_project proj =
  let open Project in
  List.iter ~f:(test_app) proj.main.modules;
  test_app proj.main
;;

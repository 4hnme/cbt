open Base
open Stdio

let test_app app =
  let open App in
  let test_filename = "test_" ^ app.name ^ ".ml" in
  match Unix.access test_filename [ Unix.F_OK ] with
  | () -> begin
    Printer.info ("debugging " ^ app.name);
    let header = "#use \"topfind\";;\n" in
    let with_libs =
      List.fold ~init:header ~f:(fun acc lib ->
        acc ^ "#require \"" ^ lib ^ "\";;\n"
      ) app.libs
    in
    let with_submoduless =
      List.fold ~init:with_libs ~f:(fun acc a ->
        acc ^ "#mod_use \"" ^ a.name ^ ".ml\";;\n"
      ) app.modules
    in
    let final_header = with_submoduless ^ "#mod_use \"" ^ app.name ^ ".ml\";;\n" in
    let file_data = In_channel.read_all test_filename in
    Out_channel.write_all ".temp_file" ~data:(final_header ^ file_data);
    let proc = Unix.open_process_in "ocaml .temp_file" in
    match Unix.close_process_in proc with
    | Unix.WEXITED 0 ->
      Printer.ok "all tests have been completed"
    | Unix.WEXITED code ->
      Printer.error ("something fucked up" ^ (Int.to_string code))
    | _ -> ()
  end
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
;;

let test_project proj =
  let open Project in
  List.iter ~f:(test_app) proj.main.modules;
  test_app proj.main
;;

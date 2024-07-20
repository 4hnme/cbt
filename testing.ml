open Base
open Stdio

let temp_filename = "./.temp_file.ml"
let env = Unix.environment ()

let parse_error_message msg offset =
  let rec aux acc words buffer chars =
    match buffer, chars with
    | [], ' ' :: tl | [], '-' :: tl | [], ',' :: tl ->
      aux acc words buffer tl
    | _, ' ' :: tl | _, '-' :: tl | _, ',' :: tl -> (
      let word = List.rev buffer |> String.of_list in
      match List.hd words with
      | Some "at" | Some "line" | Some "characters" ->
        aux (word :: acc) (word :: words) [] tl
      | _ ->
        aux acc (word :: words) [] tl
    )
    | _, hd :: tl ->
      aux acc words (hd :: buffer) tl
    | _, [] -> List.rev acc
  in
  let our_line = List.filter ~f:(fun line ->
    (* convoluted way to write String.starts_with *)
    String.substr_index line ~pattern:"Raised at"
    |> Option.value_map ~default:false ~f:(Int.equal 0)
  ) (String.split_lines msg)
  in
  match our_line with
  | line :: [] -> (
    match aux [] [] [] (String.to_list line) with
    | [func; line; col] ->
      let line_with_offset = Int.of_string line - offset in
      Ok(func, line_with_offset, col)
    | _ -> (
      let err = "could not parse error message" in
      Error err
    )
  )
  | _ ->
    let err =
      Printf.sprintf "could not parse error message (maybe syntax error?).\n\
      full message:\n%s" msg
    in Error err
;;

let test_app ?(show = false) app =
  let open App in
  let open Printf in
  let test_filename = sprintf "_test/%s_test.ml" app.name in
  match Unix.access test_filename [ Unix.F_OK ] with
  | () -> begin
    Printer.info ("testing " ^ app.name);
    let header = "#use \"topfind\";;\n" in
    let offset, with_libs =
      List.fold ~init:(2, header) ~f:(fun (offset, acc) lib ->
        offset + 1, acc ^ (sprintf "#require \"%s\";;\n" lib)
      ) app.libs
    in
    let offset, with_submoduless =
      List.fold ~init:(offset, with_libs) ~f:(fun (offset, acc) a ->
        offset + 1, acc ^ (sprintf "#mod_use \"%s.ml\";;\n" a.name)
      ) app.modules
    in
    let final_header = with_submoduless ^ (sprintf "#mod_use \"%s.ml\";;\n" app.name) in
    let file_data = In_channel.read_all test_filename in
    Out_channel.write_all temp_filename ~data:(final_header ^ file_data);
    let proc, out, err = Unix.open_process_full ("ocaml " ^ temp_filename) env in
    let error_data = In_channel.input_all err in
    match Unix.close_process_full (proc, out, err) with
    | Unix.WEXITED 0 ->
      Printer.ok "all tests have been completed\n"
    | Unix.WEXITED code -> (
      match parse_error_message error_data offset with
      | Error err ->
        Printer.error err;
      | Ok (func, line, col) ->
        Printer.error (sprintf "\n%s:%d:%s -> %s failed" test_filename line col func)
    )
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

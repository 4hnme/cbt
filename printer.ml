open Base
open Stdio

let output = ref stdout
let set_output chan = output := chan
let longest_module = ref 7

module type PRINTER = sig
  val module_compiling : ?show_cmd:bool -> string -> string -> unit
  val module_skip : string -> unit
  val project_compiling : ?show_cmd:bool -> string -> string -> unit
  val help : string -> string list -> unit
  val error : string -> unit
  val warning : string -> unit
  val ok : string -> unit
  val info : string -> unit
  val usage : string -> unit
end

module Printer_fancy : PRINTER = struct
  let reset = "\x1b[0m"
  let bold = "\x1b[1m"
  let black_on_red = "\x1b[;30;41m"
  let black_on_green = "\x1b[;30;42m"
  let black_on_yellow = "\x1b[;30;43m"
  let black_on_blue = "\x1b[;30;44m"
  let black_on_white = "\x1b[;30;47m"

  let already_displayed_skips = ref []

  let add_to_already_displayed str =
    already_displayed_skips := str :: !already_displayed_skips
  ;;

  let is_already_displayed str =
    List.fold
      ~init:false
      ~f:(fun acc name -> acc || String.equal str name)
      !already_displayed_skips
  ;;

  let pad_name name =
    let d_len = !longest_module - String.length name in
    let left = d_len / 2 in
    let right = left + d_len % 2 in
    let left_padding = String.init ~f:(fun _ -> ' ') left in
    let right_padding = String.init ~f:(fun _ -> ' ') right in
    left_padding ^ name ^ right_padding
  ;;

  let module_compiling ?(show_cmd = false) name cmd =
    let padded_name = pad_name name in
    Out_channel.fprintf !output "%s%s%s compiling...\n" black_on_blue padded_name reset;
    match show_cmd with
    | true ->
      Out_channel.fprintf !output "\t%s\n" cmd
    | false ->
      ()
    ;
    Out_channel.flush stdout
  ;;

  let module_skip name =
    match is_already_displayed name with
    | true ->
      ()
    | false ->
      add_to_already_displayed name;
      let padded_name = pad_name name in
      Out_channel.fprintf !output "%s%s%s is up to date, skip\n" black_on_blue padded_name reset
    ;
    Out_channel.flush stdout
  ;;

  let project_compiling ?(show_cmd = false) name cmd =
    let padded_name = pad_name name in
    Out_channel.fprintf !output "%s%s%s compiling...\n" black_on_green padded_name reset;
    match show_cmd with
    | true ->
      Out_channel.fprintf !output "\t%s\n" cmd
    | false ->
      ()
    ;
    Out_channel.flush stdout
  ;;

  let help name lines =
    Out_channel.fprintf !output "%s%s%s\n" black_on_white name reset;
    List.iter ~f:(Out_channel.fprintf !output "%s\n") lines;
    Out_channel.flush stdout
  ;;

  let error text =
    let padded_name = pad_name "error" in
    Out_channel.fprintf !output "%s%s%s %s\n" black_on_red padded_name reset text;
    Out_channel.flush stdout
  ;;

  let warning text =
    let padded_name = pad_name "warning" in
    Out_channel.fprintf !output "%s%s%s %s\n" black_on_yellow padded_name reset text;
    Out_channel.flush stdout
  ;;

  let ok text =
    let padded_name = pad_name "ok" in
    Out_channel.fprintf !output "%s%s%s %s\n" black_on_green padded_name reset text;
    Out_channel.flush stdout
  ;;

  let info text =
    let padded_name = pad_name "info" in
    Out_channel.fprintf !output "%s%s%s %s\n" black_on_white padded_name reset text;
    Out_channel.flush stdout
  ;;

  let usage name =
    Out_channel.fprintf !output
    "%sUsage%s %s [option] ?help\n\
     List of available options:\n\
     \t%sinit%s <project name>\n\
     \t%sbuild\n\
     \tforce-build\n\
     \tinstall\n\
     \tdrop-merlin\n
     \trestore%s\n"
   black_on_white reset name bold reset bold reset
   ;;
end

module Printer_simple = struct
  let already_displayed_skips = ref []

  let add_to_already_displayed str =
    already_displayed_skips := str :: !already_displayed_skips
  ;;

  let is_already_displayed str =
    List.fold
      ~init:false
      ~f:(fun acc name -> acc || String.equal str name)
      !already_displayed_skips
  ;;

  let module_compiling ?(show_cmd = false) name cmd =
    Out_channel.fprintf !output "compiling %s...\n" name;
    match show_cmd with
    | true -> Out_channel.fprintf !output "\t%s\n" cmd
    | false -> ()
    ;
    Out_channel.flush stdout
  ;;

  let module_skip name =
    match is_already_displayed name with
    | true -> ()
    | false ->
      Out_channel.fprintf !output "no updates in %s, skip\n" name;
      add_to_already_displayed name
    ;
    Out_channel.flush stdout
  ;;

  let project_compiling ?(show_cmd = false) (_ : string) cmd =
    Out_channel.fprintf !output "compiling project...\n";
    match show_cmd with
    | true -> Out_channel.fprintf !output "\t%s\n" cmd
    | false -> ()
    ;
    Out_channel.flush stdout
  ;;

  let help name lines =
    Out_channel.fprintf !output "%s\n" name;
    List.iter ~f:(Out_channel.fprintf !output "%s\n") lines;
    Out_channel.flush stdout
  ;;

  let error text =
    Out_channel.fprintf !output "error: %s\n" text;
    Out_channel.flush stdout
  ;;

  let warning text =
    Out_channel.fprintf !output "warning: %s\n" text;
    Out_channel.flush stdout
  ;;

  let ok text =
    Out_channel.fprintf !output "ok: %s\n" text;
    Out_channel.flush stdout
  ;;

  let info text =
    Out_channel.fprintf !output "info: %s\n" text;
    Out_channel.flush stdout
  ;;

  let usage name =
    Out_channel.fprintf !output
    "Usage: %s [option] ?help\n\
     List of available options:\n\
     \tinit <project name>\n\
     \tbuild\n\
     \tforce-build\n\
     \tinstall\n\
     \tdrop-merlin\n\
     \trestore\n"
   name
   ;;
end

include Printer_simple

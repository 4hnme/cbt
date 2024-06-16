open Base
open Stdio

module type PRINTER = sig
  val longest_module : int ref
  val module_compiling : ?show_cmd:bool -> string -> string -> unit
  val module_skip : string -> unit
  val project_compiling : ?show_cmd:bool -> string -> string -> unit
  val error : string -> unit
  val warning : string -> unit
  val ok : string -> unit
  val info : string -> unit
end

module Printer_fancy : PRINTER = struct
  let reset = "\x1b[0m"
  let black_on_red = "\x1b[;30;41m"
  let black_on_green = "\x1b[;30;42m"
  let black_on_yellow = "\x1b[;30;43m"
  let black_on_blue = "\x1b[;30;44m"
  let black_on_white = "\x1b[;30;47m"

  let longest_module = ref 7

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
    printf "%s%s%s compiling...\n" black_on_blue padded_name reset;
    match show_cmd with
    | true ->
      printf "\t%s\n" cmd
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
      printf "%s%s%s is up to date, skip\n" black_on_blue padded_name reset
    ;
    Out_channel.flush stdout
  ;;

  let project_compiling ?(show_cmd = false) name cmd =
    let padded_name = pad_name name in
    printf "%s%s%s compiling...\n" black_on_green padded_name reset;
    match show_cmd with
    | true ->
      printf "\t%s\n" cmd
    | false ->
      ()
    ;
    Out_channel.flush stdout
  ;;

  let error text =
    let padded_name = pad_name "error" in
    printf "%s%s%s %s\n" black_on_red padded_name reset text;
    Out_channel.flush stdout
  ;;

  let warning text =
    let padded_name = pad_name "warning" in
    printf "%s%s%s %s\n" black_on_yellow padded_name reset text;
    Out_channel.flush stdout
  ;;

  let ok text =
    let padded_name = pad_name "ok" in
    printf "%s%s%s %s\n" black_on_green padded_name reset text;
    Out_channel.flush stdout
  ;;

  let info text =
    let padded_name = pad_name "info" in
    printf "%s%s%s %s\n" black_on_white padded_name reset text;
    Out_channel.flush stdout
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
  let longest_module : int ref = ref 0

  let module_compiling ?(show_cmd = false) name cmd =
    printf "compiling %s...\n" name;
    match show_cmd with
    | true -> printf "\t%s\n" cmd
    | false -> ()
    ;
    Out_channel.flush stdout
  ;;

  let module_skip name =
    match is_already_displayed name with
    | true -> ()
    | false ->
      printf "no updates in %s, skip\n" name;
      add_to_already_displayed name
    ;
    Out_channel.flush stdout
  ;;

  let project_compiling ?(show_cmd = false) (_ : string) cmd =
    printf "compiling project...\n";
    match show_cmd with
    | true -> printf "\t%s\n" cmd
    | false -> ()
    ;
    Out_channel.flush stdout
  ;;

  let error text =
    printf "error: %s\n" text;
    Out_channel.flush stdout
  ;;

  let warning text =
    printf "warning: %s\n" text;
    Out_channel.flush stdout
  ;;

  let ok text =
    printf "ok: %s\n" text;
    Out_channel.flush stdout
  ;;

  let info text =
    printf "info: %s\n" text;
    Out_channel.flush stdout
  ;;
end

include Printer_fancy

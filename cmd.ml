open Base

type prefix = string
type flag = string
type value = string
type t = prefix * (flag * value) list

let empty : t = "ocamlfind ocamlopt -I _build", []

let add_flag (flag : (string * string) option) (prefix, flags) : t =
  match flag with
  | Some (name, value) -> prefix, (name, value) :: flags
  | None -> prefix, flags
;;

let add_flags (flagl : (string * string) list option) cmd =
  match flagl with
  | Some fls -> List.fold ~init:cmd ~f:(fun c fl -> add_flag (Some fl) c) fls
  | None -> cmd
;;

let to_string (prefix, flags) =
  let fs = List.rev flags in
  let flags_str =
    List.fold ~init:"" ~f:(fun a (f, v) -> a ^ " " ^ f ^ " " ^ v) fs
  in
  prefix ^ flags_str
;;

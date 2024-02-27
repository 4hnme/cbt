open Base

type prefix = string
type value = string
type name = string
type flag = Fsingle of string | Fdouble of string * value
type t = prefix * (flag list)

let empty : t = "ocamlfind ocamlopt -I _build", []

let add_flag (flag : flag option) (prefix, flags) : t =
  match flag with
  | Some (Fdouble (name, value)) -> prefix, (Fdouble (name, value) :: flags)
  | Some (Fsingle name) -> prefix, (Fsingle name) :: flags
  | None -> prefix, flags
;;

let add_flags (flagl : flag list option) cmd =
  match flagl with
  | Some fls -> List.fold ~init:cmd ~f:(fun c fl -> add_flag (Some fl) c) fls
  | None -> cmd
;;

let to_string (prefix, flags) =
  let fs = List.rev flags in
  let flags_str =
    List.fold ~init:"" ~f:(fun a flag ->
      match flag with
      | Fsingle name ->
        a ^ " " ^ name
      | Fdouble (name, value) ->
        a ^ " " ^ name ^ " " ^ value
    ) fs
  in
  prefix ^ flags_str
;;

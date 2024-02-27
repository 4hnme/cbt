open Base
open Stdio

let print_usage name =
  printf
    "Usage: %s [option] ?help\n\
     List of available options:\n\
     \tinit <project name>\n\
     \tsoft-build\n\
     \tbuild\n\
     \tinstall\n\
     \tdrop-merlin"
    name
;;

let proj_file = "./proj.cbt"

let () =
  match Sys.get_argv () with
  | [| _; "init"; "help" |] ->
    printf
      "~~~~\n\
       init\n\
       ~~~~\n\
       creates a new project directory. template is not custimizible yet\n"
  | [| _; "install"; "help" |] ->
    printf
      "~~~~~~~\n\
       install\n\
       ~~~~~~~\n\
       copies executable file into /usr/bin directory. path is customizible yet\n"
  | [| _; "drop-merlin"; "help" |] ->
    printf
      "~~~~~~~~~~~\n\
       drop-merlin\n\
       ~~~~~~~~~~~\n\
       create a .merlin file in the folder for better lsp integration. it \
       won't hurt to do \"touch dune-workspace\" either because \
       https://github.com/ocaml/ocaml-lsp/pull/1173\n"
  | [| _; "soft-build"; "help" |] ->
    printf
      "~~~~~~~~~~\n\
       soft-build\n\
       ~~~~~~~~~~\n\
       tries to compile project \"softly\". usually breaks, doesn't work \
       properly yet\n"
  | [| _; "build"; "help" |] ->
    printf "~~~~~\nbuild\n~~~~~\nsimply rebuilds the current project\n"
  | [| _; "init"; name |] -> Project.init name
  | [| _; "soft-build" |] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:false ~show_cmd:false proj
  | [| _; "build" |] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:true ~show_cmd:false proj
  | [| _; "build"; "show" |] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:true ~show_cmd:true proj
  | [| _; "install" |] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:true ~show_cmd:false proj;
    let c =
      Unix.open_process_in
        ("sudo cp ./" ^ proj.main.name ^ " /usr/bin/" ^ proj.main.name)
    in
    let _ = Unix.close_process_in c in
    ()
  | [| _; "drop-merlin" |] ->
    let proj = Project.from_file proj_file in
    Project.drop_merlin proj
  | _ -> print_usage "cbt"
;;

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
      "|~~~~|\n\
       |init|\n\
       |~~~~|\n\
       creates a new project directory. template is not custimizible yet\n"
  | [| _; "restore"; "help" |] ->
    printf
      "|~~~~~~~|\n\
       |restore|\n\
       |~~~~~~~|\n\
       creates a proj.cbt file in the current directory\n\
       might break stuff, to be used with caution\n"
  | [| _; "install"; "help" |] ->
    printf
      "|~~~~~~~|\n\
       |install|\n\
       |~~~~~~~|\n\
       copies executable file into /usr/bin directory. path is not customizible yet\n"
  | [| _; "drop-merlin"; "help" |] ->
    printf
      "|~~~~~~~~~~~|\n\
       |drop-merlin|\n\
       |~~~~~~~~~~~|\n\
       create a .merlin file in the folder for better lsp integration.\n\
       in addition, you will have to pass \"--fallback-read-dot-merlin\" flag to ocamllsp command"
  | [| _; "soft-build"; "help" |] ->
    printf
      "|~~~~~~~~~~|\n\
       |soft-build|\n\
       |~~~~~~~~~~|\n\
       tries to compile project \"softly\". usually breaks, hopefully now it works \
       properly\n\
       use \"soft-build show\" to show generated build commands during the compilation\n"
  | [| _; "build"; "help" |] ->
    printf
      "|~~~~~|\n\
       |build|\n\
       |~~~~~|\n\
       simply rebuilds the current project\n\
       use \"build show\" to show generated build commands during the compilation\n"
  | [| _; "init"; name |] -> Project.init name
  | [| _; "restore" |] -> Project.restore ()
  | [| _; "soft-build" |] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:false ~show_cmd:false proj
  | [| _; "soft-build"; "show" |] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:false ~show_cmd:true proj
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
    let () = match Unix.close_process_in c with
    | Unix.WEXITED 0 ->
      let proj = Project.from_file proj_file in
      printf "installed %s in /usr/bin/%s\n" proj.name proj.name
    | Unix.WEXITED _ ->
      let proj = Project.from_file proj_file in
      printf "failed to install %s...\n" proj.name
    | _ -> printf "something went horribly wrong..."
    in ()
  | [| _; "drop-merlin" |] ->
    let proj = Project.from_file proj_file in
    Project.drop_merlin proj
  | _ -> print_usage "cbt"
;;

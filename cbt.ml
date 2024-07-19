open Base
open Stdio

let proj_file = "./proj.cbt"

let () =
  match List.of_array (Sys.get_argv ()) with
  | _ :: "init" :: "help" :: [] ->
    Printer.help
      "init"
      [ "creates a new project directory. template is not custimizible yet" ]
  | _ :: "restore" :: "help" :: [] ->
    Printer.help
      "restore"
      [ "creates a proj.cbt file in current directory."
      ; "might break stuff, to be used with caution"
      ; "use \"restore to-stdout\" to print instead of writing into a file"
      ]
  | _ :: "install" :: "help" :: [] ->
    Printer.help
      "install"
      [ "rebuild project and copies executable file into /usr/bin directory by default."
      ; "you can specify installation path by setting \"CBT_INSTALL_PATH\" environment variable\n"
      ]
  | _ :: "drop-merlin" :: "help" :: [] ->
    Printer.help
      "drop-merlin"
       [ "create a .merlin file in project directory for better lsp integration."
       ; "in addition, you will have to pass \"--fallback-read-dot-merlin\" flag to ocamllsp command"
       ]
  | _ :: "build" :: "help" :: [] ->
    Printer.help
       "build"
       [ "tries to compile project \"softly\". usually breaks, hopefully now it works properly."
       ; "use \"build show\" to show generated build commands during compilation"
       ]
  | _ :: "test" :: "help" :: [] ->
    Printer.help
       "test"
       [ "run tests for your project."
       ; "warning: very-very early development. run at your own risk"
       ]
  | _ :: "force-build" :: "help" :: [] ->
    Printer.help
       "force-build"
       [ "simply rebuilds whole project."
       ; "use \"force-build show\" to show generated build commands during compilation"
       ]
  | _ :: "init" :: name :: [] -> Project.init name
  | _ :: "restore" :: [] -> Project.restore ()
  | _ :: "restore" :: "to-stdout" :: [] -> Project.restore ~channel:stdout ()
  | _ :: "build" :: [] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:false ~show_cmd:false proj
  | _ :: "test" :: [] ->
    let proj = Project.from_file proj_file in
    Testing.test_project proj
  | _ :: "build" :: "show" :: [] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:false ~show_cmd:true proj
  | _ :: "force-build" :: [] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:true ~show_cmd:false proj
  | _ :: "force-build" :: "show" :: [] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:true ~show_cmd:true proj
  | _ :: "install" :: [] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:true ~show_cmd:false proj;
    let install_path =
      match Sys.getenv "CBT_INSTALL_PATH" with
      | Some p ->
        (match String.chop_suffix ~suffix:"/" p with
         | Some _ -> p
         | None -> p ^ "/")
      | None -> "/usr/bin/"
    in
    let c =
      Unix.open_process_in
        ("sudo cp ./" ^ proj.main.name ^ "  " ^ install_path ^ proj.main.name)
    in
    let () =
      match Unix.close_process_in c with
      | Unix.WEXITED 0 ->
        let proj = Project.from_file proj_file in
        Printer.ok ("installed " ^ proj.name ^ " in " ^ (install_path ^ proj.name))
      | Unix.WEXITED _ ->
        let proj = Project.from_file proj_file in
        Printer.error ("failed to install" ^ proj.name)
      | _ -> Printer.error "something went horribly wrong"
    in
    ()
  | _ :: "drop-merlin" :: _ ->
    let proj = Project.from_file proj_file in
    Project.drop_merlin proj
  | name :: _ ->
    Printer.usage name;
    Stdlib.exit 1
  | _ ->
    Printer.error "something went horribly wrong";
    Stdlib.exit 1
;;

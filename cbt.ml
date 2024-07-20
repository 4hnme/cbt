open Base
open Stdio

let proj_file = "./proj.cbt"

let () =
  match List.of_array (Sys.get_argv ()) with
  | name :: command :: "help" :: [] ->
    Printer.help command
  | _ :: "init" :: name :: [] -> Project.init name
  | _ :: "restore" :: [] -> Project.restore ()
  | _ :: "restore" :: "to-stdout" :: [] -> Project.restore ~channel:stdout ()
  | _ :: "build" :: [] ->
    let proj = Project.from_file proj_file in
    Project.compile ~force:false ~show_cmd:false proj
  | _ :: "test" :: [] ->
    let proj = Project.from_file proj_file in
    Testing.test_project proj;
    Unix.unlink Testing.temp_filename (* can raise something but i don't really care enough *)
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

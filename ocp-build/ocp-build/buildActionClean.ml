(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

(* ocp-build install [OPTIONS]

  Set the options of the user preference file.

*)

(* open BuildBase *)
open BuildArgs
open BuildOptions

(* TODO: handle -arch attribute, ie:
   - remove only directories in arch/ subdir
   - don't remove other topdirectories/
*)

let distclean_arg = ref false

let arg_list = [
  "-distclean", Arg.Set distclean_arg, " Remove _obuild directory";
]

let action () =
  let project_root = BuildOptions.find_project_root () in
  let obuild_dir = File.add_basenames project_root
        [ project_build_dirname ] in
  let obuild_dir = File.to_string obuild_dir in
  if !distclean_arg then begin
    Printf.eprintf "Removing _obuild directory\n%!";
    BuildActions.delete_file_or_directory obuild_dir;
  end else begin

    Printf.eprintf "Removing build targets\n%!";


    begin
      try
        let files = Sys.readdir obuild_dir in
        Array.iter (fun file ->
          let filename = Filename.concat obuild_dir file in
          if Sys.is_directory filename then
            BuildActions.delete_file_or_directory filename;
        ) files
      with _ -> ()
    end;
    ()
  end

let subcommand = {
  sub_name = "clean";
  sub_help =  "Clean the project.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [ "Clean the project."; ];
  sub_action = action;
}


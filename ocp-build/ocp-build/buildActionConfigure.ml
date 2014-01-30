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

(* ocp-build prefs [OPTIONS]

  Set the options of the user preference file.

*)

(* open BuildBase *)
open BuildArgs
open BuildOptions

let arg_list = [

  arg_set_int_option ProjectOptions.njobs_option;
  arg_set_int_option ProjectOptions.verbosity_option;

  arg_set_true_option ProjectOptions.autoscan_option;
  arg_set_false_option ProjectOptions.autoscan_option;

  arg_set_true_option ProjectOptions.digest_option;
  arg_set_false_option ProjectOptions.digest_option;

  arg_set_true_option ProjectOptions.bytecode_option;
  arg_set_false_option ProjectOptions.bytecode_option;

  arg_set_true_option ProjectOptions.native_option;
  arg_set_false_option ProjectOptions.native_option;

(* ProjectOptions.meta_dirnames_option *)
  arg_set_string_option ProjectOptions.install_destdir_option;
  arg_set_string_option ProjectOptions.install_bindir_option;
  arg_set_string_option ProjectOptions.install_libdir_option;
  arg_set_string_option ProjectOptions.install_datadir_option;

  arg_set_string_option ProjectOptions.ocamllib_option;

  arg_set_true ProjectOptions.use_ocamlfind_option;
  arg_set_false ProjectOptions.use_ocamlfind_option;

]

let action () =
  let project_root = BuildOptions.find_project_root () in
(*  Printf.eprintf "project_root = %S\n" (File.to_string project_root); *)
  let filename = File.add_basenames project_root
      [ project_build_dirname; project_config_basename ] in

  BuildOptions.load_config ProjectOptions.config_file filename;
  BuildOptions.apply_arguments ();
  BuildOptions.save_config ProjectOptions.config_file

let subcommand = {
  sub_name = "configure";
  sub_help =  "Set the project options.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [
    "Set the project options.";
  ];
  sub_action = action;
}


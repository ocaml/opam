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


(* open BuildBase *)
open BuildArgs
open BuildOptions

(*
open BuildBase
open Stdlib2
open SimpleConfig
open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions
*)

let arg_list = []
let action () =
  let root_dir = BuildOptions.project_build_dirname in

  if Sys.file_exists root_dir then begin
    if not (Sys.is_directory root_dir) then begin
      Printf.eprintf "Error: cannot create %s/ directory:\n" root_dir;
      Printf.eprintf "  %s exists, but is not a directory\n" root_dir;
      BuildMisc.clean_exit 2
    end;
    (* TODO: we should probably do some check to verify that we have
    write permission everywhere in _obuild. *)
  end else
    BuildMisc.safe_mkdir BuildOptions.project_build_dirname

let subcommand = {
  sub_name = "init";
  sub_help =  "Set the root of a project.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [
    "Set the root of a project.";
  ];
  sub_action = action;
}


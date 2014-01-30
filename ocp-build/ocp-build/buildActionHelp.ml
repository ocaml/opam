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
(* open Stdlib2 *)
open SimpleConfig

open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions
open BuildArgs
open BuildTerm
open BuildActions


let list_ocp_prims () =
  let prims = BuildOCPInterp.primitives_help () in
  Printf.printf "Available primitives: (use -ocp-prim PRIM for details)\n";
  StringMap.iter (fun name (_, help) ->
    Printf.printf "%%%s (_) : %s\n" name
      (match help with
         s :: _ -> s
       | [] -> "(no help available)")
  ) prims;
  Printf.printf "%!"

let ocp_prim prim =
  try
    let (_, help) = StringMap.find prim (BuildOCPInterp.primitives_help()) in
    Printf.printf "%%%s (ENV) : %s\n%!"
      prim (String.concat "\n   " help)
  with Not_found ->
    Printf.eprintf "No primitive %%%s\n%!" prim;
    BuildMisc.clean_exit 2

let arg_list = [
  "-list-ocp-prims", Arg.Unit list_ocp_prims,
  " Print the list of available primitives(%prim)";

  "-ocp-prim", Arg.String ocp_prim,
   "PRIM Display help on primitive %PRIM";
]

let action () =
  ()



let subcommand = {
  sub_name = "help";
  sub_help =  "Help On ocp-build.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [ "Help on ocp-build."; ];
  sub_action = action;
}


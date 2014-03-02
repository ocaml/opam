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

(*
val add_dependency_loader :
  string -> (string -> ( string * string list list) list) -> unit
val find_dependency_loader :
  string -> (string -> ( string * string list list) list)
*)

val new_dir_id : BuildEngineTypes.build_context -> int
val new_file_id : BuildEngineTypes.build_context -> int
val new_rule_id : BuildEngineTypes.build_context -> int
val new_process_id : BuildEngineTypes.build_context -> int

val file_filename : BuildEngineTypes.build_file -> string
(* val print_indented_command : BuildEngineTypes.build_action -> unit *)



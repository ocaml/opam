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

exception MissingSourceWithNoBuildingRule of BuildEngineTypes.build_rule * string

(* [init targets] Initialize the build engine, by checking activating all the rules
 needed for the creation of the files [targets].
   raise MissingSourceWithNoBuildingRule (rule, filename) if a file is needed as
      a source and no rule is available for generating it.
*)
val init :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_file list -> unit

val fatal_errors : BuildEngineTypes.build_context -> string list list
(* val errors : unit -> string list list *)

(* [parallel_loop ncores] Start the build process on [ncores] cores. *)
val parallel_loop :
  BuildEngineTypes.build_context -> int -> unit


val sanitize :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.delete_orphans ->
  (string -> bool) -> (* return false on basename if topdir is orphan *)
  int

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

val get_pp :
  BuildTypes.package_info ->
  string -> (* source basename *)
  BuildOCPVariable.env ->
  BuildOCamlTypes.pp


(* Should probably be in BuildOCamlMisc *)
val add_pp_requires :
  BuildEngineTypes.build_rule -> BuildOCamlTypes.pp -> unit

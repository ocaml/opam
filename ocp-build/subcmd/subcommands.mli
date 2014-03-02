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

module TYPES : sig

  type subcmd_spec = {
    subcmd_list : (Arg.key * Arg.spec * Arg.doc) list;
    subcmd_usage : string list;
    subcmd_help : string list;
  }

  type subcmd_init = (unit -> unit)
  type subcmd_action = (string array -> unit)

end

open TYPES

module type SPEC = sig

  val subcmd_spec : subcmd_spec
  val subcmd_init : subcmd_init
  val subcmd_main : subcmd_action

end

exception Usage

val parse :
(Arg.key * Arg.spec * Arg.doc) list ->
  (string * subcmd_init * subcmd_spec * subcmd_action) list ->
  Arg.usage_msg ->
  unit

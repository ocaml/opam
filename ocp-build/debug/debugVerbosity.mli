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

(* var OCP_DEBUG_MODULES is a set of space separated module names *)

val add_submodules : string -> string list -> unit
val increase_verbosity : string -> int -> unit
val increase_verbosities : string list -> int -> unit

val verbose : string list -> string -> (int -> bool)

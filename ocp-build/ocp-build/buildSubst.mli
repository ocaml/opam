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

val putenv : string -> string -> unit
val add_to_global_subst : string -> string -> unit

val subst : StringSubst.subst -> string -> string
val subst_global : string -> string

val global_subst : StringSubst.subst
val add_to_local_subst : StringSubst.subst -> string -> string -> StringSubst.subst


val create_substituter :
  (string * ('a -> string)) list -> 'a StringSubst.M.subst
val apply_substituter :
  'a StringSubst.M.subst -> string -> 'a -> string


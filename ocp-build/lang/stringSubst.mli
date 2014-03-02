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

(* This module implements a simple algorithm for substituting any
   string in any string. If several strings can be substituted at the
   same place, the one starting at the earliest position is chosen,
   and among different strings at the same position, the longest match
   is chosen.

   The current worst complexity is O(N.M) where N is the length of the
   argument string and M is the longest string that can be
   substituted. We should probably implement KMP at some point for
   applications where performance matters.
*)

type subst

val empty_subst : unit -> subst

(* [add_to_subst subst src dst] mutates [subst] to add
 a transition from [src] to [dst]. *)
val add_to_subst : subst -> string -> string -> unit
(* [add_to_copy subst src dst] returns a copy of [subst] with a
   transition from [src] to [dst]. *)
val add_to_copy : subst -> string -> string -> subst

val subst_of_list : (string * string) list -> subst

val subst : subst -> string -> int * string
val iter_subst : subst -> string -> int * string

module M : sig

  type 'a subst

  val empty_subst : unit -> 'a subst

(* [add_to_subst subst src dst] mutates [subst] to add
 a transition from [src] to [dst]. *)
  val add_to_subst : 'a subst -> string -> ('a -> string) -> unit

(* [add_to_copy subst src dst] returns a copy of [subst] with a
   transition from [src] to [dst]. *)
  val add_to_copy : 'a subst -> string -> ('a -> string) -> 'a subst


  val subst_of_list : (string * ('a -> string)) list -> 'a subst
  val subst : 'a subst -> string -> 'a -> int * string
  val iter_subst : 'a subst -> string -> 'a -> int * string

end

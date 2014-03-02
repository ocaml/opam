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

(** Extension of the stdlib Hashtbl module *)

open Hashtbl

(** Convert an hash-table into a list *)
val to_list : ('a, 'b) t -> ('a * 'b) list

(** Convert a list into an hash-table *)
val of_list : ('a * 'b) list -> ('a, 'b) t

(** Increments the integer value associated to a key *)
val incr : ('a, int) t ->  'a -> unit

(** Check whether a predicate holds on all key-value pairs of an
    hash-table *)
val for_all : ('a, 'b) t -> ('a -> 'b -> bool) -> bool

(** Check wether a predicate holds on at least one key-value pair of
    an hash-table *)
val exists : ('a, 'b) t -> ('a -> 'b -> bool) -> bool

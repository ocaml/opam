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

(** Extension of the stdlib Stream module *)

open Stream

(** Convert a token stream into a token list *)
val to_list : 'a t -> 'a list

(** Convert a list of lines into a char stream *)
val of_lines : string list -> char t

(** Check whether a stream is empty *)
val is_empty : 'a t -> bool

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

(** Functions over the Option type *)

(** Map a function over an optional value *)
val map : ('a -> 'b) -> 'a option -> 'b option 

(** Return either the value contained in an optional value, or a
    default value *)
val default : 'a -> 'a option -> 'a

(** Iter a function over an optional value *)
val iter : ('a -> unit) -> 'a option -> unit


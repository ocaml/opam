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

(** Extension of the stdlib Digest module *)

open Digest

(** Return the printable hexadecimal representation of the given digest. *)
val to_hex : t -> string

(** Return the digest corresponding to the printable hexadecimal representation. *)
val of_hex : string -> t

(** Return the digest by interpreting the string as a raw digest *)
val of_direct_string : string -> t

(** Return the string with the raw digest inside *)
val to_direct_string : t -> string



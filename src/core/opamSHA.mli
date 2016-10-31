(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Pure OCaml implementation of SHA256/512 hashing functions. Functions take a
    filename and return the hash as an hex string. *)

val sha256: string -> string

val sha512: string -> string

val hash: [< `SHA256 | `SHA512 ] -> string -> string

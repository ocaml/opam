(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Pure OCaml implementation of SHA256/512 hashing functions. The hash is
    returned as an hex string. *)

val sha256_file: string -> string

val sha512_file: string -> string

val hash_file: [< `SHA256 | `SHA512 ] -> string -> string


val sha256_bytes: Bytes.t -> string

val sha512_bytes: Bytes.t -> string

val hash_bytes: [< `SHA256 | `SHA512 ] -> Bytes.t -> string


(** For compat, use the above *)

val sha256: string -> string

val sha512: string -> string

val hash: [< `SHA256 | `SHA512 ] -> string -> string

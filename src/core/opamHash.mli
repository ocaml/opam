(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2017 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Stored as hexadecimal strings *)
type computable_kind = [ `MD5 | `SHA256 | `SHA512 ]

type kind = [ computable_kind | `SWHID ]

type +'kind hash constraint 'kind = [< kind]

type t = kind hash

val kind: 'a hash -> 'a

(** The value of the hash, as a string of hexadecimal characters *)
val contents: t -> string

val string_of_kind: kind -> string

val md5: string -> [> `MD5] hash
val sha256: string -> [> `SHA256] hash
val sha512: string -> [> `SHA512] hash
val swhid : string -> [> `SWHID] hash

include OpamStd.ABSTRACT with type t := t

val to_string: t -> string

val of_string_opt: string -> t option

(** returns a sub-path specific to this hash, e.g.
    "md5/d4/d41d8cd98f00b204e9800998ecf8427e", as a list *)
val to_path: t -> string list

val check_file: string -> computable_kind hash -> bool

(** Like [check_file], but returns the actual mismatching hash of the file, or
    [None] in case of match *)
val mismatch: string -> computable_kind hash -> computable_kind hash option

(** Compute hash of the given file *)
val compute: ?kind:computable_kind -> string -> computable_kind hash

(** Compute the hash of the given string *)
val compute_from_string: ?kind:computable_kind -> string -> computable_kind hash

(** The identity function but raises on a non computable kind, e.g. `SWHID *)
val to_computable: t -> computable_kind hash

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let sha256_file file = Sha256.to_hex (Sha256.file file)
let sha512_file file = Sha512.to_hex (Sha512.file file)
let hash_file = function
  | `SHA256 -> sha256_file
  | `SHA512 -> sha512_file

let sha256_string str = Sha256.to_hex (Sha256.string str)
let sha512_string str = Sha512.to_hex (Sha512.string str)
let hash_string = function
  | `SHA256 -> sha256_string
  | `SHA512 -> sha512_string

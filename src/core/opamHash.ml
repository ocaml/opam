(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type computable_kind = [ `MD5 | `SHA256 | `SHA512 ]

type kind = [ computable_kind | `SWHID ]

let default_kind = `MD5

type +'kind hash = 'kind * string constraint 'kind = [< kind ]
type t = kind hash

let kind = fst
let contents = snd

let log msg = OpamConsole.log "HASH" msg

let pfx_sep_char = '='
let pfx_sep_str = String.make 1 pfx_sep_char

let string_of_kind = function
  | `MD5 -> "md5"
  | `SHA256 -> "sha256"
  | `SHA512 -> "sha512"
  | `SWHID -> "swhid"

let kind_of_string_computable s = match String.lowercase_ascii s with
  | "md5" -> `MD5
  | "sha256" -> `SHA256
  | "sha512" -> `SHA512
  | _s -> invalid_arg "OpamHash.kind_of_string_computable"

let kind_of_string s = match String.lowercase_ascii s with
  | "swhid" -> `SWHID
  | s ->
    try kind_of_string_computable s
    with Invalid_argument _msg -> invalid_arg "OpamHash.kind_of_string"

let is_hex_str len s =
  String.length s = len &&
  try
    String.iter (function
        | '0'..'9' | 'A'..'F' | 'a'..'f' -> ()
        | _ -> raise Exit)
      s;
    true
  with Exit -> false

let len = function
  | `MD5 -> 32
  | `SHA256 -> 64
  | `SHA512 -> 128
  | `SWHID -> 50

let is_valid_swhid s =
  String.length s = (len `SWHID) &&
  match String.split_on_char ':' s with
  | ["swh"; "1"; "rel" ; s] -> is_hex_str 40 s
  | _l -> false

let valid = function
  | `MD5 | `SHA256 | `SHA512 as kind -> is_hex_str (len kind)
  | `SWHID -> is_valid_swhid

let make kind s =
  if valid kind s then kind, String.lowercase_ascii s
  else invalid_arg ("OpamHash.make_"^string_of_kind kind)

let md5 = make `MD5
let sha256 = make `SHA256
let sha512 = make `SHA512
let swhid = make `SWHID

let of_string_opt s =
  try
    let kind, s =
      match OpamStd.String.cut_at s pfx_sep_char with
      | None -> `MD5, s
      | Some (skind, s) -> kind_of_string_computable skind, s
    in
    if valid kind s then Some (kind, String.lowercase_ascii s)
    else None
  with Invalid_argument _ -> None

let computable_of_string_opt s =
  try
    let kind, s =
      match OpamStd.String.cut_at s pfx_sep_char with
      | None -> `MD5, s
      | Some (skind, s) -> kind_of_string skind, s
    in
    if valid kind s then Some (kind, String.lowercase_ascii s)
    else None
  with Invalid_argument _ -> None

let of_string s =
  match of_string_opt s with
  | Some h -> h
  | None -> invalid_arg "OpamHash.of_string"

let computable_of_string s =
  match computable_of_string_opt s with
  | Some h -> h
  | None -> invalid_arg "OpamHash.computable_of_string"

let to_string (kind,s) =
  String.concat pfx_sep_str [string_of_kind kind; s]

let to_json s = `String (to_string s)
let of_json = function
| `String s -> of_string_opt s
| _ -> None

let to_path (kind,s) =
  match kind with
  | `SWHID -> [string_of_kind `SWHID; String.sub s 10 2; String.sub s 10 40]
  | `MD5 | `SHA256 | `SHA512 -> [string_of_kind kind; String.sub s 0 2; s]

let compute ?(kind=default_kind ) file =
  match kind with
  | `MD5  -> md5 (Digest.to_hex (Digest.file file))
  | (`SHA256 | `SHA512) as kind ->
    let sha =
      if not OpamCoreConfig.(!r.use_openssl) then
        OpamSHA.hash kind file
      else
      try
        match
          OpamSystem.read_command_output ["openssl"; string_of_kind kind; file]
        with
        | [l] ->
          let len = len kind in
          String.sub l (String.length l - len) len
        | _ ->
          log "openssl error, use internal sha library";
          OpamSHA.hash kind file
      with OpamSystem.Command_not_found _ | OpamSystem.Process_error _ | OpamSystem.Permission_denied _ ->
        log "openssl not found, use internal sha library";
        OpamSHA.hash kind file
    in
    make kind sha

let compute_from_string ?(kind=default_kind) str = match kind with
  | `MD5 -> md5 (Digest.to_hex (Digest.string str))
  | (`SHA256 | `SHA512) as kind ->
    make kind (OpamSHA.hash_bytes kind (Bytes.of_string str))

let check_file f (kind, _ as h) = compute ~kind f = h

let mismatch f (kind, _ as h) =
  let hf = compute ~kind f in
  if hf = h then None else Some hf

let to_computable (kind, h) =
  match kind with
  | `MD5 | `SHA256 | `SHA512 as kind -> kind, h
  | `SWHID -> raise (Invalid_argument "to_computable")

module O = struct
  type _t = t
  type t = _t
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
  let compare = compare
end

module Set = OpamStd.Set.Make(O)

module Map = OpamStd.Map.Make(O)

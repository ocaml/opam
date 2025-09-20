(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 ahrefs                                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* TODO: proper "kind" (cypher+bitlength? sigstore uris?) *)
type kind = [ `GPG ]

let _default_kind = `GPG
let all_kinds = [`GPG]

type t = kind * string

let kind = fst
let contents = snd

(* Order by signature strength *)
let compare_kind k l =
  match k, l with
  | `GPG, `GPG -> 0

let equal_kind k1 k2 = compare_kind k1 k2 = 0

let compare (k,h) (l,i) =
  match compare_kind k l with
  | 0 -> String.compare h i
  | cmp -> cmp

let equal h h' = compare h h' = 0

let pfx_sep_char = '='
let pfx_sep_str = String.make 1 pfx_sep_char

let string_of_kind = function
  | `GPG -> "gpg"

let kind_of_string s = match String.lowercase_ascii s with
  | "gpg" -> `GPG
  | _ -> invalid_arg "OpamSignature.kind_of_string"

let is_hex_str len s =
  String.length s = len && OpamStd.String.is_hex s

let len = function
  | `GPG -> 16 (* dummy summy *)

let valid kind = is_hex_str (len kind)

let is_null h =
  let count_not_zero c =
    function '0' -> c | _ -> succ c
  in
  OpamCompat.String.fold_left count_not_zero 0 (contents h) <> 0

let make kind s =
  if valid kind s then kind, String.lowercase_ascii s
  else invalid_arg ("OpamSignature.make_"^string_of_kind kind)

let gpg = make `GPG

let of_string_opt s =
  try
      match OpamStd.String.cut_at s pfx_sep_char with
      | None -> None
      | Some (skind, s) ->
          let kind = kind_of_string skind in
          if valid kind s then Some (kind, String.lowercase_ascii s)
          else None
  with Invalid_argument _ -> None

let of_string s =
  match of_string_opt s with
  | Some h -> h
  | None -> invalid_arg "OpamSignature.of_string"

let to_string (kind,s) =
  String.concat pfx_sep_str [string_of_kind kind; s]

let to_json s = `String (to_string s)
let of_json = function
| `String s -> of_string_opt s
| _ -> None

let sort signatures =
  List.sort (fun h h' -> compare h' h) signatures

let check_commit `TODO (_kind, _k) = failwith "TODO"

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

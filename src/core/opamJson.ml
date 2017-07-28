(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]

let addc b c = Buffer.add_char b c
let adds b s = Buffer.add_string b s
let adds_esc b s =
  let len = String.length s in
  let max_idx = len - 1 in
  let flush b start i =
    if start < len then Buffer.add_substring b s start (i - start);
  in
  let rec loop start i = match i > max_idx with
  | true -> flush b start i
  | false ->
      let next = i + 1 in
      match String.get s i with
      | '"' -> flush b start i; adds b "\\\""; loop next next
      | '\\' -> flush b start i; adds b "\\\\"; loop next next
      | '\x00' .. '\x1F' | '\x7F' (* US-ASCII control chars *) as c ->
          flush b start i;
          adds b (Printf.sprintf "\\u%04X" (Char.code c));
          loop next next
      | _ -> loop start next
  in
  loop 0 0

let enc_json_string b s = addc b '"'; adds_esc b s; addc b '"'
let enc_vsep b = addc b ','
let enc_lexeme b = function
| `Null -> adds b "null"
| `Bool true -> adds b "true"
| `Bool false -> adds b "false"
| `Float f -> Printf.bprintf b "%.16g" f
| `String s -> enc_json_string b s
| `Name n -> enc_json_string b n; addc b ':'
| `As -> addc b '['
| `Ae -> addc b ']'
| `Os -> addc b '{'
| `Oe -> addc b '}'

let enc_json b (json:t) =
  let enc = enc_lexeme in
  let enc_sep seq enc_seq k b = match seq with
  | [] -> enc_seq seq k b
  | seq -> enc_vsep b; enc_seq seq k b
  in
  let rec value v k b = match v with
    | `A vs -> arr vs k b
    | `O ms -> obj ms k b
    | `Null | `Bool _ | `Float _ | `String _ as v -> enc b v; k b
  and arr vs k b = enc b `As; arr_vs vs k b
  and arr_vs vs k b = match vs with
    | v :: vs' -> value v (enc_sep vs' arr_vs k) b
    | [] -> enc b `Ae; k b
  and obj ms k b = enc b `Os; obj_ms ms k b
  and obj_ms ms k b = match ms with
    | (n, v) :: ms -> enc b (`Name n); value v (enc_sep ms obj_ms k) b
    | [] -> enc b `Oe; k b
  in
  value json (fun _ -> ()) b

let to_string (json:t) =
  let b = Buffer.create 1024 in
  enc_json b json;
  Buffer.contents b

let json_buffer =
  ref []

let append key json =
  json_buffer := (key,json) :: !json_buffer

let flush oc =
  let b = Buffer.create 1024 in
  let json = (`O (List.rev !json_buffer)) in
  let json = enc_json b json; Buffer.contents b in
  output_string oc json; flush oc

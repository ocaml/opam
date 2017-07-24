(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

include Jsonm

type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]

let json_to_dst ~minify dst (json:t) =
  let enc e l = ignore (encode e (`Lexeme l)) in
  let rec value v k e = match v with
    | `A vs -> arr vs k e
    | `O ms -> obj ms k e
    | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
  and arr vs k e = enc e `As; arr_vs vs k e
  and arr_vs vs k e = match vs with
    | v :: vs' -> value v (arr_vs vs' k) e
    | [] -> enc e `Ae; k e
  and obj ms k e = enc e `Os; obj_ms ms k e
  and obj_ms ms k e = match ms with
    | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
    | [] -> enc e `Oe; k e
  in
  let e = encoder ~minify dst in
  let finish e = ignore (encode e `End) in
  match json with
  | `A _ | `O _ as json -> value json finish e
  | _ -> invalid_arg "invalid json text"

let to_string (json:t) =
  let buf = Buffer.create 1024 in
  json_to_dst ~minify:false (`Buffer buf) json;
  Buffer.contents buf

let json_buffer = ref []

let append key json =
  json_buffer := (key,json) :: !json_buffer

let flush oc =
  json_to_dst ~minify:false (`Channel oc)
    (`O (List.rev !json_buffer))

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.
  ---------------------------------------------------------------------------*)

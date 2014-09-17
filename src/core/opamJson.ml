(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2014 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

include Jsonm

(* String conversion *)
exception Escape of ((int * int) * (int * int)) * error
type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]

let json_of_src ?encoding src =
  let dec d = match decode d with
    | `Lexeme l -> l
    | `Error e -> raise (Escape (decoded_range d, e))
    | `End | `Await -> assert false
  in
  let rec value v k d = match v with
    | `Os -> obj [] k d  | `As -> arr [] k d
    | `Null | `Bool _ | `String _ | `Float _ as v -> k v d
    | _ -> assert false
  and arr vs k d = match dec d with
    | `Ae -> k (`A (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with
    | `Oe -> k (`O (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = decoder ?encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d) with
  | Escape (r, e) -> `Error (r, e)

let of_string str: t =
  match json_of_src (`String str) with
  | `JSON j  -> j
  | `Error _ -> failwith "json_of_string"

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

let json_output = ref None

let json_buffer = ref []

let add json =
  json_buffer := json :: !json_buffer

let set_output write =
  json_output := Some write

let verbose () =
  !json_output <> None

let output () =
  match !json_output with
  | None      -> ()
  | Some write ->
    let json = `A (List.rev !json_buffer) in
    write (to_string json)

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)

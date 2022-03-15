(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]

type 'a encoder = 'a -> t
type 'a decoder = t -> 'a option

(* adapted snippets from jsonm maintainers
   https://erratique.ch/repos/jsonm/tree/test/jtree.ml
*)
let of_string ?encoding str =
  let dec d = match Jsonm.decode d with
    | `Lexeme l -> l
    | `Error _ -> raise Exit
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
  let d = Jsonm.decoder ?encoding (`String str) in
  try Some (value (dec d) (fun v _ -> v) d)
  with Exit -> None

let to_buffer ~minify buff json =
  let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
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
  let e = Jsonm.encoder ~minify (`Buffer buff) in
  let finish e = ignore (Jsonm.encode e `End) in
  value json finish e

let to_string ?(minify=false) j =
  let b = Buffer.create 1024 in
  to_buffer ~minify b j;
  Buffer.contents b

(* General json output *)
let json_buffer =
  ref []

let append key json =
  json_buffer := (key,json) :: !json_buffer

let flush oc =
  let b = Buffer.create 1024 in
  let json = (`O (List.rev !json_buffer)) in
  let json = to_buffer ~minify:true b json; Buffer.contents b in
  output_string oc json; flush oc

(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

(* $Id: digest.ml 6044 2003-12-31 14:20:40Z doligez $ *)

(* Message digest (MD5) *)

type t = string

external unsafe_string: string -> int -> int -> t = "caml_md5_string"
external channel: in_channel -> int -> t = "caml_md5_chan"

let string str =
  unsafe_string str 0 (String.length str)

let substring str ofs len =
  if ofs < 0 || len < 0 || ofs > String.length str - len
  then invalid_arg "Digest.substring"
  else unsafe_string str ofs len

let file filename =
  let ic = open_in_bin filename in
  let d = channel ic (-1) in
  close_in ic;
  d

let output chan digest =
  output chan digest 0 16

let input chan =
  let digest = String.create 16 in
  really_input chan digest 0 16;
  digest

(*
let to_hex d =
  let result = String.create 32 in
  for i = 0 to 15 do
    String.blit (Printf.sprintf "%02x" (int_of_char d.[i])) 0 result (2*i) 2;
  done;
  result
;;
*)

(***********************************************************************)
(*                                                                     *)
(*                TypeRex : OCaml Development Tools                    *)
(*                                                                     *)
(*                       OCamlPro S.A.S.                               *)
(*                                                                     *)
(*  Copyright 2011 OCamlPro SAS                                        *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(***********************************************************************)

let _ =
  assert (int_of_char 'a' = 97);
  assert (int_of_char 'A' = 65);
  assert (int_of_char '0' = 48);
  ()

let to_hex_char x =
  if x > 9 then char_of_int (97 - 10 + x) else char_of_int (48 + x)

let of_hex_char c =
  let x = int_of_char c in
  match c with
      'a' .. 'z' -> x - 97 + 10
    | 'A' .. 'Z' -> x - 65 + 10
    | '0' .. '9' -> x - 48
    | _ -> invalid_arg "of_hex_char"

(*let to_hex_old = to_hex *)

let to_hex d =
  let result = String.create 32 in
  let rec iter result d i i2 =
    let c = d.[i] in
    let c = int_of_char c in
    let c1 = c lsr 4 in
    let c2 = c land 15 in
    result.[i2] <- to_hex_char c1;
    result.[i2+1] <- to_hex_char c2;
    if i < 15 then
      iter result d (i+1) (i2+2)
  in
  iter result d 0 0;
(*
  if result <> to_hex_old d then begin
    Printf.eprintf "to_hex new = [%s]\n" result;
    Printf.eprintf "to_hex old = [%s]\n" (to_hex_old d);
    exit 2;
  end;
*)
  result
;;

let of_hex d =
  let result = String.create 16 in
  let rec iter result d i i2 =
    let c1 = d.[i2] in
    let c2 = d.[i2+1] in
    let c1 = of_hex_char c1 in
    let c2 = of_hex_char c2 in
    result.[i] <- char_of_int ((c1 lsl 4) + c2);
    if i < 15 then
      iter result d (i+1) (i2+2)
  in
  iter result d 0 0;
(*
  if to_hex_old result <> d then begin
    Printf.eprintf "d = [%s]\n" d;
    Printf.eprintf "r = [%s]\n" (to_hex_old result);
    exit 2;
  end;
*)
  result
;;


let to_direct_string s = s
let of_direct_string s =
  if String.length s <> 16 then invalid_arg "OcpDigest.of_direct_string";
  s

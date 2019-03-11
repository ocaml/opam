(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open! Crowbar

(* this 'pair' combinator exists in crowbar master but not 0.1 *)
let pair gena genb =
  map [gena; genb] (fun a b -> (a,b))

let nice_int = int8
let nice_uint = uint8
let nice_string =
  let letter = map [range 25] (fun n -> char_of_int (int_of_char 'a' + n)) in
  with_printer Format.pp_print_string @@
  map [letter; letter; letter; letter; letter] @@
  fun a b c d e -> String.of_seq (List.to_seq [a;b;c;d;e])

let eq_of_comp comp v1 v2 = (comp v1 v2 = 0)

let check_json_roundtrip ~name gen equal to_json of_json =
  let pp ppf = function
    | None -> assert false
    | Some x -> Format.fprintf ppf "%s\n%!" (OpamJson.to_string (to_json x))
  in
  let equal x y = match x, y with
    | None, None -> true
    | Some _, None | None, Some _ -> false
    | Some x, Some y -> equal x y in
  Crowbar.add_test ~name [gen] @@ (fun x ->
      Crowbar.check_eq ~pp ~eq:equal (Some x) (of_json (to_json x))
  )

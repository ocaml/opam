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

let nice_int = int8
let nice_uint = uint8
let nice_string =
  let letter = map [range ~min:0 25] (fun n -> char_of_int (int_of_char 'a' + n)) in
  map [letter; letter; letter; letter; letter]
    (fun a b c d e -> String.of_seq (List.to_seq [a;b;c;d;e]))

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

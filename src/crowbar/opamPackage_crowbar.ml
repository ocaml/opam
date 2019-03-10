(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamPackage
open! Crowbar
open OpamCrowbar

let version =
  let main_part =
    map [list1 nice_uint] @@ fun ns ->
    List.map string_of_int ns |> String.concat "."
  in
  let weird_stuff = choose [
      map [nice_uint] (fun n -> "~" ^ string_of_int n);
      map [nice_uint] (fun n -> "+" ^ string_of_int n);
      map [nice_string] (fun s -> "+" ^ s);
    ] in
  let version_string = choose [
      main_part;
      map [main_part; weird_stuff] @@ (^);
      map
        [main_part; weird_stuff; weird_stuff; weird_stuff]
        @@ Printf.sprintf "%s%s%s%s"
    ] in
  map [version_string] Version.of_string

let name =
  let name_string = choose [
      nice_string;
      map [nice_string; nice_uint] (Printf.sprintf "%s%d");
    ] in
  map [name_string] Name.of_string

let package = map [name; version] create
  
let check () =
  check_json_roundtrip ~name:"OpamPackage.t"
    package (eq_of_comp OpamPackage.compare) to_json of_json;

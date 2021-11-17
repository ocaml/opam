(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamFilename
open! Crowbar
open OpamCrowbar

let base : Base.t gen =
  let dirname = map [nice_string] @@ Base.of_string in
  choose [
    dirname;
    map [dirname; nice_string] Base.add_extension;
  ]

let dir : Dir.t gen = 
  let dir_s = choose [
      nice_string;
      (map [nice_string] @@ fun s -> "~" ^ Filename.dir_sep ^ s);
      const "~";
    ] in
  map [dir_s] @@ OpamFilename.Dir.of_string

let filename : t gen =
  map [dir; base] @@ create

let check () =
  check_json_roundtrip ~name:"OpamFilename.Base.t"
    base (eq_of_comp Base.compare) Base.to_json Base.of_json;
  check_json_roundtrip ~name:"OpamFilename.Dir.t"
    dir (eq_of_comp Dir.compare) Dir.to_json Dir.of_json;
  check_json_roundtrip ~name:"OpamFilename.t"
    filename (eq_of_comp OpamFilename.compare) to_json of_json;

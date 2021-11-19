(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamHash
open! Crowbar
open OpamCrowbar

let kind : kind gen = choose [
    const `MD5;
    const `SHA256;
    const `SHA512;
]

let hash = map [kind; bytes] @@ fun kind string -> 
  OpamHash.compute_from_string ~kind string

let check () =
  check_json_roundtrip ~name:"OpamHash.t"
    hash (eq_of_comp OpamHash.compare) OpamHash.to_json OpamHash.of_json;

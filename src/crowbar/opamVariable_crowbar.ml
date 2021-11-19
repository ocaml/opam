(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamVariable
open! Crowbar
open OpamCrowbar

let variable = map [nice_string] @@ of_string

let full = choose [
    map [variable] @@ Full.global;
    map [variable] @@ Full.self;
    map [OpamPackage_crowbar.name; variable] @@ Full.create;
]

let check () =
  check_json_roundtrip ~name:"OpamVariable.t"
    full (eq_of_comp OpamVariable.Full.compare) Full.to_json Full.of_json;

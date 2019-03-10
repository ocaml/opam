(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamVersion
open! Crowbar
open OpamCrowbar

let version = choose [
    const current;
    const (major current);
    const current_nopatch;
    const (full ());
]

let check () =
  let equal v1 v2 = OpamVersion.compare v1 v2 = 0 in
  check_json_roundtrip ~name:"OpamVersion.t"
    version equal OpamVersion.to_json OpamVersion.of_json;

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
open OpamCrowbar

let atomic_action gen = choose [
    (map [gen] @@ fun v -> `Remove v);
    (map [gen] @@ fun v -> `Install v);
]
let highlevel_action gen = choose [
    atomic_action gen;
    (map
       [choose [const `Up; const `Down]; gen; gen]
       @@ fun dir a b -> `Change (dir, a, b));
    (map [gen] @@ fun v -> `Reinstall v);
]
let concrete_action gen = choose [
    atomic_action gen;
    (map [gen] @@ fun v -> `Build v);
]

let action gen = choose [
    atomic_action gen;
    highlevel_action gen;
    concrete_action gen;
]

module Action = OpamCudf.Action

let cudf_action = action OpamCudf_crowbar.package

let check () =
  check_json_roundtrip ~name:"OpamACtionGraph.Make(OpamCudf).t"
    cudf_action (eq_of_comp Action.compare) Action.to_json Action.of_json;

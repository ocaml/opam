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

module ActionGraph = OpamCudf.ActionGraph

let cudf_graph =
  map [list cudf_action; list (pair int int)] @@ fun vertices edge_codes ->
    if vertices = [] then ActionGraph.build [] []
    else begin
      let get_vertex =
        let array = Array.of_list vertices in
        fun i -> array.((abs i) mod Array.length array) in
      let get_edge (i, j) =
        let src = get_vertex i in
        let dst = get_vertex j in
        ActionGraph.E.create src () dst
      in
      let edges = List.map get_edge edge_codes in
      ActionGraph.build vertices edges
    end

let check () =
  check_json_roundtrip ~name:"OpamActionGraph.Make(OpamCudf).t"
    cudf_action (eq_of_comp Action.compare) Action.to_json Action.of_json;
  check_json_roundtrip ~name:"OpamActionGraph.Make(OpamCudf).g"
    cudf_graph (eq_of_comp ActionGraph.compare)
    ActionGraph.to_json ActionGraph.of_json;

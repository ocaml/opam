(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Solver heuristics *)

open OpamTypes

(** Optimized resolution *)
val resolve:
  ?verbose:bool ->
  Cudf.universe ->
  Cudf_types.vpkg request ->
  (Cudf.package action list, Algo.Diagnostic.reason list) result

(** {2 Internal functions} *)

(** These functions can be used independently of OPAM, so we document
    them here. It is not expected than any other file in OPAM use them,
    though. *)

(** A state. *)
type 'a state = 'a list

(** A state space. *)
type 'a state_space = 'a array list

(** [explore is_constent state_space] explore a state space by
    implicitely enumerating all the state in a sensitive order. *)
val brute_force: ?verbose:bool
  -> ('a state -> bool) -> 'a state_space -> 'a state option

(** Explore the given package state-space, in the given universe,
    under the assumption that the packages indeed belong to that
    universe. *)
val explore: ?verbose:bool ->
  Cudf.universe -> Cudf.package state_space -> Cudf.package state option

(** Build a state space from a list of package names. The [filter]
    option helps to reduce the size of the state-space, which is
    useful to deal with user-defined constraints (added on the command
    line for instance). *)
val state_space:
  ?filters:(Cudf_types.pkgname -> Cudf_types.constr) ->
  Cudf.universe -> Cudf_types.pkgname list -> Cudf.package state_space

(** Convert a state into a series of action (withour the full closure
    of reinstallations). Raise [Not_reachable] is the state is not
    reachable. *)
val actions_of_state: Cudf.universe -> Cudf.package state -> Cudf.package action list

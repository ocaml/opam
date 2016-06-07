(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** The "internal solver", brute-force search using Dose's checker *)

(** {2 Solver heuristics.} *)

(** This module tries to turn an efficient solution checker (such as
    the one provided by the dose3 library, writen by J. Vouillon) into
    a relatively good solution finder.

    The method we are using is the following:

    - We ultimately rely on a brute-force exploration loop, where we
      iterate over the state-space implicitely, using a monotonous
      successor function which encodes the optimization criteria we
      are interested in;

    - As brute-force exploration is costly, the goal is to provide the
      exploration function a state-space as small as possible. To do
      so, we use different kind of constraints that we deduce from the
      request;

    - We remove from the state-space every packages and versions that
      are not needed: we are only considering (i) the installed root
      packages (with no specific version constraint); (ii) the new
      packages that the user might have asking to install or upgrade
      (with some eventual version constraints); and (iii) the
      transitive closure of (i) and (ii) (with the corresponding
      version constraints);

    Finally, we run all this in a loop, until we reach a fix point. We
    use a timeout to interrupt too long explorations.
*)

open OpamTypes

(** {2 High-level API} *)

(** Optimized resolution *)
val resolve:
  ?verbose:bool ->
  version_map:int OpamPackage.Map.t ->
  (Cudf.universe -> Cudf.universe) ->
  Cudf.universe ->
  Cudf_types.vpkg request ->
  (Cudf.package atomic_action list, OpamCudf.conflict) result

(** {2 Internal API} *)

(** These functions can be used independently of OPAM, so we document
    them here. It is not expected than any other file in OPAM use them,
    though. *)

(** A state. In our case, it is a list package we would like to see
    installed. *)
type 'a state = 'a list

(** A state space. In our case, it is a collection of available
    packages: each cell contains all the versions available for one
    package, ordered by version. *)
type 'a state_space = 'a array list

(** {4 Integer space} *)

(** The hearth of the brute-force algorithm lies here. Wwe want to
    iterate on the state-space (which can be hudge) and stop the
    first time we hit a consistant state. This means two things:
    (i) we don't want to build the full universe before iterating
    on it; (ii) we need to enumerate the states in a meaningful
    order, eg. an order which should reflect the optimization
    criteria we are intersted in. *)

(** To overcome this difficulties, we use a monotonous successor
    function to compute the next state to test from a given valid
    non-consistent state, see [succ] for more details. *)

(** [zero n] returns the tuple with [n] zeros, which is the first
    state to explore. *)
val zero: int -> int state

(** Given a list of bounds and a tuple, return the next tuple which
    satisfies the bounds (each component will be stricly lesser than
    the bounds). The enumeration respect the following invariant:

    - it is complete, eg. all the state are enumerated until [None] is
      returned.

    - it it monotonous: the sum of components always increase, eg.
      [|succ x| >= |x|], where [|None|] is [max_int], [|Some x| = |x|]
      and [|(x_1,...,x_n) = x_1 + ... + x_n|].

    That enumeration encodes the heuristic we are trying to implement,
    which is: we first try to install the 'ideal' state, where all
    packages are installed with their most recent versions; if this
    does not work, we try to minimize the distance between the ideal
    state and the solution we are proposing. *)
val succ: bounds:int list -> int state -> int state option

(** {4 Polymorphic space} *)

(** [explore is_constent state_space] explore a state space by
    implicitely enumerating all the state in a sensitive order. *)
val brute_force: ?verbose:bool -> dump:('a state -> unit) ->
  ('a state -> bool) -> 'a state_space -> 'a state option

(** {4 Package space} *)

(** Build a state space from a list of package names. The [filter]
    option helps to reduce the size of the state-space, which is
    useful to deal with both user-defined constraints (added on the
    command line for instance) and refined requests (see below). *)
val state_space:
  ?filters:(Cudf_types.pkgname -> Cudf_types.constr) ->
  Cudf.universe -> Cudf_types.vpkglist -> Cudf_types.pkgname list ->
  Cudf.package state_space

(** Explore the given package state-space using the [brute_force] strategy.
    We assume that all the packages belong to the given universe. *)
val explore: ?verbose:bool ->
  Cudf.universe -> Cudf.package state_space -> Cudf.package state option

(** Find a possible good state which satisfies a request. The idea is
    call iteratively this function while refining the constraints in
    the request until reaching a fix-point. This function tries to
    minimize the state to explore, based on the request constraints:
    the more constrained request you have, the smaller the state-space
    to explore is. Once the state-space is computed using
    [state_space], it calls [explore] (which will use [brute_force])
    to get an approximate solution to the request. *)
val state_of_request: ?verbose:bool  ->
  version_map:int OpamPackage.Map.t ->
  Cudf.universe ->
  Cudf_types.vpkg request -> Cudf.package state option

(** Convert a state into a series of action (withour the full closure
    of reinstallations). Raise [Not_reachable] is the state is not
    reachable. This function is called once we get a consistent state
    to build a solution than we can propose to the user. *)
val actions_of_state:
  version_map:int OpamPackage.Map.t ->
  Cudf.universe ->
  Cudf_types.vpkg request -> Cudf.package state -> Cudf.package atomic_action list

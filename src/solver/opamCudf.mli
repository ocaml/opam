(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Solver interaction through Cudf, conversion of solver answer to an opam
    solution *)

open OpamTypes

module Package : sig
  type t = Cudf.package
end

(** Cudf sets *)
module Set: OpamStd.SET with type elt = Package.t

(** Cudf maps *)
module Map: OpamStd.MAP with type key = Package.t

(** Cudf graph *)
module Graph: sig
  (** Graph of cudf packages *)

  include module type of Dose_algo.Defaultgraphs.PackageGraph.G

  (** Build a graph from a CUDF universe. Warning: dependency edges are towards
      the dependency, which is the reverse of what happens in the action
      graph. *)
  val of_universe: Cudf.universe -> t

  (** Return the transitive closure of [g] *)
  val transitive_closure: t -> t

  (** Reverse the direction of all edges *)
  val mirror: t -> t
end


(** Cudf action graph *)
module Action: OpamActionGraph.ACTION with type package = Package.t
module ActionGraph: OpamActionGraph.SIG with type package = Package.t

(** Abstract type that may be returned in case of conflicts *)
type conflict

(** Sorts the given packages topolgically (be careful if there are cycles, e.g.
   if the universe was loaded with [post] dependencies enabled) *)
val dependency_sort: Cudf.universe -> Set.t -> Cudf.package list

(** Pre-process a universe to remove incompatible/unneeded packages and ease the
    task of the solvers *)
val trim_universe: Cudf.universe -> Set.t -> Cudf.universe

exception Cyclic_actions of Cudf.package action list list

(** Computes the actions to process from a solution, from the actions
    obtained by a simple universe diff. The 'simple' universe
    should not contain build dependencies and will be used for resolution ;
    [complete_universe] should include build-deps, it's used to get the
    dependency ordering of actions.

    Returns a graph of atomic actions, i.e. only removals and installs. Use
    [reduce_actions] to reduce it to a graph including reinstall and
    up/down-grade actions.

    @raise Cyclic_actions *)
val atomic_actions:
  simple_universe:Cudf.universe ->
  complete_universe:Cudf.universe ->
  [< Cudf.package highlevel_action ] list ->
  ActionGraph.t

(** Removes from a graph of actions the disjoint subgraphs that don't concern
   requested packages. The provided universe should *include*
   [post]-dependencies so that they don't get trimmed away.
   Note: if the specified [requested] set is empty, all actions are supposed to
   be meaningful. *)
val trim_actions: Cudf.universe -> OpamPackage.Name.Set.t -> ActionGraph.t -> unit

(** Heuristic to compute the likely cause of all actions in a graph from the set
    of packages passed in the original request. Assumes a reduced graph. Takes
    the set of requested package names, the set of packages marked for
    reinstall, and the set of all available packages. *)
val compute_root_causes:
  ActionGraph.t ->
  OpamPackage.Name.Set.t ->
  OpamPackage.Set.t ->
  OpamPackage.Set.t ->
  Cudf.package cause Map.t

exception Solver_failure of string

(** Resolve a CUDF request. The result is either a conflict holding
    an explanation of the error, or a resulting universe.
    [~extern] specifies whether the external solver should be used *)
val resolve:
  extern:bool ->
  version_map:int OpamPackage.Map.t ->
  Cudf.universe ->
  Cudf_types.vpkg request ->
  (Cudf.universe, conflict) solver_result

(** Computes a list of actions to proceed from the result of {!resolve}.
    Note however than the action list is not yet complete: the transitive closure
    of reinstallations is not yet completed, as it requires to fold over the
    dependency graph in considering the optional dependencies. *)
val to_actions:
  Cudf.universe ->
  (Cudf.universe, conflict) solver_result ->
  (Cudf.package atomic_action list, conflict) solver_result

(** Cudf labels for package fields in the cudf format
    (use for the field Cudf.pkg_extra and with Cudf.lookup_package_property) *)

(** the original OPAM package name (as string) *)
val s_source: string

(** the original OPAM package version (as string) *)
val s_source_number: string

(** a package to be reinstalled (a bool) *)
val s_reinstall: string

(** true if this package belongs to the roots ("installed manually")
    packages *)
val s_installed_root: string

(** true if the package is pinned to this version *)
val s_pinned: string

(** the number of versions of the package since this one, cubed *)
val s_version_lag: string

(** valid cudf name for the dummy package used for enforcing opam's switch
    invariants *)

(** valid cudf name and version for the dummy package used for enforcing opam's
    switch invariants *)
val opam_invariant_package: string * int

val opam_deprequest_package: string * int

val is_opam_invariant: Cudf.package -> bool

(** dummy package that shouldn't exist and encodes unavailability (by depending on it) *)
val unavailable_package_name: string

(** {2 Pretty-printing} *)

val make_conflicts:
  version_map:int package_map -> Cudf.universe ->
  Dose_algo.Diagnostic.diagnosis -> ('a, conflict) solver_result
val cycle_conflict:
  version_map:int package_map -> Cudf.universe ->
  Cudf.package action list list -> ('a, conflict) solver_result

type explanation =
  [ `Conflict of string option * string list * bool
  | `Missing of string option * string * OpamFormula.t
  ]

(** Convert a conflict to something readable by the user. The second argument
    should return a string explaining the unavailability, or the empty string,
    when called on an unavailable package (the reason can't be known this deep
    in the solver) *)
val string_of_conflicts:
  package_set -> (name * OpamFormula.version_formula -> string) -> conflict ->
  string

val string_of_explanations:
  (name * OpamFormula.version_formula -> string) ->
  explanation list * Action.t list list ->
  string

(** Returns two lists:
    - the reasons why the request can't be satisfied with conflict explanations
    - the cycles in the actions to process (exclusive with the first) *)
val conflict_explanations:
  package_set -> (name * OpamFormula.version_formula -> string) -> conflict ->
  (string * string list * string list) list * string list

val conflict_explanations_raw:
  package_set -> conflict -> explanation list * Action.t list list

(** Properly concat a single conflict as returned by {!conflict_explanations}
    for display *)
val string_of_conflict:
  ?start_column:int -> string * string list * string list -> string

(** Dumps the given cudf universe to the given channel *)
val dump_universe: out_channel -> Cudf.universe -> unit

(** Convert a cudf package back to an OPAM package *)
val cudf2opam: Cudf.package -> package

(** Like {!OpamTypesBase.action_contents} but return the single package of
    remove, install, reinstal, and change action *)
val action_contents: 'a action -> 'a

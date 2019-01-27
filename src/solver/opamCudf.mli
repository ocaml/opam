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

(** Solver interaction through Cudf, conversion of solver answer to an opam
    solution *)

open OpamTypes

(** Cudf sets *)
module Set: OpamStd.SET with type elt = Cudf.package

(** Cudf maps *)
module Map: OpamStd.MAP with type key = Cudf.package

(** Cudf graph *)
module Graph: sig
  (** Graph of cudf packages *)

  include module type of Algo.Defaultgraphs.PackageGraph.G

  (** Build a graph from a CUDF universe. Warning: dependency edges are towards
      the dependency, which is the reverse of what happens in the action
      graph. *)
  val of_universe: Cudf.universe -> t

  (** Return the transitive closure of [g] *)
  val transitive_closure: t -> t

  (** Return the transitive closure of dependencies of [set],
      sorted in topological order. *)
  val close_and_linearize: t -> Set.t -> Cudf.package list

  (** Reverse the direction of all edges *)
  val mirror: t -> t
end

(** Computation of differences between universe. Returns the sets of packages to
    install and remove respectively. *)
val diff: Cudf.universe -> Cudf.universe -> (Set.t * Set.t)

(** Cudf action graph *)
module ActionGraph: OpamActionGraph.SIG with type package = Cudf.package

(** Abstract type that may be returned in case of conflicts *)
type conflict

(** Return the transitive closure of dependencies of [set],
    sorted in topological order *)
val dependencies: Cudf.universe -> Cudf.package list -> Cudf.package list

(** Return the transitive closure of dependencies of [set],
    sorted in topological order *)
val reverse_dependencies: Cudf.universe -> Cudf.package list -> Cudf.package list

(** Check if a request is satisfiable and return the reasons why not unless
    [explain] is set to [false] *)
val check_request:
  ?explain:bool ->
  version_map:int OpamPackage.Map.t ->
  Cudf.universe ->
  Cudf_types.vpkg request ->
  (Cudf.universe, conflict) result

(** Compute the final universe state using the external solver. *)
val get_final_universe:
  version_map:int OpamPackage.Map.t ->
  Cudf.universe ->
  Cudf_types.vpkg request ->
  (Cudf.universe, conflict) result

(** Compute the list of actions to match the difference between two
    universe. Remark: the result order is unspecified, ie. need to use
    [atomic_actions] to get a solution which respects the
    topological order induced by dependencies. *)
val actions_of_diff:
  (Set.t * Set.t) -> Cudf.package atomic_action list

exception Cyclic_actions of Cudf.package action list list

(** Computes the actions to process from a solution, from the actions
    obtained by a simple universe diff. The 'simple' universe
    should not contain build dependencies and will be used for resolution ;
    [complete_universe] should include build-deps, it's used to get the
    dependency ordering of actions.

    Returns a graph of atomic actions, i.e. only removals and installs. Use
    [reduce_actions] to reduce it to a graph including reinstall and
    up/down-grade actions.

    May raise [Cyclic_actions]. *)
val atomic_actions:
  simple_universe:Cudf.universe ->
  complete_universe:Cudf.universe ->
  [< Cudf.package highlevel_action ] list ->
  ActionGraph.t

(** Heuristic to compute the likely cause of all actions in a graph from the set
    of packages passed in the original request. Assumes a reduced graph. Takes
    the set of requested package names, and the set of packages marked for
    reinstall. *)
val compute_root_causes:
  ActionGraph.t -> OpamPackage.Name.Set.t -> OpamPackage.Set.t ->
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
  (Cudf.universe, conflict) result

(** Computes a list of actions to proceed from the result of [resolve].
    Note however than the action list is not yet complete: the transitive closure
    of reinstallations is not yet completed, as it requires to fold over the
    dependency graph in considering the optional dependencies.
    The first argument specifies a function that will be applied to the starting
    universe before computation: useful to re-add orphan packages. *)
val to_actions:
  (Cudf.universe -> Cudf.universe) ->
  Cudf.universe ->
  (Cudf.universe, conflict) result ->
  (Cudf.package atomic_action list, conflict) result

(** [remove universe name constr] Remove all the packages called
    [name] satisfying the constraints [constr] in the universe
    [universe]. *)
val remove: Cudf.universe -> Cudf_types.pkgname -> Cudf_types.constr -> Cudf.universe

(** Uninstall all the package in the universe. *)
val uninstall_all: Cudf.universe -> Cudf.universe

(** Install a package in the universe. We don't care about any
    invariant here (eg. the resulting universe can have multiple
    versions of the same package installed). *)
val install: Cudf.universe -> Cudf.package -> Cudf.universe

(** Remove all the versions of a given package, but the one given as argument. *)
val remove_all_uninstalled_versions_but: Cudf.universe ->
  string -> Cudf_types.constr -> Cudf.universe

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

(** {2 Pretty-printing} *)

(** Convert a package constraint to something readable. *)
val string_of_vpkgs: Cudf_types.vpkg list -> string

val make_conflicts:
  version_map:int package_map -> Cudf.universe ->
  Algo.Diagnostic.diagnosis -> ('a, conflict) result
val cycle_conflict:
  version_map:int package_map -> Cudf.universe ->
  string list list -> ('a, conflict) result

(** Convert a conflict to something readable by the user. The second argument
    should return a string explaining the unavailability, or the empty string,
    when called on an unavailable package (the reason can't be known this deep
    in the solver) *)
val string_of_conflict:
  package_set -> (name * OpamFormula.version_formula -> string) -> conflict ->
  string

(** Returns three lists of strings:
    - the final reasons why the request can't be satisfied
    - the dependency chains explaining it
    - the cycles in the actions to process (exclusive with the other two) *)
val strings_of_conflict:
  package_set -> (name * OpamFormula.version_formula -> string) -> conflict ->
  string list * string list * string list

val conflict_chains:
  package_set -> conflict -> (name * OpamFormula.version_formula) list list

(** Dumps the given cudf universe to the given channel *)
val dump_universe: out_channel -> Cudf.universe -> unit

(** Pretty-print atoms *)
val string_of_atom: Cudf_types.vpkg -> string

(** Pretty-print requests *)
val string_of_request: Cudf_types.vpkg request -> string

(** Pretty-print the universe *)
val string_of_universe: Cudf.universe -> string

(** Pretty-print of packages *)
val string_of_packages: Cudf.package list -> string

(** Convert a cudf package back to an OPAM package *)
val cudf2opam: Cudf.package -> package

(** Returns the list of packages in a Cudf universe *)
val packages: Cudf.universe -> Cudf.package list

(** Converts an OPAM request to a Cudf request *)
val to_cudf: Cudf.universe -> Cudf_types.vpkg request
  -> Cudf.preamble * Cudf.universe * Cudf.request

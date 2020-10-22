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

(** Entry point to the solver, conversion of opam package universes to Cudf,
    dependencies computation. Front-end to Dose. *)

open OpamTypes

module Action : OpamActionGraph.ACTION with type package = package
module ActionGraph : OpamActionGraph.SIG with type package = package

type solution

val empty_universe: universe

(** {2 Solver} *)

(** Convert a request to a string *)
val string_of_request: atom request -> string

(** Compute statistics about a solution *)
val stats: solution -> stats

(** Return the new packages in the solution *)
val new_packages: solution -> package_set

(** Return all packages appearing in the solution *)
val all_packages: solution -> package_set

(** Pretty-printing of statistics *)
val string_of_stats: stats -> string

(** Is the solution empty? *)
val solution_is_empty: solution -> bool

(** Display a solution *)
val print_solution:
  messages:(package -> string list) ->
  append:(package -> string) ->
  requested:name_set -> reinstall:package_set ->
  solution -> unit

(** Serialize a solution *)
val solution_to_json : solution OpamJson.encoder
val solution_of_json : solution OpamJson.decoder

(** Computes an opam->cudf version map from a set of package *)
val cudf_versions_map: universe -> package_set -> int OpamPackage.Map.t

(** Creates a CUDF universe from an OPAM universe, including the given packages.
    Evaluation of the first 4 arguments is staged. Warning: when [depopts] is
    [true], the optional dependencies may become strong dependencies.

    Use [add_invariant] if you expect to call the solver and need the switch
    invariants to be respected; remember in that case to call
    [Cudf.remove_package universe OpamCudf.opam_invariant_package]
    before exporting the results *)
val load_cudf_universe:
  universe -> ?version_map:int package_map -> ?add_invariant:bool ->
  package_set ->
  ?depopts:bool -> build:bool -> post:bool -> unit ->
  Cudf.universe

(**  Build a request *)
val request:
  ?criteria:solver_criteria ->
  ?install:atom list ->
  ?upgrade:atom list ->
  ?remove:atom list ->
  unit -> atom request

(** Given a description of packages, return a solution preserving the
    consistency of the initial description. *)
val resolve :
  universe -> orphans:package_set -> atom request
  -> (solution, OpamCudf.conflict) result

(** Returns the graph of atomic actions (rm, inst) from a solution *)
val get_atomic_action_graph : solution -> ActionGraph.t

(** Keep only the packages that are installable. *)
val installable: universe -> package_set

(** Like [installable], but within a subset and potentially much faster *)
val installable_subset: universe -> package_set -> package_set

(** Return the transitive dependency closures
    of a collection of packages.*)
val dependencies :
  depopts:bool -> build:bool -> post:bool ->
  installed:bool ->
  ?unavailable:bool ->
  universe ->
  package_set ->
  package_set

(** Same as [dependencies] but for reverse dependencies *)
val reverse_dependencies :
  depopts:bool -> build:bool -> post:bool ->
  installed:bool ->
  ?unavailable:bool ->
  universe ->
  package_set ->
  package_set

(** Sorts the given package set in topological order (as much as possible,
    beware of cycles in particular if [post] is [true]) *)
val dependency_sort :
  depopts:bool -> build:bool -> post:bool ->
  universe ->
  package_set ->
  package list

module PkgGraph: Graph.Sig.I
  with type V.t = OpamPackage.t
val dependency_graph :
  depopts:bool -> build:bool -> post:bool ->
  installed:bool ->
  ?unavailable:bool ->
  universe -> PkgGraph.t

(** Check the current set of installed packages in a universe for
    inconsistencies *)
val check_for_conflicts : universe -> OpamCudf.conflict option

(** Checks the given package set for complete installability ; returns None if
    they can all be installed together *)
val coinstallability_check : universe -> package_set -> OpamCudf.conflict option

(** Checks if the given atoms can be honored at the same time in the given
    universe *)
val atom_coinstallability_check : universe -> atom list -> bool

(** [coinstallable_subset univ set packages] returns the subset of [packages]
    which are individually co-installable with [set], i.e. that can be installed
    if [set] while [set] remains installed. This returns the empty set if [set]
    is already not coinstallable. *)
val coinstallable_subset : universe -> package_set -> package_set -> package_set

(** Dumps a cudf file containing all available packages in the given universe,
    plus version bindings (as '#v2v' comments) for the other ones. *)
val dump_universe: universe -> out_channel -> unit

(** Filters actions in a solution. Dependents of a removed actions are removed
    to keep consistency *)
val filter_solution: (package -> bool) -> solution -> solution

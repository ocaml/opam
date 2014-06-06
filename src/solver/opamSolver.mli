(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(** SAT-solver for package dependencies and conflicts *)

open OpamTypes

module Action : OpamActionGraph.ACTION with type package = package
module ActionGraph : OpamParallel.GRAPH with type V.t = package action

type solution = (OpamPackage.t, ActionGraph.t) gen_solution

val empty_universe: universe

(** {2 Solver} *)

(** Convert a request to a string *)
val string_of_request: atom request -> string

(** Compute statistics about a solution *)
val stats: solution -> stats

(** Return the new packages in the solution *)
val new_packages: solution -> package_set

(** Pretty-printing of statistics *)
val string_of_stats: stats -> string

(** Is the solution empty ? *)
val solution_is_empty: solution -> bool

(** Does the solution implies deleting or updating a package *)
val delete_or_update : solution -> bool

(** Display a solution *)
val print_solution:
  messages:(package -> string list) ->
  rewrite:(package -> package) ->
  solution -> unit

(** Computes an opam->cudf version map from a set of package *)
val cudf_versions_map: universe -> package_set -> int OpamPackage.Map.t

(** Creates a CUDF universe from an OPAM universe, including the given
    packages *)
val load_cudf_universe: ?depopts:bool -> universe -> ?version_map:int package_map
  -> package_set -> Cudf.universe

(** Given a description of packages, return a solution preserving the
    consistency of the initial description. *)
val resolve :
  ?verbose:bool ->
  universe -> requested:OpamPackage.Name.Set.t -> atom request
  -> (solution, ((atom -> string) -> string)) result

(** Keep only the packages that are installable. *)
val installable: universe -> package_set

(** Return the topological sort of the transitive dependency closures
    of a collection of packages.*)
val dependencies :
  depopts:bool ->
  installed:bool ->
  universe ->
  package_set ->
  package list

(** Same as [dependencies] but for reverse dependencies *)
val reverse_dependencies :
  depopts:bool ->
  installed:bool ->
  universe ->
  package_set ->
  package list

(** Create a sequential solution from a list of actions *)
val sequential_solution: universe -> requested:name_set ->
  package action list -> (solution, (atom -> string) -> string) result

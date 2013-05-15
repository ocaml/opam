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

(** Cudf interface *)

open OpamTypes

(** Cudf sets *)
module Set: OpamMisc.SET with type elt = Cudf.package

(** Cudf maps *)
module Map: OpamMisc.MAP with type key = Cudf.package

(** Cudf graph *)
module Graph: sig
  (** Graph of cudf packages *)
  type t

  (** Build a graph from a CUDF universe *)
  val of_universe: Cudf.universe -> t

  (** Return the transitive closure of [g] *)
  val transitive_closure: t -> t

  (** Return the transitive closure of dependencies of [set],
      sorted in topological order. *)
  val close_and_linearize: t -> Set.t -> Cudf.package list
end

(** Difference between universes *)
module Diff: sig

  (** Differences between the versions of a given package *)
  type package = {
    installed  : Set.t;
    removed    : Set.t;
    reinstalled: Set.t;
  }

  (** Difference between universe *)
  type universe = (Cudf_types.pkgname, package) Hashtbl.t

  (** Computation of differences between universe *)
  val diff: Cudf.universe -> Cudf.universe -> universe

end

(** Cudf action graph *)
module ActionGraph: ACTION_GRAPH with type package = Cudf.package

(** Return the transitive closure of dependencies of [set],
    sorted in topological order *)
val dependencies: Cudf.universe -> Cudf.package list -> Cudf.package list

(** Return the transitive closure of dependencies of [set],
    sorted in topological order *)
val reverse_dependencies: Cudf.universe -> Cudf.package list -> Cudf.package list

(** Compute the final universe state. *)
val get_final_universe:
  Cudf.universe ->
  Cudf_types.vpkg request ->
  (Cudf.universe, Algo.Diagnostic.reason list) result

(** Compute the list of actions to match the difference between two
    universe. Remark: the result order is unspecified, ie. need to use
    [solution_of_actions] to get a solution which respects the
    topological order induced by dependencies. *)
val actions_of_diff: Diff.universe -> Cudf.package action list

(** Compution the actions to process from a solution *)
val solution_of_actions:
  simple_universe:Cudf.universe ->
  complete_universe:Cudf.universe ->
  Cudf.package action list ->
  ActionGraph.solution

(** Resolve a CUDF request. The result is either a conflict explaining
    the error, or a list of action to proceed. Note however than the
    action list is not yet complete: the transitive closure of
    reinstallations is not yet completed, as it requires to fold over
    the dependency graph in considering the optional dependencies --
    which is something that dose/cudf obviously does not handle.  *)
val resolve:
  Cudf.universe ->
  Cudf_types.vpkg request ->
  (Cudf.package action list, Algo.Diagnostic.reason list) result

(** Remove all the packages having a given universe *)
val remove: Cudf.universe -> Cudf_types.pkgname -> Cudf.universe

(** Uninstall all the package in the universe. *)
val uninstall_all: Cudf.universe -> Cudf.universe

(** Install a package in the universe. We don't care about any
    invariant here (eg. the resulting universe can have mutliple
    versions of the same package installed). *)
val install: Cudf.universe -> Cudf.package -> Cudf.universe

(** Remove all the versions of a given package, but the one given as argument. *)
val remove_all_uninstalled_versions_but:
  string -> Cudf_types.constr -> Cudf.universe -> Cudf.universe

(** The "reinstall" string *)
val s_reinstall: string

(** The "installed-root" string *)
val s_installed_root: string

(** {2 Pretty-printing} *)

(** Convert a package constraint to something readable. *)
val string_of_vpkgs: Cudf_types.vpkg list -> string

(** Convert a reason to something readable by the user *)
val string_of_reason: (Cudf.package -> package) -> Algo.Diagnostic.reason -> string option

(** Convert a list of reasons to something readable by the user *)
val string_of_reasons: (Cudf.package -> package) -> Algo.Diagnostic.reason list -> string

(** Pretty-print atoms *)
val string_of_atom: Cudf_types.vpkg -> string

(** Pretty-print requests *)
val string_of_request: Cudf_types.vpkg request -> string

(** Pretty-print the universe *)
val string_of_universe: Cudf.universe -> string

(** Pretty-print of packages *)
val string_of_packages: Cudf.package list -> string

(** {2 External solver} *)
val external_solver_available: unit -> bool

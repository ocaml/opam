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

(** Given a description of packages, return a solution preserving the
    consistency of the initial description.  An empty [list] : No solution
    found. The last argument is the set of installed packages.

    Every element in the solution [list] satisfies the problem given.
    For the ordering, the first element in the list
    is obtained by upgrading from its next element. *)
val resolve :
  ?verbose:bool ->
  universe -> atom request -> (solution, string) result

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

(** Same as [bdependencies] but for reverse dependencies *)
val reverse_dependencies :
  depopts:bool ->
  installed:bool ->
  universe ->
  package_set ->
  package list

(** Create a sequential solution from a list of actions *)
val sequential_solution: package action list -> solution

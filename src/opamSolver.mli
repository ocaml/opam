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

(** SAT-solver for package dependencies and conflicts. *)

open OpamTypes

(** {2 Package actions} *)

(** Pretty-printing of actions *)
val string_of_action: action -> string

(** Package with associated build action *)
type package_action

(** Return the corresponding build action *)
val action: package_action -> action

(** Package action graph *)
module PA_graph : sig
  include Graph.Sig.I with type V.t = package_action
  module Parallel: OpamParallel.SIG
    with type G.t = t
     and type G.V.t = V.t
end

(** {2 Solver} *)

(** Solver solution *)
type solution = PA_graph.t OpamTypes.solution

(** Convert a request to a string *)
val string_of_request: request -> string

(** Compute statistics about a solution *)
val stats: solution -> stats

(** Pretty-printing of statistics *)
val string_of_stats: stats -> string

(** Is the solution empty ? *)
val solution_is_empty: solution -> bool

(** Does the solution implies deleting or updating a package *)
val delete_or_update : solution -> bool

(** Display a solution *)
val print_solution: solution -> unit

(** Package *)
type package = Debian.Packages.package

(** Universe of packages *)
type universe = U of package list

(** Subset of packages *)
type packages = P of package list

(** Given a description of packages, return a solution preserving the
    consistency of the initial description.  An empty [list] : No solution
    found. The last argument is the set of installed packages.

    Every element in the solution [list] satisfies the problem given.
    For the ordering, the first element in the list
    is obtained by upgrading from its next element. *)
val resolve : universe -> request -> package_set -> (solution, string) result

(** Return the recursive dependencies of a package. Note : the given
    package exists in the list in input because this list describes
    the entire universe.  By convention, it also appears in output.
    If [depopts] (= [false] by default) is set to [true],
    optional dependencies are added to the dependency relation.
    The packages are return in topological order. *)
val get_backward_dependencies : ?depopts:bool -> universe -> packages -> package list

(** Same as [get_backward_dependencies] but for forward
    dependencies *)
val get_forward_dependencies : ?depopts:bool -> universe -> packages -> package list

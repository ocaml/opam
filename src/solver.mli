(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** SAT-solver for package dependencies and conflicts. *)

open Types

(** {2 Package actions} *)

(** Build action *)
type action = 

  (** The package must be installed. The package could have been
      present or not, but if present, it is another version than the
      proposed solution. *)
  | To_change of NV.t option * NV.t

  (** The package must be deleted. *)
  | To_delete of NV.t

  (** The package is already installed, but it must be recompiled. *)
  | To_recompile of NV.t

(** Package with associated build action *)
type package_action

(** Return the corresponding build action *)
val action: package_action -> action

(** Package action graph *)
module PA_graph : sig
  include Graph.Sig.I with type V.t = package_action
  module Parallel: Parallel.SIG
    with type G.t = t
     and type G.V.t = V.t
end

(** {2 Solver} *)

(** Solver request *)
type request = {
  wish_install:  Debian.Format822.vpkg list;
  wish_remove :  Debian.Format822.vpkg list;
  wish_upgrade:  Debian.Format822.vpkg list;
}

(** Convert a request to a string *)
val string_of_request: request -> string

(** Solver solution *)
type solution = {
  to_remove: NV.t list;
  to_add   : PA_graph.t;
}

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
    consistency of the initial description.  [None] : No solution
    found. *)
val resolve : universe -> request -> solution option

(** Return the recursive dependencies of a package Note : the given
    package exists in the list in input because this list describes
    the entire universe.  By convention, it also appears in output. *)
val filter_backward_dependencies : universe -> packages -> package list

(** Same as [filter_backward_dependencies] but for forward
    dependencies *)
val filter_forward_dependencies : universe -> packages -> package list

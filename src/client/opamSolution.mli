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

(** Applying solver solutions *)

open OpamTypes

(** Resolve an user request *)
val resolve:
  ?verbose:bool ->
  OpamState.state ->
  user_action ->
  requested:OpamPackage.Name.Set.t ->
  atom request ->
  (solution, string) result

(** Apply a solution returned by the solver. *)
val apply:
  ?force:bool ->
  OpamState.state ->
  user_action ->
  requested:OpamPackage.Name.Set.t ->
  solution ->
  solver_result

(** Call the solver to get a solution and then call [apply]. *)
val resolve_and_apply:
  ?force:bool ->
  OpamState.state ->
  user_action ->
  requested:OpamPackage.Name.Set.t ->
  atom request ->
  solver_result

(** Raise an error if no solution is found or in case of error. *)
val check_solution: OpamState.state -> solver_result -> unit

(** {2 Atoms} *)

(** Return an atom with a string version constraint *)
val eq_atom: name -> version -> atom

(** Return a simple atom, with no version constrain, from a package*)
val atom_of_package: package -> atom

(** Return a list of simple atoms (ie. with no version constraints)
    from a set of packages *)
val atoms_of_packages: package_set -> atom list

(** Return a list of constrained atoms from a set of packages *)
val eq_atoms_of_packages: package_set -> atom list

(** Return a list of atoms from a list of names (wich can eventually
    be of the form name.version). Unless [permissive] is set, will abort in
    case the package is not available *)
val atoms_of_names: ?permissive: bool -> OpamState.state -> name_set -> atom list

val check_availability: OpamState.state -> atom list -> unit

(** {2 Stats} *)
val sum: stats -> int

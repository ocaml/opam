(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
open OpamStateTypes

(** Resolve an user request *)
val resolve:
  ?verbose:bool ->
  'a switch_state ->
  user_action ->
  orphans:package_set ->
  atom request ->
  (OpamSolver.solution, OpamCudf.conflict) result

(** Apply a solution returned by the solver. If [ask] is not
    specified, prompts the user whenever the solution isn't
    obvious from the request *)
val apply:
  ?ask:bool ->
  [< rw ] switch_state ->
  user_action ->
  requested:OpamPackage.Name.Set.t ->
  OpamSolver.solution ->
  solver_result

(** Call the solver to get a solution and then call [apply]. If [ask] is not
    specified, prompts the user whenever the solution isn't obvious from the
    request *)
val resolve_and_apply:
  ?ask:bool ->
  [< rw ] switch_state ->
  user_action ->
  requested:OpamPackage.Name.Set.t ->
  orphans:package_set ->
  atom request ->
  solver_result

(** Raise an error if no solution is found or in case of error. *)
val check_solution: 'a switch_state -> solver_result -> unit

(** {2 Atoms} *)

(** Return an atom with a strict version constraint *)
val eq_atom: name -> version -> atom

(** Return a simple atom, with no version constrain, from a package*)
val atom_of_package: package -> atom

(** Returns an atom with a strict version constraint from a package *)
val eq_atom_of_package: package -> atom

(** Return a list of simple atoms (ie. with no version constraints)
    from a set of packages *)
val atoms_of_packages: package_set -> atom list

(** Return a list of constrained atoms from a set of packages *)
val eq_atoms_of_packages: package_set -> atom list

(** Checks that the atoms can possibly be verified (individually) in a package
    set. Displays an error and exits otherwise. [permissive] just changes the
    error message. *)
val check_availability: ?permissive: bool ->
  'a switch_state -> OpamPackage.Set.t -> atom list -> unit

(** Takes a "raw" list of atoms (from the user), and match it to existing
    packages. Match packages with the wrong capitalisation, and raises errors on
    non-existing packages, and unavailable ones unless [permissive] is set.
    Exits with a message on error. *)
val sanitize_atom_list: ?permissive: bool -> 'a switch_state -> atom list -> atom list

(** {2 Stats} *)
val sum: stats -> int

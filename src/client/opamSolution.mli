(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Interface with the solver, processing of full solutions through actions *)

open OpamTypes
open OpamStateTypes

(** Resolve an user request *)
val resolve:
  'a switch_state ->
  user_action ->
  ?reinstall:package_set ->
  requested:package_set ->
  atom request ->
  (OpamSolver.solution, OpamCudf.conflict) result

(** Apply a solution returned by the solver. If [ask] is not specified, prompts
    the user whenever the solution isn't obvious from the request. [add_roots]
    defaults to the set of newly installed packages that are part of
    [requested]. If [force_remove] is true, modified files are not kept.
    [skip] will ignore the actions on the supplied map keys, replacing them with
    the map values when printing. [print_requested] is the set of initially
    requested packages, used for printing actions reasons. *)
val apply:
  ?ask:bool ->
  rw switch_state ->
  requested:package_set ->
  ?print_requested:OpamPackage.Name.Set.t ->
  ?add_roots:OpamPackage.Name.Set.t ->
  ?skip:package OpamPackage.Map.t ->
  ?assume_built:bool ->
  ?download_only:bool ->
  ?force_remove:bool ->
  OpamSolver.solution ->
  rw switch_state * solution_result

(** Call the solver to get a solution and then call [apply]. If [ask] is not
    specified, prompts the user whenever the solution isn't obvious from the
    request. [add_roots] defaults to the set of newly installed packages that
    are part of [requested]. If [force_remove] is true, modified files are
    not kept. [print_requested] is the set of initially requested packages,
    used for printing actions reasons. *)
val resolve_and_apply:
  ?ask:bool ->
  rw switch_state ->
  user_action ->
  ?reinstall:package_set ->
  requested:package_set ->
  ?print_requested:OpamPackage.Name.Set.t ->
  ?add_roots:OpamPackage.Name.Set.t ->
  ?assume_built:bool ->
  ?download_only:bool ->
  ?force_remove:bool ->
  atom request ->
  rw switch_state * (solution_result, OpamCudf.conflict) result

(** Raise an error if no solution is found or in case of error. Unless [quiet]
    is set, print a message indicating that nothing was done on an empty
    solution. *)
val check_solution:
  ?quiet:bool -> 'a switch_state ->
  (solution_result, 'conflict) result ->
  unit

(** Simulate the new [switch_state] after applying the [solution]
    without actually performing the action(s) on disk. *)
val dry_run: 'a switch_state -> OpamSolver.solution -> 'a switch_state

(* Install external dependencies of the given package set, according the depext
   configuration. If [confirm] is false, install commands are directly
   launched, without asking user (used by the `--depext-only` option). If
   [force_depext] is true, it overrides [OpamFile.Config.depext] value. *)
val install_depexts:
  ?force_depext:bool -> ?confirm:bool -> rw switch_state -> package_set -> rw switch_state

(** {2 Atoms} *)

(** Return an atom with a strict version constraint *)
val eq_atom: name -> version -> atom

(** Return a simple atom, with no version constraint, from a package*)
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

(** Matches package names to their existing counterparts, up to capitalisation.
    If no match exists, returns the name unchanged. *)
val fuzzy_name: 'a switch_state -> name -> name

(** Takes a "raw" list of atoms (from the user), and match it to existing
    packages. Match packages with the wrong capitalisation, and raises errors on
    non-existing packages, and unavailable ones unless [permissive] is set.
    [installed] additionally accepts installed, but unavailable packages.
    Exits with a message on error. *)
val sanitize_atom_list: ?permissive: bool -> ?installed: bool ->
  'a switch_state -> atom list -> atom list

(** {2 Stats} *)
val sum: stats -> int

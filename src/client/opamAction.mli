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

(** OPAM actions *)

open OpamTypes
open OpamState.Types

(** Downloads the source for a package to the local cache. *)
val download_package: t -> package -> unit

(** Extracts and patches the source of a package found in the local cache. *)
val extract_package: t -> package -> unit

(** Perform the instructions in .install file,
  either directly via [`Install (build_directory, target_root_directory)]
  or offshoring to shell via [`Shell command_evaluator]
*)
val perform_dot_install :
  name ->
  OpamFile.Dot_install.t ->
  [ `Shell of (string list -> unit) | `Install of dirname * dirname ] ->
  OpamTypes.switch ->
  unit

(** Build and install a package from its downloaded source. *)
val build_and_install_package: t -> metadata:bool -> package -> unit

(** Remove a package. *)
val remove_package: t -> metadata:bool -> ?keep_build:bool -> ?silent:bool -> package -> unit

(** Removes auxiliary files related to a package, after checking that
    they're not needed (even in other switches) *)
val cleanup_package_artefacts: t -> package -> unit

(** Remove all the packages from a solution. This includes the package to
    delete, to upgrade and to recompile. Return the updated state and set of all
    deleted packages. *)
val remove_all_packages: t -> metadata:bool -> OpamSolver.solution
  -> (t * package_set) * [ `Successful of unit | `Exception of exn ]

(** Compute the set of packages which will need to be downloaded to apply a
    solution *)
val sources_needed: t -> OpamSolver.solution -> package_set

(** Update package metadata *)
val update_metadata:
  t ->
  installed:package_set ->
  installed_roots:package_set ->
  reinstall:package_set ->
  t

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

(** OPAM actions *)

open OpamTypes
open OpamState.Types

(** Downloads the source for a package to the local cache. Returns the file or
    dir downloaded, or None if the download failed. *)
val download_package: t -> package ->
  [ `Error of unit | `Successful of generic_file option ] OpamProcess.job

(** Extracts and patches the source of a package *)
val extract_package: t -> generic_file option -> package -> unit

(** Build a package from its downloaded source. Returns [None] on success, [Some
    exn] on error. *)
val build_package:
  t -> generic_file option -> package -> exn option OpamProcess.job

(** Installs a compiled package from its build dir. Returns [None] on success,
    [Some exn] on error. *)
val install_package:
  t -> package -> exn option OpamProcess.job

(** Find out if the package source is needed for uninstall *)
val removal_needs_download: t -> package -> bool

(** Remove a package. *)
val remove_package: t -> metadata:bool -> ?keep_build:bool -> ?silent:bool -> package -> unit OpamProcess.job

(** Removes auxiliary files related to a package, after checking that
    they're not needed (even in other switches) *)
val cleanup_package_artefacts: t -> package -> unit

(** Compute the set of packages which will need to be downloaded to apply a
    solution. Takes a graph of atomic actions. *)
val sources_needed: t -> OpamSolver.ActionGraph.t -> package_set

(** Update package metadata *)
val update_metadata:
  t ->
  installed:package_set ->
  installed_roots:package_set ->
  reinstall:package_set ->
  t

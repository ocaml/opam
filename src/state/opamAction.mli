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

(** [download t pkg] downloads the source of the package [pkg] into
    the local cache. Returns the downloaded file or directory. *)
val download_package: t -> package ->
  [ `Error of string | `Successful of generic_file option ] OpamProcess.job

(** [extract_package t source pkg] extracts and patches the already
    downloaded [source] of the package [pkg]. See {!download_package}
    to download the sources. *)
val extract_package: t -> generic_file option -> package -> unit

(** [build_package t source pkg] builds the package [pkg] from its
    already downloaded [source]. Returns [None] on success, [Some exn]
    on error. See {!download_package} to download the source. *)
val build_package:
  t -> generic_file option -> package -> exn option OpamProcess.job

(** [install_package t pkg] installs an already built package. Returns
    [None] on success, [Some exn] on error. Do not update OPAM's
    metadata. See {!build_package} to build the package. *)
val install_package:
  t -> package -> exn option OpamProcess.job

(** Find out if the package source is needed for uninstall *)
val removal_needs_download: t -> package -> bool

(** Remove a package. *)
val remove_package: t -> ?keep_build:bool -> ?silent:bool -> package -> unit OpamProcess.job

(** Removes auxiliary files related to a package, after checking that
    they're not needed (even in other switches) *)
val cleanup_package_artefacts: t -> package -> unit

(** Compute the set of packages which will need to be downloaded to apply a
    solution. Takes a graph of atomic actions. *)
val sources_needed: t -> OpamSolver.ActionGraph.t -> package_set

(** Returns the updated switch state and write it to disk (unless in dry_run
    mode) (warning: changes to [t] are written to disk even if the optional
    parameter isn't specified) *)
val update_switch_state:
  ?installed:package_set ->
  ?installed_roots:package_set ->
  ?reinstall:package_set ->
  ?pinned:pin_option name_map ->
  t ->
  t

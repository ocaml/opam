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
open OpamStateTypes

(** [download t pkg] downloads the source of the package [pkg] into
    the local cache. Returns the downloaded file or directory. *)
val download_package:
  rw switch_state -> package ->
  [ `Error of string | `Successful of generic_file option ] OpamProcess.job

(** [extract_package t source pkg] extracts and patches the already
    downloaded [source] of the package [pkg]. See {!download_package}
    to download the sources. *)
val extract_package:
  rw switch_state -> generic_file option -> package -> unit

(** [build_package t source pkg] builds the package [pkg] from its
    already downloaded [source]. Returns [None] on success, [Some exn]
    on error. See {!download_package} to download the source. *)
val build_package:
  rw switch_state -> generic_file option -> package ->
  exn option OpamProcess.job

(** [install_package t pkg] installs an already built package. Returns
    [None] on success, [Some exn] on error. Do not update OPAM's
    metadata. See {!build_package} to build the package. *)
val install_package:
  rw switch_state -> package -> exn option OpamProcess.job

(** Find out if the package source is needed for uninstall *)
val removal_needs_download: 'a switch_state -> package -> bool

(** Remove a package. *)
val remove_package:
  rw switch_state -> ?keep_build:bool -> ?silent:bool ->
  package -> unit OpamProcess.job

(** Removes auxiliary files related to a package, after checking that
    they're not needed (even in other switches) *)
val cleanup_package_artefacts: rw switch_state -> package -> unit

(** Compute the set of packages which will need to be downloaded to apply a
    solution. Takes a graph of atomic actions. *)
val sources_needed: 'a switch_state -> OpamSolver.ActionGraph.t -> package_set

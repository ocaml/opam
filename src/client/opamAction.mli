(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Handles concrete actions on packages, like installations and removals *)

open OpamTypes
open OpamStateTypes

(** [download t pkg] downloads the source of the package [pkg] into its locally
    cached source dir. Returns [Some errmsg] on error, [None] on success.

    This doesn't update dev packages that already have a locally cached
    source. *)
val download_package:
  rw switch_state -> package -> string option OpamProcess.job

(** [prepare_package_source t pkg dir] updates the given source [dir] with the
    extra downloads, overlays and patches from the package's metadata
    applied. *)
val prepare_package_source:
  rw switch_state -> package -> dirname -> exn option OpamProcess.job

(** [build_package t build_dir pkg] builds the package [pkg] within [build_dir].
    Returns [None] on success, [Some exn] on error.
    See {!download_package} and {!prepare_package_source} for the previous
    steps. *)
val build_package:
  rw switch_state -> ?test:bool -> ?doc:bool -> dirname -> package ->
  exn option OpamProcess.job

(** [install_package t pkg] installs an already built package. Returns
    [None] on success, [Some exn] on error. Do not update OPAM's
    metadata. See {!build_package} to build the package. *)
val install_package:
  rw switch_state -> ?test:bool -> ?doc:bool -> ?build_dir:dirname -> package ->
  exn option OpamProcess.job

(** Find out if the package source is needed for uninstall *)
val removal_needs_download: 'a switch_state -> package -> bool

(** Removes a package. If [changes] is unspecified, it is read from the
    package's change file. if [force] is specified, remove files marked as added
    in [changes] even if the files have been modified since. *)
val remove_package:
  rw switch_state -> ?silent:bool ->
  ?changes:OpamDirTrack.t -> ?force:bool -> ?build_dir:dirname ->
  package -> unit OpamProcess.job

(** Returns [true] whenever [remove_package] is a no-op. *)
val noop_remove_package:
  rw switch_state -> package -> bool

(** Removes auxiliary files related to a package, after checking that
    they're not needed *)
val cleanup_package_artefacts: rw switch_state -> package -> unit

(** Compute the set of packages which will need to be downloaded to apply a
    solution. Takes a graph of atomic actions. *)
val sources_needed: 'a switch_state -> OpamSolver.ActionGraph.t -> package_set

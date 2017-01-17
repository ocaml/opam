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

(** Synchronisation and downloading of repositories and package sources *)

open OpamTypes
open OpamStateTypes

(*
(** Update the given repository from its upstream. Returns a concurrency-safe
    state update function *)
val repository:
  rw repos_state -> repository ->
  ('a repos_state -> 'a repos_state) OpamProcess.job
*)

(** Update the given repositories from their upstream, and returns the updated
    state. This also saves the updated cached state, and the updated repository
    config (it may be changed by e.g. redirects). The boolean returned is true
    if no error was encountered during the update of any of the repositories. *)
val repositories: rw repos_state -> repository list -> bool * rw repos_state

(** [update_dev_packages t] checks for upstream changes for packages
    first in the switch cache and then in the global cache. Return the
    packages whose contents have changed upstream.

    Packages that are members of the [working_dir] and are bound to a local
    directory under version control are synchronised with its working state,
    bypassing version control.

    Side-effect: update the reinstall file, adding installed changed packages to
    the current switch to-reinstall set.

    The returned boolean is true if all updates were successful. *)
val dev_packages:
  rw switch_state -> ?working_dir:package_set -> package_set ->
  bool * rw switch_state * package_set

(** Updates a single dev or pinned package from its upstream. If [working_dir]
    is set, and the package is bound to a local, version-controlled dir, use the
    working dir state instead of what has been commited to version control.

    Returns true if changed, false otherwise, and a switch_state update
    function, applying possible changes in packages metadata *)
val dev_package:
  rw switch_state -> ?working_dir:bool -> package ->
  ((rw switch_state -> rw switch_state) * bool) OpamProcess.job

(** A subset of update_dev_packages that only takes packages names and only
    works on pinned packages. Also updates the reinstall file of the current
    switch *)
val pinned_packages:
  rw switch_state -> ?working_dir:name_set -> name_set ->
  rw switch_state * package_set

(** Updates a dev pinned package from its upstream; returns true if changed,
    false otherwise, and a switch_state update function that applies possible
    changes in packages metadata. Updates the on-disk overlay *)
val pinned_package:
  rw switch_state -> ?version:version -> ?working_dir:bool -> name ->
  ((rw switch_state -> rw switch_state) * bool) OpamProcess.job

(** Download or synchronise the upstream source for the given package into the
    given directory. Also places all of the package extra files (that have a
    known hash) into the cache. For non-VC remotes, verifies the checksum if
    any *)
val download_package_source:
  'a switch_state -> package -> dirname ->
  unit download option OpamProcess.job

(** [cleanup_source old_opam_option new_opam] checks if the remote URL has
    changed between [old_opam_option] and [new_opam], and, depending on that,
    cleans up the source directory of the package ([OpamPath.Switch.sources]) if
    needed. *)
val cleanup_source:
  'a switch_state -> OpamFile.OPAM.t option -> OpamFile.OPAM.t -> unit

(** Low-level function to retrieve the package source into its local cache *)
val fetch_dev_package:
  OpamFile.URL.t -> dirname -> ?working_dir:bool -> package ->
  unit download OpamProcess.job

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

open OpamTypes
open OpamStateTypes

(*
(** Update the given repository from its upstream. Returns a concurrency-safe
    state update function *)
val repository:
  [< rw ] repos_state -> repository ->
  ('a repos_state -> 'a repos_state) OpamProcess.job
*)

(** Update the given repositories from their upstream, and returns the
    updated state. *)
val repositories: ([< rw ] repos_state as 'a) -> repository list -> 'a

(** [update_dev_packages t] checks for upstream changes for packages
    first in the switch cache and then in the global cache. Return the
    packages whose contents have changed upstream.

    Side-effect: update the reinstall file, adding installed changed packages to
    the current switch to-reinstall set. *)
val dev_packages: ([< rw ] switch_state as 'a) -> package_set -> 'a * package_set

(** Updates a single dev or pinned package from its upstream; returns true
    if changed, false otherwise, and a switch_state update function, applying
    possible changes in packages metadata *)
val dev_package: ([< rw ] switch_state as 'a) -> package ->
  (('a -> 'a) * bool) OpamProcess.job

(** A subset of update_dev_packages that only takes packages names and only
    works on pinned packages. Also updates the reinstall file of the current
    switch *)
val pinned_packages: ([< rw ] switch_state as 'a) -> name_set -> 'a * package_set

(** Updates a dev pinned package from its upstream; returns true if changed,
    false otherwise, and a switch_state update function that applies possible
    changes in packages metadata. Updates the on-disk overlay *)
val pinned_package:
  ([< rw ] switch_state as 'a) -> ?fixed_version:version -> name ->
  (('a -> 'a) * bool) OpamProcess.job

(** Download or synchronise the upstream source for the given package into the
    given directory.
    For non-VC remotes, verifies the checksum if any *)
val download_upstream:
  'a switch_state -> package -> dirname ->
  generic_file download option OpamProcess.job


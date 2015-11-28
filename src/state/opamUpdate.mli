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

(** [update_dev_packages t] checks for upstream changes for packages
    first in the switch cache and then in the global cache. Return the
    packages whose contents have changed upstream.

    Side-effect: update the reinstall files (on all switches, for non-pinned
    packages). *)
val dev_packages: switch_state -> package_set -> package_set

(** Updates a single dev or pinned package from its upstream; returns true
    if changed, false otherwise *)
val dev_package: switch_state -> package -> bool OpamProcess.job

(** A subset of update_dev_packages that only takes packages names and only
    works on pinned packages. Also updates the reinstall file of the current
    switch *)
val pinned_packages: switch_state -> name_set -> package_set

(** Updates a dev pinned package from its upstream; returns true if changed,
    false otherwise *)
val pinned_package:
  switch_state -> ?fixed_version:version -> name -> bool OpamProcess.job

(** Download or synchronise the upstream source for the given package into the
    given directory.
    For non-VC remotes, verifies the checksum if any *)
val download_upstream:
  switch_state -> package -> dirname ->
  generic_file download option OpamProcess.job


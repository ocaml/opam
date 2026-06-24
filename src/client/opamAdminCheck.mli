(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

(** Runs checks on the repository at the given repository. Returns
    [all_packages], [uninstallable_roots], [uninstallable], [cycle_packages],
    [obsolete_packages]. If the corresponding option was disabled, the returned
    sets are empty. *)
val check:
  quiet:bool -> installability:bool -> cycles:bool -> obsolete:bool ->
  OpamRepositoryRoot.Dir.t ->
  package_set * package_set * package_set * package_set * package_set

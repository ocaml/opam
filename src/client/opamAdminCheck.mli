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

(** Analyses a given package universe, and returns
    [uninstallable_roots,uninstallable]. The first is a subset of the second,
    where internal dependents have been removed. *)
val installability_check: universe -> package_set * package_set

(** Analyses a universe for dependency cycles. Returns the set of packages
    involved, and the cycles (reduced to formula lists) *)
val cycle_check: universe -> package_set * formula list list

(** Runs checks on the repository at the given repository. Returns
    [uninstallable_roots], [uninstallable], [cycle_packages]. *)
val check:
  quiet:bool -> installability:bool -> cycles:bool -> ignore_test:bool ->
  dirname -> package_set * package_set * package_set

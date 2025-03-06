(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)


(** Functions handling the "opam lock" command *)

open OpamTypes
open OpamStateTypes

(** Select packages to lock. If a package have at least one of its direct
    dependencies not installed in the switch, it is dropped. Returns the state
    with non present packages pinned, and kept packages. *)
val select_packages:
  [ `Atom of atom | `Filename of filename | `Dirname of dirname ] list ->
  'a switch_state -> 'a switch_state * package_set

(** Returns the locked opam file, according its depends, depopts, and pins.
    If [only_direct] is set to true, it only adds direct dependencies specified
    in the opam file. If [keep_local] is set to true, local pins are not
    resolved to a distant url and they are all added with their local path in
    'pin_depends:' field. *)
val lock_opam:
  only_direct:bool -> keep_local:bool ->
  'a switch_state -> OpamFile.OPAM.t -> OpamFile.OPAM.t

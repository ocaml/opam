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

(** Functions handling the "opam pin" subcommand *)

open OpamTypes
open OpamStateTypes

(** Pins a package to the given version, and writes to disk. Returns the updated
    state. The main difference with [source_pin] is that a definition overlay is
    not created. Therefore, the package must exist already. *)
val version_pin: rw switch_state -> name -> version -> rw switch_state

(** Sets the package as pinned to the given target. A package definition is
    looked for in the package source and current metadata (in this order).

    If [edit], or if no package definition is found, this opens an editor (with
    a template if no definition is available).

    If no target url is given, the url from the installed package or
    repositories, if any, is used and the package is otherwise pinned without
    target.

    If [force], don't abort even if the source can't be fetched from [target] *)
val source_pin:
  rw switch_state -> name -> ?version:version -> ?edit:bool -> ?force:bool ->
  url option -> rw switch_state

(** Let the user edit a pinned package's opam file. If given, the version is put
    into the template in advance. Writes and returns the updated switch
    state. *)
val edit: rw switch_state -> ?version:version -> name -> rw switch_state

(** Unpin packages *)
val unpin: rw switch_state -> name list -> rw switch_state

(** List the pinned packages to the user. *)
val list: 'a switch_state -> short:bool -> unit

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

(** Pin subcommand. *)

open OpamTypes
open OpamStateTypes

(** Pins a package to the given version, and writes to disk. Returns the updated
    state *)
val version_pin: ([< rw ] switch_state as 'a) -> name -> version -> 'a

(** Sets the package as pinned to the given target. A package definition is
    looked for in the package source and current metadata (in this order).

    If [edit], or if no package definition is found, this opens an editor (with
    a template if no definition is available). *)
val source_pin:
  ([< rw ] switch_state as 'a) -> name -> ?version:version -> ?edit:bool ->
  url -> 'a

(** Let the user edit a pinned package's opam file.
    Writes and returns the updated switch state. *)
val edit: ([< rw ] switch_state as 'a)-> name -> 'a

(** Unpin packages *)
val unpin: ([< rw ] switch_state as 'a) -> name list -> 'a

(** List the pinned packages to the user. *)
val list: 'a switch_state -> short:bool -> unit

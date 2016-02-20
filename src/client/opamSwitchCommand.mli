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

(** Switch commands. *)

open OpamTypes
open OpamStateTypes

(** Install a new switch. Returns a continuation that must be run to install the
    packages, but only needs a switch lock. *)
val install_cont:
  quiet:bool -> update_config:bool ->
  packages:atom conjunction ->
  switch ->
  switch * (unit -> unit)

(** Like [install_cont] but runs the continuation already *)
val install:
  quiet:bool -> update_config:bool ->
  packages:atom conjunction -> switch -> unit

(** Install a compiler's base packages *)
val install_compiler_packages: switch_state -> atom conjunction -> unit

(** Import a file which contains the packages to install. *)
val import:
  global_state -> switch -> switch_selections OpamFile.t option -> unit

(** Export a file which contains the installed packages. *)
val export: switch_selections OpamFile.t option -> unit

(** Remove the given compiler switch. *)
val remove: global_state -> ?confirm:bool -> switch -> unit

(** Switch to the given compiler switch, installing it if it doesn't exist
    already (with the given compiler, or empty if unspecified). Returns a
    continuation like [install] *)
val switch_cont:
  quiet:bool -> packages:atom conjunction ->
  switch -> switch * (unit -> unit)

(** Like [switch_cont] but runs the continuation already. *)
val switch:
  quiet:bool -> packages:atom conjunction ->
  switch -> unit

(** Reinstall the given compiler switch. *)
val reinstall: global_state -> switch -> unit

(** Display the current compiler switch. *)
val show: unit -> unit

(** List all the available compiler switches. *)
val list: print_short:bool -> installed:bool -> all:bool -> unit

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

(** Specific query and handling of pinned packages *)

open OpamTypes
open OpamStateTypes

(** Returns the version the package is pinned to.
    @raise Not_found when appropriate *)
val version: 'a switch_state -> name -> version

(** Returns the package with the pinned-to version from a pinned package name.
    @raise Not_found when appropriate *)
val package: 'a switch_state -> name -> package

(** Returns the package with the pinned-to version from a package name, if
    pinned *)
val package_opt: 'a switch_state -> name -> package option

(** The set of all pinned packages with their pinning versions *)
val packages: 'a switch_state -> package_set

(** Looks up an 'opam' file for the given named package in a source directory *)
val find_opam_file_in_source: name -> dirname -> OpamFile.OPAM.t OpamFile.t option

(** Finds back the location of the opam file this package definition was loaded
    from *)
val orig_opam_file: OpamFile.OPAM.t -> OpamFile.OPAM.t OpamFile.t option

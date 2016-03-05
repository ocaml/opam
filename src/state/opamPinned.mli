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

(** Add overlay files for a pinned package. If no definition is found
    use a minimal OPAM file unless [template] is set to [true]. *)
val add_overlay:
  ?template:bool -> ?version:version -> switch_state ->
  name -> pin_option -> unit

(** Remove all overlay files *)
val remove_overlay: global_state -> switch -> name -> unit

(** Returns the version the package is pinned to. @raise [Not_found] *)
val version: switch_state -> name -> version

(** Returns the package with the pinned-to version from a pinned package name.
    @raise [Not_found] *)
val package: switch_state -> name -> package

(** Returns the package with the pinned-to version from a package name, if
    pinned *)
val package_opt: switch_state -> name -> package option

(** The set of all pinned packages with their pinning versions *)
val packages: switch_state -> package_set

(** Looks up an 'opam' file for the given named package in a source directory *)
val find_opam_file_in_source: name -> dirname -> OpamFile.OPAM.t OpamFile.t option

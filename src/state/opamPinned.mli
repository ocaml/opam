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

(** Finds all package definition files in a given source dir [opam],
    [pkgname.opam/opam], etc. *)
val files_in_source: dirname -> (name option * OpamFile.OPAM.t OpamFile.t) list

(** From an opam file location, sitting below the given project directory, find
    the corresponding package name if specified ([<name>.opam] or
    [<name>.opam/opam]). This function doesn't check the project directory name
    itself, or the package name that might be specified within the file. *)
val name_of_opam_filename: dirname -> filename -> name option

(** Finds back the location of the opam file this package definition was loaded
    from *)
val orig_opam_file: OpamFile.OPAM.t -> OpamFile.OPAM.t OpamFile.t option

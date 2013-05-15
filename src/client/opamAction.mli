(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

(** OPAM actions *)

open OpamTypes
open OpamState.Types

(** Build and install a package. *)
val build_and_install_package: t -> metadata:bool -> package -> unit

(** Remove a package. *)
val remove_package: t -> metadata:bool -> rm_build:bool -> package -> unit

(** Remove all the packages from a solution. This includes the package
    to delete, to upgrade and to recompile. Return the set of all deleted
    packages. *)
val remove_all_packages: t -> metadata:bool -> solution -> package_set

(** Update package metadata *)
val update_metadata:
  t ->
  installed:package_set ->
  installed_roots:package_set ->
  reinstall:package_set ->
  unit

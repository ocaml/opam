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

(** Repository sub-command functions. *)

open OpamState.Types
open OpamTypes

(** Update the repositories indexes ([repo/index]. *)
val update_index: t -> unit

(** Update the given dev packages. *)
val update_dev_packages: t -> verbose:bool -> package_set -> package_set

(** Fix the compiler descriptions and display the changes if [verbose]
    is set. *)
val fix_compiler_descriptions: t -> verbose:bool -> compiler_set updates

(** Fix the the package descriptions and display the changes if
    [verbose] is set. *)
val fix_package_descriptions: t -> verbose:bool -> package_set updates

(** Fix all the package and compiler descriptions. *)
val fix_descriptions: t -> verbose:bool -> unit

(** List the available repositories. *)
val list: short:bool -> unit

(** Add a new repository. *)
val add: repository_name -> repository_kind -> address -> priority:int option -> unit

(** Remove a repository. *)
val remove: repository_name -> unit

(** Set a repository priority. *)
val priority: repository_name -> priority:int -> unit

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

(** Update the repositories indexes ([repo/index], [repo/index.packages] and
    [repo/index.compilers]). Return an updated state (including the correct
    cache of repositories' [prefixes]). *)
val update_index: t -> t

(** Update the given pinned package. *)
val update_pinned_packages: t -> verbose:bool -> name_set -> unit

(** [relink_compilers old_index] relinks the compiler files. *)
val relink_compilers: t -> verbose:bool ->
  compiler_repository_state compiler_map -> unit

(** [relink_package old_index upstream_changes] relinks the package
    files and display the changes (if [verbose is set]). If
    [upstream_changes] is set, these changes are also displayed. *)
val relink_packages: t -> verbose:bool ->
  package_repository_state package_map  -> unit

(** List the available repositories. *)
val list: short:bool -> unit

(** Add a new repository. *)
val add: repository_name -> repository_kind -> address -> priority:int option -> unit

(** Remove a repository. *)
val remove: repository_name -> unit

(** Set a repository priority. *)
val priority: repository_name -> priority:int -> unit

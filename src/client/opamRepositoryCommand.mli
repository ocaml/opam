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

(** Functions handling the "opam repository" subcommand *)

open OpamTypes
open OpamStateTypes

(** List the available repositories. *)
val list: 'a repos_state -> short:bool -> unit

(* !X FIXME: once switches define their repositories, we should remove the
   `repositories:` field from ~/.opam/config and rely on the switch priority
   list, and ~/.opam/repos/*/config only. Then we don't need a lock on
   global_state anymore. *)

(** Add a new repository. *)
val add:
  rw global_state -> rw repos_state ->
  repository_name -> url -> priority:int option ->
  rw global_state * rw repos_state

(** Remove a repository. *)
val remove:
  rw global_state -> rw repos_state -> repository_name ->
  rw global_state * rw repos_state

(** Set a repository priority, i.e. its rank in the configured repos (lower
    value is higher priority) *)
val priority:
  rw global_state -> repository_name -> priority:int -> rw global_state

(** Change the registered address of a repo *)
val set_url: rw repos_state -> repository_name -> url -> rw repos_state

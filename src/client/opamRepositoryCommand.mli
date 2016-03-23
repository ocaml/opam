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

(** Repository sub-command functions. *)

open OpamTypes
open OpamStateTypes

(** List the available repositories. *)
val list: 'a global_state -> short:bool -> unit

(* !X FIXME: once switches define their repositories, we should remove the
   `repositories:` field from ~/.opam/config and rely on the switch priority
   list, and ~/.opam/repos/*/config only. Then we don't need a lock on
   global_state anymore. *)

(** Add a new repository. *)
val add:
  [< rw ] global_state -> repository_name -> url -> priority:int option ->
  [< rw ] repos_state

(** Remove a repository. *)
val remove: ([< rw ] global_state as 'a) -> repository_name -> 'a

(** Set a repository priority. *)
val priority:
  'a global_state -> repository_name -> priority:int -> rw repos_state

(** Change the registered address of a repo *)
val set_url: 'a global_state -> repository_name -> url -> rw repos_state

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

(** Functions handling the "opam repository" subcommand *)

open OpamTypes
open OpamStateTypes

(** List the selected repositories in the global default and/or selected
    switches. *)
val list:
  'a repos_state -> global:bool -> switches:switch list ->
  short:bool -> unit

(** Lists all configured repositories, and, if not [short], the switches they
    are selected in. *)
val list_all: 'a repos_state -> short:bool -> unit

(** Add a new repository to ~/.opam/repos, without updating any selections *)
val add:
  rw repos_state -> repository_name -> url -> trust_anchors option ->
  rw repos_state

(** Remove a repository from ~/.opam/repos, without updating any selections *)
val remove: rw repos_state -> repository_name -> rw repos_state

(** Updates the global switch selection, used as default for switches that don't
    specify their selections (e.g. newly created switches) *)
val update_global_selection:
  rw global_state -> (repository_name list -> repository_name list) ->
  rw global_state

(** Updates the specified selections using the given functions, taking locks as
    required *)
val update_selection:
  'a global_state -> global:bool -> switches:switch list ->
  (repository_name list -> repository_name list) ->
  'a global_state

(** Change the registered address of a repo *)
val set_url:
  rw repos_state -> repository_name -> url -> trust_anchors option ->
  rw repos_state

(** Update the given repositories, as per [OpamUpdate.repositories], checks for
    their version and runs the upgrade script locally if they are for an earlier
    opam. Returns [true] if no update or upgrade errors were encountered. *)
val update_with_auto_upgrade:
  rw repos_state -> repository_name list -> bool * rw repos_state

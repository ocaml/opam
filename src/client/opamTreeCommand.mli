(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Functions handling the "opam tree" subcommand *)

open OpamTypes
open OpamStateTypes

(** Speficy the type of the forest to build *)
type mode =
  | Deps        (** Dependency forest (roots -> leaves)  *)
  | ReverseDeps (** Reverse-dependency forest (leaves -> roots) *)

(** Given a list of packages, thin the forest so that it has *)
type tree_filter =
  | Roots_from (** only the trees of the packages *)
  | Leads_to   (** only the trees which have the packages as their leaves *)

(** Duplicated tree symbol *)
val duplicate_symbol: string

(** Outputs a dependency forest of the installed packages as a
    Unicode/ASCII-art tree. *)
val run :
  [< unlocked > `Lock_write ] switch_state ->
  (* package selection options *)
  OpamListCommand.dependency_toggles ->
  (* output format options *)
  ?no_constraint:bool ->
  (* do no keep switch consistency *)
  ?no_switch:bool ->
  mode -> tree_filter -> name list -> unit

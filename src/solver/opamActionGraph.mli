(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 OCamlPro                                             *)
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

module type ACTION = sig
  type package
  module Pkg: GenericPackage with type t = package
  include OpamParallel.VERTEX with type t = package action
  val to_string: [< t ] -> string
  val to_aligned_strings: [< t ] list -> string list
end

module MakeAction (P: GenericPackage) : ACTION with type package = P.t and type t = P.t OpamTypes.action

module type SIG = sig
  type package
  include OpamParallel.GRAPH with type V.t = package OpamTypes.action

  (** Reduces a graph of atomic or concrete actions (only removals, installs and
      builds) by turning removal+install to reinstalls or up/down-grades, best
      for display. Dependency ordering won't be as accurate though, as there is
      no proper ordering of (reinstall a, reinstall b) if b depends on a. The
      resulting graph contains at most one action per package name.

      There is no guarantee however that the resulting graph is acyclic. *)
  val reduce: t -> t

  (** Expand install actions, adding a build action preceding them. *)
  val explicit: t -> t

  (** To minimise the consequences of failures: adds edges to the graph to
      ensure removals are done as late as possible. Since they're fast, the loss
      in parallelism is negligible. *)
  val removals_last: t -> t
end

module Make (A: ACTION) : SIG with type package = A.package

(** Some messages that may be used for displaying actions. Single utf8 chars if
    the corresponding option is set, otherwise words. *)
val action_strings:
  ?utf8:bool -> 'a highlevel_action -> string

(** Colorise string according to the action *)
val action_color: 'a highlevel_action -> string -> string

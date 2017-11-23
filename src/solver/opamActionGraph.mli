(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 OCamlPro                                             *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Handles graphs of actions (package changes), based on ocamlgraph *)

open OpamTypes

module type ACTION = sig
  type package
  module Pkg: GenericPackage with type t = package
  include OpamParallel.VERTEX with type t = package action
  val to_string: [< t ] -> string
  val to_aligned_strings:
    ?append:(package -> string) -> [< t ] list -> string list list
  module Set: OpamStd.SET with type elt = package action
  module Map: OpamStd.MAP with type key = package action
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

  (** Expand install actions, adding a build action preceding them.
      The argument [noop_remove] is a function that should return `true`
      for package where the `remove` action is known not to modify the
      filesystem (such as `conf-*` package). *)
  val explicit: ?noop_remove:(package -> bool) -> t -> t

  (** Folds on all recursive successors of the given action, including itself,
      depth-first. Assumes an acyclic graph. *)
  val fold_descendants: (V.t -> 'a -> 'a) -> 'a -> t -> V.t -> 'a
end

module Make (A: ACTION) : SIG with type package = A.package

(** Some messages that may be used for displaying actions. Single utf8 chars if
    the corresponding option is set, otherwise words. *)
val action_strings:
  ?utf8:bool -> 'a action -> string

(** Colorise string according to the action *)
val action_color: 'a action -> string -> string

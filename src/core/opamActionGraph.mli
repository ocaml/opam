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
end

module MakeAction (P: GenericPackage) : ACTION with type package = P.t and type t = P.t OpamTypes.action

module type SIG = sig
  type package
  include OpamParallel.GRAPH with type V.t = package OpamTypes.action

  (** Reduces a graph of atomic actions (only removals and installs) by turning
      removal+install to reinstalls or up/down-grades, best for display.
      Dependency ordering won't be as accurate though, as there is no proper
      ordering of (reinstall a, reinstall b) if b depends on a. The resulting
      graph contains at most one action per package name.

      There is no guarantee however that the resulting graph is acyclic. *)
  val reduce: t -> t
end

module Make (A: ACTION) : SIG with type package = A.package

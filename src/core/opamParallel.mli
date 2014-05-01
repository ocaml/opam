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

module type G = sig
  include Graph.Sig.I
  include Graph.Topological.G with type t := t and module V := V
  val has_cycle: t -> bool
  val scc_list: t -> V.t list list
  val string_of_vertex: V.t -> string
end

type error =
  | Process_error of OpamProcess.result
  | Internal_error of string
  | Package_error of string

module type SIG = sig

  module G : G

  val iter: int -> G.t ->
    pre:(G.V.t -> unit) ->
    child:(G.V.t -> unit) ->
    post:(G.V.t -> unit) ->
    unit

  val iter_l: int -> G.vertex list ->
    pre:(G.V.t -> unit) ->
    child:(G.V.t -> unit) ->
    post:(G.V.t -> unit) ->
    unit

  val map_reduce: int -> G.t ->
    map:(G.V.t -> 'a) ->
    merge:('a -> 'a -> 'a) ->
    init:'a ->
    'a

  val map_reduce_l: int -> G.vertex list ->
    map:(G.V.t -> 'a) ->
    merge:('a -> 'a -> 'a) ->
    init:'a ->
    'a

  val create: G.V.t list -> G.t

  exception Errors of (G.V.t * error) list * G.V.t list
  exception Cyclic of G.V.t list list
end

module Make (G : G) : SIG with module G = G
                           and type G.V.t = G.V.t

module type VERTEX = sig
  include Graph.Sig.COMPARABLE
  val to_string: t -> string
end

module type GRAPH = sig
  include Graph.Sig.I
  include Graph.Oper.S with type g = t
  module Topological : sig
    val fold : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
    val iter : (V.t -> unit) -> t -> unit
  end
  module Parallel : SIG with type G.t = t
                         and type G.V.t = vertex
  module Dot : sig val output_graph : out_channel -> t -> unit end
end

module MakeGraph (V: VERTEX) : GRAPH with type V.t = V.t

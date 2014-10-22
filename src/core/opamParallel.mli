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

module type VERTEX = sig
  include OpamMisc.OrderedType
  include Graph.Sig.COMPARABLE with type t := t
end

module type G = sig
  include Graph.Sig.I
  module Vertex: VERTEX with type t = V.t
  module Topological: sig
    val fold: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  end
  val has_cycle: t -> bool
  val scc_list: t -> V.t list list
end

(** Simply parallel execution of tasks *)

val iter: jobs:int -> command:('a -> unit OpamProcess.job) -> 'a list -> unit

val map: jobs:int -> command:('a -> 'b OpamProcess.job) -> 'a list -> 'b list

val reduce: jobs:int -> command:('a -> 'b OpamProcess.job) ->
  merge:('b -> 'b -> 'b) -> nil:'b ->
  'a list -> 'b

(** More complex parallelism with dependency graphs *)

module type SIG = sig

  module G : G

  val iter:
    jobs:int ->
    command:(pred:(G.V.t * 'a) list -> G.V.t -> 'a OpamProcess.job) ->
    G.t ->
    unit

  exception Errors of (G.V.t * exn) list * G.V.t list
  exception Cyclic of G.V.t list list
end

module Make (G : G) : SIG with module G = G
                           and type G.V.t = G.V.t

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

module MakeGraph (V: OpamMisc.OrderedType) : GRAPH with type V.t = V.t

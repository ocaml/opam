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

(** Concurrent process execution *)

(** Functor argument *)
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

(** Functor signature *)
module type SIG = sig

  module G : G

  (** [iter n t pre child paren] parallel iteration on [n]
      cores. [child] is evaluated in a remote process and when it as
      finished, whereas [pre] and [post] are evaluated on the current
      process (respectively before and after the child process has
      been created). *)
  val iter: int -> G.t ->
    pre:(G.V.t -> unit) ->
    child:(G.V.t -> unit) ->
    post:(G.V.t -> unit) ->
    unit

  (** Parallel iteration on a list. *)
  val iter_l: int -> G.vertex list ->
    pre:(G.V.t -> unit) ->
    child:(G.V.t -> unit) ->
    post:(G.V.t -> unit) ->
    unit

  (** Map-reduce on a graph *)
  val map_reduce: int -> G.t ->
    map:(G.V.t -> 'a) ->
    merge:('a -> 'a -> 'a) ->
    init:'a ->
    'a

  (** Map-reduce on a list. *)
  val map_reduce_l: int -> G.vertex list ->
    map:(G.V.t -> 'a) ->
    merge:('a -> 'a -> 'a) ->
    init:'a ->
    'a

  (** Build a graph on concurrent tasks from a list of tasks. *)
  val create: G.V.t list -> G.t

  (** Errors ([errors], [remaining]) *)
  exception Errors of (G.V.t * error) list * G.V.t list

  (** The graph is cyclic. *)
  exception Cyclic of G.V.t list list

end

(** Functor *)
module Make (G : G) : SIG with module G = G

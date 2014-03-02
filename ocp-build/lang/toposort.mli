(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

(** Topological Sort *)


(** Abstract type for a node *)
type node

(** Node creation *)
val new_node : unit -> node

module Make :
  functor
    (M : sig
      type t
      val node : t -> node
      val iter_edges : (t -> unit) -> t -> unit
    end) ->
      sig
        exception RecursiveDependency of M.t
        val sort : M.t list -> M.t list
	val sort_sorted : M.t list -> M.t list
      end

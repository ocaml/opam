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
       val name : t -> string
       val debug : bool ref
     end) ->
      sig
        val sort : M.t list ->
    M.t list * (* sorted list *)
      (M.t * M.t list * M.t list) list *  (* a cycle *)
      M.t list (* other non-sorted nodes *)

      end

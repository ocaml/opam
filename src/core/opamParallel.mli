(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Concurrent process execution *)

(** Functor argument *)
module type G = sig
  include Graph.Sig.G
  include Graph.Topological.G with type t := t and module V := V
  val has_cycle: t -> bool
  val scc_list: t -> V.t list list
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
  val parallel_iter: int -> G.t ->
    pre:(G.V.t -> unit) ->
    child:(G.V.t -> unit) ->
    post:(G.V.t -> unit) ->
    unit

  (** Errors ([errors], [remaining]) *)
  exception Errors of (G.V.t * error) list * G.V.t list

  (** The graph is cyclic. *)
  exception Cyclic of G.V.t list list

end

(** Functor *)
module Make (G : G) : SIG with module G = G

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module String : sig
  (* NOTE: OCaml >= 4.13 *)
  val exists: (char -> bool) -> string -> bool

  (* NOTE: OCaml >= 4.13 *)
  val starts_with: prefix:string -> string -> bool

  (* NOTE: OCaml >= 4.13 *)
  val ends_with: suffix:string -> string -> bool

  (* NOTE: OCaml >= 4.13 *)
  val for_all: (char -> bool) -> string -> bool

  (* NOTE: OCaml >= 4.13 *)
  val fold_left: ('a -> char -> 'a) -> 'a -> string -> 'a
end

module Seq : sig
  (* NOTE: OCaml >= 4.14 *)
  val find_map: ('a -> 'b option) -> 'a Seq.t -> 'b option
end

module Either : sig
  (* NOTE: OCaml >= 4.12 *)
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
end

module Lazy : sig
  (* NOTE: OCaml >= 4.13 *)
  val map : ('a -> 'b) -> 'a Lazy.t -> 'b Lazy.t
end

module Unix : sig
  (* `realpath` for OCaml >= 4.13.0,
     implementation with double chdir otherwise *)
  val realpath: string -> string
end

module Filename: sig
  (** NOTE: OCaml >= 4.10 *)

  val quote_command :
    string -> ?stdin:string -> ?stdout:string -> ?stderr:string
    -> string list -> string
end

module List : sig
  (** NOTE: OCaml >= 4.11 *)
  val fold_left_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list

  (** NOTE: OCaml >= 4.12 *)
  val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
end

module type MAP = sig
  include Stdlib.Map.S

  (** NOTE: OCaml >= 4.11 *)
  val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
end

module Map(Ord : Stdlib.Map.OrderedType) : MAP with type key = Ord.t

module Pair : sig
  (** NOTE: OCaml >= 5.4 *)
  val equal :
    ('a -> 'a -> bool) -> ('b -> 'b -> bool) ->
    ('a * 'b) -> ('a * 'b) -> bool
end

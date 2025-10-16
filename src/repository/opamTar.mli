(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val fold_reg_files :
  ('acc -> string -> string -> 'acc) ->
  'acc -> OpamFilename.t -> 'acc

module Inplace : sig
  type t

  val with_open_out : OpamFilename.t -> (t -> 'a) -> 'a
  val fold_reg_files :
    ('acc -> string -> string -> 'acc) ->
    'acc -> t -> 'acc
  val add : fname:string -> content:string -> t -> t
  val remove : string -> t -> t
  val write : t -> unit
end

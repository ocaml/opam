(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val nice_int : int Crowbar.gen
val nice_uint : int Crowbar.gen
val nice_string : string Crowbar.gen

val eq_of_comp : ('a -> 'a -> int) -> ('a -> 'a -> bool)

val pair : 'a Crowbar.gen -> 'b Crowbar.gen -> ('a * 'b) Crowbar.gen

val check_json_roundtrip :
  name:string -> 'a Crowbar.gen ->
  ('a -> 'a -> bool) -> 'a OpamJson.encoder -> 'a OpamJson.decoder -> unit

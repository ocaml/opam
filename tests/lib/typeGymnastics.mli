(**************************************************************************)
(*                                                                        *)
(*    Copyright 2024 David Allsopp Ltd.                                   *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Tests to ensure that uses of %identity with OpamTypes.env_update_op_kind are
   sound. *)

open OpamTypes

val open_env_updates :
  ('a, euok_writeable) env_update list
    -> ('a, [> euok_writeable]) env_update list

val op_of_raw: OpamParserTypes.FullPos.env_update_op_kind -> euok_writeable env_update_op_kind
val raw_of_op: euok_writeable env_update_op_kind -> OpamParserTypes.FullPos.env_update_op_kind

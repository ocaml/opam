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

(* Pure implementation of the external declaration used in OpamFile *)
let open_env_updates updates =
  let f ({envu_op; _} as update : ('a, euok_writeable) env_update) =
    let envu_op = match envu_op with
    | Eq -> Eq
    | PlusEq -> PlusEq
    | EqPlus -> EqPlus
    | ColonEq -> ColonEq
    | EqColon -> EqColon
    | EqPlusEq -> EqPlusEq
    in
      {update with envu_op} in
  List.map f updates

(* Returns the integer being used for a constant variant constructor. Fails
   if passed a block. *)
let get_constructor variant =
  let v = (Obj.repr variant) in
  if Obj.is_block v then
    invalid_arg "get_constructor"
  else
    (Obj.obj v : int)

(* This list should be the constructors of
   OpamParserTypes.FullPos.env_update_op_kind with the
   OpamTypes.euok_writeable OpamTypes.env_update_op_kind constructors, in order.
 *)
let (ops : (OpamParserTypes.FullPos.env_update_op_kind *
            OpamTypes.euok_writeable OpamTypes.env_update_op_kind) list) = [
  (OpamParserTypes.Eq, Eq);
  (OpamParserTypes.PlusEq, PlusEq);
  (OpamParserTypes.EqPlus, EqPlus);
  (OpamParserTypes.ColonEq, ColonEq);
  (OpamParserTypes.EqColon, EqColon);
  (OpamParserTypes.EqPlusEq, EqPlusEq);
]

(* Pure implementation of the external declaration used in OpamTypesBase *)
let op_of_raw op =
  match (op : OpamParserTypes.FullPos.env_update_op_kind) with
  | Eq -> Eq
  | PlusEq -> PlusEq
  | EqPlus -> EqPlus
  | ColonEq -> ColonEq
  | EqColon -> EqColon
  | EqPlusEq -> EqPlusEq

let raw_of_op op =
  match (op : euok_writeable env_update_op_kind) with
  | Eq -> OpamParserTypes.Eq
  | PlusEq -> OpamParserTypes.PlusEq
  | EqPlus -> OpamParserTypes.EqPlus
  | ColonEq -> OpamParserTypes.ColonEq
  | EqColon -> OpamParserTypes.EqColon
  | EqPlusEq -> OpamParserTypes.EqPlusEq


(* Verify that the constructor order is the same *)
let _unique =
  let f i (op_l, op_r) =
    let op_l = get_constructor op_l in
    let op_r = get_constructor op_r in
    if op_l <> op_r || op_l <> i then
      failwith "Constructor order failure"
    else
      succ i
  in
  List.fold_left f 0 ops

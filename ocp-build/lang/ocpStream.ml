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

(* {{extend Stream}} *)
include Stream

let to_list stream =
  let list = ref [] in
  Stream.iter (
    fun token ->
      list := token :: !list
  ) stream;
  List.rev !list

(* Build a stream from a list of lines *)
let of_lines lines =
  let lines = Array.of_list lines in
  let fn = OcpString.indexes lines in
  let elt i =
    try let n,j = fn i in Some lines.(n).[j]
    with OcpString.Out_of_bounds -> None in
  from elt

let is_empty s =
  try ignore (Stream.peek s); false
  with Stream.Failure -> true

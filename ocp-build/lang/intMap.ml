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

module Map = Map.Make(struct type t = int
                             let compare (x:int) y = compare x y
end)

include Map

let to_list map =
  let list = ref [] in
  iter (fun x y -> list := (x,y) :: !list) map;
  List.rev !list

let to_list1 map =
  let list = ref [] in
  iter (fun x y -> list := x :: !list) map;
  List.rev !list

let to_list2 map =
  let list = ref [] in
  iter (fun x y -> list := y :: !list) map;
  List.rev !list

exception MinElt
let exn_MinElt = MinElt

let min_elt map =
  let x = ref None in
  try
    iter (fun key v -> x := Some (key, v); raise exn_MinElt) map;
    None
  with MinElt -> !x



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

open Hashtbl

let to_list h =
  fold (fun k v accu -> (k,v) :: accu) h []

let of_list l =
  let h = create (List.length l) in
  List.iter (fun (k,v) -> add h k v) l;
  h

let incr h key =
  if mem h key then
    let n = find h key in
    replace h key (n+1)
  else
    add h key 1

let for_all h fn =
  fold (fun k v accu -> accu && fn k v) h true

let exists h fn =
  fold (fun k v accu -> accu || fn k v) h false

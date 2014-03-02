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

(* Reentrant buffers :
   If you call a function that needs a buffer, you might want to use this
   module to reuse such buffers, instead of reallocating them everytime.

   This module is not thread-safe. Reentrance is only provided for a function
   that uses a buffer, and might call another function using a similar
   buffer.

   Buffer sizes should be between 4kB and 1MB.
*)

let sizes = Array.init 10 (fun _ -> Queue.create ())

let invalid_size size =
  Printf.kprintf failwith
    "ReentrantBuffer.get: size %d is not a power of two" size

let get_power size =
  let rec find_power pos size =
    if size = 1 then pos else
      let size2 = size lsr 1 in
      if size2 lsl 1 <> size then invalid_size size;
      find_power (pos+1) size2
  in
  if (size lsr 10) lsl 10 <> size then invalid_size size;
  find_power 0 (size lsr 10)

let _ =
  assert (get_power 1024 = 0);
  assert (get_power 2048 = 1);
  ()

let get size =
  let pos = get_power size in
  try
    Queue.take sizes.(pos)
  with Queue.Empty ->
    String.create size

let free s =
  let size = String.length s in
  let pos = get_power size in
  Queue.add s sizes.(pos)

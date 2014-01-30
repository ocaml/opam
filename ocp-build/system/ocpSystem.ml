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

module MinUnix = struct
  include MinUnix
  include OcpUnix
end

module Filename = struct
  include Filename
  include OcpFilename
end


(* write a little-endian int to a string *)
let write_le_int buf off i =
  buf.[off]   <- Char.unsafe_chr (i          land 0xff);
  buf.[off+1] <- Char.unsafe_chr ((i lsr 8)  land 0xff);
  buf.[off+2] <- Char.unsafe_chr ((i lsr 16) land 0xff);
  buf.[off+3] <- Char.unsafe_chr ((i lsr 24) land 0xff);
  if Sys.word_size = 64 then begin
    buf.[off+4] <- Char.unsafe_chr ((i lsr 32) land 0xff);
    buf.[off+5] <- Char.unsafe_chr ((i lsr 40) land 0xff);
    buf.[off+6] <- Char.unsafe_chr ((i lsr 48) land 0xff);
    buf.[off+7] <- Char.unsafe_chr ((i lsr 56) land 0xff);
  end

(* write 1 int *)
let output_le_int oc i =
  let buf = String.create (Sys.word_size / 8) in
  write_le_int buf 0 i;
  output oc buf 0 (String.length buf)

(* write a list of ints *)
let output_le_ints oc il =
  let is = Sys.word_size / 8 in
  let buf = String.create (is * List.length il) in
  OcpList.iteri (fun off i -> write_le_int buf (off * is) i) il;
  output oc buf 0 (String.length buf)

(* read a little-endian int from a string *)
let read_le_int buf off =
  let n = Char.code buf.[off]
    + ((Char.code buf.[off+1]) lsl 8)
    + ((Char.code buf.[off+2]) lsl 16)
    + ((Char.code buf.[off+3]) lsl 24) in
  if Sys.word_size = 64 then begin
    n
    + ((Char.code buf.[off+4]) lsl 32)
    + ((Char.code buf.[off+5]) lsl 40)
    + ((Char.code buf.[off+6]) lsl 48)
    + ((Char.code buf.[off+7]) lsl 56)
  end else
    n

(* read 1 int *)
let input_le_int ic =
  let buf = String.create (Sys.word_size / 8) in
  really_input ic buf 0 (String.length buf);
  read_le_int buf 0


(* Read n ints *)
let input_le_ints ic n =
  let is = Sys.word_size / 8 in
  let buf = String.create (n* is) in
  really_input ic buf 0 (String.length buf);
  let il = ref [] in
  for off = 0 to n-1 do
    il := read_le_int buf (off*is) :: !il
  done;
  List.rev !il

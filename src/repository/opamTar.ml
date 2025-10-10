(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Tar.Syntax

let rec safe_read fd buf off len =
  try Unix.read fd buf off len
  with Unix.Unix_error (Unix.EINTR, _, _) -> safe_read fd buf off len

let rec run : type a. Unix.file_descr -> (a, _, _) Tar.t -> a = fun fd -> function
  | Tar.Read len ->
      let b = Bytes.create len in
      let read = safe_read fd b 0 len in
      if read = 0 then
        failwith "unexpected end of file"
      else if len = (read : int) then
        Bytes.unsafe_to_string b
      else
        Bytes.sub_string b 0 read
  | Tar.Really_read len ->
      let rec loop fd buf offset len =
        if offset < (len : int) then
          let n = safe_read fd buf offset (len - offset) in
          if n = 0 then
            failwith "unexpected end of file"
          else
            loop fd buf (offset + n) len
      in
      let buf = Bytes.create len in
      loop fd buf 0 len;
      Bytes.unsafe_to_string buf
  | Tar.Return (Ok x) -> x
  | Tar.Return (Error _) -> failwith "something's gone wrong"
  | Tar.High _ | Tar.Write _ | Tar.Seek _ -> assert false
  | Tar.Bind (x, f) -> run fd (f (run fd x))

let fold_reg_files f acc fd =
  let go ?global:_ hdr acc =
    match hdr.Tar.Header.link_indicator with
    | Normal ->
      let* content = Tar.really_read (Int64.to_int hdr.file_size) in
      let acc = f acc hdr.file_name content in
      Tar.return (Ok acc)
    | Directory -> Tar.return (Ok acc)
    | Hard -> failwith "hardlinks unsupported"
    | Symbolic -> failwith "symlinks unsupported"
    | Character -> failwith "char devices unsupported"
    | Block -> failwith "block devices unsupported"
    | FIFO -> failwith "fifo unsupported"
    | GlobalExtendedHeader -> failwith "global extended header unsupported"
    | PerFileExtendedHeader -> failwith "perfile extended header unsupported"
    | LongLink -> failwith "longlinks unsupported"
    | LongName -> failwith "longnames unsupported"
  in
  run fd (Tar_gz.in_gzipped (Tar.fold go acc))

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

let fold_reg_files_aux f acc fd =
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

let fold_reg_files f acc fname =
  let fd = Unix.openfile (OpamFilename.to_string fname) [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  fold_reg_files_aux f acc fd

module Inplace = struct
  module Map = OpamStd.String.Map
  type t = Unix.file_descr * string Map.t

  let with_open_out fname f =
    let fd = Unix.openfile (OpamFilename.to_string fname) [Unix.O_RDWR] 0o640 in
    Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
    f (fd, fold_reg_files_aux (fun acc k x -> Map.add k x acc) Map.empty fd)

  let fold_reg_files f acc (_fd, t) =
    Map.fold (fun k x acc -> f acc k x) t acc

  let add ~fname ~content (fd, t) = (fd, Map.add fname content t)

  let remove fname (fd, t) = (fd, Map.remove fname t)

  let write (fd, t) =
    let to_buffer buf t =
      let rec run : type a. Buffer.t -> (a, 'err, _) Tar.t -> a = fun buf -> function
        | Tar.Write str -> Buffer.add_string buf str
        | Tar.Read _ | Tar.Really_read _ | Tar.Seek _ | Tar.High _ -> assert false
        | Tar.Return (Ok value) -> value
        | Tar.Return (Error _) -> failwith "something went wrong"
        | Tar.Bind (x, f) -> run buf (f (run buf x))
      in
      run buf t
    in
    let entries =
      let x =
        Map.fold (fun path content acc ->
            let hdr =
              Tar.Header.make ~file_mode:0 ~mod_time:0L ~user_id:0 ~group_id:0
                path (Int64.of_int (String.length content))
            in
            (Some Tar.Header.Ustar, hdr, fun () -> Tar.return (Ok (Some content))) :: acc)
          t []
      in
      let r = ref x in
      fun () ->
        match !r with
        | [] -> Tar.return (Ok None)
        | x::xs -> r := xs; Tar.return (Ok (Some x))
    in
    let t = Tar.out ~level:Ustar entries in
    let t = Tar_gz.out_gzipped ~level:4 ~mtime:0l Gz.Unix t in
    let buf = Buffer.create 10_485_760 in
    to_buffer buf t;
    let str = Buffer.contents buf in
    let _ : int = Unix.write_substring fd str 0 (String.length str) in
    ()
end

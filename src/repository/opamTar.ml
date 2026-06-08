(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025-2026 Kate Deplaix                                    *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open Tar.Syntax

type archive = filename
type archived_file = OpamFilename.Unix.t
type archived_file_content = string

let log ?level fmt = OpamConsole.log "TAR" ?level fmt

exception Tar of archive * string
let raise_error tar fmt =
  Printf.ksprintf (fun str -> raise (Tar (tar, str))) fmt

let rec safe_read fd buf off len =
  try Unix.read fd buf off len
  with Unix.Unix_error (Unix.EINTR, _, _) -> safe_read fd buf off len

let run archive =
  let raise_error fmt = raise_error archive fmt in
  let rec run : type a. Unix.file_descr -> (a, _, _) Tar.t -> a = fun fd -> function
    | Tar.Read len ->
      let len = Int64.to_int len in
      let b = Bytes.create len in
      let read = safe_read fd b 0 len in
      if read = 0 then
        raise_error "unexpected end of file"
      else if len = (read : int) then
        Bytes.unsafe_to_string b
      else
        Bytes.sub_string b 0 read
    | Tar.Really_read len ->
      let rec loop fd buf offset len =
        if offset < (len : int) then
          let n = safe_read fd buf offset (len - offset) in
          if n = 0 then
            raise_error "unexpected end of file"
          else
            loop fd buf (offset + n) len
      in
      let len = Int64.to_int len in
      let buf = Bytes.create len in
      loop fd buf 0 len;
      Bytes.unsafe_to_string buf
    | Tar.Return (Ok x) -> x
    | Tar.Return (Error e) ->
      raise_error "%s"
        (match e with
         | `Fatal e -> Format.asprintf "Fatal: %a" Tar.pp_error e
         | `Eof -> "EOF"
         | `Gz s -> "gz: "^s)
    | Tar.High _ | Tar.Write _ | Tar.Seek _ -> assert false
    | Tar.Bind (x, f) -> run fd (f (run fd x))
  in run

let fold_reg_files_aux archive f acc fd =
  let go ?global:_ hdr acc =
    match hdr.Tar.Header.link_indicator with
    | Normal ->
      let* content = Tar.really_read hdr.file_size in
      let acc = f acc (OpamFilename.Unix.of_string hdr.file_name) content in
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
  run archive fd (Tar_gz.in_gzipped (Tar.fold go acc))

let fold_reg_files f acc archive =
  let fd = Unix.openfile (OpamFilename.to_string archive) [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  fold_reg_files_aux archive f acc fd

module Inplace = struct
  module Map = OpamFilename.Unix.Map
  type t = {
    archive: archive;
    fd : Unix.file_descr;
    content : archived_file_content Map.t;
  }

  let with_open_out archive f =
    let fd = Unix.openfile (OpamFilename.to_string archive) [Unix.O_RDWR] 0o640 in
    Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
    f {
      archive = archive;
      fd;
      content =
        fold_reg_files_aux archive (fun acc k x -> Map.add k x acc) Map.empty fd
    }

  let fold_reg_files f acc t =
    Map.fold (fun k x acc -> f acc k x) t.content acc

  let exists fname t = Map.mem fname t.content

  let read fname t = Map.find fname t.content

  let add fname content t =
    { t with content = Map.add fname content t.content }

  let mv ~src ~dst t =
    let file_content = read src t in
    let content =
      Map.remove src t.content
      |> Map.add dst file_content
    in
    { t with content }

  let remove fname t =
    { t with content = Map.remove fname t.content }

  let remove_dir dname t =
    let content =
      Map.filter (fun fname _ ->
          not (OpamFilename.Unix.starts_with dname fname))
        t.content
    in
    { t with content }

  let write (t:t) =
    let to_buffer (buf:Buffer.t) tar =
      let rec run : type a. Buffer.t -> (a, 'err, _) Tar.t -> a = fun buf -> function
        | Tar.Write str ->
          Buffer.add_string buf str
        | Tar.Read _ | Tar.Really_read _ | Tar.Seek _ | Tar.High _ ->
          assert false
        | Tar.Return (Ok value) ->
          value
        | Tar.Return (Error e) ->
          raise_error t.archive "%s" (match e with | `Msg e -> e)
        | Tar.Bind (x, f) ->
          run buf (f (run buf x))
      in
      run buf tar
    in
    let entries =
      let dispenser =
        Map.to_seq t.content
        |> Seq.map (fun (path, content) ->
            let path = OpamFilename.Unix.to_string path in
            let hdr =
              Tar.Header.make ~file_mode:0o640 ~mod_time:0L ~user_id:0 ~group_id:0
                path (Int64.of_int (String.length content))
            in
            let data =
              let closed = ref false in
              fun () -> match !closed with
                | false -> closed := true; Tar.return (Ok (Some content))
                | true -> Tar.return (Ok None) in
            Some Tar.Header.Ustar, hdr, data)
        |> OpamCompat.Seq.to_dispenser
      in
      fun () ->
        match dispenser () with
        | None -> Tar.return (Ok None)
        | Some x -> Tar.return (Ok (Some x))
    in
    let tar = Tar.out ~level:Ustar entries in
    let tar = Tar_gz.out_gzipped ~level:4 ~mtime:0l Gz.Unix tar in
    let buf = Buffer.create 10_485_760 in
    to_buffer buf tar;
    let str = Buffer.contents buf in
    let _ : int = Unix.lseek t.fd 0 Unix.SEEK_SET in
    Unix.ftruncate t.fd 0;
    log ~level:3 "Writing archive %s" (OpamFilename.to_string t.archive);
    let _ : int = Unix.write_substring t.fd str 0 (String.length str) in
    ()

end

let create ?(flat=false) ?(except_vcs=false) tar dir =
  log "creating archive %s from %s"
    (OpamFilename.to_string tar)
    (OpamFilename.Dir.to_string dir);
  let fd =
    Unix.openfile (OpamFilename.to_string tar)
      [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o640
  in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let files = OpamFilename.rec_files ~except_vcs dir in
  let content =
    let remove_prefix =
      let dir =
        if flat then dir else OpamFilename.dirname_dir dir
      in
      OpamFilename.remove_prefix dir
    in
    List.fold_left (fun map f ->
        let k = OpamFilename.Unix.of_string (remove_prefix f) in
        Inplace.Map.add k (OpamFilename.read f) map)
      Inplace.Map.empty files
  in
  Inplace.write { archive = tar; fd; content }

module PatchFS = struct
  type root = OpamFilename.t
  module Tar = Inplace
  type file = OpamFilename.Unix.t
  type target = Tar.t
  let root_label = "archive"
  let translate_patch = false
  let root_to_string = OpamFilename.to_string
  let file_to_string = OpamFilename.Unix.to_string
  let equal_file = OpamFilename.Unix.equal
  let get_path ~fail _target file =
  let file = OpamFilename.Unix.of_string file in
    match OpamFilename.Unix.to_relative_canonical file with
    | Ok file -> file
    | Error _ -> fail (); file
  let on_unclean_accept _ _ = ()
  let on_unclean_reject _ _ _ = ()
  let write = Tar.add
  let exists = Tar.exists
  let exists_dir _file _target = false
  let read = Tar.read
  let remove = Tar.remove
  let remove_dir file target =
    Tar.remove_dir (OpamFilename.Unix.dirname file) target
  let same_dirname ~src ~dst =
    OpamFilename.Unix.Dir.equal
      (OpamFilename.Unix.dirname src)
      (OpamFilename.Unix.dirname dst)
  let mv = Tar.mv
  let open_ = Tar.with_open_out
  let save = Tar.write
end

let patch ~allow_unclean patch_source tar =
  OpamPatch.patch (module PatchFS) ~allow_unclean patch_source tar

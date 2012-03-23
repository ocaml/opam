(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open ExtString
open Namespace

let log fmt = Globals.log "RUN" fmt

let mkdir f f_to = 
  let rec aux f_to = 
    if not (Sys.file_exists f_to) then begin
      aux (Filename.dirname f_to);
      Unix.mkdir f_to 0o755;
    end in
  aux (Filename.dirname f_to);
  f f_to

let copy src dst =
  log "Copying %s to %s" src dst;
  let n = 1024 in
  let b = String.create n in
  let read = ref min_int in
  let ic = open_in_bin src in
  let oc =
    if Sys.file_exists dst then
      open_out_bin dst
    else
      let perm = (Unix.stat src).Unix.st_perm in
      mkdir (open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] perm) dst
  in
  while !read <>0 do
    read := input ic b 0 n;
    output oc b 0 !read;
  done;
  close_in ic;
  close_out oc

let read file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  s

(**************************)
(* from ocplib-system ... *)
(**************************)

let in_dir dir fn =
  let cwd = Unix.getcwd () in
  Unix.chdir dir;
  try
    let r = fn () in
    Unix.chdir cwd;
    r
  with e ->
    Unix.chdir cwd;
    raise e

let directories () =
  let d = Sys.readdir (Unix.getcwd ()) in
  let d = Array.to_list d in
  List.filter (fun f -> try Sys.is_directory f with _ -> false) d

let files () =
  let d = Sys.readdir (Unix.getcwd ()) in
  let d = Array.to_list d in
  List.filter (fun f -> try not (Sys.is_directory f) with _ -> true) d

let safe_unlink file =
  try Unix.unlink file
  with Unix.Unix_error _ -> ()

let rec safe_rmdir dir =
  if Sys.file_exists dir then begin
    in_dir dir (fun () ->
      let dirs = directories () in
      let files = files () in
      List.iter safe_unlink files;
      List.iter safe_rmdir dirs;
    );
    Unix.rmdir dir;
  end

let getchdir s = 
  let p = Unix.getcwd () in
  let () = Unix.chdir s in
  p

let rec root path =
  let d = Filename.dirname path in
  if d = path || d = "" || d = "." then
    path
  else
    root d

let tmp_dir = Filename.concat Filename.temp_dir_name "opam-archives"

let untar file nv =
  log "untar %s" file;
  let files = ref [] in
  let ic = Unix.open_process_in ("tar tf " ^ file) in
  (try while true do
      let line = input_line ic in
      if not (Filename.concat line "" = line) then
        (* the file is not a directory *)
        files := line :: !files;
    done;
   with _ -> ());
  log "%d files found: %s" (List.length !files) (String.concat ", " !files);
  let dirname = Namespace.string_of_nv (fst nv) (snd nv) in
  let aux name =
    if String.starts_with name dirname then
      Filename.concat tmp_dir name, name
    else
      let root = root name in
      let n = String.length root in
      let rest = String.sub name n (String.length name - n) in 
      Filename.concat tmp_dir name, dirname ^  rest in
  let moves = List.map aux !files in
  if not (Sys.file_exists tmp_dir) then
    Unix.mkdir tmp_dir 0o750;
  let err =
    if Filename.check_suffix file "tar.gz" then
      Sys.command (Printf.sprintf "tar xvfz %s -C %s" file tmp_dir)
    else if Filename.check_suffix file "tar.bz2" then
      Sys.command (Printf.sprintf "tar xvfj %s -C %s" file tmp_dir)
    else
      Globals.error_and_exit "%s is not a valid archive" file in
  List.iter (fun (src, dst) ->
    mkdir (copy src) dst
  ) moves;
  err

let download url =
  log "download %s" url;
  let command = match Globals.os with
    | Globals.Darwin -> "ftp"
    | _              -> "wget" in
  let dst = Filename.concat tmp_dir (Filename.basename url) in
  if not (Sys.file_exists tmp_dir) then
    Unix.mkdir tmp_dir 0o750;
  if Sys.file_exists dst then
    Unix.unlink dst;
  let err =
    in_dir tmp_dir (function () ->
      Sys.command (Printf.sprintf "%s %s" command url)
    ) in
  if err = 0 then
    Some dst
  else
    None

let patch dir patch =
  in_dir dir (function () ->
    Sys.command (Printf.sprintf "patch -p1 < %s" patch)
  )

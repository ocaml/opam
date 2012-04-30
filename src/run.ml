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
open ExtList
open Types
open Uri

let log fmt = Globals.log "RUN" fmt
let tmp_dir = Filename.concat Filename.temp_dir_name "opam-archives"

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

let write file contents =
  let oc = open_out file in
  output_string oc contents;
  close_out oc

(* the [Unix.wait] could return a processus which has not been
   created by [Unix.fork]. This part waits until a known pid is
   returned. *)
(* XXX: this will not work under windows *)
let wait map_pid = 
  let open BatMap in
      let rec aux () = 
        let pid, error = Unix.wait () in
        if IntMap.mem pid map_pid then
        pid, error
        else
        aux () in
      aux ()

(**************************)
(* from ocplib-system ... *)
(**************************)
let in_dir dir fn x =
  let cwd = Unix.getcwd () in
  Unix.chdir dir;
  try
    let r = fn x in
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
    ) ();
    Unix.rmdir dir;
  end
  
let safe_rm file =
  if Sys.file_exists file && Sys.is_directory file then
    safe_rmdir file
  else
    safe_unlink file
    
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
    
(* XXX: the function might block for ever for some channels kinds *)
let read_lines ic =
  let lines = ref [] in
  try while true do
      let line = input_line ic in
      if not (Filename.concat line "" = line) then
      lines := line :: !lines;
    done;
      !lines
  with _ ->
    !lines
      
let read_command_output_ cmd =
  let ic = Unix.open_process_in cmd in
  let lines = read_lines ic in
  if Unix.close_process_in ic <> Unix.WEXITED 0 then
  None
  else
  Some lines
    
let read_command_output cmd =
  match read_command_output_ cmd with
  | None -> Globals.error_and_exit "command %s failed" cmd
  | Some lines -> lines

let normalize s =
  if Sys.file_exists s then
    getchdir (getchdir s)
  else
    s

let real_path p =
  let dir = Filename.dirname p in
  let dir = normalize dir in
  let base = Filename.basename p in
  let (/) = Filename.concat in
  if Filename.is_relative dir then
  (Sys.getcwd ()) / dir / base
  else
  dir / base
 
let add_path bins = 
  let path = ref "<not set>" in
  let env = Unix.environment () in
  for i = 0 to Array.length env - 1 do
    let k,v = String.split env.(i) "=" in
    if k = "PATH" then
    let new_path = match List.filter Sys.file_exists bins with
      | [] -> v
      | l  -> String.concat ":" l ^ ":" ^ v in
    env.(i) <- "PATH=" ^ new_path;
    path := new_path;
  done;
  env, !path
  
let command_with_path bins fmt =
  Printf.kprintf (fun cmd ->
    let env, path = add_path bins in 
    log "cwd=%s path=%s %s" (Unix.getcwd ()) path cmd;
    let (o,i,e as chans) = Unix.open_process_full cmd env in
    (* we MUST read the input_channels otherwise [close_process] will fail *)
    let err = read_lines e in
    let out = read_lines o in
    let str () = Printf.sprintf "out: %s\nerr: %s" (String.concat "\n" out) (String.concat "\n" err) in
    let msg () = Globals.msg "%s\n" (str ()) in
    match Unix.close_process_full chans with
      | Unix.WEXITED 0 -> 0
      | Unix.WEXITED i -> msg (); i
      | _              -> msg (); 1
  ) fmt
 
let command fmt =
  Printf.kprintf (fun str ->
    log "cwd=%s '%s'" (Unix.getcwd ()) str;
    Sys.command str;
  ) fmt
  
(* not used anymore *)
(*
let clone cmd repo cwd nv =
  let b_name = Filename.chop_extension (Filename.basename repo) in
  let dst_git = Filename.concat tmp_dir b_name in
  log "cloning %s into %s" repo dst_git;
  if Sys.file_exists dst_git then
    safe_rm dst_git;
  let err = cmd repo b_name in
  if err = 0 then
    let s_from = Filename.concat (Unix.getcwd ()) b_name in
    let s_to = Filename.concat cwd (Namespace.string_of_nv (fst nv) (snd nv)) in
    (* XXX: mv -i will surely not work on windows *)
    if command "mv -i %s %s" s_from s_to = 0 then
      Folder s_to
    else
      Globals.error_and_exit "moving failed"
  else
    Globals.error_and_exit "cloning failed"
*)

let fold_0 f = List.fold_left (function 0 -> f | err -> fun _ -> err) 0

let commands = fold_0 (command "%s")
let commands_with_path bins = fold_0 (command_with_path bins "%s")

let is_archive file =
  List.fold_left
    (function
      | Some s -> fun _ -> Some s
      | None -> fun (ext, c) -> 
        if List.exists (Filename.check_suffix file) ext then
          Some (Printf.kprintf Sys.command "tar xvf%c %s -C %s" c file)
        else
          None)
    None
    [ [ "tar.gz" ; "tgz" ], 'z'
    ; [ "tar.bz2" ; "tbz" ], 'j' ]

let untar file nv =
  log "untar %s" file;
  let files = read_command_output ("tar tf " ^ file) in
  log "%d files found: %s" (List.length files) (String.concat ", " files);
  let dirname = NV.to_string nv in
  let aux name =
    if String.starts_with name dirname then
      Filename.concat tmp_dir name, name
    else
      let root = root name in
      let n = String.length root in
      let rest = String.sub name n (String.length name - n) in 
      Filename.concat tmp_dir name, dirname ^  rest in
  let moves = List.map aux files in
  if not (Sys.file_exists tmp_dir) then
    Unix.mkdir tmp_dir 0o750;
  let err =
    match is_archive file with
      | Some f_cmd -> f_cmd tmp_dir
      | None -> Globals.error_and_exit "%s is not a valid archive" file in
  List.iter (fun (src, dst) ->
    mkdir (copy src) dst
  ) moves;
  err

(* Unused for now on *)
(*
let download_aux uri dst = 
  let aux cmd =
    let file = Uri.to_string uri in
    let path = Uri.path uri in
    log "download %s" file;
    let dst = Filename.concat tmp_dir (Filename.basename path) in
    if Sys.file_exists dst then
      safe_rm dst;
    if command "%s %s" cmd file = 0 then
      dst
    else
      Globals.error_and_exit "download failed" in
  match Uri.scheme, Globals.os with
    | ("http" | "https"), Globals.Darwin -> aux "ftp" 
    | ("http" | "https"), _              -> aux "wget"
    | s, _ -> Globals.error_and_exit "unknown uri scheme: %s" s

let download url nv =
  if not (Sys.file_exists tmp_dir) then
    Unix.mkdir tmp_dir 0o750;
  in_dir tmp_dir (fun s -> download_aux url s nv) (Unix.getcwd ())

let patch p nv =
  let dirname = Namespace.string_of_nv (fst nv) (snd nv) in
  log "patching %s using %s" dirname p;
  in_dir dirname (fun () ->
    System.Command.one "patch -p1 -f -i %s" p
  ) ()
*)












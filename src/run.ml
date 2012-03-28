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
open Uri

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

let read_command_output cmd =
  let lines = ref [] in
  let ic = Unix.open_process_in cmd in
  try while true do
    let line = input_line ic in
    if not (Filename.concat line "" = line) then
      lines := line :: !lines;
    done;
    close_in ic;
    !lines
  with _ ->
    close_in ic;
    !lines

let tmp_dir = Filename.concat Filename.temp_dir_name "opam-archives"

let normalize s =
  if Sys.file_exists s then
    getchdir (getchdir s)
  else
    s

let untar file nv =
  log "untar %s" file;
  let files = read_command_output ("tar tf " ^ file) in
  log "%d files found: %s" (List.length files) (String.concat ", " files);
  let dirname = Namespace.string_of_nv (fst nv) (snd nv) in
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

let sys_command fmt =
  Printf.kprintf (fun str ->
    log "cwd=%s '%s'" (Unix.getcwd ()) str;
    Sys.command str;
  ) fmt

let sys_commands = 
  List.fold_left (function 0 -> sys_command "%s" | err -> fun _ -> err) 0

(* Git wrappers *)
module Git = struct

  (* Init a git repository in [dirname] *)
  let init dirname =
    in_dir dirname (fun () ->
      let (_ : int) = sys_command "git init" in
      ()
    ) ()

  (* tentative to build a unique branch name from a repo name *)
  (*  ':' is forbidden and it cannot start by '/'  *)
  let remote_name url =
    let name = "R" ^ snd (uri_of_url url) in
    for i = 0 to String.length name - 1 do
      if name.[i] = ':' then
        name.[i] <- 'T';
    done;
    name

  (* Add a remote url in dirname *)
  let remote_add dirname url =
    in_dir dirname (fun () ->
      sys_command "git remote add %s %s" (remote_name url) url
    ) ()

  (* internal command *)
  let get_remotes () = read_command_output "git remote" 

  let safe_remote_add dirname url =
    in_dir dirname (fun () ->
      if not (List.mem (remote_name url) (get_remotes ())) then
        if remote_add dirname url <> 0 then
          Globals.error_and_exit "%s is not a valid git repository" dirname
    ) ()

  (* Return the list of modified files of the git repository located
     at [dirname] *)
  let get_updates dirname =
    in_dir dirname (fun () ->
      let fetches = List.map (Printf.sprintf "git fetch %s") (get_remotes ()) in
      let diff remote =
        read_command_output (Printf.sprintf "git diff remotes/%s/master --name-only" remote) in
      if sys_commands fetches = 0 then
        List.flatten (List.map diff (get_remotes ()))
      else
        Globals.error_and_exit "Cannot fetch git repository %s" dirname
    ) ()

  (* Update the git repository located at [dirname] *)
  let update dirname =
    in_dir dirname (fun () ->
      let commands = List.map (Printf.sprintf "git pull %s master") (get_remotes ()) in
      if sys_commands commands <> 0 then
        Globals.error_and_exit "Cannot update git repository %s" dirname
    ) ()

  (* Clone [repo] into the directory [dst] *)
  let clone repo dst =
    sys_command "git clone %s %s" repo dst
 
end

type download_result = 
  | From_http of string (* file *)
  | From_git
  | Url_error

let clone repo last_pwd nv =
  if Filename.check_suffix repo "git" then
    let b_name = Filename.chop_extension (Filename.basename repo) in
    let dst_git = Filename.concat tmp_dir b_name in
    log "cloning %s into %s" repo dst_git;
    if Sys.file_exists repo then
      safe_rm repo;
    let err = Git.clone repo b_name in
    if err = 0 then
      let s_from = Printf.sprintf "%s/%s" (Unix.getcwd ()) b_name in
      let s_to = Printf.sprintf "%s/%s" last_pwd (Namespace.string_of_nv (fst nv) (snd nv)) in
      if sys_command "mv -i %s %s" s_from s_to = 0 then
        From_git
      else
        Globals.error_and_exit "moving failed"
    else
      Globals.error_and_exit "cloning failed"
  else
    Globals.error_and_exit "Cannot infer the VCS type of %s" repo

let exec_download = 
  let http s_wget url _ _ = 
    log "download %s" url;
    let dst = Filename.concat tmp_dir (Filename.basename url) in
    if Sys.file_exists dst then
      safe_rm dst;
    if sys_command "%s %s" s_wget url = 0 then
      From_http dst
    else
      Url_error in
  function
  | Http, url ->
      (match Globals.os with
      | Globals.Darwin -> http "fpt"  url
      | _              -> http "wget" url)
  | Git, repo       -> clone repo
  | Config, _
  | Install, _
  | Local, _ 
  | Ocp, _ -> assert false

let download url nv =
  if not (Sys.file_exists tmp_dir) then
    Unix.mkdir tmp_dir 0o750;
  in_dir tmp_dir (fun s -> exec_download url s nv) (Unix.getcwd ())

let patch p nv =
  let dirname = Namespace.string_of_nv (fst nv) (snd nv) in
  log "patching %s using %s" dirname p;
  in_dir dirname (fun () ->
    sys_command "patch -p1 -f -i %s" p
  ) ()

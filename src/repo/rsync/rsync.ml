let (+) = Filename.concat

module F = Filename
open Repo_helpers
open Types

let log fmt = Globals.log "rsync" fmt

(* if rsync -arv return 4 lines, this means that no files have changed *)
let trim = function
  | [] -> []
  | _ :: t ->
      match List.rev t with
      | _ :: _ :: _ :: l -> l
      | _ -> []

let rsync ?(delete=true) src dst =
  log "rsync: delete:%b src:%s dst:%s" delete src dst;
  Run.mkdir src;
  Run.mkdir dst;
  let delete = if delete then ["--delete"] else [] in
  match
    Run.read_command_output (["rsync" ; "-arv"; src; dst] @ delete)
  with
  | None       -> []
  | Some lines ->
      let lines = trim lines in
      List.iter (fun f -> log "updated: %s %s" (Run.cwd ()) f) lines;
      lines

let rsync_dirs ?delete src dst =
  let src_s = Dirname.to_string src + "" in
  let dst_s = Dirname.to_string dst in
  let dst_files0 = Filename.rec_list dst in
  let lines = rsync ?delete src_s dst_s in
  let src_files = Filename.rec_list src in
  let dst_files = Filename.rec_list dst in
  if delete = Some true && List.length src_files <> List.length dst_files then (
    List.iter (fun f -> Globals.msg "src-file: %s\n" (Filename.to_string f)) src_files;
    List.iter (fun f -> Globals.msg "dst-file0: %s\n" (Filename.to_string f)) dst_files0;
    List.iter (fun f -> Globals.msg "dst-file: %s\n" (Filename.to_string f)) dst_files;
    Globals.error_and_exit "rsync_dir failed!"
  );
  Filename.Set.of_list (List.map Filename.of_string lines)

let rsync_file src dst =
  (* We assume that we rsync locally *)
  Filename.copy src dst

module Repo = struct

  type t = unit

  let (++) = Filename.Set.union

  let sync state =
    let aux fn = rsync_dirs ~delete:true (fn state.remote_repo) (fn state.local_repo) in
    aux Path.R.packages_dir ++ aux Path.R.compilers_dir

  let upload state remote_dir =
    log "Upload.dir %s" (Dirname.to_string remote_dir);
    let local_dir = Repo_helpers.local_of_remote_dir state remote_dir in
    (* we assume that rsync is only used locally *)
    if Dirname.exists (Dirname.dirname remote_dir)
    && not (Dirname.exists remote_dir) then
      Dirname.mkdir remote_dir;
    if Dirname.exists local_dir then
      rsync_dirs ~delete:false local_dir remote_dir
    else
      Filename.Set.empty

end    

module M = Repo_helpers.Make(Repo)
include M

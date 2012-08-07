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

let rsync_file state remote_file local_dir =
  log "rsync_file: remote-file:%s local-dir%s"
    (Filename.to_string remote_file)
    (Dirname.to_string local_dir);
  Dirname.mkdir local_dir;
  match
    Dirname.in_dir local_dir (fun () ->
      Run.read_command_output [
        "rsync" ; "-arv"; Filename.to_string remote_file ; "."
      ]
    )
  with
  | None       -> None
  | Some lines ->
      let local_file = Repo_helpers.local_of_remote_file state remote_file in
      match trim lines with
      | []  -> Some (local_file, false)
      | [x] -> Some (local_file, true)
      | _   -> failwith "This could happen if rsync ouput format is \
                           not similar on the different platforms"

let rsync_dir ?(delete=true) remote_dir local_dir =
  log "rsync_dir: delete:%b remote-dir:%s local-dir%s"
    delete
    (Dirname.to_string remote_dir)
    (Dirname.to_string local_dir);
  Dirname.mkdir local_dir;
  let delete = if delete then "--delete" else "" in
  match
    Run.read_command_output [
      "rsync" ; "-arv"; delete;
      Dirname.to_string remote_dir;
      Dirname.to_string (Dirname.dirname local_dir)
    ]
  with
  | None       -> []
  | Some lines ->
      let lines = trim lines in
      List.iter (fun f -> log "rsync_dir-files: %s %s" (Run.cwd ()) f) lines;
      lines

module Repo = struct

  type t = unit

  let file state remote_file =
    let local_file = Repo_helpers.local_of_remote_file state remote_file in
    log "Sync.file %s with %s" (Filename.to_string remote_file) (Filename.to_string local_file);
    match rsync_file state remote_file (Filename.dirname local_file) with
    | Some (f,true) -> Some f
    | _             -> None

  let dir state remote_dir =
    log "Sync.dir %s" (Dirname.to_string remote_dir);
    let local_dir = Repo_helpers.local_of_remote_dir state remote_dir in
    let lines = rsync_dir ~delete:true remote_dir local_dir in
    let set =
      Filename.Set.of_list (List.map Filename.of_string lines) in
    Filename.Set.iter (fun f -> log "found %s" (Filename.to_string f)) set;
    set

  let (++) = Filename.Set.union

  let sync state =
    let archives =
      Filename.Set.filter (fun f -> 
        let remote_file = remote_of_local_file state f in
        file state remote_file <> None
      ) (Path.R.available_archives state.local_repo) in
    let sync fn = dir state (fn state.remote_repo) in
       archives
    ++ sync Path.R.packages_dir
    ++ sync Path.R.compilers_dir

  let upload state remote_dir =
    log "Upload.dir %s" (Dirname.to_string remote_dir);
    let local_dir = Repo_helpers.local_of_remote_dir state remote_dir in
    if Dirname.exists local_dir then
      Filename.Set.of_list
        (List.map Filename.of_string (rsync_dir ~delete:false local_dir remote_dir))
    else
      Filename.Set.empty

end    

module M = Repo_helpers.Make(Repo)
include M

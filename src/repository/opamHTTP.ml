(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStd.Op
open OpamProcess.Job.Op

let log msg = OpamConsole.log "CURL" msg
let slog = OpamConsole.slog

type state = {
  local_remote : (url * int * OpamHash.t) filename_map;
  (* map of local files to address, perms, md5 *)
  remote_local : filename OpamUrl.Map.t;
  (* reverse map of addresses to local files *)
}

let state_cache = ref []

let index_file = "urls.txt"
let index_file_new = "urls.txt.1"
let index_archive = "index.tar.gz"

let local_files repo =
  let all =
    (OpamFile.filename (OpamRepositoryPath.repo repo)) ::
      OpamFilename.rec_files (OpamRepositoryPath.packages_dir repo) @
      OpamFilename.rec_files (OpamRepositoryPath.archives_dir repo) in
  List.filter OpamFilename.exists all

let as_index_file f = (OpamFile.make f : file_attribute_set OpamFile.t)

(* Generate urls.txt (for opam-admin, or rebuilding if interrupted during
   update *)
let rebuild_local_state ~write repo =
  let local_index_file =
    as_index_file OpamFilename.Op.(repo.repo_root // index_file)
  in
  log "Rebuilding urls.txt at %a" (slog OpamFilename.Dir.to_string)
    repo.repo_root;
  let index =
    List.fold_left (fun set f ->
      if not (OpamFilename.exists f) then set
      else
        let attr = OpamFilename.to_attribute repo.repo_root f in
        OpamFilename.Attribute.Set.add attr set
    ) OpamFilename.Attribute.Set.empty (local_files repo)
  in
  if write then OpamFile.File_attributes.write local_index_file index;
  index

let state_of_index_file repo index_file =
  OpamFilename.Attribute.Set.fold (fun r state ->
      let base = OpamFilename.Attribute.base r in
      let perm = match OpamFilename.Attribute.perm r with
        | None  ->  0o640
        | Some p -> p in
      let digest = OpamFilename.Attribute.md5 r in
      let remote =
        OpamUrl.Op.(repo.repo_url / OpamFilename.Base.to_string base)
      in
      let local = OpamFilename.create repo.repo_root base in
      { remote_local =
          OpamUrl.Map.add remote local state.remote_local;
        local_remote =
          OpamFilename.Map.add local (remote,perm,digest) state.local_remote; }
    )
    index_file
    { local_remote = OpamFilename.Map.empty;
      remote_local = OpamUrl.Map.empty; }

let get_state repo =
  try List.assoc repo.repo_url !state_cache with Not_found ->
    let urls =
      try OpamFile.File_attributes.read
            (as_index_file OpamFilename.Op.(repo.repo_root // index_file))
      with e -> OpamStd.Exn.fatal e; rebuild_local_state ~write:true repo
    in
    let state = state_of_index_file repo urls in
    state_cache := (repo.repo_url, state) :: !state_cache;
    state

let sync_state repo =
  let old_state = get_state repo in
  let remote_index_file = OpamUrl.Op.(repo.repo_url / index_file) in
  let remote_index_archive = OpamUrl.Op.(repo.repo_url / index_archive) in
  let index_file =
    as_index_file OpamFilename.Op.(repo.repo_root // index_file) in
  let index_file_new =
    as_index_file OpamFilename.Op.(repo.repo_root // index_file_new) in
  let index_archive = OpamFilename.Op.(repo.repo_root // index_archive) in
  OpamDownload.download_as ~compress:true ~overwrite:true
    remote_index_file (OpamFile.filename index_file_new)
  @@+ fun () ->
  let urls = OpamFile.File_attributes.read index_file_new in
  let new_state = state_of_index_file repo urls in
  let changed_files =
    OpamFilename.Map.merge (fun _ oldf newf -> match oldf, newf with
        | Some (_,_,oldck), Some (rem,_,newck) ->
          if oldck = newck then None else Some (Some rem)
        | _, None -> Some None
        | None, Some (rem,_,_) -> Some (Some rem))
      old_state.local_remote
      new_state.local_remote
  in
  log "sync repo state: %a file changes"
    (slog @@ string_of_int @* OpamFilename.Map.cardinal)
    changed_files;
  (* Remove the index before making changes, so that it'll be consistently
     rebuilt in case of interruption *)
  OpamFilename.remove (OpamFile.filename index_file);
  (if OpamFilename.Map.cardinal changed_files > 4 then (
      log "-> downloading the full archive";
      OpamProcess.Job.catch
        (fun e ->
           OpamStd.Exn.fatal e;
           OpamConsole.msg
             "Cannot find index.tar.gz on the OPAM repository. \
              Initialisation may take some time.\n";
           Done false) @@
      OpamDownload.download_as ~overwrite:true
        remote_index_archive index_archive
      @@+ fun () ->
      OpamFilename.Map.iter (fun f _ ->
          if OpamFilename.Map.mem f old_state.local_remote
          then OpamFilename.remove f)
        changed_files;
      OpamFilename.extract_in index_archive repo.repo_root;
      Done true
    ) else
     Done false)
  @@+ fun got_archive ->
  (if not got_archive then
     OpamFilename.Map.fold (fun f remote job ->
         match remote with
         | None -> OpamFilename.remove f; job
         | Some remote ->
           job @@+ fun () ->
           OpamDownload.download_as ~overwrite:true remote f)
       changed_files (Done ())
   else Done ())
  @@+ fun () ->
  OpamFilename.move
    ~src:(OpamFile.filename index_file_new)
    ~dst:(OpamFile.filename index_file);
  state_cache := (repo.repo_url, new_state) ::
                 (List.remove_assoc repo.repo_url !state_cache);
  OpamConsole.msg "[%s] synchronized from %s\n"
    (OpamConsole.colorise `blue
       (OpamRepositoryName.to_string repo.repo_name))
    (OpamUrl.to_string repo.repo_url);
  Done ()

let is_up_to_date state local_file =
  try
    let _,_,md5 = OpamFilename.Map.find local_file state.local_remote in
    OpamFilename.exists local_file &&
    not (Sys.is_directory (OpamFilename.to_string local_file))
    && OpamHash.check_file (OpamFilename.to_string local_file) md5
  with Not_found -> false
(* XXX unused ?
let get_checksum state local_file =
  try
    let _,_,expected = OpamFilename.Map.find local_file state.local_remote in
    if OpamFilename.exists local_file &&
       not (Sys.is_directory (OpamFilename.to_string local_file))
    then
      let actual = OpamFilename.digest local_file in
      Some (actual, expected)
    else None
  with Not_found -> None
*)
module B = struct

  let name = `http
(*
  let repo repo_root repo_address = {
    repo_name     = OpamRepositoryName.of_string "http";
    repo_priority = 0;
    repo_kind     = `http;
    repo_root; repo_address;
  }
*)
  let pull_repo repo =
    log "pull-repo";
    sync_state repo

  let pull_url package dirname checksum remote_url =
    log "pull-file into %a: %a"
      (slog OpamFilename.Dir.to_string) dirname
      (slog OpamUrl.to_string) remote_url;
    let local_file =
      OpamFilename.create dirname
        (OpamFilename.Base.of_string (OpamUrl.basename remote_url))
    in
    let check_sum f = match checksum with
      | None   -> false
      | Some c -> OpamHash.check_file (OpamFilename.to_string f) c
    in
    let files = OpamFilename.files dirname in
    let uptodate =
      let found, _extra =
        List.partition (fun f -> f = local_file && check_sum f) files
      in
      (* For removals, multiple versions of the same pkg may be needed.
         This breaks this case by removing what was just downloaded.
         !X fixme: if the package was switched from e.g. http to git this may
         still be broken, it would be best to have per-version dirs
      if extra <> [] then
        (log "Removing stale files in download dir: %a"
           (slog @@ List.map OpamFilename.to_string @>
                    OpamStd.Format.pretty_list ?last:None)
           extra;
         List.iter OpamFilename.remove extra);
      *)
      found <> []
    in
    if uptodate then Done (Result (F local_file))
    else
    OpamProcess.Job.catch
      (fun e ->
         OpamStd.Exn.fatal e;
         let msg = match e with
           | Failure msg ->
             Printf.sprintf "%s (%s)" (OpamUrl.to_string remote_url) msg
           | _ -> OpamUrl.to_string remote_url
         in
         Done (Not_available msg)) @@
    OpamDownload.download ~overwrite:true ?checksum remote_url dirname
    @@+ fun local_file ->
    if OpamRepositoryBackend.check_digest local_file checksum then
      (OpamConsole.msg "[%s] %s downloaded\n"
         (OpamConsole.colorise `green (OpamPackage.to_string package))
         (OpamUrl.to_string remote_url);
       Done (Result (F local_file)))
    else
      (OpamFilename.remove local_file;
       Done (Not_available (OpamUrl.to_string remote_url)))

  let pull_archive repo url =
    log "pull-archive";
    let state = get_state repo in
    match OpamUrl.Map.find_opt url state.remote_local with
    | None -> Done (Not_available (OpamUrl.to_string url))
    | Some local_file ->
    if is_up_to_date state local_file then
      Done (Up_to_date local_file)
    else
      OpamProcess.Job.catch
        (function
          | Not_found ->
            Done (Not_available (OpamUrl.to_string url))
          | e -> raise e) @@
      OpamDownload.download_as ~overwrite:true url local_file
      @@+ fun () ->
      OpamConsole.msg "[%s] %s downloaded\n"
        (OpamConsole.colorise `blue
           (OpamRepositoryName.to_string repo.repo_name))
        (OpamUrl.to_string url);
      Done (Result local_file)

  let revision _ =
    Done None

end

(* Helper functions used by opam-admin *)

let make_urls_txt ~write repo_root =
  rebuild_local_state ~write {
    repo_name = OpamRepositoryName.of_string "local";
    repo_root;
    repo_url = OpamUrl.empty;
    repo_priority = 0;
  }

let make_index_tar_gz repo_root =
  OpamFilename.in_dir repo_root (fun () ->
    let dirs = [ "version"; "compilers"; "packages"; "repo" ] in
    match List.filter Sys.file_exists dirs with
    | [] -> ()
    | d  -> OpamSystem.command ("tar" :: "czhf" :: "index.tar.gz" :: d)
  )

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamMisc.OP
open OpamFilename.OP
open OpamProcess.Job.Op

let log msg = OpamGlobals.log "CURL" msg
let slog = OpamGlobals.slog

type state = {
  local_remote : (filename * int * string) filename_map;
  (* map of local files to address, perms, md5 *)
  remote_local : filename filename_map;
  (* reverse map of addresses to local files *)
}

let state_cache = ref []

let make_index_file path = path // "urls.txt"
let make_index_file_new local_path = local_path // "urls.txt.1"
let make_index_archive path = path // "index.tar.gz"

let local_files repo =
  let all =
    OpamPath.Repository.repo repo ::
      OpamFilename.rec_files (OpamPath.Repository.packages_dir repo) @
      OpamFilename.rec_files (OpamPath.Repository.archives_dir repo) @
      OpamFilename.rec_files (OpamPath.Repository.compilers_dir repo) in
  List.filter OpamFilename.exists all

(* Generate urls.txt (for opam-admin, or rebuilding if interrupted during
   update *)
let rebuild_local_state ~write repo =
  let local_index_file = make_index_file repo.repo_root in
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
  let repo_address = OpamFilename.raw_dir (fst repo.repo_address) in
  OpamFilename.Attribute.Set.fold (fun r state ->
      let base = OpamFilename.Attribute.base r in
      let perm = match OpamFilename.Attribute.perm r with
        | None  ->  0o640
        | Some p -> p in
      let digest = OpamFilename.Attribute.md5 r in
      let remote = repo_address // OpamFilename.Base.to_string base in
      let local = OpamFilename.create repo.repo_root base in
      { remote_local =
          OpamFilename.Map.add remote local state.remote_local;
        local_remote =
          OpamFilename.Map.add local (remote,perm,digest) state.local_remote; }
    )
    index_file
    { local_remote = OpamFilename.Map.empty;
      remote_local = OpamFilename.Map.empty; }

let get_state repo =
  try List.assoc repo.repo_address !state_cache with Not_found ->
    let urls =
      try OpamFile.File_attributes.read (make_index_file repo.repo_root)
      with e -> OpamMisc.fatal e; rebuild_local_state ~write:true repo
    in
    let state = state_of_index_file repo urls in
    state_cache := (repo.repo_address, state) :: !state_cache;
    state

let preload_state repo =
  ignore (get_state repo)

let sync_state repo =
  let old_state = get_state repo in
  let repo_address = OpamFilename.raw_dir (fst repo.repo_address) in
  let remote_index_file = make_index_file repo_address in
  let remote_index_archive = make_index_archive repo_address in
  let index_file = make_index_file repo.repo_root in
  let index_file_new = make_index_file_new repo.repo_root in
  let index_archive = make_index_archive repo.repo_root in
  OpamFilename.download_as ~compress:true ~overwrite:true
    remote_index_file index_file_new
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
  OpamFilename.remove index_file;
  (if OpamFilename.Map.cardinal changed_files > 4 then (
      log "-> downloading the full archive";
      OpamProcess.Job.catch
        (fun e ->
           OpamMisc.fatal e;
           OpamGlobals.msg
             "Cannot find index.tar.gz on the OPAM repository. \
              Initialisation may take some time.\n";
           Done false) @@
      OpamFilename.download_as ~overwrite:true
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
           OpamFilename.download_as ~overwrite:true remote f)
       changed_files (Done ())
   else Done ())
  @@+ fun () ->
  OpamFilename.move ~src:index_file_new ~dst:index_file;
  state_cache := (repo.repo_address, new_state) ::
                 (List.remove_assoc repo.repo_address !state_cache);
  OpamGlobals.msg "[%s] synchronized from %s\n"
    (OpamGlobals.colorise `blue
       (OpamRepositoryName.to_string repo.repo_name))
    (OpamTypesBase.string_of_address repo.repo_address);
  Done ()

let is_up_to_date state local_file =
  try
    let _,_,md5 = OpamFilename.Map.find local_file state.local_remote in
    OpamFilename.exists local_file &&
    not (Sys.is_directory (OpamFilename.to_string local_file))
    && md5 = OpamFilename.digest local_file
  with Not_found -> false

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

module B = struct

  let repo repo_root repo_address = {
    repo_name     = OpamRepositoryName.of_string "http";
    repo_priority = 0;
    repo_kind     = `http;
    repo_root; repo_address;
  }

  let pull_repo repo =
    log "pull-repo";
    sync_state repo

  let pull_url package dirname checksum remote_url =
    let remote_url = OpamTypesBase.string_of_address remote_url in
    log "pull-file into %a: %s"
      (slog OpamFilename.Dir.to_string) dirname
      remote_url;
    let filename = OpamFilename.of_string remote_url in
    let base = OpamFilename.basename filename in
    let local_file = OpamFilename.create dirname base in
    let check_sum f = match checksum with
      | None   -> false
      | Some c -> OpamFilename.digest f = c
    in
    let files = OpamFilename.files dirname in
    let uptodate =
      let found, extra =
        List.partition (fun f -> f = local_file && check_sum f) files
      in
      if extra <> [] &&
         OpamMisc.starts_with (* Just a safeguard *)
           ~prefix:(OpamFilename.Dir.to_string (OpamPath.root ()))
           (OpamFilename.Dir.to_string dirname)
      then
        (log "Removing stale files in download dir: %a"
           (slog @@ List.map OpamFilename.to_string @> OpamMisc.pretty_list)
           extra;
         List.iter OpamFilename.remove extra);
      found <> []
    in
    if uptodate then Done (Result (F local_file))
    else
    OpamProcess.Job.catch
      (fun e -> OpamMisc.fatal e; Done (Not_available remote_url)) @@
    OpamFilename.download ~overwrite:true filename dirname
    @@+ fun local_file ->
    if OpamRepository.check_digest local_file checksum then
      (OpamGlobals.msg "[%s] %s downloaded\n"
         (OpamGlobals.colorise `green (OpamPackage.to_string package))
         (OpamFilename.to_string filename);
       Done (Result (F local_file)))
    else
      (OpamFilename.remove local_file;
       Done (Not_available remote_url))

  let pull_archive repo filename =
    log "pull-archive";
    let state = get_state repo in
    let local_file = OpamFilename.Map.find filename state.remote_local in
    if is_up_to_date state local_file then
      Done (Up_to_date local_file)
    else
      OpamProcess.Job.catch
        (function
          | Not_found ->
            Done (Not_available (OpamFilename.to_string filename))
          | e -> raise e) @@
      OpamFilename.download_as ~overwrite:true filename local_file
      @@+ fun () ->
      OpamGlobals.msg "[%s] %s downloaded\n"
        (OpamGlobals.colorise `blue
           (OpamRepositoryName.to_string repo.repo_name))
        (OpamFilename.prettify filename);
      Done (Result local_file)

  let revision _ =
    Done None

end

let make_urls_txt ~write repo_root =
  rebuild_local_state ~write (OpamRepository.local repo_root)

let make_index_tar_gz repo_root =
  OpamFilename.in_dir repo_root (fun () ->
    let dirs = [ "version"; "compilers"; "packages"; "repo" ] in
    match List.filter Sys.file_exists dirs with
    | [] -> ()
    | d  -> OpamSystem.command ("tar" :: "czf" :: "index.tar.gz" :: d)
  )

let register () =
  OpamRepository.register_backend `http (module B : OpamRepository.BACKEND)

(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open OpamTypes
open OpamFilename.OP

let log msg = OpamGlobals.log "CURL" msg

type state = {
  remote_dir          : dirname;
  local_dir           : dirname;
  remote_index_archive: filename;
  local_index_archive : filename;
  local_files         : filename_set;
  remote_local        : filename filename_map;
  local_remote        : filename filename_map;
  file_permissions    : (filename * int) list;
  file_digests        : (filename * string) list;
}

let state_cache = ref []

let index_file local_path = local_path // "urls.txt"
let index_file_save local_path = local_path // "urls.txt.0"
let index_archive local_path = local_path // "index.tar.gz"

let local_files repo =
  OpamPath.Repository.version repo ::
  OpamFilename.rec_files (OpamPath.Repository.packages_dir repo) @
  OpamFilename.rec_files (OpamPath.Repository.archives_dir repo) @
  OpamFilename.rec_files (OpamPath.Repository.compilers_dir repo)

let make_state ~download_index remote_dir =
  if List.mem_assoc remote_dir !state_cache then
    List.assoc remote_dir !state_cache
  else (
    let local_dir = OpamFilename.cwd () in
    let remote_index_file = remote_dir // "urls.txt" in
    let local_index_file = index_file local_dir in
    let local_index_file_save = index_file_save local_dir in
    let remote_index_archive = remote_dir // "index.tar.gz" in
    let local_index_archive = local_dir // "index.tar.gz" in
    let index =
      if download_index then (
        if OpamFilename.exists local_index_file then
          OpamFilename.move ~src:local_index_file ~dst:local_index_file_save;
        try
          let file =
            OpamFilename.download ~overwrite:false remote_index_file local_dir in
          OpamFilename.remove local_index_file_save;
          file;
        with e ->
          if OpamFilename.exists local_index_file_save then
            OpamFilename.move ~src:local_index_file_save ~dst:local_index_file;
          raise e
      ) else
        local_index_file in
    let remote_local, local_remote, local_files, file_permissions, file_digests =
      let urls = OpamFile.Urls_txt.read index in
      let remote_local, local_remote, locals, perms, digests =
        OpamFilename.Attribute.Set.fold (fun r (rl, lr, locals, perms, digests) ->
          let base = OpamFilename.Attribute.base r in
          let perm = match OpamFilename.Attribute.perm r with
            | None  ->  0o640
            | Some p -> p in
          let digest = OpamFilename.Attribute.md5 r in
          let remote = OpamFilename.create remote_dir base in
          let local = OpamFilename.create local_dir base in
          OpamFilename.Map.add remote local rl,
          OpamFilename.Map.add local remote lr,
          OpamFilename.Set.add local locals,
          (local, perm) :: perms,
          (local, digest) :: digests
        ) urls
          (OpamFilename.Map.empty,
           OpamFilename.Map.empty,
           OpamFilename.Set.empty,
           [], []) in
      remote_local, local_remote, locals, perms, digests in
    let state = {
      remote_dir; local_dir;
      remote_index_archive; local_index_archive;
      local_files; remote_local; local_remote;
      file_permissions; file_digests;
    } in
    state_cache := (remote_dir, state) :: !state_cache;
    state
  )

let is_up_to_date state local_file =
  List.mem_assoc local_file state.file_digests
  && OpamFilename.exists local_file
  && not (Sys.is_directory (OpamFilename.to_string local_file))
  && List.assoc local_file state.file_digests = OpamFilename.digest local_file

let get_checksum state local_file =
  if List.mem_assoc local_file state.file_digests
  && OpamFilename.exists local_file
  && not (Sys.is_directory (OpamFilename.to_string local_file))
  then
    let expected = List.assoc local_file state.file_digests in
    let actual = OpamFilename.digest local_file in
    Some (actual, expected)
  else None

module B = struct

  let repo repo_root repo_address = {
    repo_name     = OpamRepositoryName.of_string "http";
    repo_priority = 0;
    repo_kind     = `http;
    repo_root; repo_address;
  }

  let init repo =
    try
      (* Download urls.txt *)
      let state = make_state ~download_index:true repo.repo_address in
      try
        (* Download index.tar.gz *)
        let file =
          OpamFilename.download ~overwrite:true
            state.remote_index_archive state.local_dir in
        OpamFilename.extract_in file state.local_dir
      with _ ->
        OpamGlobals.msg
          "Cannot find index.tar.gz on the OPAM repository. \
           Initialisation might take some time.\n"
    with _ ->
      OpamGlobals.error_and_exit
        "Error: %s is unavailable."
        (OpamFilename.Dir.to_string repo.repo_address)

  let curl ~remote_file ~local_file =
    log "dowloading %s" (OpamFilename.to_string remote_file);
    let local_dir = OpamFilename.dirname local_file in
    OpamFilename.mkdir local_dir;
    OpamFilename.download ~overwrite:true remote_file local_dir

  let pull_files ~all repo =
    let local_files =
      if all then OpamFilename.rec_files repo.repo_root
      else local_files repo in
    let state = make_state ~download_index:true repo.repo_address in
    OpamGlobals.msg "Synchronizing %s with %s.\n"
      (OpamFilename.prettify_dir state.local_dir)
      (OpamFilename.prettify_dir repo.repo_address);
    if state.local_dir <> state.remote_dir then (
      let (--) = OpamFilename.Set.diff in
      let current = OpamFilename.Set.of_list local_files in
      let to_keep = OpamFilename.Set.filter (is_up_to_date state) state.local_files in
      let to_delete = current -- to_keep in
      let new_files =
        let archives_dir = OpamPath.Repository.archives_dir repo in
        (OpamFilename.Set.filter
           (fun f -> not (OpamFilename.starts_with archives_dir f)) state.local_files)
        -- to_keep in
      log "current: %d files" (OpamFilename.Set.cardinal current);
      log "to_keep: %d files" (OpamFilename.Set.cardinal to_keep);
      log "to_delete: %s" (OpamFilename.Set.to_string to_delete);
      log "new_files: %s" (OpamFilename.Set.to_string new_files);
      OpamFilename.Set.iter OpamFilename.remove to_delete;

      let prefix, packages = OpamRepository.packages repo in
      OpamPackage.Set.iter (fun nv ->
        let prefix = OpamRepository.find_prefix prefix nv in
        let opam_f = OpamPath.Repository.opam repo prefix nv in
        if not (OpamFilename.exists opam_f) then (
          OpamFilename.rmdir (OpamPath.Repository.package repo prefix nv);
          OpamFilename.rmdir (OpamPath.Repository.tmp_dir repo nv);
          OpamFilename.remove (OpamPath.Repository.archive repo nv);
        )
      ) packages;

      if OpamFilename.Set.cardinal new_files > 4 then
        init repo
      else
        OpamFilename.Set.iter (fun local_file ->
          let remote_file = OpamFilename.Map.find local_file state.local_remote in
          ignore (curl ~remote_file ~local_file)
        ) new_files;
      if OpamFilename.Set.is_empty new_files then
        Up_to_date repo.repo_root
      else
        Result repo.repo_root
    ) else
      Up_to_date repo.repo_root

  let pull_dir dirname address =
    let repo = repo dirname address in
    pull_files ~all:true repo

  let pull_repo repo =
    ignore (pull_files ~all:false repo)

  (* XXX: add a proxy *)
  let pull_file dirname filename =
    OpamGlobals.msg "Downloading %s.\n" (OpamFilename.prettify filename);
    try
      let local_file = OpamFilename.download ~overwrite:true filename dirname in
      Result local_file
    with _ ->
      Not_available

end

let make_urls_txt repo_root =
  let repo = OpamRepository.local repo_root in
  let local_index_file = OpamFilename.of_string "urls.txt" in
  let index =
    List.fold_left (fun set f ->
      if not (OpamFilename.exists f) then set
      else (
        let basename =
          OpamFilename.Base.of_string
            (OpamFilename.remove_prefix (OpamFilename.cwd()) f) in
        let perm =
          let s = Unix.stat (OpamFilename.to_string f) in
          s.Unix.st_perm in
        let digest = OpamFilename.digest f in
        let attr = OpamFilename.Attribute.create basename digest perm in
        OpamFilename.Attribute.Set.add attr set
      )
    ) OpamFilename.Attribute.Set.empty (local_files repo)
  in
  if not (OpamFilename.Attribute.Set.is_empty index) then
    OpamFile.Urls_txt.write local_index_file index;
  index

let make_index_tar_gz repo_root =
  OpamFilename.in_dir repo_root (fun () ->
    let dirs = [ "compilers"; "packages" ] in
    match List.filter Sys.file_exists dirs with
    | [] -> ()
    | d  -> OpamSystem.command ("tar" :: "czf" :: "index.tar.gz" :: d)
  )

let register () =
  OpamRepository.register_backend `http (module B : OpamRepository.BACKEND)

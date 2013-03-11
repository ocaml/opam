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
          let file = OpamFilename.download ~overwrite:false remote_index_file local_dir in
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
        ) urls (OpamFilename.Map.empty, OpamFilename.Map.empty, OpamFilename.Set.empty, [], []) in
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

  let init ~address =
    try
      (* Download urls.txt *)
      let state = make_state ~download_index:true address in
      try
        (* Download index.tar.gz *)
        let file = OpamFilename.download ~overwrite:true state.remote_index_archive state.local_dir in
        OpamFilename.extract_in file state.local_dir
      with _ ->
        OpamGlobals.msg
          "Cannot find index.tar.gz on the OPAM repository. \
           Initialisation might take some time.\n"
    with _ ->
      OpamGlobals.error_and_exit
        "Error: %s is unavailable."
        (OpamFilename.Dir.to_string address)

  let curl ~remote_file ~local_file =
    log "dowloading %s" (OpamFilename.to_string remote_file);
    let local_dir = OpamFilename.dirname local_file in
    OpamFilename.mkdir local_dir;
    OpamFilename.download ~overwrite:true remote_file local_dir

  let update ~address =
    let state = make_state ~download_index:true address in
    OpamGlobals.msg "Synchronizing %s with %s.\n"
      (OpamFilename.prettify_dir state.local_dir)
      (OpamFilename.prettify_dir address);
    if state.local_dir <> state.remote_dir then begin
      let (--) = OpamFilename.Set.diff in
      let indexes =
        OpamFilename.Set.add
          (index_file state.local_dir)
          (OpamFilename.Set.singleton (index_archive state.local_dir)) in
      let current = OpamFilename.Set.of_list (OpamFilename.rec_files state.local_dir) in
      let to_keep = OpamFilename.Set.filter (is_up_to_date state) state.local_files in
      let config = OpamFilename.Set.singleton (OpamPath.Repository.config state.local_dir) in
      let to_delete = current -- to_keep -- indexes -- config in
      let local_repo = OpamRepository.local_repo () in
      let archive_dir = OpamPath.Repository.archives_dir local_repo in
      let new_files =
        (OpamFilename.Set.filter (fun f -> not (OpamFilename.starts_with archive_dir f)) state.local_files)
        -- to_keep in
      log "current: %s" (OpamFilename.Set.to_string current);
      log "to_keep: %s" (OpamFilename.Set.to_string to_keep);
      log "to_delete: %s" (OpamFilename.Set.to_string to_delete);
      log "new_files: %s" (OpamFilename.Set.to_string new_files);
      OpamFilename.Set.iter OpamFilename.remove to_delete;
      let prefix, packages = OpamRepository.packages local_repo in
      OpamPackage.Set.iter (fun nv ->
        let prefix = OpamRepository.find_prefix prefix nv in
        let opam_f = OpamPath.Repository.opam local_repo prefix nv in
        if not (OpamFilename.exists opam_f) then (
          OpamFilename.rmdir (OpamPath.Repository.package local_repo prefix nv);
          OpamFilename.rmdir (OpamPath.Repository.tmp_dir local_repo nv);
          OpamFilename.remove (OpamPath.Repository.archive local_repo nv);
        )
      ) packages;
      if OpamFilename.Set.cardinal new_files > 4 then
        init ~address
      else
        OpamFilename.Set.iter (fun local_file ->
          let remote_file = OpamFilename.Map.find local_file state.local_remote in
          ignore (curl ~remote_file ~local_file)
        ) new_files;
      new_files
    end else
      OpamFilename.Set.empty

  let download_archive ~address nv =
    let remote_file = OpamPath.Repository.archive address nv in
    let state = make_state ~download_index:false address in
    if not (OpamFilename.Map.mem remote_file state.remote_local) then
      Not_available
    else begin
      let local_file = OpamFilename.Map.find remote_file state.remote_local in
      if is_up_to_date state local_file then
        Up_to_date local_file
      else begin
        log "dowloading %s" (OpamFilename.to_string remote_file);
        let local_dir = OpamFilename.dirname local_file in
        OpamFilename.mkdir local_dir;
        OpamGlobals.msg "Downloading %s.\n" (OpamFilename.prettify remote_file);
        let local_file = OpamFilename.download ~overwrite:true remote_file local_dir in
        if not (OpamFilename.exists local_file) then
          (* This may happen with empty files *)
          OpamFilename.touch local_file;
        begin
          try
            let perm = List.assoc local_file state.file_permissions in
            OpamFilename.chmod local_file perm
          with Not_found ->
            ()
        end;
        if not !OpamGlobals.no_checksums && not (is_up_to_date state local_file) then (
          match get_checksum state local_file with
          | None -> OpamSystem.internal_error "%s is not up-to-date" (OpamFilename.to_string remote_file)
          | Some (actual, expected) -> OpamRepository.invalid_checksum remote_file ~actual ~expected
        ) else
          Result local_file
      end
    end

  let download_file ?checksum nv remote_file =
    let local_repo = OpamRepository.local_repo () in
    let dest_dir = OpamPath.Repository.tmp_dir local_repo nv in
    let local_file = OpamFilename.create dest_dir (OpamFilename.basename remote_file) in
    let up_to_date = match checksum with
      | None   -> false
      | Some c -> OpamFilename.exists local_file && OpamFilename.digest local_file = c in
    if up_to_date then
      Up_to_date local_file
    else (
      OpamGlobals.msg "Downloading %s.\n" (OpamFilename.prettify remote_file);
      try
        let file = OpamFilename.download ~overwrite:true remote_file dest_dir in
        Result file
      with _ ->
        Not_available
    )

  let not_supported action =
    failwith (action ^ ": not supported by CURL backend")

  let download_dir _ ?dst:_ dir =
    not_supported ("Downloading " ^ OpamFilename.Dir.to_string dir)

  let upload_dir ~address:_ remote_dir =
    not_supported ("Uploading to " ^ OpamFilename.Dir.to_string remote_dir)

end

let make_urls_txt local_repo =
  let local_index_file = OpamFilename.of_string "urls.txt" in
  let index = OpamFilename.Attribute.Set.of_list (List.rev_map (fun f ->
    let basename =
      OpamFilename.Base.of_string (OpamFilename.remove_prefix (OpamFilename.cwd()) f) in
    let perm =
      let s = Unix.stat (OpamFilename.to_string f) in
      s.Unix.st_perm in
    let digest = OpamFilename.digest f in
    OpamFilename.Attribute.create basename digest perm
  ) (OpamFilename.rec_files (OpamPath.Repository.packages_dir local_repo)
   @ OpamFilename.rec_files (OpamPath.Repository.archives_dir local_repo)
   @ OpamFilename.rec_files (OpamPath.Repository.compilers_dir local_repo)
  )) in
  if not (OpamFilename.Attribute.Set.is_empty index) then
    OpamFile.Urls_txt.write local_index_file index;
  index

let make_index_tar_gz local_repo =
  OpamFilename.in_dir (OpamPath.Repository.root local_repo) (fun () ->
    let dirs = [ "compilers"; "packages" ] in
    match List.filter Sys.file_exists dirs with
    | [] -> ()
    | d  -> OpamSystem.command ("tar" :: "czf" :: "index.tar.gz" :: d)
  )

let register () =
  OpamRepository.register_backend `http (module B : OpamRepository.BACKEND)

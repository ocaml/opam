(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
open OpamTypesBase
open OpamStateTypes
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "RSTATE" fmt
let slog = OpamConsole.slog

module Cache = struct
  type t = {
    cached_opams: (package * OpamFile.OPAM.t) list;
  }

  let check_marshaled_file file =
    let ic = open_in_bin (OpamFilename.to_string file) in
    let this_magic = OpamVersion.magic () in
    let magic_len = String.length this_magic in
    let file_magic =
      let b = Bytes.create magic_len in
      really_input ic b 0 magic_len;
      Bytes.to_string b in
    if file_magic <> this_magic then (
      close_in ic;
      OpamConsole.note
        "Clearing cache (wrong magic string %s, expected %s)."
        file_magic this_magic;
      None
    ) else
    let header = Bytes.create Marshal.header_size in
    really_input ic header 0 Marshal.header_size;
    let expected_size = magic_len + Marshal.total_size header 0 in
    let current_size = in_channel_length ic in
    if expected_size <> current_size then (
      close_in ic;
      OpamConsole.note "Clearing cache (wrong length %d, expected %d)."
        current_size expected_size;
      None
    ) else (
      seek_in ic magic_len;
      Some ic
    )

  let marshal_from_file file =
    let chrono = OpamConsole.timer () in
    match check_marshaled_file file with
    | Some ic ->
      let (cache: t) = Marshal.from_channel ic in
      close_in ic;
      log "Loaded %a in %.3fs" (slog OpamFilename.to_string) file (chrono ());
      Some (OpamPackage.Map.of_list cache.cached_opams)
    | None ->
      log "Invalid cache, removing";
      OpamFilename.remove file;
      None

  let load root =
    let file = OpamPath.state_cache root in
    if OpamFilename.exists file
    then marshal_from_file file
    else None

  let save rt =
    let chrono = OpamConsole.timer () in
    let file = OpamPath.state_cache rt.repos_global.root in
    assert (OpamPackage.Map.is_empty rt.package_index ||
            not (OpamPackage.Map.is_empty rt.repo_opams));
    OpamFilename.remove file;
    log "Writing the cache of metadata to %s ...\n"
      (OpamFilename.prettify file);
    let oc = open_out_bin (OpamFilename.to_string file) in
    output_string oc (OpamVersion.magic ());
    Marshal.to_channel oc
      { cached_opams = OpamPackage.Map.bindings rt.repo_opams }
      [Marshal.No_sharing];
    close_out oc;
    log "%a written in %.3fs" (slog OpamFilename.prettify) file (chrono ())

  let remove () =
    let root = OpamStateConfig.(!r.root_dir) in
    let file = OpamPath.state_cache root in
    OpamFilename.remove file

end

(* Returns the directory holding the original metadata of the package. *)
let package_repo_dir root repositories package_index nv =
  if OpamFilename.exists (OpamPath.opam root nv) then
    OpamPath.packages root nv
  else
  let repo_name, prefix = OpamPackage.Map.find nv package_index in
  let repo = OpamRepositoryName.Map.find repo_name repositories in
  OpamRepositoryPath.packages repo prefix nv

let load ?(save_cache=true) ?(lock=Lock_none) gt =
  (* let t = load_global_state () in *)
  log "LOAD-REPOSITORY-STATE";

  let opams = Cache.load gt.root in
  let cached = opams <> None in
  if cached then log "Cache found";
  let compilers =
    let files = OpamFilename.rec_files (OpamPath.compilers_dir gt.root) in
    let comp = OpamStd.List.filter_map OpamCompiler.of_filename files in
    OpamCompiler.Set.of_list comp
  in
  let repositories =
    let names = OpamFile.Config.repositories gt.config in
    List.fold_left (fun map repo_name ->
        let repo = OpamFile.Repo_config.read
            (OpamRepositoryPath.raw_config gt.root repo_name) in
        OpamRepositoryName.Map.add repo_name repo map
      ) OpamRepositoryName.Map.empty names
  in
  let package_index =
    OpamFile.Package_index.safe_read (OpamPath.package_index gt.root) in
  let compiler_index =
    OpamFile.Compiler_index.safe_read (OpamPath.compiler_index gt.root) in
  let load_opam_file nv =
    let dir = package_repo_dir gt.root repositories package_index nv in
    OpamFileHandling.read_opam dir
  in
  let repo_opams =
    match opams with Some o -> o | None ->
      let packages =
        OpamPackage.Set.of_list (OpamPackage.Map.keys package_index)
      in
      OpamPackage.Set.fold (fun nv map ->
          match load_opam_file nv with
          | Some o -> OpamPackage.Map.add nv o map
          | None -> map
        ) packages OpamPackage.Map.empty
  in
  let rt =
    { repos_global = gt;
      repos_lock = lock;
      repositories; compilers; package_index; compiler_index; repo_opams }
  in
  if save_cache && not cached then Cache.save rt;
  rt

let sorted_repositories rt =
  OpamRepository.sort rt.repositories

let compiler_index rt =
  OpamRepository.compiler_index rt.repositories

let package_index rt =
  OpamRepository.package_index rt.repositories

let package_state_one gt all nv =
  let opam    = OpamPath.opam gt.root nv in
  let descr   = OpamPath.descr gt.root nv in
  let url     = OpamPath.url gt.root nv in
  let files   = OpamPath.files gt.root nv in
  let archive = OpamPath.archive gt.root nv in
  if not (OpamFilename.exists opam) then []
  else match all with
    | `all ->
      OpamFilename.checksum opam
      @ OpamFilename.checksum descr
      @ OpamFilename.checksum url
      @ OpamFilename.checksum_dir files
      @ OpamFilename.checksum archive
    | `partial true ->
      OpamRepository.url_checksum url
      @ OpamFilename.checksum_dir files
      @ OpamFilename.checksum archive
    | `partial false ->
      OpamRepository.url_checksum url
      @ OpamFilename.checksum_dir files

let package_state rt =
  let gt = rt.repos_global in
  let installed = OpamPackage.Set.fold (fun nv map ->
      let state = package_state_one gt `all nv in
      OpamPackage.Map.add nv state map
    ) (OpamGlobalState.all_installed gt) OpamPackage.Map.empty in
  OpamPackage.Map.fold (fun nv (repo, prefix) map ->
      if OpamPackage.Map.mem nv map then map
      else if OpamFilename.exists (OpamPath.opam gt.root nv) then
        let state = package_state_one gt `all nv in
        OpamPackage.Map.add nv state map
      else
        let repo = OpamRepositoryName.Map.find repo rt.repositories in
        let state = OpamRepository.package_state repo prefix nv `all in
        OpamPackage.Map.add nv state map
    ) rt.package_index installed

let package_partial_state rt nv ~archive =
  match package_state_one rt.repos_global (`partial archive) nv with
  | []    -> false, []
  | state ->
    let archive = OpamPath.archive rt.repos_global.root nv in
    OpamFilename.exists archive, state

let package_repository_state rt =
  OpamPackage.Map.fold (fun nv (repo, prefix) map ->
      let repo = OpamRepositoryName.Map.find repo rt.repositories in
      match OpamRepository.package_state repo prefix nv `all with
      | []    -> map
      | state -> OpamPackage.Map.add nv state map
    ) rt.package_index OpamPackage.Map.empty

let package_repository_partial_state rt nv ~archive =
  let repo, prefix = OpamPackage.Map.find nv rt.package_index in
  let repo = OpamRepositoryName.Map.find repo rt.repositories in
  let exists_archive = OpamFilename.exists (OpamRepositoryPath.archive repo nv) in
  exists_archive, OpamRepository.package_state repo prefix nv (`partial archive)

let repository_and_prefix_of_package rt nv =
  try
    let repo, prefix = OpamPackage.Map.find nv rt.package_index in
    let repo = OpamRepositoryName.Map.find repo rt.repositories in
    Some (repo, prefix)
   with Not_found ->
     None

let repository_of_package rt nv =
  try
    let repo, _ = OpamPackage.Map.find nv rt.package_index in
    let repo = OpamRepositoryName.Map.find repo rt.repositories in
    Some repo
  with Not_found ->
    None

let compiler_state_one gt c =
  let comp = OpamPath.compiler_comp gt.root c in
  let descr = OpamPath.compiler_descr gt.root c in
  if OpamFilename.exists comp then
    Some (OpamFilename.checksum comp @ OpamFilename.checksum descr)
  else
    None

let compiler_state rt =
  OpamCompiler.Set.fold (fun c map ->
      match compiler_state_one rt.repos_global c with
      | None   -> map
      | Some s -> OpamCompiler.Map.add c s map
    ) rt.compilers OpamCompiler.Map.empty

let compiler_repository_state rt =
  OpamCompiler.Map.fold (fun comp (repo, prefix) map ->
      let repo = OpamRepositoryName.Map.find repo rt.repositories in
      match OpamRepository.compiler_state repo prefix comp with
      | [] -> map
      | l  -> OpamCompiler.Map.add comp l map
    ) rt.compiler_index OpamCompiler.Map.empty

let repository_and_prefix_of_compiler rt comp =
  try
    let repo, prefix = OpamCompiler.Map.find comp rt.compiler_index in
    let repo = OpamRepositoryName.Map.find repo rt.repositories in
    Some (repo, prefix)
  with Not_found ->
    None

(* Try to download $name.$version+opam.tar.gz *)
let download_archive rt nv =
  log "get_archive %a" (slog OpamPackage.to_string) nv;
  let dst = OpamPath.archive rt.repos_global.root nv in
  match repository_of_package rt nv with
  | None -> Done None
  | Some repo ->
    let text =
      OpamProcess.make_command_text
        (OpamPackage.name_to_string nv)
        ~args:[OpamRepositoryName.to_string repo.repo_name]
        "from"
    in
    OpamProcess.Job.with_text text @@
    OpamRepository.pull_archive repo nv
    @@+ function
    | Not_available _ ->
      if OpamCoreConfig.(!r.verbose_level) >= 2 then
        OpamConsole.msg "%s Repo archive not found\n" text;
      Done None
    | Up_to_date f ->
      OpamConsole.msg "[%s] Archive in cache\n"
        (OpamConsole.colorise `green (OpamPackage.name_to_string nv));
      OpamFilename.copy ~src:f ~dst; Done (Some dst)
    | Result f ->
      OpamFilename.copy ~src:f ~dst; Done (Some dst)

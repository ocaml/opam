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
  let repo_name, prefix = OpamPackage.Map.find nv package_index in
  let repo = OpamRepositoryName.Map.find repo_name repositories in
  OpamRepositoryPath.packages repo prefix nv

let load ?(save_cache=true) ?(lock=Lock_none) gt =
  (* let t = load_global_state () in *)
  log "LOAD-REPOSITORY-STATE";

  let opams = Cache.load gt.root in
  let cached = opams <> None in
  if cached then log "Cache found";
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
  let load_opam_file nv =
    let dir = package_repo_dir gt.root repositories package_index nv in
    OpamFileHandling.read_opam dir
  in
  let repo_opams =
    match opams with Some o -> o | None ->
      OpamPackage.Set.fold (fun nv map ->
          match load_opam_file nv with
          | Some o -> OpamPackage.Map.add nv o map
          | None -> map
        ) (OpamPackage.keys package_index) OpamPackage.Map.empty
  in
  let rt =
    { repos_global = gt;
      repos_lock = lock;
      repositories; package_index; repo_opams }
  in
  if save_cache && not cached then Cache.save rt;
  rt

let package_index rt =
  OpamRepository.package_index rt.repositories

let repository_of_package rt nv =
  try
    let repo, _ = OpamPackage.Map.find nv rt.package_index in
    let repo = OpamRepositoryName.Map.find repo rt.repositories in
    Some repo
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

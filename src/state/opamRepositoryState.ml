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
open OpamStd.Op
open OpamStateTypes
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "RSTATE" fmt
let slog = OpamConsole.slog

module Cache = struct
  type t = {
    cached_opams: (repository_name * (package * OpamFile.OPAM.t) list) list;
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
      let repos_map =
        OpamRepositoryName.Map.map OpamPackage.Map.of_list
          (OpamRepositoryName.Map.of_list cache.cached_opams)
      in
      Some repos_map
    | None ->
      log "Invalid cache, removing";
      OpamFilename.remove file;
      None

  let load root =
    match OpamFilename.opt_file (OpamPath.state_cache root) with
    | Some file ->
      OpamFilename.with_flock `Lock_read file @@ fun () ->
      marshal_from_file file
    | None -> None

  let save rt =
    let chrono = OpamConsole.timer () in
    let file = OpamPath.state_cache rt.repos_global.root in
    OpamFilename.with_flock `Lock_write file @@ fun () ->
    log "Writing the cache of repository metadata to %s ...\n"
      (OpamFilename.prettify file);
    let oc = open_out_bin (OpamFilename.to_string file) in
    output_string oc (OpamVersion.magic ());
    Marshal.to_channel oc
      { cached_opams =
          List.map
            (fun (repo_name, opams) ->
               repo_name, OpamPackage.Map.bindings opams)
            (OpamRepositoryName.Map.bindings rt.repo_opams) }
      [Marshal.No_sharing];
    close_out oc;
    log "%a written in %.3fs" (slog OpamFilename.prettify) file (chrono ())

  let remove () =
    let root = OpamStateConfig.(!r.root_dir) in
    let file = OpamPath.state_cache root in
    OpamFilename.remove file

end

let load_repo_opams repo =
  OpamPackage.Map.mapi
    (fun nv prefix ->
       match
         OpamFileHandling.read_opam
           (OpamRepositoryPath.packages repo prefix nv)
       with
       | None -> assert false
       | Some o -> o)
    (OpamPackage.prefixes (OpamRepositoryPath.packages_dir repo))

let load ~lock:lock_kind gt =
  log "LOAD-REPOSITORY-STATE";
  let lock = OpamFilename.flock lock_kind (OpamPath.repos_lock gt.root) in
  let repositories =
    let names = OpamFile.Config.repositories gt.config in
    List.fold_left (fun map repo_name ->
        let repo = OpamFile.Repo_config.read
            (OpamRepositoryPath.raw_config gt.root repo_name) in
        OpamRepositoryName.Map.add repo_name repo map
      ) OpamRepositoryName.Map.empty names
  in
  let make_rt opams =
    { repos_global = (gt :> unlocked global_state);
      repos_lock = lock;
      repositories;
      repo_opams = opams; }
  in
  match Cache.load gt.root with
  | Some opams ->
    log "Cache found";
    make_rt opams
  | None ->
    OpamFilename.with_flock_upgrade `Lock_read lock @@ fun () ->
    let rt =
      make_rt (OpamRepositoryName.Map.map load_repo_opams repositories)
    in
    Cache.save rt;
    rt

let find_package_opt rt repo_list nv =
  List.fold_left (function
      | None ->
        fun repo_name ->
          let opams = OpamRepositoryName.Map.find repo_name rt.repo_opams in
          OpamStd.Option.Op.(
            OpamPackage.Map.find_opt nv opams
            >>| fun opam -> repo_name, opam
          )
      | some -> fun _ -> some)
    None repo_list

let build_index rt repo_list =
  List.fold_left (fun acc repo_name ->
      let repo_opams =
        OpamRepositoryName.Map.find repo_name rt.repo_opams
      in
      OpamPackage.Map.union (fun a _ -> a) acc repo_opams)
    OpamPackage.Map.empty
    repo_list

let repos_list rt =
  let repos = OpamRepositoryName.Map.bindings rt.repositories in
  let repos =
    List.sort
      (fun (_, conf1) (_, conf2) -> conf2.repo_priority - conf1.repo_priority)
      repos
  in
  List.map fst repos

(* Try to download $name.$version+opam.tar.gz *)
let download_archive rt repo_list nv =
  log "get_archive %a" (slog OpamPackage.to_string) nv;
  let dst = OpamPath.archive rt.repos_global.root nv in
  match find_package_opt rt repo_list nv with
  | None -> Done None
  | Some (repo_name, _) ->
    let repo = OpamRepositoryName.Map.find repo_name rt.repositories in
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

let unlock rt =
  OpamSystem.funlock rt.repos_lock;
  (rt :> unlocked repos_state)

let with_write_lock ?dontblock rt f =
  OpamFilename.with_flock_upgrade `Lock_write ?dontblock rt.repos_lock @@ fun () ->
  f ({ rt with repos_lock = rt.repos_lock } : rw repos_state)
(* We don't actually change the field value, but this makes restricting the
   phantom lock type possible*)

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
open OpamTypesBase
open OpamStd.Op
open OpamStateTypes

let log fmt = OpamConsole.log "RSTATE" fmt
let slog = OpamConsole.slog

module Cache = struct
  type t = {
    cached_repofiles: (repository_name * OpamFile.Repo.t) list;
    cached_opams: (repository_name * (package * OpamFile.OPAM.t) list) list;
  }

  let check_marshaled_file fd =
    try
    let ic = Unix.in_channel_of_descr fd in
    let this_magic = OpamVersion.magic () in
    let magic_len = String.length this_magic in
    let file_magic =
      let b = Bytes.create magic_len in
      really_input ic b 0 magic_len;
      Bytes.to_string b in
    if not OpamCoreConfig.developer &&
      file_magic <> this_magic then (
      log "Bad cache: incompatible magic string %S (expected %S)."
        file_magic this_magic;
      None
    ) else
    let header = Bytes.create Marshal.header_size in
    really_input ic header 0 Marshal.header_size;
    let expected_size = magic_len + Marshal.total_size header 0 in
    let current_size = in_channel_length ic in
    if expected_size <> current_size then (
      log "Bad cache: wrong length %d (advertised %d)."
        current_size expected_size;
      None
    ) else (
      seek_in ic magic_len;
      Some ic
    )
    with e ->
      OpamStd.Exn.fatal e;
      log "Bad cache: %s" (Printexc.to_string e);
      None

  let marshal_from_file file fd =
    let chrono = OpamConsole.timer () in
    let f ic =
      let (cache: t) = Marshal.from_channel ic in
      log "Loaded %a in %.3fs" (slog OpamFilename.to_string) file (chrono ());
      let repofiles_map =
        OpamRepositoryName.Map.of_list cache.cached_repofiles
      in
      let repo_opams_map =
        OpamRepositoryName.Map.map OpamPackage.Map.of_list
          (OpamRepositoryName.Map.of_list cache.cached_opams)
      in
      (repofiles_map, repo_opams_map)
    in
    OpamStd.Option.map f (check_marshaled_file fd)

  let load root =
    match OpamFilename.opt_file (OpamPath.state_cache root) with
    | Some file ->
        let r =
          OpamFilename.with_flock `Lock_read file @@ fun fd ->
          marshal_from_file file fd
        in
        if r = None then begin
          log "Invalid cache, removing";
          OpamFilename.remove file
        end;
        r
    | None -> None

  let save rt =
    if OpamCoreConfig.(!r.safe_mode) then
      log "Running in safe mode, not upgrading the repository cache"
    else
    let chrono = OpamConsole.timer () in
    let file = OpamPath.state_cache rt.repos_global.root in
    OpamFilename.with_flock `Lock_write file @@ fun fd ->
    log "Writing the cache of repository metadata to %s ...\n"
      (OpamFilename.prettify file);
    let oc = Unix.out_channel_of_descr fd in
    output_string oc (OpamVersion.magic ());
    (* Repository without remote are not cached, they are intended to be
       manually edited *)
    let filter_out_nourl repos_map =
      OpamRepositoryName.Map.filter
        (fun name _ ->
           try
             (OpamRepositoryName.Map.find name rt.repositories).repo_url <>
             OpamUrl.empty
           with Not_found -> false)
        repos_map
    in
    Marshal.to_channel oc
      { cached_repofiles =
          OpamRepositoryName.Map.bindings
            (filter_out_nourl rt.repos_definitions);
        cached_opams =
          OpamRepositoryName.Map.bindings
            (OpamRepositoryName.Map.map OpamPackage.Map.bindings
               (filter_out_nourl rt.repo_opams));
      }
      [Marshal.No_sharing];
    flush oc;
    log "%a written in %.3fs" (slog OpamFilename.prettify) file (chrono ())

  let remove () =
    let root = OpamStateConfig.(!r.root_dir) in
    let file = OpamPath.state_cache root in
    OpamFilename.remove file

end

let load_repo_opams repo =
  let t = OpamConsole.timer () in
  let rec aux r dir =
    if OpamFilename.exists_dir dir then
      let fnames = Sys.readdir (OpamFilename.Dir.to_string dir) in
      if Array.fold_left (fun a f -> a || f = "opam") false fnames then
        match OpamFileTools.read_opam dir with
        | Some opam ->
          (try
             let nv =
               OpamPackage.of_string
                 OpamFilename.(Base.to_string (basename_dir dir))
             in
             OpamPackage.Map.add nv opam r
           with Failure _ ->
             log "ERR: directory name not a valid package: ignored %s"
               OpamFilename.(to_string Op.(dir // "opam"));
             r)
        | None ->
          log "ERR: Could not load %s, ignored"
            OpamFilename.(to_string Op.(dir // "opam"));
          r
      else
        Array.fold_left (fun r name -> aux r OpamFilename.Op.(dir / name))
          r fnames
    else r
  in
  let r =
    aux OpamPackage.Map.empty
      (OpamRepositoryPath.packages_dir repo.repo_root)
  in
  log "loaded opam files from repo %s in %.3fs"
    (OpamRepositoryName.to_string repo.repo_name)
    (t ());
  r

let load lock_kind gt =
  log "LOAD-REPOSITORY-STATE @ %a" (slog OpamFilename.Dir.to_string) gt.root;
  let lock = OpamFilename.flock lock_kind (OpamPath.repos_lock gt.root) in
  let repos_map =
    OpamFile.Repos_config.safe_read (OpamPath.repos_config gt.root)
  in
  let mk_repo name url_opt = {
    repo_root = OpamRepositoryPath.create gt.root name;
    repo_name = name;
    repo_url = OpamStd.Option.Op.((url_opt >>| fst) +! OpamUrl.empty);
    repo_trust = OpamStd.Option.Op.((url_opt >>= snd));
  } in
  let uncached =
    (* Don't cache repositories without remote, as they should be editable
       in-place *)
    OpamRepositoryName.Map.filter (fun _ url -> url = None) repos_map
  in
  let repositories = OpamRepositoryName.Map.mapi mk_repo repos_map in
  let load_repos_definitions repositories =
    OpamRepositoryName.Map.map (fun r ->
        OpamFile.Repo.safe_read
          OpamRepositoryPath.(repo (create gt.root r.repo_name)) |>
        OpamFile.Repo.with_root_url r.repo_url)
      repositories
  in
  let make_rt repos_definitions opams =
    { repos_global = (gt :> unlocked global_state);
      repos_lock = lock;
      repositories;
      repos_definitions;
      repo_opams = opams; }
  in
  match Cache.load gt.root with
  | Some (repofiles, opams) when OpamRepositoryName.Map.is_empty uncached ->
    log "Cache found";
    make_rt repofiles opams
  | Some (repofiles, opams) ->
    log "Cache found, loading repositories without remote only";
    OpamFilename.with_flock_upgrade `Lock_read lock @@ fun _ ->
    let uncached_repos = OpamRepositoryName.Map.mapi mk_repo uncached in
    let uncached_repofiles = load_repos_definitions uncached_repos in
    let uncached_opams =
      OpamRepositoryName.Map.map load_repo_opams uncached_repos
    in
    make_rt
      (OpamRepositoryName.Map.union (fun _ x -> x) repofiles uncached_repofiles)
      (OpamRepositoryName.Map.union (fun _ x -> x) opams uncached_opams)
  | None ->
    log "No cache found";
    OpamFilename.with_flock_upgrade `Lock_read lock @@ fun _ ->
    let repos = OpamRepositoryName.Map.mapi mk_repo repos_map in
    let rt =
      make_rt
        (load_repos_definitions repos)
        (OpamRepositoryName.Map.map load_repo_opams repos)
    in
    Cache.save rt;
    rt

let find_package_opt rt repo_list nv =
  List.fold_left (function
      | None ->
        fun repo_name ->
          OpamStd.Option.Op.(
            OpamRepositoryName.Map.find_opt repo_name rt.repo_opams >>=
            OpamPackage.Map.find_opt nv >>| fun opam ->
            repo_name, opam
          )
      | some -> fun _ -> some)
    None repo_list

let build_index rt repo_list =
  List.fold_left (fun acc repo_name ->
      try
        let repo_opams = OpamRepositoryName.Map.find repo_name rt.repo_opams in
        OpamPackage.Map.union (fun a _ -> a) acc repo_opams
      with Not_found ->
        (* A repo is unavailable, error should have been already reported *)
        acc)
    OpamPackage.Map.empty
    repo_list

let get_repo rt name = OpamRepositoryName.Map.find name rt.repositories

let unlock rt =
  OpamSystem.funlock rt.repos_lock;
  (rt :> unlocked repos_state)

let with_write_lock ?dontblock rt f =
  let ret, rt =
    OpamFilename.with_flock_upgrade `Lock_write ?dontblock rt.repos_lock
    @@ fun _ -> f ({ rt with repos_lock = rt.repos_lock } : rw repos_state)
    (* We don't actually change the field value, but this makes restricting the
       phantom lock type possible *)
  in
  ret, { rt with repos_lock = rt.repos_lock }

let with_ lock gt f =
  let rt = load lock gt in
  try let r = f rt in ignore (unlock rt); r
  with e -> OpamStd.Exn.finalise e (fun () -> ignore (unlock rt))

let write_config rt =
  OpamFile.Repos_config.write (OpamPath.repos_config rt.repos_global.root)
    (OpamRepositoryName.Map.map (fun r ->
         if r.repo_url = OpamUrl.empty then None
         else Some (r.repo_url, r.repo_trust))
        rt.repositories)

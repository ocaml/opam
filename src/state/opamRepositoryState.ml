(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStateTypes

let log fmt = OpamConsole.log "RSTATE" fmt
let slog = OpamConsole.slog

module Cache = struct
  type t = {
    cached_repofiles: (repository_name * OpamFile.Repo.t) list;
    cached_opams: (repository_name * OpamFile.OPAM.t OpamPackage.Map.t) list;
  }

  module C = OpamCached.Make (struct
      type nonrec t = t
      let name = "repository"
    end)

  let remove () =
    let root = OpamStateConfig.(!r.root_dir) in
    let cache_dir = OpamPath.state_cache_dir root in
    let remove_cache_file file =
      if OpamFilename.check_suffix file ".cache" then
        OpamFilename.remove file
    in
    List.iter remove_cache_file (OpamFilename.files cache_dir)

  let marshall rt =
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
      { cached_repofiles =
          OpamRepositoryName.Map.bindings
            (filter_out_nourl rt.repos_definitions);
        cached_opams =
          OpamRepositoryName.Map.bindings
            (filter_out_nourl rt.repo_opams);
      }

  let file rt =
    OpamPath.state_cache rt.repos_global.root

  let save rt =
    remove ();
    C.save (file rt) (marshall rt)

  let save_new rt =
    C.save (file rt) (marshall rt)

  let load root =
    let file = OpamPath.state_cache root in
    match C.load file with
    | Some cache ->
      Some
        (OpamRepositoryName.Map.of_list cache.cached_repofiles,
         OpamRepositoryName.Map.of_list cache.cached_opams)
    | None -> None

end

let get_root_raw root name =
  let tar = OpamRepositoryPath.tar root name in
  if OpamRepositoryRoot.Tar.exists tar then
    OpamRepositoryRoot.Tar tar
  else
    OpamRepositoryRoot.Dir (OpamRepositoryPath.root root name)

let get_root rt name =
  get_root_raw rt.repos_global.root name

let get_repo_root rt repo =
  get_root_raw rt.repos_global.root repo.repo_name

let get_repo_files rt name dir =
  match get_root rt name with
  | OpamRepositoryRoot.Tar _ -> assert false (* TODO *)
  | OpamRepositoryRoot.Dir repo_root ->
    let dir = OpamRepositoryRoot.Dir.Op.(repo_root / dir) in
    let files = OpamFilename.rec_files dir in
    List.map (fun file ->
        OpamFilename.Base.of_string
          (OpamSystem.back_to_forward (OpamFilename.remove_prefix dir file)),
        lazy (OpamFilename.read file))
      files

let read_package_opam ~repo_name ~repo_root package_dir =
  match OpamFileTools.read_repo_opam ~repo_name ~repo_root package_dir with
  | Some opam ->
    (try
       let nv =
         OpamPackage.of_string
           (OpamFilename.Base.to_string (OpamFilename.basename_dir package_dir))
       in
       Some (nv, opam)
     with Failure _ ->
       log "ERR: directory name not a valid package: ignored %s"
         (OpamFilename.to_string OpamFilename.Op.(package_dir // "opam"));
       None)
  | None ->
    log "ERR: Could not load %s, ignored"
      (OpamFilename.to_string OpamFilename.Op.(package_dir // "opam"));
    None

let load_opams_from_tar_gz _repo_name tar =
  OpamTar.fold_reg_files (fun opams filename content ->
      if OpamCompat.String.ends_with ~suffix:"/opam" filename then
        let opam = OpamFile.OPAM.read_from_string content in
        let pkg =
          let list = String.split_on_char '/' filename |> List.rev in
          (* TODO: handle errors *)
          OpamPackage.of_string (List.nth list 1)
        in
        (* TODO: Do like OpamFileTools.read_repo_opam and also merge the metadata files as they come up *)
        OpamPackage.Map.add pkg opam opams
      else
        opams
    ) OpamPackage.Map.empty
    (Unix.openfile (OpamRepositoryRoot.Tar.to_string tar) [Unix.O_RDONLY] 0)

let load_repo_from_tar_gz repo_name tar =
  let repo_def =
    let exception Found of string in
    try
      OpamTar.fold_reg_files (fun () filename content ->
          if filename = "/repo" then
            raise (Found content);
        ) () (Unix.openfile (OpamRepositoryRoot.Tar.to_string tar) [Unix.O_RDONLY] 0);
      OpamFile.Repo.empty
    with Found content -> OpamFile.Repo.read_from_string content
  in
  let opams = load_opams_from_tar_gz repo_name tar in
  repo_def, opams

let load_opams_from_dir repo_name repo_root =
  if OpamConsole.disp_status_line () || OpamConsole.verbose () then
    OpamConsole.status_line "Processing: [%s: loading data]"
      (OpamConsole.colorise `blue (OpamRepositoryName.to_string repo_name));
  (* FIXME: why is this different from OpamPackage.list ? *)
  let rec aux r dir =
    if OpamFilename.exists_dir dir then
      let fnames = Sys.readdir (OpamFilename.Dir.to_string dir) in
      if Array.exists (fun f -> f = "opam") fnames then
        match read_package_opam ~repo_name ~repo_root dir with
        | Some (nv, opam) -> OpamPackage.Map.add nv opam r
        | None -> r
      else
        Array.fold_left (fun r name -> aux r OpamFilename.Op.(dir / name))
          r fnames
    else r
  in
  Fun.protect
    (fun () -> aux OpamPackage.Map.empty (OpamRepositoryPath.packages_dir repo_root))
    ~finally:OpamConsole.clear_status

let load_opams_from_diff repo diffs rt =
  if OpamConsole.disp_status_line () || OpamConsole.verbose () then
    OpamConsole.status_line "Processing: [%s: loading data]"
      (OpamConsole.colorise `blue (OpamRepositoryName.to_string repo.repo_name));
  let existing_opams =
    OpamRepositoryName.Map.find repo.repo_name rt.repo_opams
  in
  let process_file =
    match get_repo_root rt repo with
    | OpamRepositoryRoot.Tar _ -> assert false (* TODO *)
    | OpamRepositoryRoot.Dir repo_root ->
      fun (opams, processed_dirs) file ~is_removal ->
        let pkg_dir =
          let file = OpamFilename.raw file in
          let dirname = OpamFilename.dirname file in
          let basename = OpamFilename.basename_dir dirname in
          let full_path =
            Filename.concat
              (OpamRepositoryRoot.Dir.to_string repo_root)
              (OpamFilename.Dir.to_string dirname)
          in
          if OpamFilename.Base.to_string basename = "files" then
            OpamFilename.Dir.of_string (Filename.dirname full_path)
          else
            OpamFilename.Dir.of_string full_path
        in
        if OpamFilename.Dir.Set.mem pkg_dir processed_dirs then
          opams, processed_dirs
        else
          let processed_dirs = OpamFilename.Dir.Set.add pkg_dir processed_dirs in
          match read_package_opam ~repo_name:repo.repo_name ~repo_root pkg_dir with
          | Some (nv, opam) -> OpamPackage.Map.add nv opam opams, processed_dirs
          | None ->
            if is_removal then
              match OpamPackage.of_dirname pkg_dir with
              | None ->
                log "ERR: directory name not a valid package: ignored %s"
                  (OpamFilename.Dir.to_string pkg_dir);
                opams, processed_dirs
              | Some nv ->
                OpamPackage.Map.remove nv opams, processed_dirs
            else
              let processed_dirs = OpamFilename.Dir.Set.add pkg_dir processed_dirs in
              match read_package_opam ~repo_name:repo.repo_name ~repo_root pkg_dir with
              | Some (nv, opam) -> OpamPackage.Map.add nv opam opams, processed_dirs
              | None ->
                if is_removal then
                  match OpamPackage.of_dirname pkg_dir with
                  | None ->
                    log "ERR: directory name not a valid package: ignored %s"
                      (OpamFilename.Dir.to_string pkg_dir);
                    opams, processed_dirs
                  | Some nv ->
                    OpamPackage.Map.remove nv opams, processed_dirs
                else
                  opams, processed_dirs
  in
  let remove_file file acc = process_file acc file ~is_removal:true in
  let add_file file acc = process_file acc file ~is_removal:false in
  let process_operation acc = function
    | Patch.Edit (old_file, new_file) ->
      if String.equal old_file new_file
      then
        add_file new_file acc
      else
        remove_file old_file acc |> add_file new_file
    | Patch.Delete file -> remove_file file acc
    | Patch.Create file -> add_file file acc
    | Patch.Git_ext (file1, file2, git_ext) ->
      match git_ext with
      | Patch.Rename_only (_, _) -> remove_file file1 acc |> add_file file2
      | Patch.Delete_only -> remove_file file1 acc
      | Patch.Create_only -> add_file file2 acc
  in
  Fun.protect
    (fun () -> List.fold_left process_operation
        (existing_opams, OpamFilename.Dir.Set.empty) diffs |> fst)
    ~finally:OpamConsole.clear_status

let load_repo_from_dir repo repo_root =
  let repo_def =
    OpamFile.Repo.safe_read (OpamRepositoryPath.repo repo_root)
    |> OpamFile.Repo.with_root_url repo.repo_url
  in
  let opams = load_opams_from_dir repo.repo_name repo_root in
  repo_def, opams

let load_repo repo repo_root =
  let t = OpamConsole.timer () in
  let loaded_repo =
    match repo_root with
    | OpamRepositoryRoot.Tar tar ->
      load_repo_from_tar_gz repo tar
    | OpamRepositoryRoot.Dir dir ->
      load_repo_from_dir repo dir
  in
  log "loaded opam files from repo %s in %.3fs"
    (OpamRepositoryName.to_string repo.repo_name)
    (t ());
  loaded_repo

let load lock_kind gt =
  log "LOAD-REPOSITORY-STATE %@ %a" (slog OpamFilename.Dir.to_string) gt.root;
  let lock = OpamFilename.flock lock_kind (OpamPath.repos_lock gt.root) in
  let repos_map =
    match OpamFormatUpgrade.as_necessary_repo lock_kind gt with
    | Some repos_map -> repos_map
    | None -> OpamStateConfig.Repos.safe_read ~lock_kind gt
  in
  if OpamStateConfig.is_newer_than_self ~lock_kind gt then
    log "root version (%s) is greater than running binary's (%s); \
         load with best-effort (read-only)"
      (OpamVersion.to_string (OpamFile.Config.opam_root_version gt.config))
      (OpamVersion.to_string (OpamFile.Config.root_version));
  let mk_repo name (url, ta) = {
    repo_name = name;
    repo_url = url;
    repo_trust = ta;
  } in
  let repositories = OpamRepositoryName.Map.mapi mk_repo repos_map in
  let make_rt repos_definitions opams =
    let rt = {
      repos_global = (gt :> unlocked global_state);
      repos_lock = lock;
      repositories;
      repos_definitions;
      repo_opams = opams;
    } in
    rt
  in
  match Cache.load gt.root with
  | Some (repofiles, opams) ->
    log "Cache found";
    make_rt repofiles opams
  | None ->
    log "No cache found";
    OpamFilename.with_flock_upgrade `Lock_read lock @@ fun _ ->
    let repofiles, opams =
      OpamRepositoryName.Map.fold (fun name url (defs, opams) ->
          let repo = mk_repo name url in
          let repo_def, repo_opams =
            load_repo repo (get_root_raw gt.root name)
          in
          OpamRepositoryName.Map.add name repo_def defs,
          OpamRepositoryName.Map.add name repo_opams opams)
        repos_map (OpamRepositoryName.Map.empty, OpamRepositoryName.Map.empty)
    in
    let rt = make_rt repofiles opams in
    Cache.save_new rt;
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

let drop rt =
  let _ = unlock rt in ()

let with_write_lock ?dontblock rt f =
  if OpamStateConfig.is_newer_than_self ~lock_kind:`Lock_write rt.repos_global
  then
    OpamConsole.error_and_exit `Locked
      "The opam root has been upgraded by a newer version of opam-state \
       and cannot be written to";
  let ret, rt =
    OpamFilename.with_flock_upgrade `Lock_write ?dontblock rt.repos_lock
    @@ fun _ -> f ({ rt with repos_lock = rt.repos_lock } : rw repos_state)
    (* We don't actually change the field value, but this makes restricting the
       phantom lock type possible *)
  in
  ret, { rt with repos_lock = rt.repos_lock }

let with_ lock gt f =
  let rt = load lock gt in
  OpamStd.Exn.finally (fun () -> drop rt) (fun () -> f rt)

let write_config rt =
  OpamFile.Repos_config.write (OpamPath.repos_config rt.repos_global.root)
    (OpamRepositoryName.Map.filter_map (fun _ r ->
         if r.repo_url = OpamUrl.empty then None
         else Some (r.repo_url, r.repo_trust))
        rt.repositories)

let check_last_update () =
  if OpamCoreConfig.(!r.debug_level) < 0 then () else
  let last_update =
    OpamFilename.written_since
      (OpamPath.state_cache (OpamStateConfig.(!r.root_dir)))
  in
  if last_update > float_of_int (3600*24*21) then
    OpamConsole.note "It seems you have not updated your repositories \
                      for a while. Consider updating them with:\n%s\n"
      (OpamConsole.colorise `bold "opam update");

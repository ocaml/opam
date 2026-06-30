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

let log ?level fmt = OpamConsole.log ?level "RSTATE" fmt
let slog = OpamConsole.slog

module Cache = struct
  type t = {
    cached_repofiles: (repository_name * OpamFile.Repo.t) list;
    cached_opams: (repository_name * OpamFile.OPAM.t OpamPackage.Map.t) list;
    cached_sys_available_pkgs: repo_syspkgs_available;
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
      cached_sys_available_pkgs = rt.repos_syspkgs_available;
    }

  let file rt =
    OpamPath.state_cache rt.repos_global.root

  let save_new rt =
    C.save (file rt) (marshall rt)

  let save rt =
    remove ();
    save_new rt

  let load root =
    let file = OpamPath.state_cache root in
    match C.load file with
    | Some cache ->
      Some
        (OpamRepositoryName.Map.of_list cache.cached_repofiles,
         OpamRepositoryName.Map.of_list cache.cached_opams,
         cache.cached_sys_available_pkgs)
    | None -> None

end

let get_root_raw root name =
  let tgz = OpamRepositoryRoot.Tgz.Path.root root name in
  if OpamRepositoryRoot.Tgz.exists tgz then
    OpamRepositoryRoot.Tgz tgz
  else
    OpamRepositoryRoot.Dir (OpamRepositoryRoot.Dir.Path.root root name)

let get_root rt name =
  get_root_raw rt.repos_global.root name

let get_repo_root rt repo =
  get_root_raw rt.repos_global.root repo.repo_name

(* it is simpler to keep it as is and not factorise dir & tgz *)
let get_repo_files rt name dir =
  match get_root rt name with
  | OpamRepositoryRoot.Dir repo_root ->
    let dir = OpamRepositoryRoot.Dir.Op.(repo_root / dir) in
    let files = OpamFilename.rec_files dir in
    List.map (fun file ->
        OpamFilename.Base.of_string
          (OpamSystem.back_to_forward (OpamFilename.remove_prefix dir file)),
        lazy (OpamFilename.read file))
      files
  | OpamRepositoryRoot.Tgz tgz ->
    let xfiles_dir = OpamFilename.Unix.Dir.of_string dir in
    OpamRepositoryRoot.Tgz.fold (fun acc filename content ->
        if OpamFilename.Unix.starts_with xfiles_dir filename then
          let content = Lazy.from_val content in
          let basename =
            filename
            |> OpamFilename.Unix.remove_prefix xfiles_dir
            |> OpamFilename.Base.of_string
          in
          (basename, content)::acc
        else acc)
      [] tgz

let read_package_opam_dir ~repo_name ~repo_root package_dir =
  match OpamFileTools.read_repo_opam_dir ~repo_name ~repo_root package_dir with
  | Some opam ->
    (try
       let nv =
         OpamPackage.of_string
           (OpamFilename.Base.to_string (OpamFilename.basename_dir package_dir))
       in
       Some (nv, opam)
     with Failure _ ->
       log "ERR: directory name not a valid package: ignored %s"
         (OpamFilename.to_string
            OpamFilename.Op.(package_dir // OpamRepositoryPathName.opam_f));
       None)
  | None ->
    log "ERR: Could not load %s, ignored"
      (OpamFilename.to_string
         OpamFilename.Op.(package_dir // OpamRepositoryPathName.opam_f));
    None

let read_package_opam_tgz ~repo_name ~repo_root package_dir
    filename content extrafiles =
  match OpamFileTools.read_repo_opam_tgz ~repo_name ~repo_root
          package_dir filename content extrafiles with
  | Some opam ->
    (try
       let nv =
         OpamPackage.of_string
           (OpamFilename.Unix.Base.to_string
              (OpamFilename.Unix.Dir.basename package_dir))
       in
       Some (nv, opam)
     with Failure _ ->
       log "ERR: directory name not a valid package: ignored %s"
         (OpamFilename.Unix.to_string
            OpamFilename.Unix.Op.(package_dir // OpamRepositoryPathName.opam_f));
       None)
  | None ->
    log "ERR: Could not load %s, ignored"
      (OpamFilename.Unix.to_string
         OpamFilename.Unix.Op.(package_dir // OpamRepositoryPathName.opam_f));
    None

let load_raw_opams_and_aux_from_tgz _repo_name tgz =
  (* We first retrieve the content of the repository *)
  let raw_repository =
    OpamRepositoryRoot.Tgz.fold (fun acc filename content ->
        (filename, content) :: acc)
      [] tgz
  in
  (* we extract the repo file, the repository definition *)
  let repo_def =
    let filename = OpamFilename.Unix.of_string OpamRepositoryPathName.repo_f in
    match List.assoc_opt filename raw_repository with
    | Some content ->
      let unix_filename = filename in
      let filename = OpamFile.make (OpamFilename.Unix.to_filename filename) in
      log ~level:5 "read %s"
        (OpamFilename.Unix.to_string unix_filename);
      OpamRepositoryRoot.read_file ~safe:true (module OpamFile.Repo)
        (OpamRepositoryRoot.Tgz tgz) ~filename content
    | None -> OpamFile.Repo.empty
  in
  (* Then we begin the construction of the raw opam map. It contains as key the
     dirname, and as value the opam filename, content, and a map of (filename,
     content) for extrafiles. *)
  (* We extract the opam files and their content *)
  let opams_map =
    List.fold_left (fun acc (filename, content) ->
        if OpamFilename.Unix.starts_with
            (OpamFilename.Unix.Dir.of_string OpamRepositoryPathName.packages_d)
            filename
        && String.equal OpamPathName.opam_f
             OpamFilename.Unix.(Base.to_string (basename filename)) then
          let key = OpamFilename.Unix.dirname filename in
          let value = filename, content, OpamFilename.Unix.Map.empty in
          OpamFilename.Unix.Dir.Map.add key value acc
        else acc)
      OpamFilename.Unix.Dir.Map.empty raw_repository
  in
  (* Map each opam file to its extrafiles, by comparing packages directories *)
  let opams_map =
    let exception Found of
        OpamFilename.Unix.Dir.t
        * (OpamFilename.Unix.t * string * string OpamFilename.Unix.Map.t)
    in
    List.fold_left (fun acc (filename, content) ->
        try
          OpamFilename.Unix.Dir.Map.iter (fun dir value ->
              if OpamFilename.Unix.starts_with dir filename then
                raise (Found (dir, value))) acc;
          acc
        with Found (key, value) ->
          let fo, co, map = value in
          let map = OpamFilename.Unix.Map.add filename content map in
          OpamFilename.Unix.Dir.Map.add key (fo, co, map) acc)
      opams_map raw_repository
  in
  repo_def, opams_map

let load_repo_from_tgz repo_name tgz =
  if OpamConsole.disp_status_line () || OpamConsole.verbose () then
    OpamConsole.status_line "Processing: [%s: loading data]"
      (OpamConsole.colorise `blue (OpamRepositoryName.to_string repo_name));
  log ~level:3 "load repo %a from tgz"
    (slog OpamRepositoryName.to_string) repo_name;
  let repo_root = tgz in
  let aux () =
    let repo_def, opams_map =
      load_raw_opams_and_aux_from_tgz repo_name tgz
    in
    (* repo_url is added in load_repo to avoid having it as argument *)
    let opams =
      OpamFilename.Unix.Dir.Map.fold
        (fun pkgdir (filename, content, otherfiles) opams ->
           match read_package_opam_tgz ~repo_name ~repo_root
                   pkgdir filename content otherfiles with
           | Some (nv, opam) -> OpamPackage.Map.add nv opam opams
           | None -> opams)
        opams_map OpamPackage.Map.empty
    in
    repo_def, opams
  in
  Fun.protect (fun () -> aux ()) ~finally:OpamConsole.clear_status

let load_opams_from_tgz repo_name tgz =
  snd (load_repo_from_tgz repo_name tgz)

let load_opams_from_dir repo_name repo_root =
  if OpamConsole.disp_status_line () || OpamConsole.verbose () then
    OpamConsole.status_line "Processing: [%s: loading data]"
      (OpamConsole.colorise `blue (OpamRepositoryName.to_string repo_name));
  (* FIXME: why is this different from OpamPackage.list ? *)
  log ~level:3 "load repo %a from dir"
    (slog OpamRepositoryName.to_string) repo_name;
  let rec aux r dir =
    if OpamFilename.exists_dir dir then
      let dir_str = OpamFilename.Dir.to_string dir in
      let fnames = Sys.readdir dir_str in
      let ( / ) = Filename.concat in
      Array.sort String.compare fnames;
      if Array.exists (fun f -> String.equal f OpamPathName.opam_f &&
                                not (Sys.is_directory (dir_str / f))) fnames then
        match read_package_opam_dir ~repo_name ~repo_root dir with
        | Some (nv, opam) -> OpamPackage.Map.add nv opam r
        | None -> r
      else
        Array.fold_left (fun r name -> aux r OpamFilename.Op.(dir / name))
          r fnames
    else r
  in
  Fun.protect
    (fun () ->
       aux OpamPackage.Map.empty
         (OpamRepositoryRoot.Dir.Path.packages_dir repo_root))
    ~finally:OpamConsole.clear_status

let load_opams repo_name repo_root =
  match repo_root with
  | OpamRepositoryRoot.Dir dir ->
    load_opams_from_dir repo_name dir
  | OpamRepositoryRoot.Tgz tgz ->
    load_opams_from_tgz repo_name tgz

let load_opams_from_diff repo diffs rt =
  if OpamConsole.disp_status_line () || OpamConsole.verbose () then
    OpamConsole.status_line "Processing: [%s: loading data]"
      (OpamConsole.colorise `blue (OpamRepositoryName.to_string repo.repo_name));
  log ~level:3 "load repo %a from diff"
    (slog OpamRepositoryName.to_string) repo.repo_name;
  let existing_opams =
    OpamRepositoryName.Map.find repo.repo_name rt.repo_opams
  in
  let repo_root = get_repo_root rt repo in
  let read_package_opam =
    match repo_root with
    | OpamRepositoryRoot.Dir dir ->
      let repo_root = dir in
      fun dir ->
        let dir = OpamFilename.Unix.Dir.to_dir dir in
        let dir =
          OpamRepositoryRoot.Dir.Op.(repo_root
                                     / (OpamFilename.Dir.to_string dir))
        in
        read_package_opam_dir ~repo_name:repo.repo_name ~repo_root dir
    | OpamRepositoryRoot.Tgz tgz ->
      let repo_root = tgz in
      let _, opams_map =
        load_raw_opams_and_aux_from_tgz repo.repo_name tgz
      in
      fun dir ->
        let open OpamStd.Option.Op in
        OpamFilename.Unix.Dir.Map.find_opt dir opams_map
        >>= fun (filename, content, xfiles) ->
        read_package_opam_tgz ~repo_name:repo.repo_name
          ~repo_root dir filename content xfiles

  in
  (*  processed_dirs: used to avoid re-read in case of diff generated by extra files.
      added_pkgs: used to skip removing version-equivalent packages *)
  let process_file (opams, processed_dirs, added_pkgs) file ~is_removal =
    let pkg_dir =
      let file = OpamFilename.Unix.of_string file in
      let dirname = OpamFilename.Unix.dirname file in
      let basename = OpamFilename.Unix.Dir.basename dirname in
      if String.equal OpamRepositoryPathName.files_d
          (OpamFilename.Unix.Base.to_string basename) then
        OpamFilename.Unix.Dir.dirname dirname
      else
        dirname
    in
    if OpamFilename.Unix.Dir.Set.mem pkg_dir processed_dirs then
      opams, processed_dirs, added_pkgs
    else
      let processed_dirs = OpamFilename.Unix.Dir.Set.add pkg_dir processed_dirs in
      match read_package_opam pkg_dir with
      | Some (nv, opam) ->
        let added_pkgs = OpamPackage.Set.add nv added_pkgs in
        OpamPackage.Map.add nv opam opams, processed_dirs, added_pkgs
      | None ->
        if is_removal then
          match
            OpamPackage.of_dirname (OpamFilename.Unix.Dir.to_dir pkg_dir)
          with
          | None ->
            log "ERR: directory name not a valid package: ignored %s"
              (OpamFilename.Unix.Dir.to_string pkg_dir);
            opams, processed_dirs, added_pkgs
          | Some nv ->
            if OpamPackage.Set.mem nv added_pkgs then
              opams, processed_dirs, added_pkgs
            else
              OpamPackage.Map.remove nv opams, processed_dirs, added_pkgs
        else
          opams, processed_dirs, added_pkgs
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
    (fun () ->
       let opams, _, _ =
         List.fold_left process_operation
           (existing_opams, OpamFilename.Unix.Dir.Set.empty, OpamPackage.Set.empty)
           diffs
       in
       opams)
    ~finally:OpamConsole.clear_status

let load_repo_from_dir repo repo_root =
  let repo_def =
    (* Have a non repo_root dependant version for this ? *)
    OpamRepositoryRoot.Dir.Path.repo repo_root
    |> OpamFile.Repo.safe_read
    |> OpamFile.Repo.with_root_url repo.repo_url
  in
  let opams = load_opams_from_dir repo.repo_name repo_root in
  repo_def, opams

let load_repo repo repo_root =
  let t = OpamConsole.timer () in
  let loaded_repo =
    match repo_root with
    | OpamRepositoryRoot.Tgz tgz ->
      let repo_def, opams =
        load_repo_from_tgz repo.repo_name tgz
      in
      let repo_def =
        repo_def
        |> OpamFile.Repo.with_root_url repo.repo_url
      in
      repo_def, opams
    | OpamRepositoryRoot.Dir dir ->
      load_repo_from_dir repo dir
  in
  log "loaded opam files from repo %s in %.3fs"
    (OpamRepositoryName.to_string repo.repo_name)
    (t ());
  loaded_repo

let cleanup _rt = ()

let syspkgs_available ?env = function
  | None -> None
  | Some (family, availability) ->
    if OpamSysInteract.same_os_family family ?env then
      Some availability
    else None

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
  let make_rt repos_definitions opams repos_syspkgs_available =
    let rt = {
      repos_global = (gt :> unlocked global_state);
      repos_lock = lock;
      repositories;
      repos_definitions;
      repo_opams = opams;
      repos_syspkgs_available;
    } in
    OpamStd.Sys.at_exit (fun () -> cleanup rt);
    rt
  in
  let ro =
    let ro = lazy (OpamGlobalState.is_root_read_only gt) in
    fun () -> Lazy.force ro
  in
  let get_depexts opams =
    if not OpamStateConfig.(!r.depexts)
    || OpamCoreConfig.(!r.safe_mode)
    || ro ()
    then
      None
    else
      (let repo_depexts =
         let env = OpamPackageVar.resolve_global gt in
         OpamRepositoryName.Map.fold (fun _ opams all_depexts ->
             let repo_depexts =
               OpamFileTools.opams_depexts opams ~env
             in
             let repo_depexts =
               OpamSysPkg.Set.Op.(repo_depexts ++ all_depexts)
             in
             repo_depexts)
           opams OpamSysPkg.Set.empty
       in
       if OpamSysPkg.Set.is_empty repo_depexts then None else
         try
           let env = gt.global_variables in
           Some (OpamSysInteract.available_packages_and_family ~env
                   gt.config repo_depexts)
         with Failure _ ->
           (* We print nothing here because the printing will occur when packages
              need to be computed with depexts (other polling attempt) *)
           None)
  in
  match Cache.load gt.root with
  | Some (repofiles, opams, sys_available_pkgs) ->
    log "Cache found";
    let sys_available_pkgs, depexts_updated =
      match syspkgs_available ~env:gt.global_variables sys_available_pkgs with
      | Some _ -> sys_available_pkgs, false
      | None ->
        match get_depexts opams with
        | None -> None, false
        | some -> some, true
    in
    let rt = make_rt repofiles opams sys_available_pkgs in
    if depexts_updated && not (ro ()) then Cache.save rt;
    rt
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
        repos_map (OpamRepositoryName.Map.empty,
                   OpamRepositoryName.Map.empty)
    in
    let repos_syspkgs_available = get_depexts opams in
    let rt = make_rt repofiles opams repos_syspkgs_available in
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

let unlock ?cleanup:(cln=true) rt =
  if cln then cleanup rt;
  OpamSystem.funlock rt.repos_lock;
  (rt :> unlocked repos_state)

let drop ?cleanup rt =
  let _ = unlock ?cleanup rt in
  ()

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

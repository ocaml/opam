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
open OpamStateTypes
open OpamStd.Op
open OpamProcess.Job.Op
open OpamFilename.Op

let log fmt = OpamConsole.log "UPDATE" fmt
let slog = OpamConsole.slog

let eval_redirect gt repo =
  if repo.repo_url.OpamUrl.backend <> `http then None else
  let redirect =
    repo.repo_root
    |> OpamRepositoryPath.repo
    |> OpamFile.Repo.safe_read
    |> OpamFile.Repo.redirect
  in
  let redirect = List.fold_left (fun acc (redirect, filter) ->
      if OpamFilter.opt_eval_to_bool (OpamPackageVar.resolve_global gt) filter
      then (redirect, filter) :: acc
      else acc
    ) [] redirect in
  match redirect with
  | []         -> None
  | (redirect, f) :: _ ->
    let redirect_url =
      if OpamStd.String.contains ~sub:"://" redirect
      then OpamUrl.of_string redirect
      else OpamUrl.Op.(repo.repo_url / redirect)
    in
    if redirect_url = repo.repo_url then None
    else Some (redirect_url, f)

let repository gt repo =
  let max_loop = 10 in
  if repo.repo_url = OpamUrl.empty then Done (fun rt -> rt) else
  (* Recursively traverse redirection links, but stop after 10 steps or if
     we cycle back to the initial repo. *)
  let rec job r n =
    if n = 0 then
      (OpamConsole.warning "%s: Too many redirections, stopping."
         (OpamRepositoryName.to_string repo.repo_name);
       Done r)
    else
    let text =
      OpamProcess.make_command_text ~color:`blue
        (OpamRepositoryName.to_string repo.repo_name)
        OpamUrl.(string_of_backend repo.repo_url.backend)
    in
    OpamProcess.Job.with_text text @@
    OpamRepository.update r @@+ fun () ->
    if n <> max_loop && r = repo then
      (OpamConsole.warning "%s: Cyclic redirections, stopping."
         (OpamRepositoryName.to_string repo.repo_name);
       Done r)
    else match eval_redirect gt r with
      | None -> Done r
      | Some (new_url, f) ->
        OpamFilename.cleandir repo.repo_root;
        let reason = match f with
          | None   -> ""
          | Some f -> Printf.sprintf " (%s)" (OpamFilter.to_string f) in
        OpamConsole.note
          "The repository '%s' will be *%s* redirected to %s%s"
          (OpamRepositoryName.to_string repo.repo_name)
          (OpamConsole.colorise `bold "permanently")
          (OpamUrl.to_string new_url)
          reason;
        job { r with repo_url = new_url } (n-1)
  in
  job repo max_loop @@+ fun repo ->
  let repo_file_path = OpamRepositoryPath.repo repo.repo_root in
  if not (OpamFile.exists repo_file_path) then
    OpamConsole.warning
      "The repository '%s' at %s doesn't have a 'repo' file, and might not be \
       compatible with this version of opam."
      (OpamRepositoryName.to_string repo.repo_name)
      (OpamUrl.to_string repo.repo_url);
  let repo_file = OpamFile.Repo.safe_read repo_file_path in
  let repo_file = OpamFile.Repo.with_root_url repo.repo_url repo_file in
  let repo_vers =
    OpamStd.Option.default OpamVersion.current_nopatch @@
    OpamFile.Repo.opam_version repo_file in
  if not OpamFormatConfig.(!r.skip_version_checks) &&
     OpamVersion.compare repo_vers OpamVersion.current > 0 then
    Printf.ksprintf failwith
      "repository format version is %s, and this is only opam %s"
      (OpamVersion.to_string repo_vers)
      (OpamVersion.to_string OpamVersion.current);
  List.iter (fun (msg, filter) ->
      if OpamFilter.opt_eval_to_bool (OpamPackageVar.resolve_global gt) filter
      then
        OpamConsole.formatted_msg ~indent:4 "%s (at %s): %s\n"
          (OpamConsole.colorise' [`bold;`green]
             (OpamRepositoryName.to_string repo.repo_name))
          (OpamConsole.colorise `bold (OpamUrl.to_string repo.repo_url))
          msg)
    (OpamFile.Repo.announce repo_file);
  let opams = OpamRepositoryState.load_repo_opams repo in
  Done (
    (* Return an update function to make parallel execution possible *)
    fun rt ->
      { rt with
        repositories =
          OpamRepositoryName.Map.add repo.repo_name repo rt.repositories;
        repos_definitions =
          OpamRepositoryName.Map.add repo.repo_name repo_file
            rt.repos_definitions;
        repo_opams =
          OpamRepositoryName.Map.add repo.repo_name opams rt.repo_opams;
      }
  )

let repositories rt repos =
  let command repo =
    OpamProcess.Job.catch
      (fun ex ->
         OpamStd.Exn.fatal ex;
         OpamConsole.error "Could not update repository %S: %s"
           (OpamRepositoryName.to_string repo.repo_name)
           (match ex with Failure s -> s | ex -> Printexc.to_string ex);
         Done ([repo], fun t -> t)) @@
    fun () -> repository rt.repos_global repo @@|
    fun f -> [], f
  in
  let failed, rt_update =
    OpamParallel.reduce
      ~jobs:OpamStateConfig.(!r.dl_jobs)
      ~command
      ~merge:(fun (failed1, f1) (failed2, f2) -> failed1 @ failed2, f1 @* f2)
      ~nil:([], fun x -> x)
      ~dry_run:OpamStateConfig.(!r.dryrun)
      repos
  in
  let rt = rt_update rt in
  OpamRepositoryState.write_config rt;
  OpamRepositoryState.Cache.save rt;
  failed, rt

let fetch_dev_package url srcdir ?(working_dir=false) nv =
  let remote_url = OpamFile.URL.url url in
  let mirrors = remote_url :: OpamFile.URL.mirrors url in
  let checksum = OpamFile.URL.checksum url in
  log "updating %a" (slog OpamUrl.to_string) remote_url;
  OpamRepository.pull_tree
    ~cache_dir:(OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir))
    (OpamPackage.to_string nv) srcdir checksum ~working_dir mirrors

let pinned_package st ?version ?(working_dir=false) name =
  log "update-pinned-package %s%a" (OpamPackage.Name.to_string name)
    (slog @@ function true -> " (working dir)" | false -> "") working_dir;
  let open OpamStd.Option.Op in
  let root = st.switch_global.root in
  let overlay_dir = OpamPath.Switch.Overlay.package root st.switch name in
  let overlay_opam = OpamFileTools.read_opam overlay_dir in
  match overlay_opam >>| fun opam -> opam, OpamFile.OPAM.url opam with
  | None | Some (_, None) -> Done ((fun st -> st), false)
  | Some (opam, Some urlf) ->
    let url = OpamFile.URL.url urlf in
    let version =
      OpamFile.OPAM.version_opt opam ++
      version +!
      OpamPackage.Version.of_string "dev"
    in
    let nv = OpamPackage.create name version in
    let srcdir = OpamPath.Switch.pinned_package root st.switch name in
    (* Four versions of the metadata: from the old and new versions
       of the package, from the current overlay, and also the original one
       from the repo *)
    let add_extra_files srcdir file opam =
      if OpamFilename.dirname (OpamFile.filename file) <> srcdir
      then OpamFileTools.add_aux_files ~files_subdir_hashes:true opam
      else opam
    in
    let old_source_opam_hash, old_source_opam =
      match OpamPinned.find_opam_file_in_source name srcdir with
      | None -> None, None
      | Some f ->
        Some (OpamHash.compute (OpamFile.to_string f)),
        try
          Some (OpamFile.OPAM.read f |> OpamFile.OPAM.with_name name |>
                add_extra_files srcdir f)
        with e -> OpamStd.Exn.fatal e; None
    in
    let repo_opam =
      let packages =
        OpamPackage.Map.filter (fun nv _ -> nv.name = name) st.repos_package_index
      in
      (* get the latest version below v *)
      match OpamPackage.Map.split nv packages with
      | _, (Some opam), _ -> Some opam
      | below, None, _ when not (OpamPackage.Map.is_empty below) ->
        Some (snd (OpamPackage.Map.max_binding below))
      | _, None, above when not (OpamPackage.Map.is_empty above) ->
        Some (snd (OpamPackage.Map.min_binding above))
      | _ -> None
    in
    (if working_dir then Done () else
       (match url.OpamUrl.hash with
        | None -> Done true
        | Some h ->
          OpamRepository.current_branch url @@| fun branch -> branch = Some h)
       @@+ function false -> Done () | true ->
         OpamRepository.is_dirty url
         @@| function false -> () | true ->
           OpamConsole.note
             "Ignoring uncommitted changes in %s (`--working-dir' not active)."
             url.OpamUrl.path)
    @@+ fun () ->
    (* Do the update *)
    fetch_dev_package urlf srcdir ~working_dir nv @@+ fun result ->
    let new_source_opam =
      OpamPinned.find_opam_file_in_source name srcdir >>= fun f ->
      let warns, opam_opt = OpamFileTools.lint_file f in
      let warns, opam_opt = match opam_opt with
        | Some opam0 ->
          let opam = OpamFormatUpgrade.opam_file ~quiet:true ~filename:f opam0 in
          if opam <> opam0 then OpamFileTools.lint opam, Some opam
          else warns, Some opam0
        | None -> warns, opam_opt
      in
      if warns <> [] &&
         match old_source_opam_hash with
         | None -> true
         | Some h -> not (OpamHash.check_file (OpamFile.to_string f) h)
      then
        (OpamConsole.warning
           "%s opam file from upstream of %s:"
           (if opam_opt = None then "Fatal errors, not using"
            else "Failed checks in")
           (OpamConsole.colorise `bold (OpamPackage.Name.to_string name));
         OpamConsole.errmsg "%s\n"
           (OpamFileTools.warns_to_string warns));
      opam_opt >>| OpamFile.OPAM.with_name name >>| add_extra_files srcdir f
    in
    let equal_opam a b =
      let cleanup_opam o =
        let v =
          OpamStd.Option.default (OpamPackage.Version.of_string "~dev")
            (OpamFile.OPAM.version_opt o)
        in
        OpamFile.OPAM.effective_part
          (o |> OpamFile.OPAM.with_url_opt None
           |> OpamFile.OPAM.with_version v)
      in
      cleanup_opam (OpamFormatUpgrade.opam_file a) = cleanup_opam b
    in
    let changed_opam old new_ = match old, new_ with
      | None, Some _ -> true
      | _, None -> false
      | Some a, Some b -> not (equal_opam a b)
    in
    let save_overlay opam =
      OpamFilename.mkdir overlay_dir;
      let opam_file = OpamPath.Switch.Overlay.opam root st.switch name in
      List.iter OpamFilename.remove
        OpamPath.Switch.Overlay.([
            OpamFile.filename opam_file;
            OpamFile.filename (url root st.switch name);
            OpamFile.filename (descr root st.switch name);
          ]);
      let files_dir = OpamPath.Switch.Overlay.files root st.switch name in
      OpamFilename.rmdir files_dir;
      let opam =
        OpamFile.OPAM.with_url urlf @@
        OpamFile.OPAM.with_name name opam
      in
      let opam =
        if OpamFile.OPAM.version_opt opam = None
        then OpamFile.OPAM.with_version version opam
        else opam
      in
      List.iter (fun (file, rel_file, hash) ->
          if OpamHash.check_file (OpamFilename.to_string file) hash then
            OpamFilename.copy ~src:file
              ~dst:(OpamFilename.create files_dir rel_file)
          else
            OpamConsole.warning "Ignoring file %s with invalid hash"
              (OpamFilename.to_string file))
        (OpamFile.OPAM.get_extra_files opam);
      OpamFile.OPAM.write opam_file
        (OpamFile.OPAM.with_extra_files_opt None opam);
      opam
    in
    match result, new_source_opam with
    | Result (), Some new_opam
      when changed_opam old_source_opam new_source_opam &&
           changed_opam overlay_opam new_source_opam ->
      log "Metadata from the package source of %s changed"
        (OpamPackage.to_string nv);
      let interactive_part st =
        if not (changed_opam old_source_opam overlay_opam) ||
           not (changed_opam repo_opam overlay_opam)
        then
          (* No manual changes *)
          (OpamConsole.formatted_msg
             "[%s] Installing new package description from upstream %s\n"
             (OpamConsole.colorise `green (OpamPackage.Name.to_string name))
             (OpamUrl.to_string url);
           let opam = save_overlay new_opam in
           OpamSwitchState.update_pin nv opam st)
        else if
          OpamConsole.formatted_msg
            "[%s] Conflicting update of the metadata from %s."
            (OpamConsole.colorise `green (OpamPackage.Name.to_string name))
            (OpamUrl.to_string url);
          OpamConsole.confirm "\nOverride files in %s (there will be a backup) ?"
            (OpamFilename.Dir.to_string overlay_dir)
        then (
          let bak =
            OpamPath.backup_dir root / (OpamPackage.Name.to_string name ^ ".bak")
          in
          OpamFilename.mkdir (OpamPath.backup_dir root);
          OpamFilename.rmdir bak;
          OpamFilename.copy_dir ~src:overlay_dir ~dst:bak;
          OpamConsole.formatted_msg "User metadata backed up in %s\n"
            (OpamFilename.Dir.to_string bak);
          let opam = save_overlay new_opam in
          OpamSwitchState.update_pin nv opam st)
        else
          st
      in
      Done (interactive_part, true)
    | (Up_to_date _ | Not_available _), _ ->
      Done ((fun st -> st), false)
    | Result  (), Some new_opam ->
      (* The new opam is not _effectively_ different from the old, so no need to
         confirm, but use it still (e.g. descr may have changed) *)
      let opam = save_overlay new_opam in
      Done
        ((fun st -> {st with opams = OpamPackage.Map.add nv opam st.opams}),
         true)
    | Result  (), None ->
      Done ((fun st -> st), true)

let dev_package st ?working_dir nv =
  log "update-dev-package %a" (slog OpamPackage.to_string) nv;
  if OpamSwitchState.is_pinned st nv.name &&
     not (OpamSwitchState.is_version_pinned st nv.name) then
    pinned_package st ~version:nv.version ?working_dir nv.name
  else
  match OpamSwitchState.url st nv with
  | None     -> Done ((fun st -> st), false)
  | Some url ->
    if (OpamFile.URL.url url).OpamUrl.backend = `http then
      Done ((fun st -> st), false)
    else
      fetch_dev_package url (OpamSwitchState.source_dir st nv) ?working_dir nv
      @@| fun result ->
      (fun st -> st), match result with Result () -> true | _ -> false

let dev_packages st ?(working_dir=OpamPackage.Set.empty) packages =
  log "update-dev-packages";
  let command nv =
    let working_dir = OpamPackage.Set.mem nv working_dir in
    OpamProcess.Job.ignore_errors
      ~default:(false, (fun st -> st), OpamPackage.Set.empty)
    @@ fun () ->
    dev_package st ~working_dir nv @@| fun (st_update, changed) ->
    true, st_update, match changed with
    | true -> OpamPackage.Set.singleton nv
    | false -> OpamPackage.Set.empty
  in
  let merge (ok1, st_update1, set1) (ok2, st_update2, set2) =
    ok1 && ok2,
    (fun st -> st_update1 (st_update2 st)),
    OpamPackage.Set.union set1 set2
  in
  let success, st_update, updated_set =
    OpamParallel.reduce ~jobs:OpamStateConfig.(!r.dl_jobs)
      ~command
      ~merge
      ~nil:(true, (fun st -> st), OpamPackage.Set.empty)
      (OpamPackage.Set.elements packages)
  in
  let selections0 = OpamSwitchState.selections st in
  let st = st_update st in
  let st =
    OpamSwitchAction.add_to_reinstall st ~unpinned_only:false updated_set
  in
  (* The following is needed for pinned packages that may have changed
     version *)
  let selections1 = OpamSwitchState.selections st in
  if selections0 <> selections1 then
    OpamFile.SwitchSelections.write
      (OpamPath.Switch.selections st.switch_global.root st.switch)
      selections1;
  success, st, updated_set

let pinned_packages st ?(working_dir=OpamPackage.Name.Set.empty) names =
  log "update-pinned-packages";
  let command name =
    let working_dir = OpamPackage.Name.Set.mem name working_dir in
    OpamProcess.Job.ignore_errors
      ~default:((fun st -> st), OpamPackage.Name.Set.empty)
    @@ fun () ->
    pinned_package st ~working_dir name @@| fun (st_update, changed) ->
    st_update,
    match changed with
    | true -> OpamPackage.Name.Set.singleton name
    | false -> OpamPackage.Name.Set.empty
  in
  let merge (st_update1, set1) (st_update2, set2) =
    (fun st -> st_update1 (st_update2 st)),
    OpamPackage.Name.Set.union set1 set2
  in
  let st_update, updates =
    OpamParallel.reduce
      ~jobs:(OpamFile.Config.jobs st.switch_global.config)
      ~command
      ~merge
      ~nil:((fun st -> st), OpamPackage.Name.Set.empty)
      (OpamPackage.Name.Set.elements names)
  in
  let st = st_update st in
  let updates =
    OpamPackage.Name.Set.fold (fun name acc ->
        OpamPackage.Set.add (OpamPinned.package st name) acc)
      updates OpamPackage.Set.empty
  in
  OpamSwitchAction.add_to_reinstall st ~unpinned_only:false updates,
  updates

let active_caches st nv =
  let global_cache = OpamFile.Config.dl_cache st.switch_global.config in
  let rt = st.switch_repos in
  let repo_cache =
    match OpamRepositoryState.find_package_opt rt
            (OpamSwitchState.repos_list st) nv
    with
    | None -> []
    | Some (repo, _) ->
      let repo_def = OpamRepositoryName.Map.find repo rt.repos_definitions in
      let root_url = match OpamFile.Repo.root_url repo_def with
        | None -> OpamSystem.internal_error "repo file of unknown origin"
        | Some u -> u
      in
      List.map (fun rel ->
          if OpamStd.String.contains ~sub:"://" rel
          then OpamUrl.of_string rel
          else OpamUrl.Op.(root_url / rel))
        (OpamFile.Repo.dl_cache repo_def)
  in
  repo_cache @ global_cache

let cleanup_source st old_opam_opt new_opam =
  let open OpamStd.Option.Op in
  let base_url urlf =
    let u = OpamFile.URL.url urlf in
    { u with OpamUrl.hash = None }
  in
  let url_remote opam = OpamFile.OPAM.url opam >>| base_url in
  if url_remote new_opam <> (old_opam_opt >>= url_remote) then
    OpamFilename.rmdir
      (OpamSwitchState.source_dir st (OpamFile.OPAM.package new_opam))

let download_package_source st nv dirname =
  let opam = OpamSwitchState.opam st nv in
  let cache_dir = OpamRepositoryPath.download_cache st.switch_global.root in
  let cache_urls = active_caches st nv in

  let fetch_source_job =
    match OpamFile.OPAM.url opam with
    | None   -> Done None
    | Some u ->
      OpamRepository.pull_tree (OpamPackage.to_string nv)
        ~cache_dir ~cache_urls
        dirname
        (OpamFile.URL.checksum u)
        (OpamFile.URL.url u :: OpamFile.URL.mirrors u)
      @@| OpamStd.Option.some
  in
  let fetch_extra_source_job (name, u) = function
    | Some (Not_available _) as err -> Done err
    | ret ->
      OpamRepository.pull_file_to_cache
        (OpamPackage.to_string nv ^"/"^ OpamFilename.Base.to_string name)
        ~cache_dir ~cache_urls
        (OpamFile.URL.checksum u)
        (OpamFile.URL.url u :: OpamFile.URL.mirrors u)
      @@| function
      | Not_available _ as na -> Some na
      | _ -> ret
  in
  fetch_source_job @@+
  OpamProcess.Job.seq
    (List.map fetch_extra_source_job
       (OpamFile.OPAM.extra_sources opam))

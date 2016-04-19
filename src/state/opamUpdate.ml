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
open OpamStateTypes
open OpamStd.Op
open OpamProcess.Job.Op
open OpamFilename.Op

let log fmt = OpamConsole.log "UPDATE" fmt
let slog = OpamConsole.slog

let eval_redirect gt repo =
  if repo.repo_url.OpamUrl.backend <> `http then None else
  let redirect =
    repo
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
  | (r,f) :: _ ->
    let config_f = OpamRepositoryPath.config repo in
    let config = OpamFile.Repo_config.read config_f in
    let repo_url = OpamUrl.of_string r in
    if repo_url <> config.repo_url then (
      let config = { config with repo_url } in
      OpamFile.Repo_config.write config_f config;
      Some (config, f)
    ) else
      None

let repository rt repo =
  let max_loop = 10 in
  (* Recursively traverse redirection links, but stop after 10 steps or if
     we start to cycle. *)
  let rec job r n =
    if n = 0 then
      (OpamConsole.warning "%s: Too many redirections, stopping."
         (OpamRepositoryName.to_string repo.repo_name);
       Done ())
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
         Done ())
      else match eval_redirect rt.repos_global r with
        | None -> Done ()
        | Some (new_repo, f) ->
          OpamFilename.rmdir repo.repo_root;
          OpamFile.Repo_config.write (OpamRepositoryPath.config repo) new_repo;
          let reason = match f with
            | None   -> ""
            | Some f -> Printf.sprintf " (%s)" (OpamFilter.to_string f) in
          OpamConsole.note
            "The repository '%s' will be *%s* redirected to %s%s"
            (OpamRepositoryName.to_string repo.repo_name)
            (OpamConsole.colorise `bold "permanently")
            (OpamUrl.to_string new_repo.repo_url)
            reason;
          job new_repo (n-1)
  in
  job repo max_loop @@+ fun () ->
  let repo =
    repo
    |> OpamRepositoryPath.config
    |> OpamFile.Repo_config.safe_read
  in
  OpamRepository.check_version repo @@+ fun () ->
  let opams = OpamRepositoryState.load_repo_opams repo in
  Done (
    fun rt ->
      { rt with
        repositories =
          OpamRepositoryName.Map.add repo.repo_name repo rt.repositories;
        repo_opams =
          OpamRepositoryName.Map.add repo.repo_name opams rt.repo_opams;
      }
  )

let repositories rt repos =
  let command repo =
    OpamProcess.Job.ignore_errors ~default:(fun t -> t)
      ~message:("Could not update repository " ^
                OpamRepositoryName.to_string repo.repo_name) @@
    repository rt repo
  in
  let rt =
    OpamParallel.reduce
      ~jobs:OpamStateConfig.(!r.dl_jobs)
      ~command
      ~merge:( @* )
      ~nil:(fun x -> x)
      ~dry_run:OpamStateConfig.(!r.dryrun)
      repos
      rt
  in
  OpamRepositoryState.Cache.save rt;
  rt

(* fixme: this doesn't extract the archive, so we won't get the source package's
   opam file unless we're going through VC. *)
let fetch_dev_package url srcdir nv =
  let remote_url = OpamFile.URL.url url in
  let mirrors = remote_url :: OpamFile.URL.mirrors url in
  let checksum = OpamFile.URL.checksum url in
  log "updating %a" (slog OpamUrl.to_string) remote_url;
  let text =
    OpamProcess.make_command_text
      (OpamPackage.Name.to_string nv.name)
      (OpamUrl.string_of_backend remote_url.OpamUrl.backend) in
  OpamProcess.Job.with_text text @@
  OpamRepository.pull_url nv srcdir checksum mirrors

let pinned_package st ?fixed_version name =
  log "update-pinned-package %s" (OpamPackage.Name.to_string name);
  let open OpamStd.Option.Op in
  let root = st.switch_global.root in
  let overlay_dir = OpamPath.Switch.Overlay.package root st.switch name in
  let overlay_opam = OpamFileTools.read_opam overlay_dir in
  match overlay_opam >>| fun opam -> opam, OpamFile.OPAM.url opam with
  | None | Some (_, None) -> Done ((fun st -> st), false)
  | Some (opam, Some urlf) ->
  let url = OpamFile.URL.url urlf in
  let version = fixed_version +! OpamFile.OPAM.version opam in
  let nv = OpamPackage.create name version in
  let srcdir = OpamPath.Switch.dev_package root st.switch name in
  (* Four versions of the metadata: from the old and new versions
     of the package, from the current overlay, and also the original one
     from the repo *)
  let add_extra_files srcdir file opam =
    if OpamFilename.dirname (OpamFile.filename file) <> srcdir
    then OpamFileTools.add_aux_files opam
    else opam
  in
  let old_source_opam_hash, old_source_opam =
    match OpamPinned.find_opam_file_in_source name srcdir with
    | None -> None, None
    | Some f ->
      Some (OpamFilename.digest (OpamFile.filename f)),
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
  (* Do the update *)
  fetch_dev_package urlf srcdir nv @@+ fun result ->
  let new_source_opam =
    OpamPinned.find_opam_file_in_source name srcdir >>= fun f ->
    let warns, opam_opt = OpamFileTools.lint_file f in
    if warns <> [] &&
       Some (OpamFilename.digest (OpamFile.filename f)) <> old_source_opam_hash
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
      OpamFile.OPAM.effective_part
        (OpamFile.OPAM.with_version_opt None
           (OpamFile.OPAM.with_url_opt None o))
    in
    cleanup_opam a = cleanup_opam b
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
      OpamFile.OPAM.with_name name @@
      match fixed_version with
      | Some v -> OpamFile.OPAM.with_version v opam
      | None -> opam
    in
    List.iter (fun (file, rel_file, hash) ->
        if OpamFilename.digest file = hash then
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
  | Result _, Some new_opam
    when changed_opam old_source_opam new_source_opam &&
         changed_opam overlay_opam new_source_opam ->
    let interactive_part st =
      (* Metadata from the package source changed *)
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
  | Result  _, _ ->
    Done ((fun st -> st), true)

let dev_package st nv =
  log "update-dev-package %a" (slog OpamPackage.to_string) nv;
  if OpamPackage.Set.mem nv st.pinned then
    pinned_package st nv.name
  else
  match OpamSwitchState.url st nv with
  | None     -> Done ((fun st -> st), false)
  | Some url ->
    if (OpamFile.URL.url url).OpamUrl.backend = `http then
      Done ((fun st -> st), false)
    else
      fetch_dev_package url
        (OpamPath.Switch.dev_package st.switch_global.root st.switch nv.name) nv
      @@| fun result ->
      (fun st -> st), match result with Result _ -> true | _ -> false

let dev_packages st packages =
  log "update-dev-packages";
  let command nv =
    OpamProcess.Job.ignore_errors
      ~default:((fun st -> st), OpamPackage.Set.empty) @@
    dev_package st nv @@| fun (st_update, changed) ->
    st_update, match changed with
    | true -> OpamPackage.Set.singleton nv
    | false -> OpamPackage.Set.empty
  in
  let merge (st_update1, set1) (st_update2, set2) =
    (fun st -> st_update1 (st_update2 st)),
    OpamPackage.Set.union set1 set2
  in
  let st_update, updated_set =
    OpamParallel.reduce ~jobs:OpamStateConfig.(!r.dl_jobs)
      ~command
      ~merge
      ~nil:((fun st -> st), OpamPackage.Set.empty)
      (OpamPackage.Set.elements packages)
  in
  let st = st_update st in
  let st =
    OpamSwitchAction.add_to_reinstall st ~unpinned_only:false updated_set
  in
  st, updated_set

let pinned_packages st names =
  log "update-pinned-packages";
  let command name =
    OpamProcess.Job.ignore_errors
      ~default:((fun st -> st), OpamPackage.Name.Set.empty) @@
    pinned_package st name @@| fun (st_update, changed) ->
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

(* Download a package from its upstream source, using 'cache_dir' as cache
   directory. *)
let download_upstream st nv dirname =
  match OpamSwitchState.url st nv with
  | None   -> Done None
  | Some u ->
    let remote_url = OpamFile.URL.url u in
    let mirrors = remote_url :: OpamFile.URL.mirrors u in
    let checksum = OpamFile.URL.checksum u in
    let text =
      OpamProcess.make_command_text (OpamPackage.name_to_string nv)
        (OpamUrl.string_of_backend remote_url.OpamUrl.backend)
    in
    OpamProcess.Job.with_text text @@
    OpamRepository.pull_url nv dirname checksum mirrors
    @@| OpamStd.Option.some

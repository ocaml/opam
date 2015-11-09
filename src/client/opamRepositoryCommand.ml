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
open OpamState.Types
open OpamStd.Op
open OpamPackage.Set.Op
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "REPOSITORY" fmt
let slog = OpamConsole.slog

let update t repo =
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
      else match OpamState.redirect t r with
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
  Done (
    fun t ->
      { t with
        repositories =
          OpamRepositoryName.Map.add repo.repo_name repo t.repositories }
  )

let print_updated_compilers updates =

  let print singular plural map =
    if not (OpamCompiler.Set.is_empty map) then (
      if OpamCompiler.Set.cardinal map = 1 then
        OpamConsole.msg "%s:\n" singular
      else
        OpamConsole.msg "%s:\n" plural;
      OpamCompiler.Set.iter (fun comp ->
        OpamConsole.msg " - %s\n" (OpamCompiler.to_string comp)
      ) map
    ) in

  print
    "The following NEW compiler is available"
    "The following NEW compilers are available"
    updates.created;

  print
    "The following compiler description has been UPDATED"
    "The following compiler descriptions have been UPDATED"
    updates.updated;

  print
    "The following compiler description has been DELETED"
    "The following compiler descriptions have been DELETED"
    updates.deleted

let fix_compiler_descriptions t ~verbose =
  log "Updating %a/ ...\n"
    (slog (OpamFilename.prettify_dir @* OpamPath.compilers_dir))
    t.root;
  let global_index = OpamState.compiler_state t in
  let repo_index = OpamState.compiler_repository_state t in
  let niet = String.concat ":" in
  log "global-index: %a" (slog @@ OpamCompiler.Map.to_string niet) global_index;
  log "repo-index  : %a" (slog @@ OpamCompiler.Map.to_string niet) repo_index;

  let updated_compilers, new_compilers =
    let updated_compilers =
      OpamCompiler.Map.fold (fun comp state set ->
          if not (OpamCompiler.Map.mem comp global_index)
          || OpamCompiler.Map.find comp global_index <> state then
            OpamCompiler.Set.add comp set
          else
            set
        ) repo_index OpamCompiler.Set.empty in
    OpamCompiler.Set.partition (OpamState.is_compiler_installed t) updated_compilers in
  log "updated-compilers: %a" (slog OpamCompiler.Set.to_string) updated_compilers;
  log "new-compilers    : %a" (slog OpamCompiler.Set.to_string) new_compilers;

  let deleted_compilers =
    OpamCompiler.Set.fold (fun comp map ->
        if comp = OpamCompiler.system             (* system *)
        || OpamCompiler.Map.mem comp repo_index   (* OR available *)
        then
          map
        else
          OpamCompiler.Set.add comp map
      ) t.compilers OpamCompiler.Set.empty in
  log "deleted-compilers: %a" (slog OpamCompiler.Set.to_string) deleted_compilers;

  (* Delete compiler descritions (unless they are still installed) *)
  OpamCompiler.Set.iter (fun comp ->
      if not (OpamState.is_compiler_installed t comp) then
        let dir = OpamPath.compilers t.root comp in
        OpamFilename.rmdir dir;
    ) deleted_compilers;

  (* Update the compiler description *)
  OpamCompiler.Set.iter (fun comp ->
      match OpamState.repository_and_prefix_of_compiler t comp with
      | None                -> ()
      | Some (repo, prefix) ->
        let files = OpamRepository.compiler_files repo prefix comp in
        let dir = OpamPath.compilers t.root comp in
        OpamFilename.rmdir dir;
        OpamFilename.mkdir dir;
        List.iter (fun file ->
            OpamFilename.copy_in file dir
          ) files;
    ) (OpamCompiler.Set.union updated_compilers new_compilers);

  let updates = {
    created = new_compilers;
    updated = updated_compilers;
    deleted = deleted_compilers;
    changed = OpamCompiler.Set.empty; (* we don't reinstall compilers yet *)
  } in

  if verbose then print_updated_compilers updates;

  updates

let print_updated_packages t updates =

  let print singular plural map fn =
    if not (OpamPackage.Set.is_empty map) then (
      if OpamPackage.Set.cardinal map = 1 then
        OpamConsole.msg "%s:\n" singular
      else
        OpamConsole.msg "%s:\n" plural;
      OpamPackage.Set.iter (fun nv ->
        match fn nv with
        | None   -> OpamConsole.msg " - %s\n" (OpamPackage.to_string nv)
        | Some s -> OpamConsole.msg " - %s [%s]\n" (OpamPackage.to_string nv) s
      ) map
    ) in

  let installed_switches nv =
    let installed = OpamState.installed_versions t (OpamPackage.name nv) in
    let installed =
      try OpamPackage.Map.find nv installed
      with Not_found -> [] in (* XXX: should never happen *)
    match installed with
    | [] -> None
    | _  -> Some (
        Printf.sprintf "%s (%s)"
          (OpamPackage.Version.to_string (OpamPackage.version nv))
          (OpamStd.Format.pretty_list (List.map OpamSwitch.to_string installed))
      ) in

  let none _ = None in

  print
    "The following NEW package is available"
    "The following NEW packages are available"
    updates.created
    none;

  print
    "The following package has been CHANGED upstream and needs to be recompiled"
    "The following packages have been CHANGED upstream and need to be recompiled"
    updates.changed
    installed_switches;

  print
    "The following package has been UPDATED upstream"
    "The following packages have been UPDATED upstream"
    updates.updated
    none;

  print
    "The following package has been DELETED"
    "The following packages have been DELETED"
    updates.deleted
    none

let print_updated_dev_packages pinned_packages =
  let print singular plural map =
    if not (OpamPackage.Set.is_empty map) then (
      if OpamPackage.Set.cardinal map = 1 then
        OpamConsole.msg "%s:\n" singular
      else
        OpamConsole.msg "%s:\n" plural;
      OpamPackage.Set.iter (fun nv ->
          OpamConsole.msg " - %s\n" (OpamPackage.to_string nv)
        ) map
    ) in
  print
    "The following DEV package needs to be upgraded"
    "The following DEV packages need to be upgraded"
    pinned_packages

(* Check for updates in pinned packages *)
let update_dev_packages t ~verbose packages =
  log "update-dev-packages updates %a" (slog OpamPackage.Set.to_string) packages;
  let updates = OpamState.update_dev_packages t packages in
  if verbose then print_updated_dev_packages updates ;
  updates

(* Update the package contents, display the new packages and update
   reinstall *)
let fix_package_descriptions t ~verbose =
  log "Updating %a/ ...\n"
    (slog (OpamFilename.prettify_dir @* OpamPath.packages_dir)) t.root;

  let global_index = OpamState.package_state t in
  let repo_index   = OpamState.package_repository_state t in
  (* let niet = String.concat ":" in *)
  (* log "global-index: %s" (OpamPackage.Map.to_string niet global_index); *)
  (* log "repo-index  : %s" (OpamPackage.Map.to_string niet repo_index); *)

  let updated_packages, new_packages =
    let updated_packages =
      OpamPackage.Map.fold (fun nv state set ->
          try
            if OpamPackage.Map.find nv global_index <> state
            then OpamPackage.Set.add nv set
            else set
          with Not_found -> OpamPackage.Set.add nv set
        ) repo_index OpamPackage.Set.empty in
    OpamPackage.Set.partition (fun nv ->
        OpamPackage.Map.mem nv global_index
      ) updated_packages in

  let all_installed = OpamState.all_installed t in
  let changed_packages = OpamPackage.Set.inter all_installed updated_packages in
  let missing_installed_packages =
    OpamPackage.Set.filter (fun nv ->
        try
          match OpamPackage.Map.find nv global_index with
          | [] -> OpamPackage.Map.mem nv repo_index
          | _  -> false
        with Not_found ->
          true
      ) all_installed in

  log "new-packages     : %a" (slog OpamPackage.Set.to_string) new_packages;
  log "updated-packages : %a" (slog OpamPackage.Set.to_string) updated_packages;
  log "changed-packages : %a" (slog OpamPackage.Set.to_string) changed_packages;
  log "missing-installed: %a" (slog OpamPackage.Set.to_string) missing_installed_packages;

  let deleted_packages =
    t.packages -- OpamPackage.keys repo_index -- OpamState.pinned_packages t
  in
  log "deleted-packages: %a" (slog OpamPackage.Set.to_string) deleted_packages;

  (* Notify only about deleted packages that are installed or were just removed
     (ie ignore the one that were removed from upstream but still have data
     locally because they used to be installed) *)
  let upstream_deleted_packages =
    OpamPackage.Set.filter
      (fun nv ->
         OpamPackage.Set.mem nv all_installed ||
         not (OpamFilename.exists (OpamPath.opam t.root nv)))
      deleted_packages in

  (* Remove the deleted packages' data (unless they are still installed) *)
  OpamPackage.Set.iter (fun nv ->
      if not (OpamPackage.Set.mem nv all_installed) then (
        OpamFilename.rmdir  (OpamPath.packages t.root nv);
        OpamFilename.remove (OpamPath.archive t.root nv);
      )) deleted_packages;

  (* that's not a good idea *at all* to enable this hook if you
           are not in a testing environment *)
  if OpamClientConfig.(!r.sync_archives) then
    OpamParallel.iter
      ~jobs:(OpamState.dl_jobs t)
      ~command:(fun nv ->
          log "download %a"
            (slog @@ OpamFilename.to_string @* OpamPath.archive t.root) nv;
          OpamState.download_archive t nv @@+ fun _ -> Done ())
      (OpamPackage.Map.keys repo_index);

  (* Do not recompile a package if only OPAM or descr files have
     changed. We recompile a package:

     - if both global and repo states have an archive file and the
       checksums of important files have changed;

     - if both global and repo states don't have an archive file and
       the checksums of important files have changed;

     - if only one of them have an archive file, if the checksums of
       the important files without the archive have changed. *)
  let changed_packages = OpamPackage.Set.filter (fun nv ->
      let archive_g, checksums_g =
        OpamState.package_partial_state t nv ~archive:true in
      let archive_r, checksums_r =
        OpamState.package_repository_partial_state t nv ~archive:true in
      if archive_g = archive_r then
        checksums_g <> checksums_r
      else
        let _, checksums_g =
          OpamState.package_partial_state t nv ~archive:false in
        let _, checksums_r =
          OpamState.package_repository_partial_state t nv ~archive:false in
        checksums_g <> checksums_r
    ) changed_packages in
  log "packages-to-reinstall: %a" (slog OpamPackage.Set.to_string) changed_packages;

  (* Update the package descriptions *)
  OpamPackage.Set.iter (fun nv ->
      match OpamState.repository_and_prefix_of_package t nv with
      | None                -> ()
      | Some (repo, prefix) ->
        let dir = OpamPath.packages t.root nv in
        if OpamFilename.exists_dir dir then OpamFilename.rmdir dir;
        if OpamPackage.Set.mem nv all_installed then
          let root = OpamRepositoryPath.packages repo prefix nv in
          let files = OpamRepository.package_files repo prefix nv ~archive:false in
          assert (files <> []);
          OpamFilename.mkdir dir;
          List.iter (fun file ->
              OpamFilename.copy_in ~root file dir
            ) files;
          OpamFilename.remove (OpamPath.archive t.root nv);
          OpamFilename.remove (OpamRepositoryPath.archive repo nv);
    ) (OpamPackage.Set.union missing_installed_packages updated_packages);

  (* Remove archives of non-installed packages (these may no longer be
     up-to-date) *)
  OpamPackage.Set.iter (fun nv ->
      let f = OpamPath.archive t.root nv in
      if OpamFilename.exists f then
        (log "Cleaning up obsolete archive %a" (slog OpamFilename.to_string) f;
         OpamFilename.remove f))
    (OpamPackage.keys global_index -- all_installed);

  (* Display some warnings/errors *)
  OpamPackage.Set.iter (fun nv ->
      let file =
        OpamPath.Switch.Overlay.opam t.root t.switch (OpamPackage.name nv) in
      let file =
        if OpamFilename.exists file then file else OpamPath.opam t.root nv in
      if not (OpamFilename.exists file) then
        if OpamPackage.Map.mem nv repo_index then
          OpamConsole.error_and_exit "fatal: %s is missing" (OpamFilename.prettify file)
        else
          let installed = OpamState.installed_versions t (OpamPackage.name nv) in
          let switches = OpamPackage.Map.find nv installed in
          let switches_string =
            OpamStd.Format.pretty_list (List.map OpamSwitch.to_string switches) in
          OpamConsole.warning
            "%s is installed in %s but it does not have metadata."
            (OpamPackage.to_string nv) switches_string
    ) t.installed;

  let updates = {
    created = new_packages;
    updated = OpamPackage.Set.diff updated_packages changed_packages;
    deleted = upstream_deleted_packages;
    changed = changed_packages;
  } in

  if verbose then print_updated_packages t updates;

  (* update $opam/$oversion/reinstall for all installed switches *)
  OpamState.add_to_reinstall ~all_unpinned:true t updates.changed;

  updates

let update_package_index t =
  let file = OpamPath.package_index t.root in
  log "Updating %a ...\n" (slog OpamFilename.prettify) file;
  let package_index = OpamState.package_index t in
  OpamFile.Package_index.write file package_index;
  { t with package_index }

let update_compiler_index t =
  let file = OpamPath.compiler_index t.root in
  log "Updating %a ...\n" (slog OpamFilename.prettify) file;
  let compiler_index = OpamState.compiler_index t in
  OpamFile.Compiler_index.write file compiler_index;
  { t with compiler_index }

(* update the repository config file:
   ~/.opam/repo/<repo>/config *)
let update_config t repos =
  log "update-config %a"
    (slog @@ OpamStd.List.concat_map ", " OpamRepositoryName.to_string)
    repos;
  let new_config = OpamFile.Config.with_repositories t.config repos in
  OpamStateConfig.write t.root new_config

let fix_descriptions
    ?(save_cache=true) ?(verbose = OpamCoreConfig.(!r.verbose_level) >= 3) t =
  let t = update_compiler_index t in
  let _ = fix_compiler_descriptions t ~verbose in
  let t = update_package_index t in
  let _ = fix_package_descriptions t ~verbose in
  if save_cache then OpamState.Cache.save t

let () =
  OpamState.fix_descriptions_hook := fix_descriptions

(* Remove any remaining of [repo] from OPAM state *)
let cleanup t repo =
   log "cleanup %a" (slog OpamRepositoryName.to_string) repo.repo_name;
  let repos = OpamRepositoryName.Map.keys t.repositories in
  update_config t (List.filter ((<>) repo.repo_name) repos);
  OpamFilename.rmdir repo.repo_root;
  OpamState.Cache.remove ();
  fix_descriptions ~save_cache:false t

(* XXX: this module uses OpamState.load_state, which loads the full switch state; 
   actually the switch is completely unneeded *)

let priority repo_name ~priority =
  log "repository-priority";

  (* 1/ update the config file *)
  let t = OpamState.load_state ~save_cache:false "repository-priority"
      OpamStateConfig.(!r.current_switch) in
  let repo = OpamState.find_repository t repo_name in
  let config_f = OpamRepositoryPath.config repo in
  let config =
    let config = OpamFile.Repo_config.read config_f in
    { config with repo_priority = priority } in
  OpamFile.Repo_config.write config_f config;
  (* relink the compiler and package descriptions *)
  fix_descriptions t

let add name url ~priority:prio =
  log "repository-add";
  let t =
    OpamState.load_state "repository-add" OpamStateConfig.(!r.current_switch)
  in
  if OpamState.mem_repository t name then
    OpamConsole.error_and_exit "%s is already a remote repository"
      (OpamRepositoryName.to_string name);
  let prio = match prio with
    | Some p -> p
    | None ->
      OpamRepositoryName.Map.fold
        (fun _ { repo_priority; _ } m -> max (repo_priority + 10) m)
        t.repositories 0
  in
  let repo = {
    repo_name     = name;
    repo_url      = url;
    repo_priority = prio;
    repo_root     = OpamRepositoryPath.create t.root name;
  } in
  if OpamUrl.local_dir url <> None &&
     OpamUrl.local_dir (OpamRepositoryPath.Remote.packages_url repo) = None &&
     OpamUrl.local_dir (OpamRepositoryPath.Remote.compilers_url repo) = None &&
     not (OpamConsole.confirm
            "%S doesn't contain a \"packages\" nor a \"compilers\" directory.\n\
             Is it really the directory of your repo ?"
            (OpamUrl.to_string url))
    then OpamStd.Sys.exit 1;
  OpamProcess.Job.run (OpamRepository.init repo);
  log "Adding %a" (slog OpamRepositoryBackend.to_string) repo;
  let repositories = OpamRepositoryName.Map.add name repo t.repositories in
  update_config t (OpamRepositoryName.Map.keys repositories);
  let t = { t with repositories } in
  OpamState.Cache.remove ();
  try
    let t = OpamProcess.Job.run (update t repo) t in
    fix_descriptions ~save_cache:false t
  with
  | e ->
    cleanup t repo;
    OpamStd.Exn.fatal e;
    OpamConsole.error_and_exit "Could not fetch repo: %s"
      (Printexc.to_string e)

let remove name =
  log "repository-remove";
  let t = OpamState.load_state "repository-remove"
      OpamStateConfig.(!r.current_switch) in
  let repo = OpamState.find_repository t name in
  cleanup t repo

let set_url name url =
  log "repository-remove";
  let t = OpamState.load_state ~save_cache:false "repository-set-url"
      OpamStateConfig.(!r.current_switch) in
  let repo = OpamState.find_repository t name in
  if repo.repo_url.OpamUrl.backend <> url.OpamUrl.backend then
    (remove name;
     add name url ~priority:(Some repo.repo_priority))
  else
  let config_f = OpamRepositoryPath.config repo in
  let config =
    let config = OpamFile.Repo_config.read config_f in
    { config with repo_url = url } in
  OpamFile.Repo_config.write config_f config;
  OpamState.Cache.remove ()

let list ~short =
  log "repository-list";
  let t = OpamState.load_state "repository-list"
      OpamStateConfig.(!r.current_switch) in
  if short then
    List.iter
      (fun r ->
         OpamConsole.msg "%s\n" (OpamRepositoryName.to_string r.repo_name))
      (OpamState.sorted_repositories t)
  else
    let pretty_print r =
      OpamConsole.msg "%4d %-7s %10s     %s\n"
        r.repo_priority
        (Printf.sprintf "[%s]"
           (OpamUrl.string_of_backend r.repo_url.OpamUrl.backend))
        (OpamRepositoryName.to_string r.repo_name)
        (OpamUrl.to_string r.repo_url) in
    let repos = OpamState.sorted_repositories t in
    List.iter pretty_print repos

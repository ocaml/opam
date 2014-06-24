(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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
open OpamState.Types
open OpamMisc.OP
open OpamPackage.Set.Op

let log fmt = OpamGlobals.log "REPOSITORY" fmt
let slog = OpamGlobals.slog

let update t repo =
  let max_loop = 10 in
  (* Recursively traverse redirection links, but stop after 10 steps or if
     we start to cycle. *)
  let rec loop r n =
    if n = 0 then
      OpamGlobals.warning "%s: Too many redirections, stopping."
        (OpamRepositoryName.to_string repo.repo_name)
    else (
      OpamRepository.update repo;
      if n <> max_loop && r = repo then
        OpamGlobals.warning "%s: Cyclic redirections, stopping."
          (OpamRepositoryName.to_string repo.repo_name)
      else match OpamState.redirect t r with
        | None        -> ()
        | Some (new_repo, f) ->
          OpamFilename.rmdir repo.repo_root;
          OpamFile.Repo_config.write (OpamPath.Repository.config repo) new_repo;
          let reason = match f with
            | None   -> ""
            | Some f -> Printf.sprintf " (%s)" (OpamFilter.to_string f) in
          OpamGlobals.note
            "The repository '%s' will be *%s* redirected to %s%s"
            (OpamRepositoryName.to_string repo.repo_name)
            ((OpamGlobals.colorise `bold) "permanently")
            (OpamMisc.prettify_path (string_of_address new_repo.repo_address))
            reason;
          loop new_repo (n-1)
    ) in
  loop repo max_loop

let print_updated_compilers updates =

  let print singular plural map =
    if not (OpamCompiler.Set.is_empty map) then (
      if OpamCompiler.Set.cardinal map = 1 then
        OpamGlobals.msg "%s:\n" singular
      else
        OpamGlobals.msg "%s:\n" plural;
      OpamCompiler.Set.iter (fun comp ->
        OpamGlobals.msg " - %s\n" (OpamCompiler.to_string comp)
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

let filter_compiler_checksums cs =
  let keep f = OpamFilename.check_suffix f ".comp" in
  List.filter keep cs

let fix_compiler_descriptions t ~verbose =
  OpamGlobals.msg "Updating %s/ ...\n"
    (OpamFilename.prettify_dir (OpamPath.compilers_dir t.root));
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
        if OpamFilename.exists_dir dir then OpamFilename.rmdir dir;
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
        OpamGlobals.msg "%s:\n" singular
      else
        OpamGlobals.msg "%s:\n" plural;
      OpamPackage.Set.iter (fun nv ->
        match fn nv with
        | None   -> OpamGlobals.msg " - %s\n" (OpamPackage.to_string nv)
        | Some s -> OpamGlobals.msg " - %s [%s]\n" (OpamPackage.to_string nv) s
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
          (OpamMisc.pretty_list (List.map OpamSwitch.to_string installed))
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
        OpamGlobals.msg "%s:\n" singular
      else
        OpamGlobals.msg "%s:\n" plural;
      OpamPackage.Set.iter (fun nv ->
          OpamGlobals.msg " - %s\n" (OpamPackage.to_string nv)
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

let filter_package_checksums cs =
  let keep f =
    match OpamFilename.Base.to_string (OpamFilename.basename f) with
    | "opam" | "descr" -> false
    | _ -> true in
  List.filter keep cs

(* Update the package contents, display the new packages and update
   reinstall *)
let fix_package_descriptions t ~verbose =
  OpamGlobals.msg "Updating %s/ ...\n"
    (OpamFilename.prettify_dir (OpamPath.packages_dir t.root));

  let global_index = OpamState.package_state t in
  let repo_index   = OpamState.package_repository_state t in
  (* let niet = String.concat ":" in *)
  (* log "global-index: %s" (OpamPackage.Map.to_string niet global_index); *)
  (* log "repo-index  : %s" (OpamPackage.Map.to_string niet repo_index); *)

  let updated_packages, new_packages =
    let updated_packages =
      OpamPackage.Map.fold (fun nv state set ->
          if not (OpamPackage.Map.mem nv global_index)
          || OpamPackage.Map.find nv global_index <> state then
            OpamPackage.Set.add nv set
          else
            set
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
  OpamPackage.Map.iter (fun nv _ ->
      if !OpamGlobals.sync_archives then (
        log "download %a"
          (slog @@ OpamFilename.to_string @* OpamPath.archive t.root) nv;
        match OpamState.download_archive t nv with
        | None | Some _ -> ()
      )
    ) repo_index;

  (* Do not recompile a package if only only OPAM or descr files have
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
          let root = OpamPath.Repository.packages repo prefix nv in
          let files = OpamRepository.package_files repo prefix nv ~archive:false in
          assert (files <> []);
          OpamFilename.mkdir dir;
          List.iter (fun file ->
              OpamFilename.copy_in ~root file dir
            ) files;
          OpamFilename.remove (OpamPath.archive t.root nv);
          OpamFilename.remove (OpamPath.Repository.archive repo nv);
    ) (OpamPackage.Set.union missing_installed_packages updated_packages);

  (* Display some warnings/errors *)
  OpamPackage.Set.iter (fun nv ->
      let file =
        OpamPath.Switch.Overlay.opam t.root t.switch (OpamPackage.name nv) in
      let file =
        if OpamFilename.exists file then file else OpamPath.opam t.root nv in
      if not (OpamFilename.exists file) then
        if OpamPackage.Map.mem nv repo_index then
          OpamGlobals.error_and_exit "fatal: %s is missing" (OpamFilename.prettify file)
        else
          let installed = OpamState.installed_versions t (OpamPackage.name nv) in
          let switches = OpamPackage.Map.find nv installed in
          let switches_string =
            OpamMisc.pretty_list (List.map OpamSwitch.to_string switches) in
          OpamGlobals.warning
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
  OpamState.add_to_reinstall ~all:true t updates.changed;

  updates

let compare_repo t r1 r2 =
  OpamRepository.compare
    (OpamState.find_repository t r1)
    (OpamState.find_repository t r2)

let update_package_index t =
  let file = OpamPath.package_index t.root in
  OpamGlobals.msg "Updating %s ...\n" (OpamFilename.prettify file);
  let package_index = OpamState.package_index t in
  OpamFile.Package_index.write file package_index;
  { t with package_index }

let update_compiler_index t =
  let file = OpamPath.compiler_index t.root in
  OpamGlobals.msg "Updating %s ...\n" (OpamFilename.prettify file);
  let compiler_index = OpamState.compiler_index t in
  OpamFile.Compiler_index.write file compiler_index;
  { t with compiler_index }

(* update the repository config file:
   ~/.opam/repo/<repo>/config *)
let update_config t repos =
  log "update-config %a"
    (slog @@ OpamMisc.pretty_list @* List.map OpamRepositoryName.to_string)
    repos;
  let new_config = OpamFile.Config.with_repositories t.config repos in
  OpamFile.Config.write (OpamPath.config t.root) new_config

let fix_descriptions ?(save_cache=true) t ~verbose =
  let t = update_compiler_index t in
  let _ = fix_compiler_descriptions t ~verbose in
  let t = update_package_index t in
  let _ = fix_package_descriptions t ~verbose in
  if save_cache then OpamState.rebuild_state_cache ()

let () =
  OpamState.fix_descriptions_hook := fix_descriptions

(* Remove any remaining of [repo] from OPAM state *)
let cleanup t repo =
   log "cleanup %a" (slog OpamRepositoryName.to_string) repo.repo_name;
  let repos = OpamRepositoryName.Map.keys t.repositories in
  update_config t (List.filter ((<>) repo.repo_name) repos);
  OpamFilename.rmdir repo.repo_root;
  fix_descriptions t ~verbose:true

let priority repo_name ~priority =
  log "repository-priority";

  (* 1/ update the config file *)
  let t = OpamState.load_state ~save_cache:false "repository-priority" in
  let repo = OpamState.find_repository t repo_name in
  let config_f = OpamPath.Repository.config repo in
  let config =
    let config = OpamFile.Repo_config.read config_f in
    { config with repo_priority = priority } in
  OpamFile.Repo_config.write config_f config;
  (* relink the compiler and package descriptions *)
  fix_descriptions t ~verbose:true

(*
let priority_t t ~priority =
  let repo = OpamState.find_repository t repo_name in
  let config_f = OpamPath.Repository.config repo in
  let config =
    let config = OpamFile.Repo_config.read config_f in
    { config with repo_priority = priority } in
  OpamFile.Repo_config.write config_f config;

let priority repo_name ~priority =
  log "repository-priority";
  (* 1/ update the config file *)
  let t = OpamState.load_state ~save_cache:false "repository-priority" in
  priority_t t ~priority
  (* relink the compiler and package descriptions *)
  fix_descriptions t ~verbose
*)
let add name kind address ~priority:prio =
  log "repository-add";
  let t = OpamState.load_state "repository-add" in
  if OpamState.mem_repository t name then
    OpamGlobals.error_and_exit
      "%s is already a remote repository"
      (OpamRepositoryName.to_string name);
  if kind = `local then (
    let repo_dir = OpamFilename.Dir.of_string (string_of_address address) in
    let pkgdir = OpamPath.packages_dir repo_dir in
    let compdir = OpamPath.compilers_dir repo_dir in
    if not (OpamFilename.exists_dir pkgdir) &&
       not (OpamFilename.exists_dir compdir) &&
       not (OpamState.confirm
              "%S doesn't contain a \"packages\" nor a \"compilers\" directory.\n\
               Is it really the directory of your repo ?"
              (OpamFilename.Dir.to_string repo_dir))
    then OpamGlobals.exit 1
  );
  let prio = match prio with
    | Some p -> p
    | None ->
      if OpamRepositoryName.Map.is_empty t.repositories then 0 else
      let max_prio =
        OpamRepositoryName.Map.fold
          (fun _ { repo_priority } m -> max repo_priority m)
          t.repositories min_int in
      10 + max_prio in
  let repo = {
    repo_name     = name;
    repo_kind     = kind;
    repo_address  = address;
    repo_priority = prio;
    repo_root     = OpamPath.Repository.create t.root name;
  } in
  (try OpamRepository.init repo with
   | OpamRepository.Unknown_backend ->
     OpamGlobals.error_and_exit
       "\"%s\" is not a supported backend"
       (string_of_repository_kind repo.repo_kind)
   | e ->
     cleanup t repo;
     raise e
  );
  log "Adding %a" (slog OpamRepository.to_string) repo;
  update_config t (repo.repo_name :: OpamRepositoryName.Map.keys t.repositories);
  try
    OpamState.remove_state_cache ();
    priority name ~priority:prio;
  with e ->
    cleanup t repo;
    raise e

let remove name =
  log "repository-remove";
  let t = OpamState.load_state "repository-remove" in
  let repo = OpamState.find_repository t name in
  cleanup t repo

let list ~short =
  log "repository-list";
  let t = OpamState.load_state "repository-list" in
  if short then (
    let repos =
      List.map
        (fun r -> OpamRepositoryName.to_string r.repo_name)
        (OpamState.sorted_repositories t) in
    let pinned =
      List.map OpamPackage.Name.to_string (OpamPackage.Name.Map.keys t.pinned) in
    let all = repos @ pinned in
    OpamGlobals.msg "%s\n" (String.concat " " all)
  ) else (
    let pretty_print r =
      OpamGlobals.msg "%4d %-7s %10s     %s\n"
        r.repo_priority
        (Printf.sprintf "[%s]" (string_of_repository_kind r.repo_kind))
        (OpamRepositoryName.to_string r.repo_name)
        (string_of_address r.repo_address) in
    let repos = OpamState.sorted_repositories t in
    List.iter pretty_print repos
  )

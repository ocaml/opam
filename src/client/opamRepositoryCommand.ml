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
open OpamState.Types

let log fmt = OpamGlobals.log "REPOSITORY" fmt

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
    "The following compiler descriptions are been DELETED"
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
  log "global-index: %s" (OpamCompiler.Map.to_string niet global_index);
  log "repo-index  : %s" (OpamCompiler.Map.to_string niet repo_index);

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
  log "updated-compilers: %s" (OpamCompiler.Set.to_string updated_compilers);
  log "new-compilers    : %s" (OpamCompiler.Set.to_string new_compilers);

  let deleted_compilers =
    OpamCompiler.Set.fold (fun comp map ->
        if comp = OpamCompiler.system             (* system *)
        || OpamCompiler.Map.mem comp repo_index   (* OR available *)
        || OpamState.is_compiler_installed t comp (* OR installed *)
        then
          map
        else
          OpamCompiler.Set.add comp map
      ) t.compilers OpamCompiler.Set.empty in
  log "deleted-compilers: %s" (OpamCompiler.Set.to_string deleted_compilers);

  (* Delete compiler descritions *)
  OpamCompiler.Set.iter (fun comp ->
      let dir = OpamPath.compilers t.root comp in
      OpamFilename.rmdir dir;
    ) deleted_compilers;

  (* Update the compiler description *)
  OpamCompiler.Set.iter (fun comp ->
      match OpamState.repository_of_compiler t comp with
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
  log "update-dev-packages updates %s" (OpamPackage.Set.to_string packages);
  let updates = OpamState.update_dev_packages t in
  if verbose then print_updated_dev_packages updates;
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
  let niet = String.concat ":" in
  log "global-index: %s" (OpamPackage.Map.to_string niet global_index);
  log "repo-index  : %s" (OpamPackage.Map.to_string niet repo_index);

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
  log "new-packages    : %s" (OpamPackage.Set.to_string new_packages);

  let all_installed = OpamState.all_installed t in
  let changed_packages, updated_packages =
    OpamPackage.Set.partition (fun nv ->
        OpamPackage.Set.mem nv all_installed
      ) updated_packages in
  log "updated-packages: %s" (OpamPackage.Set.to_string updated_packages);
  log "changed-packages: %s" (OpamPackage.Set.to_string changed_packages);

  let deleted_packages =
    OpamPackage.Set.filter (fun nv ->
        not (OpamPackage.Map.mem nv repo_index         (* available *)
             || OpamPackage.Set.mem nv all_installed)  (* OR installed *)
      ) t.packages in
  log "deleted-packages: %s" (OpamPackage.Set.to_string deleted_packages);

  (* Remove the deleted packages *)
  OpamPackage.Set.iter (fun nv ->
      OpamFilename.rmdir  (OpamPath.packages t.root nv);
      OpamFilename.remove (OpamPath.archive t.root nv);
    ) deleted_packages;

  (* Update the package descriptions *)
  let (++) = OpamPackage.Set.union in
  OpamPackage.Set.iter (fun nv ->
      match OpamState.repository_of_package t nv with
      | None                -> ()
      | Some (repo, prefix) ->
        let root = OpamPath.Repository.packages repo prefix nv in
        let files = OpamRepository.package_files repo prefix nv ~archive:false in
        let dir = OpamPath.packages t.root nv in
        if OpamFilename.exists_dir dir then OpamFilename.rmdir dir;
        OpamFilename.mkdir dir;
        List.iter (fun file ->
            OpamFilename.copy_in ~root file dir
          ) files;
        OpamFilename.remove (OpamPath.archive t.root nv);
    ) (new_packages ++ updated_packages ++ changed_packages);

  (* that's not a good idea *at all* to enable this hook if you
           are not in a testing environment *)
  OpamPackage.Map.iter (fun nv _ ->
      if !OpamGlobals.sync_archives then (
        log "download %s" (OpamFilename.to_string (OpamPath.archive t.root nv));
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
  log "packages-to-upgrade: %s" (OpamPackage.Set.to_string changed_packages);

  let updates = {
    created = new_packages;
    updated = updated_packages;
    deleted = deleted_packages;
    changed = changed_packages;
  } in

  if verbose then print_updated_packages t updates;

  (* update $opam/$oversion/reinstall for all installed switches *)
  OpamState.add_to_reinstall ~all:true t updates.updated;

  updates

let compare_repo t r1 r2 =
  OpamRepository.compare
    (OpamState.find_repository t r1)
    (OpamState.find_repository t r2)

let update_index t =
  let file = OpamPath.repo_index t.root in
  OpamGlobals.msg "Updating %s ...\n" (OpamFilename.prettify file);

  let repositories = OpamState.sorted_repositories t in
  let repo_index = OpamFile.Repo_index.safe_read file in

  (* All the existing packages *)
  let packages = ref OpamPackage.Set.empty in

  (* Cache of packages per repository *)
  let packages_repo = ref [] in
  let get_packages repo_name =
    if List.mem_assoc repo_name !packages_repo then
      List.assoc repo_name !packages_repo
    else
      match OpamState.find_repository_opt t repo_name with
      | Some repo ->
        let pkgs = OpamRepository.packages repo in
        packages_repo := (repo_name, pkgs) :: !packages_repo;
        pkgs
      | None -> OpamPackage.Set.empty in

  (* Remove package without any valid repository *)
  let repo_index =
    OpamPackage.Name.Map.fold (fun n repos repo_index ->
      let valid_repos = List.filter (fun repo ->
        let available = get_packages repo in
        OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = n) available
      ) repos in
      match valid_repos with
      | [] -> repo_index
      | _  -> OpamPackage.Name.Map.add n valid_repos repo_index
    ) repo_index OpamPackage.Name.Map.empty in

  (* Add new repositories *)
  let repo_index =
    List.fold_left (fun repo_index repo ->
      let available = get_packages repo.repo_name in
      packages := OpamPackage.Set.fold (fun nv set ->
          OpamPackage.Set.add nv set
        ) available !packages;
      OpamPackage.Set.fold (fun nv repo_index ->
          let name = OpamPackage.name nv in
          if not (OpamPackage.Name.Map.mem name repo_index) then
            OpamPackage.Name.Map.add name [repo.repo_name] repo_index
          else
            let repos = OpamPackage.Name.Map.find name repo_index in
            if not (List.mem repo.repo_name repos) then
              let repo_index = OpamPackage.Name.Map.remove name repo_index in
              let repos = OpamMisc.insert (compare_repo t) repo.repo_name repos in
              OpamPackage.Name.Map.add name repos repo_index
            else
              repo_index
        ) available repo_index
      ) repo_index repositories in

  (* Write ~/.opam/repo/index *)
  OpamFile.Repo_index.write (OpamPath.repo_index t.root) repo_index

(* update the repository config file:
   ~/.opam/repo/<repo>/config *)
let update_config t repos =
  log "update-config %s"
    (OpamMisc.pretty_list (List.map OpamRepositoryName.to_string repos));
  let new_config = OpamFile.Config.with_repositories t.config repos in
  OpamFile.Config.write (OpamPath.config t.root) new_config

let fix_descriptions t ~verbose =
  update_index t;
  let _ = fix_compiler_descriptions t ~verbose in
  let _ = fix_package_descriptions t ~verbose in
  OpamState.rebuild_state_cache ()

let () =
  OpamState.fix_descriptions_hook := fix_descriptions

(* Remove any remaining of [repo] from OPAM state *)
let cleanup t repo =
   log "cleanup %s" (OpamRepositoryName.to_string repo.repo_name);
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

  (* 2/ shuffle the repository index *)
  let repo_index_f = OpamPath.repo_index t.root in
  let repo_index =
    let repo_index = OpamFile.Repo_index.safe_read (OpamPath.repo_index t.root) in
    OpamPackage.Name.Map.map (fun repos ->
        if not (List.mem repo_name repos) then repos
        else
          let repos = List.filter ((<>)repo_name) repos in
          let prios = ref [] in
          let priority_of n =
            if List.mem_assoc n !prios then
              List.assoc n !prios
            else
              let repo = OpamState.find_repository t repo_name in
              let config_f = OpamPath.Repository.config repo in
              let config = OpamFile.Repo_config.read config_f in
              let prio = config.repo_priority in
              prios := (n, prio) :: !prios;
              prio in
          let compare n1 n2 =
            let p1 = priority_of n1 in
            let p2 = priority_of n2 in
            (* highest is better priority *)
            p2 - p1 in
          OpamMisc.insert compare repo_name repos
      ) repo_index in
  OpamFile.Repo_index.write repo_index_f repo_index;

  (* relink the compiler and package descriptions *)
  fix_descriptions t ~verbose:true

let add name kind address ~priority:prio =
  log "repository-add";
  let t = OpamState.load_state "repository-add" in
  let repo = {
    repo_name     = name;
    repo_kind     = kind;
    repo_address  = address;
    repo_priority = min_int; (* we initially put it as low-priority *)
    repo_root     = OpamPath.Repository.create t.root name;
  } in
  if OpamState.mem_repository t name then
    OpamGlobals.error_and_exit
      "%s is already a remote repository"
      (OpamRepositoryName.to_string name)
  else (
    try OpamRepository.init repo with
    | OpamRepository.Unknown_backend ->
      OpamGlobals.error_and_exit
        "\"%s\" is not a supported backend"
        (string_of_repository_kind repo.repo_kind)
    | e ->
      cleanup t repo;
      raise e
  );
  log "Adding %s" (OpamRepository.to_string repo);
  update_config t (repo.repo_name :: OpamRepositoryName.Map.keys t.repositories);
  try
    let max_prio =
      OpamRepositoryName.Map.fold
        (fun _ { repo_priority } m -> max repo_priority m)
        t.repositories min_int in
    let prio = match prio with
      | None   -> 10 + max_prio
      | Some p -> p in
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

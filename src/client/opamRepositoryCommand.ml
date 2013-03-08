(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012-2013 OCamlPro                                     *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open OpamTypes
open OpamState.Types

let log fmt = OpamGlobals.log "REPOSITORY" fmt

let compare_repo t r1 r2 =
  OpamRepository.compare
    (OpamRepositoryName.Map.find r1 t.repositories)
    (OpamRepositoryName.Map.find r2 t.repositories)

let update_index t =

  (* Update repo_index *)
  let repositories = OpamState.sorted_repositories t in

  (* All the packages in the repositories *)
  let packages = ref OpamPackage.Set.empty in

  (* Add new repositories *)
  let repo_index =
    List.fold_left (fun repo_index r ->
      let p = OpamPath.Repository.create t.root r.repo_name in
      let prefix, available = OpamRepository.packages p in
      let prefix_f = OpamPath.Repository.prefix p in
      if not (OpamPackage.Name.Map.is_empty prefix) then
        OpamFile.Prefix.write prefix_f prefix
      else if OpamFilename.exists prefix_f then
        OpamFilename.remove prefix_f;
      packages := OpamPackage.Set.union available !packages;
      OpamPackage.Set.fold (fun nv repo_index ->
        let name = OpamPackage.name nv in
        if not (OpamPackage.Name.Map.mem name repo_index) then
          OpamPackage.Name.Map.add name [r.repo_name] repo_index
        else
          let repo_s = OpamPackage.Name.Map.find name repo_index in
          if not (List.mem r.repo_name repo_s) then
            let repo_index = OpamPackage.Name.Map.remove name repo_index in
            let repo_s = OpamMisc.insert (compare_repo t) r.repo_name repo_s in
            OpamPackage.Name.Map.add name repo_s repo_index
          else
            repo_index
      ) available repo_index
    ) t.repo_index repositories in

  (* Remove package without any valid repository *)
  let repo_index =
    OpamPackage.Name.Map.fold (fun n repo_s repo_index ->
      match List.filter (OpamState.mem_repository_name t) repo_s with
      | []     -> repo_index
      | repo_s -> OpamPackage.Name.Map.add n repo_s repo_index
    ) repo_index OpamPackage.Name.Map.empty in

  (* Write ~/.opam/repo/index *)
  OpamFile.Repo_index.write (OpamPath.repo_index t.root) repo_index;
  let t = { t with repo_index } in

  (* suppress previous links, but keep metadata of installed packages
     but deleted from the repository (to be still be able to uninstall
     the package) *)
  let lonely = OpamPackage.Set.diff  (OpamState.all_installed t) !packages in
  OpamPackage.Set.iter (fun nv ->
    if not (OpamPackage.Set.mem nv lonely) then (
      List.iter
        (fun f -> OpamFilename.remove (f t.root nv))
        [ OpamPath.opam; OpamPath.descr; OpamPath.archive ]
    )
  ) t.packages;

  (* Create symbolic links from $repo dirs to main dir *)
  let map = OpamState.package_repository_map t in
  OpamPackage.Map.iter (fun nv repo ->
    let repo_p = OpamPath.Repository.create t.root repo.repo_name in
    let prefix = OpamRepository.prefix repo_p nv in
    List.iter (fun (g, r) ->
      let global_file = g t.root nv in
      let repo_file = r repo_p nv in
      if OpamFilename.exists repo_file then
        OpamFilename.link ~src:repo_file ~dst:global_file
    ) ([ (OpamPath.opam   , fun r -> OpamPath.Repository.opam r prefix);
         (OpamPath.descr  , fun r -> OpamPath.Repository.descr r prefix);
         (OpamPath.archive, OpamPath.Repository.archive) ])
  ) map;
  map

(* update the repository config file:
   ~/.opam/repo/<repo>/config *)
let update_config t repos =
  let new_config = OpamFile.Config.with_repositories t.config repos in
  OpamFile.Config.write (OpamPath.config t.root) new_config

(* Remove any remaining of [repo] from OPAM state *)
let cleanup t repo =
  let repos = OpamRepositoryName.Map.keys t.repositories in
  update_config t (List.filter ((<>) repo) repos);
  let t = OpamState.load_state "repository-cleanup-repo" in
  let _ = update_index t in
  OpamFilename.rmdir (OpamPath.Repository.root (OpamPath.Repository.create t.root repo));
  OpamState.rebuild_state_cache ()

let priority name ~priority =
  log "repository-priority";
  let t = OpamState.load_state ~save_cache:false "repository-priority" in
  if OpamState.mem_repository_name t name then (
    let config_f = OpamPath.Repository.config (OpamPath.Repository.create t.root name) in
    let config = OpamFile.Repo_config.read config_f in
    let config = { config with repo_priority = priority } in
    OpamFile.Repo_config.write config_f config;
    let repo_index_f = OpamPath.repo_index t.root in
    let repo_index = OpamPackage.Name.Map.map (List.filter ((<>)name)) t.repo_index in
    OpamFile.Repo_index.write repo_index_f repo_index;
    let t = OpamState.load_state ~save_cache:false "repository-3" in
    let _ = update_index t in
    OpamState.rebuild_state_cache ()
  ) else
    OpamGlobals.error_and_exit
      "%s is not a a valid remote name"
      (OpamRepositoryName.to_string name)

let add name kind address ~priority:prio =
  log "repository-add";
  let t = OpamState.load_state "repository-add" in
  let repo = {
    repo_name     = name;
    repo_kind     = kind;
    repo_address  = address;
    repo_priority = min_int; (* we initially put it as low-priority *)
  } in
  if OpamState.mem_repository_name t name then
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
      cleanup t name;
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
    !OpamState.update_hook ~save_cache:false [name];
    priority name ~priority:prio;
  with e ->
    cleanup t name;
    raise e

let remove name =
  log "repository-remove";
  let t = OpamState.load_state "repository-remove" in
  if OpamState.mem_repository_name t name then
    cleanup t name
  else
    OpamGlobals.error_and_exit "%s is not a a valid remote name"
      (OpamRepositoryName.to_string name)

let list ~short =
  log "repository-list";
  let t = OpamState.load_state "repository-list" in
  if short then (
    let repos =
      List.map
        (fun r -> OpamRepositoryName.to_string r.repo_name)
        (OpamState.sorted_repositories t) in
    let pinned = List.map OpamPackage.Name.to_string (OpamPackage.Name.Map.keys t.pinned) in
    let all = repos @ pinned in
    OpamGlobals.msg "%s\n" (String.concat " " all)
  ) else (
    let pretty_print r =
      OpamGlobals.msg "%4d %-7s %10s     %s\n"
        r.repo_priority
        (Printf.sprintf "[%s]" (string_of_repository_kind r.repo_kind))
        (OpamRepositoryName.to_string r.repo_name)
        (OpamFilename.Dir.to_string r.repo_address) in
    let repos = OpamState.sorted_repositories t in
    List.iter pretty_print repos
  )


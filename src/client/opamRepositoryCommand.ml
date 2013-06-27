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

let print_updated_compilers ~new_compilers ~updated_compilers ~deleted_compilers =

  let print singular plural map =
    if not (OpamCompiler.Map.is_empty map) then (
      if OpamCompiler.Map.cardinal map = 1 then
        OpamGlobals.msg "%s:\n" singular
      else
        OpamGlobals.msg "%s:\n" plural;
      OpamCompiler.Map.iter (fun comp _ ->
        OpamGlobals.msg " - %s\n" (OpamCompiler.to_string comp)
      ) map
    ) in

  print
    "The following NEW compiler is available"
    "The following NEW compilers are available"
    new_compilers;

  print
    "The following compiler has been updated"
    "Some compiler descriptions have been updated"
    updated_compilers;

  print
    "The following compiler is not available anymore"
    "Some following compiler are not available anymore"
    deleted_compilers

let relink_compilers t ~verbose old_index =
  OpamGlobals.msg "Updating %s/\n"
    (OpamFilename.prettify_dir (OpamPath.compilers_dir t.root));
  let old_index = OpamCompiler.Map.filter (fun comp _ ->
      OpamFilename.exists (OpamPath.compiler t.root comp)
    ) old_index in
  let compiler_index = OpamState.compiler_state_index t in
  log "old-index: %s" (OpamMisc.string_of_list OpamCompiler.to_string
                         (OpamCompiler.Map.keys old_index));
  log "new-index: %s" (OpamMisc.string_of_list OpamCompiler.to_string
                         (OpamCompiler.Map.keys compiler_index));
  let updated_compilers =
    OpamCompiler.Map.filter (fun comp state ->
        not (OpamCompiler.Map.mem comp old_index)
        || OpamCompiler.Map.find comp old_index <> state
      ) compiler_index in
  let deleted_compilers =
    OpamCompiler.Set.fold (fun comp map ->
        if comp = OpamCompiler.system
        || OpamCompiler.Map.mem comp compiler_index
        || OpamState.compiler_installed t comp then
          map
        else
          OpamCompiler.Map.add comp true map
      ) t.compilers OpamCompiler.Map.empty in

  (* Delete compiler descritions, but keep the ones who disapeared and
     are still installed *)
  OpamCompiler.Map.iter (fun comp _ ->
    if not (OpamState.compiler_installed t comp) then
      match OpamState.compiler_repository_state t comp with
      | None   -> ()
      | Some s ->
        OpamFilename.remove s.comp_file;
        match s.comp_descr with
        | None   -> ()
        | Some f -> OpamFilename.remove f
  ) deleted_compilers;

  (* Link existing compiler description files, following the
     repository priorities *)
  OpamCompiler.Map.iter (fun comp _ ->
    match OpamState.compiler_repository_state t comp with
    | None   -> ()
    | Some s ->
      let comp_file  = OpamPath.compiler t.root comp in
      let comp_descr = OpamPath.compiler_descr t.root comp in
      OpamFilename.remove comp_file;
      OpamFilename.remove comp_descr;
      OpamFilename.copy ~src:s.comp_file ~dst:comp_file;
      match s.comp_descr with
      | None   -> ()
      | Some d ->
        if OpamFilename.exists d then OpamFilename.copy ~src:d ~dst:comp_descr
  ) updated_compilers;

  if verbose then
    let updated_compilers, new_compilers =
      OpamCompiler.Map.partition
        (fun c _ -> OpamState.compiler_installed t c)
        updated_compilers in
    print_updated_compilers ~new_compilers ~updated_compilers ~deleted_compilers

let print_updated_packages t
    ~new_packages ~updated_packages ~packages_to_upgrade ~deleted_packages =

  let print singular plural map fn =
    if not (OpamPackage.Map.is_empty map) then (
      if OpamPackage.Map.cardinal map = 1 then
        OpamGlobals.msg "%s:\n" singular
      else
        OpamGlobals.msg "%s:\n" plural;
      OpamPackage.Map.iter (fun nv _ ->
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
    new_packages
    none;

  print
    "The following package has been updated upstream and needs to be UPGRADED"
    "The following packages have been updated upstream and need to be UPGRADED"
    packages_to_upgrade
    installed_switches;

  print
    "The following package has been UPDATED upstream"
    "The following packages have been UPDATED upstream"
    updated_packages
    none;

  print
    "The following package has been DELETED"
    "The following packages have been DELETED"
    deleted_packages
    none

let print_updated_pinned_packages pinned_packages =
  let print singular plural map =
    if not (OpamPackage.Set.is_empty map) then (
      if OpamPackage.Set.cardinal map = 1 then
        OpamGlobals.msg "%s:\n" singular
      else
        OpamGlobals.msg "%s:\n" plural;
      OpamPackage.Set.iter (fun nv ->
        OpamGlobals.msg " - %s [pinned]\n"
          (OpamPackage.Name.to_string (OpamPackage.name nv))
      ) map
    ) in
  print
    "The following PINNED package needs to be upgraded"
    "The following PINNED packages need to be upgraded"
    pinned_packages

(* Check for updates in pinned packages *)
let update_pinned_packages t ~verbose packages =
  log "check-pinned-packages updates %s" (OpamPackage.Name.Set.to_string packages);
  let pinned =
    OpamPackage.Name.Map.filter
      (fun n _ -> OpamPackage.Name.Set.mem n packages)
      t.pinned in
  let pinned = OpamPackage.Name.Map.bindings pinned in
  (* Check if a pinned packages has been updated. *)
  let is_updated = function
    | n, (Local p | Git p | Darcs p) ->
      if OpamState.mem_installed_package_by_name t n then
        let nv = OpamState.find_installed_package_by_name t n in
        match OpamState.update_pinned_package t n with
        | Up_to_date _  -> None
        | Result _      -> Some nv
        | Not_available ->
          OpamGlobals.error "%s is not available" (OpamFilename.Dir.to_string p);
          None
      else
        None
    | _ -> None
  in
  let updates = OpamMisc.filter_map is_updated pinned in
  let updates = OpamPackage.Set.of_list updates in

  if verbose then print_updated_pinned_packages updates;

  (* update $opam/$oversion/reinstall for all installed switches *)
  OpamState.add_to_reinstall ~all:true t updates

(* Update the package contents, display the new packages and update
   reinstall *)
let relink_packages t ~verbose old_index =
  OpamGlobals.msg "Updating %s/ and %s/\n"
    (OpamFilename.prettify_dir (OpamPath.opam_dir t.root))
    (OpamFilename.prettify_dir (OpamPath.descr_dir t.root));
  let old_index = OpamPackage.Map.filter (fun nv _ ->
      OpamFilename.exists (OpamPath.opam t.root nv)
    ) old_index in
  let package_index = OpamState.package_state_index t in
  log "old-index: %s"
    (OpamMisc.string_of_list OpamPackage.to_string (OpamPackage.Map.keys old_index));
  log "new-index: %s"
    (OpamMisc.string_of_list
       OpamPackage.to_string
       (OpamPackage.Map.keys package_index));
  let updated_packages =
    OpamPackage.Map.filter (fun nv state ->
        not (OpamPackage.Map.mem nv old_index)
        || OpamPackage.Map.find nv old_index <> state
      ) package_index in
  let all_installed = OpamState.all_installed t in
  let deleted_packages =
    OpamPackage.Set.fold (fun nv map ->
        if OpamPackage.Map.mem nv package_index
        || OpamPackage.Set.mem nv all_installed then
          map
        else
          OpamPackage.Map.add nv true map
      ) t.packages OpamPackage.Map.empty in

  (* Check all the dependencies exist *)
  let all_packages =
    let new_packages = OpamPackage.Set.of_list (OpamPackage.Map.keys package_index) in
    OpamPackage.Set.union t.packages new_packages in
  let check_package (nv, state) =
    let opam = OpamFile.OPAM.read state.pkg_opam in
    let name = OpamFile.OPAM.name opam in
    let version = OpamFile.OPAM.version opam in
    if nv <> OpamPackage.create name version then (
      OpamGlobals.warning
        "The file %s is not consistent with the package %s (%s)"
        (OpamFilename.to_string (OpamPath.opam t.root nv))
        (OpamPackage.Name.to_string name)
        (OpamPackage.Version.to_string version);
      false
    ) else (
      let opam = OpamFile.OPAM.read state.pkg_opam in
      let map_b b = OpamFormula.fold_left (fun accu (n,_) -> (b, n) :: accu) [] in
      let depends = map_b true (OpamFile.OPAM.depends opam) in
      let depopts = map_b false (OpamFile.OPAM.depopts opam) in
      List.for_all (fun (mandatory, d) ->
        if OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = d) all_packages
        then true
        else (
          if mandatory then
            OpamGlobals.warning
              "Package %s depends on the unknown package %s"
              (OpamPackage.to_string nv) (OpamPackage.Name.to_string d)
          else
            OpamGlobals.warning
              "Package %s depends optionally on the unknown package %s"
              (OpamPackage.to_string nv) (OpamPackage.Name.to_string d);
          false
        )
      ) (depends @ depopts)
    ) in

  let apply fn = function
    | None   -> ()
    | Some f -> fn f in

  (* Remove the deleted packages (which are not yet unininstalled) *)
  OpamPackage.Map.iter (fun nv _ ->
      let remove fn =
        let file = fn t.root nv in
        if OpamFilename.exists file then OpamFilename.remove file in
      remove OpamPath.opam;
      remove OpamPath.descr;
      remove OpamPath.archive;
    ) deleted_packages;

  (* Create symbolic links from $repo dirs to main dir *)
  OpamPackage.Map.iter (fun nv _ ->
    match OpamState.package_repository_state t nv with
    | None       -> ()
    | Some state ->
      if check_package (nv, state) then (
        let pkg_opam    = OpamPath.opam t.root nv in
        let pkg_descr   = OpamPath.descr t.root nv in
        let pkg_archive = OpamPath.archive t.root nv in
        OpamFilename.remove pkg_opam;
        OpamFilename.remove pkg_descr;
        OpamFilename.remove pkg_archive;
        let copy src ~dst =
          if OpamFilename.exists src then OpamFilename.copy ~src ~dst in
        copy state.pkg_opam ~dst:pkg_opam;
        apply (copy ~dst:pkg_descr)   state.pkg_descr ;
        apply (copy ~dst:pkg_archive) state.pkg_archive;
      );
  ) updated_packages;

  if verbose then (
    let updated_packages, new_packages = OpamPackage.Map.partition (fun nv _ ->
        OpamPackage.Map.mem nv old_index
      ) updated_packages in
    let packages_to_upgrade, updated_packages = OpamPackage.Map.partition (fun nv _ ->
        OpamPackage.Set.mem nv all_installed
      ) updated_packages in
    print_updated_packages t
      ~new_packages ~updated_packages ~packages_to_upgrade ~deleted_packages;
  );

  (* update $opam/$oversion/reinstall for all installed switches *)
  let updates = OpamPackage.Set.of_list (OpamPackage.Map.keys updated_packages) in
  OpamState.add_to_reinstall ~all:true t updates

let compare_repo t r1 r2 =
  OpamRepository.compare
    (OpamState.find_repository t r1)
    (OpamState.find_repository t r2)

let update_index t =
  let file = OpamPath.repo_index t.root in
  OpamGlobals.msg "Updating %s\n" (OpamFilename.prettify file);

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
      | None -> OpamPackage.Name.Map.empty, OpamPackage.Set.empty in

  (* Remove package without any valid repository *)
  let repo_index =
    OpamPackage.Name.Map.fold (fun n repos repo_index ->
      let valid_repos = List.filter (fun repo ->
        let _, available = get_packages repo in
        OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = n) available
      ) repos in
      match valid_repos with
      | [] -> repo_index
      | _  -> OpamPackage.Name.Map.add n valid_repos repo_index
    ) repo_index OpamPackage.Name.Map.empty in

  (* Add new repositories *)
  let repo_index, prefixes =
    List.fold_left (fun (repo_index, prefixes) repo ->
      let prefix, available = get_packages repo.repo_name in
      let prefix_f = OpamPath.Repository.prefix repo in
      if not (OpamPackage.Name.Map.is_empty prefix) then
        OpamFile.Prefix.write prefix_f prefix
      else if OpamFilename.exists prefix_f then
        OpamFilename.remove prefix_f;
      packages := OpamPackage.Set.union available !packages;
      let repo_index =
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
        ) available repo_index in
      let prefixes = OpamRepositoryName.Map.add repo.repo_name prefix prefixes in
      (repo_index, prefixes)
    ) (repo_index, OpamRepositoryName.Map.empty) repositories in

  (* Write ~/.opam/repo/index *)
  OpamFile.Repo_index.write (OpamPath.repo_index t.root) repo_index;

  let package_index = OpamState.package_index t.repositories repo_index in
  OpamFile.Package_index.write (OpamPath.package_index t.root) (Some package_index);

  let compiler_index = OpamState.compiler_index repositories in
  OpamFile.Compiler_index.write (OpamPath.compiler_index t.root) compiler_index;

  { t with prefixes; package_index; compiler_index }

(* update the repository config file:
   ~/.opam/repo/<repo>/config *)
let update_config t repos =
  log "update-config %s"
    (OpamMisc.pretty_list (List.map OpamRepositoryName.to_string repos));
  let new_config = OpamFile.Config.with_repositories t.config repos in
  OpamFile.Config.write (OpamPath.config t.root) new_config

let relink_all t ~verbose fn =
  log "relink-all";
  let old_compiler_index = OpamState.compiler_state_index t in
  let old_package_index = OpamState.package_state_index t in
  fn t;
  let _ = update_index t in
  let t = OpamState.load_state ~save_cache:false "relink-all" in
  relink_compilers t ~verbose old_compiler_index;
  relink_packages t ~verbose old_package_index;
  OpamState.rebuild_state_cache ()

(* Remove any remaining of [repo] from OPAM state *)
let cleanup t repo =
  log "cleanup %s" (OpamRepositoryName.to_string repo.repo_name);
  relink_all t ~verbose:true (fun t ->
      let prefix, packages = OpamRepository.packages repo in
      OpamPackage.Set.iter (fun nv ->
          let prefix = OpamRepository.find_prefix prefix nv in
          let relink r g =
            let r = r repo prefix nv in
            let g = g t.root nv in
            if OpamFilename.exists g && OpamFilename.readlink g = r then
              OpamFilename.move ~src:r ~dst:g in
          relink OpamPath.Repository.opam OpamPath.opam;
          relink OpamPath.Repository.descr OpamPath.descr;
          relink (fun r _ -> OpamPath.Repository.archive r) OpamPath.archive;
        ) packages;
      let compilers = OpamRepository.compilers repo in
      OpamCompiler.Map.iter (fun comp (comp_f, descr_f) ->
          let relink r g =
            if OpamFilename.exists g && OpamFilename.readlink g = r then
              OpamFilename.move ~src:r ~dst:g in
          relink comp_f (OpamPath.compiler t.root comp);
          match descr_f with
          | Some descr_f -> relink descr_f (OpamPath.compiler_descr t.root comp)
          | None         -> ()
        ) compilers;
      let repos = OpamRepositoryName.Map.keys t.repositories in
      update_config t (List.filter ((<>) repo.repo_name) repos);
      OpamFilename.rmdir repo.repo_root;
    )

let priority repo_name ~priority =
  log "repository-priority";
  let t = OpamState.load_state ~save_cache:false "repository-priority" in
  relink_all t ~verbose:true (fun t ->
      let repo = OpamState.find_repository t repo_name in
      let config_f = OpamPath.Repository.config repo in
      let config =
        let config = OpamFile.Repo_config.read config_f in
        { config with repo_priority = priority } in
      OpamFile.Repo_config.write config_f config;
      let repo_index_f = OpamPath.repo_index t.root in
      let repo_index =
        let repo_index = OpamFile.Repo_index.safe_read (OpamPath.repo_index t.root) in
        let repo_index =
          OpamPackage.Name.Map.map (List.filter ((<>)repo_name)) repo_index in
        OpamPackage.Name.Map.filter (fun _ rs -> rs<>[]) repo_index in
      OpamFile.Repo_index.write repo_index_f repo_index;
    )

let add name kind address ~priority:prio =
  log "repository-add";
  let t = OpamState.load_state "repository-add" in
  let repo = {
    repo_name     = name;
    repo_kind     = kind;
    repo_address  = address;
    repo_priority = min_int; (* we initially put it as low-priority *)
    repo_root     = OpamPath.Repository.create name;
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
        (OpamFilename.Dir.to_string r.repo_address) in
    let repos = OpamState.sorted_repositories t in
    List.iter pretty_print repos
  )

(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
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
open OpamFilename.OP
open OpamMisc.OP

let log fmt =
  OpamGlobals.log "STATE" fmt

let () =
  OpamCurl.register ();
  OpamGit.register ();
  OpamRsync.register ()

let check f =
  let root = OpamPath.default () in
  let with_switch_lock a f = OpamFilename.with_flock (OpamPath.Switch.lock root a) f in
  if OpamFilename.exists_dir root then
    match f with

    | Global_lock f ->
      (* Take the global lock *)
      OpamFilename.with_flock (OpamPath.lock root) (fun () ->
        (* Take all the switch locks *)
        let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
        let f = OpamSwitch.Map.fold (fun a _ f -> with_switch_lock a (fun () -> f ())) aliases f in
        f ()
      ) ()

    | Read_lock f ->
      (* Simply check that OPAM is correctly initialized *)
      if OpamFilename.exists_dir (OpamPath.root root) then
        f ()
      else
        OpamGlobals.error_and_exit
          "Cannot find %s. Have you run 'opam init first ?"
          (OpamFilename.Dir.to_string root)

    | Switch_lock f ->
      (* Take a switch lock (and check that the global lock is free). *)
      let switch =
        OpamFilename.with_flock
          (OpamPath.lock root)
          (fun () -> match !OpamGlobals.switch with
          | None   -> OpamFile.Config.switch (OpamFile.Config.read (OpamPath.config root))
          | Some a -> OpamSwitch.of_string a)
          () in
      (* XXX: We can have a small race just here ... *)
      with_switch_lock switch f ()

type state = {
  root: OpamPath.t;
  switch: switch;
  compiler: compiler;
  compiler_version: compiler_version;
  opams: OpamFile.OPAM.t package_map;
  repositories: OpamFile.Repo_config.t repository_name_map;
  packages: package_set;
  available_packages: package_set Lazy.t;
  aliases: OpamFile.Aliases.t;
  pinned: OpamFile.Pinned.t;
  installed: OpamFile.Installed.t;
  reinstall: OpamFile.Reinstall.t;
  config: OpamFile.Config.t;
  repo_index: OpamFile.Repo_index.t;
}

let universe t action = {
  u_action    = action;
  u_installed = t.installed;
  u_available = Lazy.force t.available_packages;
  u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends t.opams;
  u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts t.opams;
  u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts t.opams;
}

let string_of_repositories r =
  OpamMisc.string_of_list
    OpamRepositoryName.to_string
    (OpamRepositoryName.Map.keys r)

let print_state t =
  log "ROOT      : %s" (OpamFilename.Dir.to_string (OpamPath.root t.root));
  log "SWITCH     : %s" (OpamSwitch.to_string t.switch);
  log "COMPILER  : %s" (OpamCompiler.to_string t.compiler);
  log "REPOS     : %s" (string_of_repositories t.repositories);
  if !OpamGlobals.debug then
    log "AVAILABLE : %s" (OpamPackage.Set.to_string (Lazy.force t.available_packages))
  else
    log "PACKAGES  : %s" (OpamPackage.Set.to_string t.packages);
  log "INSTALLED : %s" (OpamPackage.Set.to_string t.installed);
  log "REINSTALL : %s" (OpamPackage.Set.to_string t.reinstall)

let compiler_of_switch t switch =
  try Some (OpamSwitch.Map.find switch t.aliases)
  with Not_found -> None

let config t =
  OpamFile.Config.read (OpamPath.config t.root)

let compilers t =
  OpamCompiler.list (OpamPath.compilers_dir t.root)

let repositories t =
  OpamFile.Config.repositories t.config

let opam t nv =
  OpamPackage.Map.find nv t.opams

let archives t =
  OpamFilename.Set.of_list (OpamFilename.list_files (OpamPath.archives_dir t.root))

let compiler t c =
  OpamFile.Comp.safe_read (OpamPath.compiler t.root c)

let mem_installed_package_by_name_aux installed name =
  let set = OpamPackage.Set.filter (fun nv -> OpamPackage.name nv = name) installed in
  not (OpamPackage.Set.is_empty set)

let mem_installed_package_by_name t name =
  mem_installed_package_by_name_aux t.installed name

let find_installed_package_by_name_aux installed name =
  try OpamPackage.Set.find (fun nv -> OpamPackage.name nv = name) installed
  with Not_found ->
    OpamGlobals.error_and_exit "Package %s is not installed" (OpamPackage.Name.to_string name)

let find_installed_package_by_name t name =
  find_installed_package_by_name_aux t.installed name

let find_packages_by_name t name =
  let r = OpamPackage.Set.filter (fun nv -> OpamPackage.name nv = name) t.packages in
  if OpamPackage.Set.is_empty r then
    None
  else
    Some r

let installed_map t =
  OpamPackage.Name.Map.map OpamPackage.Version.Set.choose_one (OpamPackage.to_map t.installed)

let dot_config t nv =
  OpamFile.Dot_config.safe_read (OpamPath.Switch.config t.root t.switch nv)

let reinstall t =
  OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall t.root t.switch)

let mem_repository_name t name =
  OpamRepositoryName.Map.exists (fun n _ -> n = name) t.repositories

let find_repository_name t name =
  OpamRepositoryName.Map.find name t.repositories

let find_repository_aux repositories root repo_index nv =
  log "find_repository %s" (OpamPackage.to_string nv);
  let name = OpamPackage.name nv in
  let rec aux = function
    | []          -> None
    | r :: repo_s ->
        let repo = OpamRepositoryName.Map.find r repositories in
        let repo_p = OpamPath.Repository.create root r in
        let opam_f = OpamPath.Repository.opam repo_p nv in
        if OpamFilename.exists opam_f then (
          Some (repo_p, repo)
        ) else
          aux repo_s in
  if OpamPackage.Name.Map.mem name repo_index then
    aux (OpamPackage.Name.Map.find name repo_index)
  else
    None

let find_repository t nv =
  find_repository_aux t.repositories t.root t.repo_index nv

let mem_repository t nv =
  find_repository t nv <> None

let with_repository t nv fn =
  match find_repository t nv with
  | None ->
    OpamGlobals.error_and_exit
      "Unable to find a repository containing %s"
      (OpamPackage.to_string nv)
  | Some (repo_p, repo) -> fn repo_p repo

  (* List the packages which does fullfil the compiler constraints *)
let available_packages root opams repositories repo_index compiler_version config pinned packages =
  let filter nv =
    let opam = OpamPackage.Map.find nv opams in
    let available () =
      find_repository_aux repositories root repo_index nv <> None in
    let consistent_ocaml_version () =
      let atom (r,v) = OpamCompiler.Version.compare compiler_version r v in
      match OpamFile.OPAM.ocaml_version opam with
      | None   -> true
      | Some c -> OpamFormula.eval atom c in
    let consistent_pinned_version () =
      not (OpamPackage.Name.Map.mem (OpamPackage.name nv) pinned) ||
        match OpamPackage.Name.Map.find (OpamPackage.name nv) pinned with
        | Version v -> v = OpamPackage.version nv
        | _         -> true (* any version is fine, as this will be overloaded on install *) in
    available ()
    && consistent_ocaml_version ()
    && consistent_pinned_version () in
  OpamPackage.Set.filter filter packages

let load_state () =
  let root = OpamPath.default () in
  log "load_state root=%s" (OpamFilename.Dir.to_string root);

  let config = OpamFile.Config.read (OpamPath.config root) in
  let switch = match !OpamGlobals.switch with
    | None   -> OpamFile.Config.switch config
    | Some a -> OpamSwitch.of_string a in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let compiler =
    try OpamSwitch.Map.find switch aliases
    with Not_found ->
      OpamGlobals.error "%s is an unknown compiler switch" (OpamSwitch.to_string switch);
      log "%S does not contain the compiler name associated to the switch %s"
        (OpamFilename.to_string (OpamPath.aliases root))
        (OpamSwitch.to_string switch);
      OpamGlobals.exit 2 in
  let compiler_version =
    let comp = OpamFile.Comp.read (OpamPath.compiler root compiler) in
    OpamFile.Comp.version comp in
  let opams =
    OpamPackage.Set.fold (fun nv map ->
      try
        let opam = OpamFile.OPAM.read (OpamPath.opam root nv) in
        OpamPackage.Map.add nv opam map
      with _ ->
        map
    ) (OpamPackage.list (OpamPath.opam_dir root)) OpamPackage.Map.empty in
  let repositories =
    List.fold_left (fun map repo ->
      let repo_p = OpamPath.Repository.create root repo in
      let config = OpamFile.Repo_config.read (OpamPath.Repository.config repo_p) in
      OpamRepositoryName.Map.add repo config map
    ) OpamRepositoryName.Map.empty (OpamFile.Config.repositories config) in
  let repo_index = OpamFile.Repo_index.safe_read (OpamPath.repo_index root) in
  let pinned = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned root switch) in
  let installed = OpamFile.Installed.safe_read (OpamPath.Switch.installed root switch) in
  let reinstall = OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall root switch) in
  let packages = OpamPackage.list (OpamPath.opam_dir root) in
  let available_packages =
    lazy (available_packages root opams repositories repo_index compiler_version config pinned packages) in
  let t = {
    root; switch; compiler; compiler_version; repositories; opams;
    packages; available_packages; installed; reinstall;
    repo_index; config; aliases; pinned;
  } in
  print_state t;
  (* update from opam 0.7 to 0.8: Remove spurious conf-ocaml packages *)
  if mem_installed_package_by_name t OpamPackage.Name.default then
    OpamFile.Installed.write (OpamPath.Switch.installed t.root t.switch)
      (OpamPackage.Set.filter (fun nv -> OpamPackage.name nv <> OpamPackage.Name.default) t.installed);
  (match find_packages_by_name t OpamPackage.Name.default with
  | None          -> ()
  | Some packages ->
    OpamPackage.Set.iter (fun nv ->
      OpamFilename.remove (OpamPath.opam t.root nv);
      OpamFilename.remove (OpamPath.descr t.root nv);
    ) packages);
  t

module Types = struct
  type t = state = {
    root: OpamPath.t;
    switch: switch;
    compiler: compiler;
    compiler_version: compiler_version;
    opams: OpamFile.OPAM.t package_map;
    repositories: OpamFile.Repo_config.t repository_name_map;
    packages: package_set;
    available_packages: package_set Lazy.t;
    aliases: OpamFile.Aliases.t;
    pinned: OpamFile.Pinned.t;
    installed: OpamFile.Installed.t;
    reinstall: OpamFile.Reinstall.t;
    config: OpamFile.Config.t;
    repo_index: OpamFile.Repo_index.t;
  }
end

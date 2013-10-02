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
open OpamMisc.OP
open OpamFilename.OP

let log fmt =
  OpamGlobals.log "STATE" fmt

let () =
  OpamHTTP.register ();
  OpamGit.register ();
  OpamDarcs.register();
  OpamLocal.register ();
  OpamHg.register ()

let confirm fmt =
  Printf.ksprintf (fun msg ->
    OpamGlobals.msg "%s [Y/n] %!" msg;
    if not !OpamGlobals.yes then
      match read_line () with
      | "y" | "Y"
      | "" -> true
      | _  -> false
    else
      true
  ) fmt

let read fmt =
  Printf.ksprintf (fun msg ->
    OpamGlobals.msg "%s %!" msg;
    if not !OpamGlobals.yes then (
      try match read_line () with
        | "" -> None
        | s  -> Some s
      with _ ->
        OpamGlobals.msg "\n";
        None
    ) else
      None
  ) fmt

let switch_reinstall_hook = ref (fun _ -> assert false)

module Types = struct
  type t = {
    partial: bool;
    root: OpamPath.t;
    switch: switch;
    compiler: compiler;
    compiler_version: compiler_version lazy_t;
    opams: OpamFile.OPAM.t package_map;
    repositories: OpamFile.Repo_config.t repository_name_map;
    packages: package_set;
    available_packages: package_set Lazy.t;
    aliases: OpamFile.Aliases.t;
    compilers: compiler_set;
    pinned: OpamFile.Pinned.t;
    installed: OpamFile.Installed.t;
    installed_roots: OpamFile.Installed_roots.t;
    reinstall: OpamFile.Reinstall.t;
    config: OpamFile.Config.t;
    package_index: OpamFile.Package_index.t;
    compiler_index: OpamFile.Compiler_index.t;
  }
end

type state = Types.t
open Types

let universe t action =
  let opams =
    OpamPackage.Map.mapi (fun nv opam ->
        let overlay = OpamPath.Switch.Overlay.opam t.root t.switch nv in
        if OpamFilename.exists overlay then OpamFile.OPAM.read overlay
        else opam)
      t.opams in
  {
    u_packages  = OpamPackage.Set.union t.installed t.packages;
    u_action    = action;
    u_installed = t.installed;
    u_available = Lazy.force t.available_packages;
    u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends opams;
    u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts opams;
    u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams;
    u_installed_roots = t.installed_roots;
    u_pinned    =
      OpamPackage.Name.Map.fold
        (fun k _ set -> OpamPackage.Name.Set.add k set)
        t.pinned OpamPackage.Name.Set.empty;
  }

let string_of_repositories r =
  OpamMisc.string_of_list
    OpamRepositoryName.to_string
    (OpamRepositoryName.Map.keys r)

let print_state t =
  let packages =
    if OpamPackage.Set.cardinal t.packages <= 20 then
      OpamPackage.Set.to_string t.packages
    else
      Printf.sprintf "%d packages" (OpamPackage.Set.cardinal t.packages) in
  log "ROOT      : %s" (OpamFilename.Dir.to_string t.root);
  log "SWITCH    : %s" (OpamSwitch.to_string t.switch);
  log "COMPILER  : %s" (OpamCompiler.to_string t.compiler);
  log "COMPILERS : %s" (OpamCompiler.Set.to_string t.compilers);
  log "REPOS     : %s" (string_of_repositories t.repositories);
  log "PACKAGES  : %s" packages;
  log "INSTALLED : %s" (OpamPackage.Set.to_string t.installed);
  log "ROOTS     : %s" (OpamPackage.Set.to_string t.installed_roots);
  log "REINSTALL : %s" (OpamPackage.Set.to_string t.reinstall)

let compiler_comp t c =
  OpamFile.Comp.read (OpamPath.compiler_comp t.root c)

let is_name_installed_aux installed name =
  let set = OpamPackage.Set.filter (fun nv -> OpamPackage.name nv = name) installed in
  not (OpamPackage.Set.is_empty set)

let is_name_installed t name =
  is_name_installed_aux t.installed name

let find_installed_package_by_name_aux installed name =
  try OpamPackage.Set.find (fun nv -> OpamPackage.name nv = name) installed
  with Not_found ->
    OpamGlobals.error_and_exit
      "Package %s is not installed" (OpamPackage.Name.to_string name)

let find_installed_package_by_name t name =
  find_installed_package_by_name_aux t.installed name

let find_packages_by_name t name =
  let r = OpamPackage.Set.filter (fun nv -> OpamPackage.name nv = name) t.packages in
  if OpamPackage.Set.is_empty r then None
  else Some r

let installed_map t =
  OpamPackage.Name.Map.map OpamPackage.Version.Set.choose_one
    (OpamPackage.to_map t.installed)

let dot_config t nv =
  OpamFile.Dot_config.safe_read (OpamPath.Switch.config t.root t.switch nv)

let is_package_installed t nv =
  OpamPackage.Set.mem nv t.installed

let jobs t =
  match !OpamGlobals.jobs with
  | None   -> OpamFile.Config.jobs t.config
  | Some j -> j

(* filter handling *)
let env_filter t local_variables v =
  let name = OpamVariable.Full.package v in
  let var = OpamVariable.Full.variable v in
  let var_str = OpamVariable.to_string var in
  let string str = Some (S str) in
  let bool b = Some (B b) in
  let int i = string (string_of_int i) in
  let dirname dir = string (OpamFilename.Dir.to_string dir) in
  let read_var name =
    let c = dot_config t name in
    try match OpamVariable.Full.section v with
      | None   -> OpamFile.Dot_config.variable c var
      | Some s -> OpamFile.Dot_config.Section.variable c s var
    with Not_found ->
      OpamGlobals.error "%s is not defined" (OpamVariable.Full.to_string v);
      None in
  let local_var =
    try Some (OpamVariable.Map.find var local_variables)
    with Not_found -> None
  in
  if local_var <> None then local_var else
  if name = OpamPackage.Name.global_config then (
    try match var_str with
      | "ocaml-version" -> string (OpamMisc.getenv "ocaml_version")
      | "opam-version"  -> string (OpamMisc.getenv "opam_version")
      | _               -> string (OpamMisc.getenv var_str)
    with Not_found ->
      match var_str with
      | "ocaml-version" -> string (OpamCompiler.Version.to_string
                                     (Lazy.force t.compiler_version))
      | "opam-version"  -> string (OpamVersion.to_string OpamVersion.current)
      | "preinstalled"  -> bool (OpamFile.Comp.preinstalled
                                   (compiler_comp t t.compiler))
      | "switch"        -> string (OpamSwitch.to_string t.switch)
      | "jobs"          -> int (jobs t)
      | _               -> read_var name
  ) else (
    let exists = find_packages_by_name t name <> None in
    let name_str = OpamPackage.Name.to_string name in
    if not exists then None
    else
      try
        let var_hook = Printf.sprintf "OPAM_%s_%s" name_str var_str in
        match OpamMisc.getenv var_hook with
        | "true"  | "1" -> bool true
        | "false" | "0" -> bool false
        | s             -> string s
      with Not_found ->
        let is_installed = is_name_installed t name in
        let no_section = OpamVariable.Full.section v = None in
        if var = OpamVariable.enable && is_installed && no_section then
          string "enable"
        else if var = OpamVariable.enable && not is_installed && no_section then
          string "disable"
        else if var = OpamVariable.installed && no_section then
          bool is_installed
        else if var = OpamVariable.installed || var = OpamVariable.enable then (
          OpamGlobals.error
            "Syntax error: invalid section argument in '%s'.\nUse '%s:%s' instead."
            (OpamVariable.Full.to_string v)
            name_str
            (OpamVariable.to_string var);
          None
        ) else if var = OpamVariable.of_string "pinned" then
          bool (OpamPackage.Name.Map.mem name t.pinned)
        else if is_installed then (
          match OpamVariable.to_string var with
          | "bin"     -> dirname (OpamPath.Switch.bin     t.root t.switch)
          | "sbin"    -> dirname (OpamPath.Switch.sbin    t.root t.switch)
          | "lib"     -> dirname (OpamPath.Switch.lib     t.root t.switch name)
          | "man"     -> dirname (OpamPath.Switch.man_dir t.root t.switch)
          | "doc"     -> dirname (OpamPath.Switch.doc     t.root t.switch name)
          | "share"   -> dirname (OpamPath.Switch.share   t.root t.switch name)
          | "etc"     -> dirname (OpamPath.Switch.etc     t.root t.switch name)
          | "version" ->
            let nv = find_installed_package_by_name t name in
            string (OpamPackage.Version.to_string (OpamPackage.version nv))
          | _         -> read_var name
        ) else
          None
  )

let eval_filter t local_variables =
  OpamFilter.eval_opt (env_filter t local_variables)

let filter_commands t local_variables =
  OpamFilter.commands (env_filter t local_variables)

let substitute_file t local_variables =
  OpamFilter.substitute_file (env_filter t local_variables)

let substitute_string t local_variables =
  OpamFilter.substitute_string (env_filter t local_variables)

let contents_of_variable t local_variables =
  OpamFilter.contents_of_variable (env_filter t local_variables)

let contents_of_variable_exn t local_variables =
  OpamFilter.contents_of_variable_exn (env_filter t local_variables)

let redirect t repo =
  let redirect =
    repo
    |> OpamPath.Repository.repo
    |> OpamFile.Repo.safe_read
    |> OpamFile.Repo.redirect
  in
  let redirect = List.fold_left (fun acc (redirect, filter) ->
      if eval_filter t OpamVariable.Map.empty filter then
        (redirect, filter) :: acc
      else
        acc
    ) [] redirect in
  match redirect with
  | []         -> None
  | (r,f) :: _ ->
    let config_f = OpamPath.Repository.config repo in
    let config = OpamFile.Repo_config.read config_f in
    let repo_address = address_of_string r in
    if repo_address <> config.repo_address then (
      let config = { config with repo_address } in
      OpamFile.Repo_config.write config_f config;
      Some (config, f)
    ) else
      None

let sorted_repositories t =
  OpamRepository.sort t.repositories

let mem_repository t repo_name =
  OpamRepositoryName.Map.mem repo_name t.repositories

let find_repository_aux repo_name repositories =
  try OpamRepositoryName.Map.find repo_name repositories
  with Not_found ->
    OpamGlobals.error_and_exit
      "%s is not a valid repository name."
      (OpamRepositoryName.to_string repo_name)

let find_repository t repo_name =
  find_repository_aux repo_name t.repositories

let find_repository_exn t repo_name =
  OpamRepositoryName.Map.find repo_name t.repositories

let find_repository_opt t repo_name =
  try Some (find_repository_exn t repo_name)
  with Not_found -> None

let compiler_index t =
  OpamRepository.compiler_index t.repositories

let package_index t =
  OpamRepository.package_index t.repositories

let package_state_one t all nv =
  let opam    = OpamPath.opam t.root nv in
  let descr   = OpamPath.descr t.root nv in
  let url     = OpamPath.url t.root nv in
  let files   = OpamPath.files t.root nv in
  let archive = OpamPath.archive t.root nv in
  if not (OpamFilename.exists opam) then []
  else match all with
    | `all ->
      OpamFilename.checksum opam
      @ OpamFilename.checksum descr
      @ OpamFilename.checksum url
      @ OpamFilename.checksum_dir files
      @ OpamFilename.checksum archive
    | `partial true ->
      OpamFilename.checksum url
      @ OpamFilename.checksum_dir files
      @ OpamFilename.checksum archive
    | `partial false ->
      OpamFilename.checksum url
      @ OpamFilename.checksum_dir files

let all_installed t =
  OpamSwitch.Map.fold (fun switch _ accu ->
    let installed_f = OpamPath.Switch.installed t.root switch in
    let installed = OpamFile.Installed.safe_read installed_f in
    OpamPackage.Set.union installed accu
  ) t.aliases OpamPackage.Set.empty

let package_state t =
  let installed = OpamPackage.Set.fold (fun nv map ->
      let state = package_state_one t `all nv in
      OpamPackage.Map.add nv state map
    ) (all_installed t) OpamPackage.Map.empty in
  OpamPackage.Map.fold (fun nv (repo, prefix) map ->
      if OpamPackage.Map.mem nv map then map
      else if OpamFilename.exists (OpamPath.opam t.root nv) then
        let state = package_state_one t `all nv in
        OpamPackage.Map.add nv state map
      else
        let repo = find_repository_exn t repo in
        let state = OpamRepository.package_state repo prefix nv `all in
        OpamPackage.Map.add nv state map
    ) t.package_index installed

let package_partial_state t nv ~archive =
  match package_state_one t (`partial archive) nv with
  | []    -> false, []
  | state ->
    let archive = OpamPath.archive t.root nv in
    OpamFilename.exists archive, state

let package_repository_state t =
  OpamPackage.Map.fold (fun nv (repo, prefix) map ->
      let repo = find_repository_exn t repo in
      match OpamRepository.package_state repo prefix nv `all with
      | []    -> map
      | state -> OpamPackage.Map.add nv state map
    ) t.package_index OpamPackage.Map.empty

let package_repository_partial_state t nv ~archive =
  let repo, prefix = OpamPackage.Map.find nv t.package_index in
  let repo = find_repository_exn t repo in
  let exists_archive = OpamFilename.exists (OpamPath.Repository.archive repo nv) in
  exists_archive, OpamRepository.package_state repo prefix nv (`partial archive)

let repository_and_prefix_of_package t nv =
   try
    let repo, prefix = OpamPackage.Map.find nv t.package_index in
    let repo = find_repository_exn t repo in
    Some (repo, prefix)
   with Not_found ->
     None

let repository_of_package t nv =
  try
    let repo, _ = OpamPackage.Map.find nv t.package_index in
    let repo = OpamRepositoryName.Map.find repo t.repositories in
    Some repo
  with Not_found ->
    None

let compiler_state_one t c =
  let comp = OpamPath.compiler_comp t.root c in
  let descr = OpamPath.compiler_descr t.root c in
  if OpamFilename.exists comp then
    Some (OpamFilename.checksum comp @ OpamFilename.checksum descr)
  else
    None

let compiler_state t =
  OpamCompiler.Set.fold (fun c map ->
      match compiler_state_one t c with
      | None   -> map
      | Some s -> OpamCompiler.Map.add c s map
    ) t.compilers OpamCompiler.Map.empty

let compiler_repository_state t =
  OpamCompiler.Map.fold (fun comp (repo, prefix) map ->
      let repo = find_repository_exn t repo in
      match OpamRepository.compiler_state repo prefix comp with
      | [] -> map
      | l  -> OpamCompiler.Map.add comp l map
    ) t.compiler_index OpamCompiler.Map.empty

let repository_and_prefix_of_compiler t comp =
  try
    let repo, prefix = OpamCompiler.Map.find comp t.compiler_index in
    let repo = find_repository_exn t repo in
    Some (repo, prefix)
  with Not_found ->
    None

let is_pinned t n =
  OpamPackage.Name.Map.mem n t.pinned

(* is the current package locally pinned *)
let is_locally_pinned t name =
  if OpamPackage.Name.Map.mem name t.pinned then
    match OpamPackage.Name.Map.find name t.pinned with
    | Edit | Unpin | Version _ -> false
    | _ -> true
  else
    false

let locally_pinned_package t n =
  let option = OpamPackage.Name.Map.find n t.pinned in
  let path = string_of_pin_option option in
  match kind_of_pin_option option with
  | None      -> OpamGlobals.error_and_exit
                   "%s has a wrong pinning kind." (OpamPackage.Name.to_string n)
  | Some kind ->
    match repository_kind_of_pin_kind kind with
    | None    -> OpamSystem.internal_error "locally pinned"
    | Some kind -> (address_of_string path, kind)

let url_of_locally_pinned_package t n =
  let path, kind = locally_pinned_package t n in
  OpamFile.URL.create (Some kind) path

let repository_of_locally_pinned_package t n =
  let url = url_of_locally_pinned_package t n in
  let repo_address = OpamFile.URL.url url in
  let repo_kind = guess_repository_kind (OpamFile.URL.kind url) repo_address in
  let repo_root = OpamPath.Switch.dev_package t.root t.switch (OpamPackage.pinned n) in
  { repo_name     = OpamRepositoryName.of_string (OpamPackage.Name.to_string n);
    repo_priority = 0;
    repo_root; repo_address; repo_kind }

let real_package t nv =
  let name = OpamPackage.name nv in
  if is_pinned t name then OpamPackage.pinned name
  else nv

let opam_opt_no_pin t nv =
  let overlay = OpamPath.Switch.Overlay.opam t.root t.switch nv in
  if OpamFilename.exists overlay then Some (OpamFile.OPAM.read overlay)
  else
    try Some (OpamPackage.Map.find nv t.opams)
    with Not_found ->
      if OpamPackage.Set.mem nv t.installed then
        (* Work-around for installed packages with no more metadata *)
        Some (OpamFile.OPAM.create nv)
      else
        None

let opam_no_pin t nv =
  match opam_opt_no_pin t nv with
  | None    -> OpamPackage.unknown (OpamPackage.name nv) (Some (OpamPackage.version nv))
  | Some nv -> nv

let rec readn exists fn = function
  | []   -> None
  | h::t ->
    match h () with
    | None      -> readn exists fn t
    | Some file -> if exists file then Some (fn file) else readn exists fn t

let get_opt_no_pin overlay global repo exits read t nv =
  readn exits read [
    (fun () -> Some (overlay t.root t.switch nv));
    (fun () -> Some (global t.root nv));
    (fun () ->
      try
        let r, prefix = OpamPackage.Map.find nv t.package_index in
        let r = find_repository_exn t r in
        Some (repo r prefix nv)
      with Not_found ->
        None)
  ]

let descr_opt_no_pin =
  get_opt_no_pin
    OpamPath.Switch.Overlay.descr OpamPath.descr OpamPath.Repository.descr
    OpamFilename.exists OpamFile.Descr.read

let descr_no_pin t nv =
  match descr_opt_no_pin t nv with
  | None   -> OpamFile.Descr.empty
  | Some f -> f

let url_no_pin =
  get_opt_no_pin
    OpamPath.Switch.Overlay.url OpamPath.url OpamPath.Repository.url
    OpamFilename.exists OpamFile.URL.read

let files_no_pin =
  get_opt_no_pin
    OpamPath.Switch.Overlay.files OpamPath.files OpamPath.Repository.files
    OpamFilename.exists_dir (fun d -> d)

let install_metadata t nv =
  if OpamFilename.exists (OpamPath.opam t.root nv) then ()
  else
    let opam = opam_no_pin t nv in
    let descr = descr_opt_no_pin t nv in
    let url = url_no_pin t nv in
    let files = files_no_pin t nv in
    OpamFile.OPAM.write (OpamPath.opam t.root nv) opam;
    (match descr with
     | None   -> ()
     | Some d -> OpamFile.Descr.write (OpamPath.descr t.root nv) d);
    (match url with
     | None   -> ()
     | Some u -> OpamFile.URL.write (OpamPath.url t.root nv) u);
    (match files with
     | None   -> ()
     | Some d -> OpamFilename.copy_dir ~src:d ~dst:(OpamPath.files t.root nv))

let remove_metadata t packages =
  let all_installed = all_installed t in
  let packages = OpamPackage.Set.inter all_installed packages in
  OpamPackage.Set.iter (fun nv ->
      let dir = OpamPath.packages t.root nv in
      OpamFilename.rmdir dir;
      let archive = OpamPath.archive t.root nv in
      OpamFilename.remove archive;
    ) packages

let overlay_of_name t name =
  let versions = OpamPackage.versions_of_name t.packages name in
  let versions =
    OpamPackage.Version.Set.filter ((<>) OpamPackage.Version.pinned) versions in
  let version = OpamPackage.Version.Set.max_elt versions in
  OpamPackage.create name version

let add_opam_overlay t nv opam =
  let dst = OpamPath.Switch.Overlay.opam t.root t.switch nv in
  OpamFile.OPAM.write dst opam

let add_url_overlay t nv url =
  let dst = OpamPath.Switch.Overlay.url t.root t.switch nv in
  OpamFile.URL.write dst url

let add_descr_overlay t nv descr =
  let dst = OpamPath.Switch.Overlay.descr t.root t.switch nv in
  OpamFile.Descr.write dst descr

let add_files_overlay t nv root files =
  let dst = OpamPath.Switch.Overlay.files t.root t.switch nv in
  List.iter (fun file ->
      let base = OpamFilename.remove_prefix root file in
      OpamFilename.copy ~src:file ~dst:(dst // base)
    ) files

let add_pinned_overlay t name =
  let nv = OpamPackage.pinned name in
  let local_pin, rv =
    match OpamPackage.Name.Map.find name t.pinned with
    | Version v -> false, OpamPackage.create name v
    | _         -> true , overlay_of_name t name in
  let opam_f =
    if local_pin then
      let path, _ = locally_pinned_package t name in
      let dir = OpamFilename.raw_dir (fst path) in
      let dir =
        if OpamFilename.exists_dir (dir / "opam") then
          dir / "opam"
        else
          dir in
      let local_opam = dir // "opam" in
      if OpamFilename.exists local_opam then
        let opam = OpamFile.OPAM.read local_opam in
        let opam = OpamFile.OPAM.with_name opam name in
        OpamFile.OPAM.with_version opam (OpamPackage.version rv)
      else opam_no_pin t rv
    else opam_no_pin t rv in
  let descr_f = descr_opt_no_pin t rv in
  let url_f =
    if local_pin then Some (url_of_locally_pinned_package t name)
    else url_no_pin t rv in
  let files_f = files_no_pin t rv in
  log "opam: %s" (OpamPackage.Version.to_string (OpamFile.OPAM.version opam_f));
  add_opam_overlay t nv opam_f;
  (match descr_f with
   | None   -> ()
   | Some d -> add_descr_overlay t nv d);
  (match url_f with
   | None   -> ()
   | Some u -> add_url_overlay t nv u);
  (match files_f with
   | None   -> ()
   | Some d ->  add_files_overlay t nv d (OpamFilename.files d))

let remove_overlay t nv =
  OpamFilename.rmdir (OpamPath.Switch.Overlay.package t.root t.switch nv)

let has_url_overlay t nv =
  OpamFilename.exists (OpamPath.Switch.Overlay.url t.root t.switch nv)

let dev_package t nv =
  if has_url_overlay t nv then OpamPath.Switch.dev_package t.root t.switch nv
  else OpamPath.dev_package t.root nv

let pinning_version t nv =
  let name = OpamPackage.name nv in
  try
    match OpamPackage.Name.Map.find name t.pinned with
    | Version v -> OpamPackage.create name v
    | _ ->
      let nv1 = OpamPackage.pinned name in
      let overlay = OpamPath.Switch.Overlay.opam t.root t.switch nv1 in
      if not (OpamFilename.exists overlay) then
        add_pinned_overlay t name;
      let opam = OpamFile.OPAM.read overlay in
      if OpamFile.OPAM.version opam = OpamPackage.Version.pinned then
        (OpamGlobals.warning
           "Package %s is pinned locally but with unspecified version:\n\
            is your OPAM directory up-to-date ? Please try running \
            'mkdir ~/.opam/opam && opam list'."
           (OpamPackage.Name.to_string name);
         nv)
      else
        OpamPackage.create name (OpamFile.OPAM.version opam)
  with Not_found ->
    nv

let opam_opt t nv =
  opam_opt_no_pin t (real_package t nv)

let opam t nv =
  opam_no_pin t (real_package t nv)

let descr_opt t nv =
  descr_opt_no_pin t (real_package t nv)

let descr t nv =
  descr_no_pin t (real_package t nv)

let url t nv =
  url_no_pin t (real_package t nv)

let files t nv =
  files_no_pin t (real_package t nv)

let copy_files t nv dst =
  match files t nv with
  | None     -> ()
  | Some src -> OpamFilename.copy_files ~src ~dst

(* List the packages which do fulfil the compiler and OS constraints *)
let available_packages t system =
  let env = env_filter t OpamVariable.Map.empty in
  let filter nv =
    match opam_opt t nv with
    | None -> false
    | Some opam ->
      let consistent_ocaml_version () =
        let atom (r,v) =
          match OpamCompiler.Version.to_string v with
          | "system" ->
            begin match r with
              | `Eq  -> system
              | `Neq -> not system
              | _    -> OpamSystem.internal_error
                          "%s is not a valid constraint for the system compiler \
                           (only '=' and '!=' are valid)."
                          (OpamFormula.string_of_relop r)
            end
          | _ -> OpamCompiler.Version.compare (Lazy.force t.compiler_version) r v in
        match OpamFile.OPAM.ocaml_version opam with
        | None   -> true
        | Some c -> OpamFormula.eval atom c in
      let consistent_os () =
        match OpamFile.OPAM.os opam with
        | Empty -> true
        | f ->
          let atom (b, os) =
            let ($) = if b then (=) else (<>) in
            os $ OpamGlobals.os_string () in
          OpamFormula.eval atom f in
      let has_reposiotry () =
        OpamPackage.Map.mem nv t.package_index in
      let available () =
        OpamFilter.eval env (OpamFile.OPAM.available opam) in
      consistent_ocaml_version ()
      && consistent_os ()
      && available ()
      && has_reposiotry ()
  in
  let _pinned, set =
    OpamPackage.Map.fold (fun nv _ (pinned, set) ->
        if OpamPackage.Name.Set.mem (OpamPackage.name nv) pinned then
          (* Package has already been added to the set *)
          (pinned, set)
        else
          let nv1 = pinning_version t nv in
          let pinned =
            if nv1 != nv
            then OpamPackage.Name.Set.add (OpamPackage.name nv1) pinned
            else pinned in
          if filter nv1 then pinned, OpamPackage.Set.add nv1 set
          else pinned, set
      ) t.opams (OpamPackage.Name.Set.empty, OpamPackage.Set.empty) in
  set

let base_packages =
  List.map OpamPackage.Name.of_string [ "base-unix"; "base-bigarray"; "base-threads" ]

let create_system_compiler_description root = function
  | None         -> ()
  | Some version ->
    log "create-system-compiler-description %s"
      (OpamCompiler.Version.to_string version);
    match Lazy.force OpamSystem.system_ocamlc_where with
    | None     -> ()
    | Some dir ->
      let comp = OpamPath.compiler_comp root OpamCompiler.system in
      OpamFilename.remove comp;
      let f =
        OpamFile.Comp.create_preinstalled
          OpamCompiler.system version
          (if not !OpamGlobals.no_base_packages then base_packages else [])
          [ "CAML_LD_LIBRARY_PATH", "=",
            "%{lib}%/stublibs" ^ ":" ^ Filename.concat dir "stublibs" ] in
    OpamFile.Comp.write comp f

let system_needs_upgrade_displayed = ref false
let system_needs_upgrade t =
  t.compiler = OpamCompiler.system
  && match OpamCompiler.Version.system () with
  | None   ->
    if not !system_needs_upgrade_displayed then (
      system_needs_upgrade_displayed := true;
      OpamGlobals.error
        "You current switch use the system compiler, but no OCaml compiler \
         has been found in the current path.\n\
         You should either:\n\
        \  (i)  reinstall OCaml version %s on your system; or\n\
        \  (ii) use a working compiler switch."
        (OpamCompiler.Version.to_string (Lazy.force t.compiler_version))
    );
    false
  | Some v ->
    OpamFilename.exists (OpamPath.compiler_comp t.root t.compiler)
    && (Lazy.force t.compiler_version) <> v

let read_repositories root config =
  let names = OpamFile.Config.repositories config in
  List.fold_left (fun map repo_name ->
    let repo = OpamFile.Repo_config.read
        (OpamPath.Repository.raw_config root repo_name) in
    OpamRepositoryName.Map.add repo_name repo map
  ) OpamRepositoryName.Map.empty names

(* load partial state to be able to read env variables *)
let load_env_state call_site =
  log "LOAD-ENV-STATE(%s)" call_site;
  let root = OpamPath.root () in
  let config_p = OpamPath.config root in
  let config = OpamFile.Config.read config_p in
  let switch = match !OpamGlobals.switch with
    | `Command_line s
    | `Env s   -> OpamSwitch.of_string s
    | `Not_set -> OpamFile.Config.switch config in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let compiler =
    try OpamSwitch.Map.find switch aliases
    with Not_found ->
      OpamGlobals.error_and_exit
        "The current switch (%s) is an unknown compiler switch."
        (OpamSwitch.to_string switch) in
  let partial = true in

  (* evertything else is empty *)
  let compilers = OpamCompiler.Set.empty in
  let repositories = OpamRepositoryName.Map.empty in
  let compiler_version = lazy (OpamCompiler.Version.of_string "none") in
  let opams = OpamPackage.Map.empty in
  let packages = OpamPackage.Set.empty in
  let available_packages = lazy OpamPackage.Set.empty in
  let installed = OpamPackage.Set.empty in
  let installed_roots = OpamPackage.Set.empty in
  let reinstall = OpamPackage.Set.empty in
  let pinned = OpamPackage.Name.Map.empty in
  let package_index = OpamPackage.Map.empty in
  let compiler_index = OpamCompiler.Map.empty in
  {
    partial; root; switch; compiler; compiler_version; repositories; opams;
    packages; available_packages; installed; installed_roots; reinstall;
    config; aliases; pinned; compilers;
    package_index; compiler_index;
  }

let get_compiler_packages t comp =
  let comp = compiler_comp t comp in
  let available = OpamPackage.to_map (Lazy.force t.available_packages) in

  if OpamPackage.Name.Map.is_empty available then
    []

  else (
    let pkg_available, pkg_not =
      List.partition
        (fun (n, _) -> OpamPackage.Name.Map.mem n available)
        (OpamFormula.atoms (OpamFile.Comp.packages comp)) in

    (* check that all packages in [comp] are in [available] except for
       "base-..."  (depending if "-no-base-packages" is set or not) *)
    let pkg_not = List.rev_map (function (n, _) -> n) pkg_not in
    let pkg_not =
      if not !OpamGlobals.no_base_packages then
        pkg_not
      else
        List.filter (fun n -> not (List.mem n base_packages)) pkg_not in
    if pkg_not <> [] then (
      List.iter
        (OpamPackage.Name.to_string ++ OpamGlobals.error "Package %s not found")
        pkg_not;
      OpamGlobals.exit 1
    );

    pkg_available
  )

let is_compiler_installed t comp =
  OpamSwitch.Map.exists (fun _ c -> c = comp) t.aliases

let is_switch_installed t switch =
  OpamSwitch.Map.mem switch t.aliases

let check_base_packages t =
  let base_packages = get_compiler_packages t t.compiler in
  let missing_packages =
    List.filter
      (fun (name,_) -> not (is_name_installed t name))
      base_packages in
  if missing_packages <> [] then (
    let names = List.map (fst ++ OpamPackage.Name.to_string) missing_packages in
    OpamGlobals.warning "Some of the compiler base packages are not installed. \
                         You should run:\n\n    $ opam install %s\n"
      (String.concat " " names)
  )

let installed_versions t name =
  OpamSwitch.Map.fold (fun switch _ map ->
    let installed =
      OpamFile.Installed.safe_read (OpamPath.Switch.installed t.root switch) in
    if is_name_installed_aux installed name then
      let nv = find_installed_package_by_name_aux installed name in
      if OpamPackage.Map.mem nv map then
        let aliases = OpamPackage.Map.find nv map in
        let map = OpamPackage.Map.remove nv map in
        OpamPackage.Map.add nv (switch :: aliases) map
      else
        OpamPackage.Map.add nv [switch] map
    else
      map
  ) t.aliases OpamPackage.Map.empty

(* Checks:
   * correct opam version
   * only installed packages have something in $repo/tmp
   * only installed packages have something in $opam/pinned.cache *)
let clean_dir dir nv =
  if OpamFilename.exists_dir dir then (
    OpamGlobals.note "%s exists although %s is not installed. Removing it."
      (OpamFilename.Dir.to_string dir) (OpamPackage.to_string nv);
    OpamFilename.rmdir dir
  )

let clean_file file nv =
  if OpamFilename.exists file then (
    OpamGlobals.note "%s exists although %s is not installed. Removing it."
      (OpamFilename.to_string file) (OpamPackage.to_string nv);
    OpamFilename.remove file
  )

let dev_packages_of_dir dir =
  let dirs = OpamFilename.dirs dir in
  List.fold_left (fun map dir ->
      match OpamPackage.of_dirname dir with
      | None     ->
        OpamGlobals.error "Removing %s.\n" (OpamFilename.Dir.to_string dir);
        OpamFilename.rmdir dir;
        map
      | Some nv  ->
        OpamPackage.Map.add nv dir map
    ) OpamPackage.Map.empty dirs

let global_dev_packages t =
  dev_packages_of_dir (OpamPath.dev_packages_dir t.root)

let switch_dev_packages t =
  dev_packages_of_dir (OpamPath.Switch.dev_packages_dir t.root t.switch)

let keys map =
  OpamPackage.Map.fold (fun nv _ set ->
      OpamPackage.Set.add nv set
    ) map OpamPackage.Set.empty

(* Check that the dev packages are installed -- if not, just remove
   the tempory files. *)
let consistency_checks dir pinned installed all_installed =
  let dev_packages = dev_packages_of_dir dir in
  OpamPackage.Map.iter (fun nv dir ->
      let name = OpamPackage.name nv in
      if OpamPackage.is_pinned nv then (
        if not (OpamPackage.Name.Map.mem name pinned)
        || OpamPackage.Set.for_all (fun nv -> OpamPackage.name nv <> name) installed
        then
          clean_dir dir nv
      ) else if not (OpamPackage.Set.mem nv all_installed) then
        clean_dir dir nv
    ) dev_packages;
  let files = OpamFilename.files dir in
  List.iter (fun f ->
      OpamGlobals.error "Removing %s.\n" (OpamFilename.to_string f);
      OpamFilename.remove f;
    ) files

let is_dev_package t nv =
  match url t nv with
  | None     -> false
  | Some url ->
    match guess_repository_kind (OpamFile.URL.kind url) (OpamFile.URL.url url) with
    | `http  -> false
    | _      -> true

let dev_packages t =
  let global = global_dev_packages t in
  let switch = switch_dev_packages t in
  let all = OpamPackage.Set.union (keys global) (keys switch) in
  OpamPackage.Set.filter (is_dev_package t) all

let global_consistency_checks t =
  consistency_checks (OpamPath.dev_packages_dir t.root)
    t.pinned t.installed (all_installed t);
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases t.root) in
  if OpamSwitch.Map.exists (fun _ c -> c =  OpamCompiler.system) aliases then
    let comp_f = OpamPath.compiler_comp t.root OpamCompiler.system in
    if not (OpamFilename.exists comp_f) then (
      OpamGlobals.msg "Regenerating the system compiler description.\n";
      create_system_compiler_description t.root (OpamCompiler.Version.system ());
    )

let switch_consistency_checks t =
  consistency_checks (OpamPath.Switch.dev_packages_dir t.root t.switch)
    t.pinned t.installed t.installed

type cache = {
  cached_opams: OpamFile.OPAM.t OpamPackage.Map.t;
}

let check_marshaled_file file =
  let ic = open_in_bin (OpamFilename.to_string file) in
  let magic_len = String.length OpamVersion.magic in
  let magic = String.create magic_len in
  really_input ic magic 0 magic_len;
  if magic <> OpamVersion.magic then (
    close_in ic;
    OpamSystem.internal_error
      "Wrong magic string in the cache (actual:%s expected:%s)."
      magic OpamVersion.magic;
  );
  let header = String.create Marshal.header_size in
  really_input ic header 0 Marshal.header_size;
  let expected_size = magic_len + Marshal.total_size header 0 in
  let current_size = in_channel_length ic in
  if not (expected_size = current_size) then (
    close_in ic;
    OpamGlobals.error "The local-state cache is corrupted, removing it.";
    OpamSystem.internal_error "Corrupted cache";
  );
  seek_in ic magic_len;
  ic

let marshal_from_file file =
  try
    let chrono = OpamGlobals.timer () in
    let ic = check_marshaled_file file in
    let (cache: cache) = Marshal.from_channel ic in
    close_in ic;
    log "Loaded %s in %.3fs" (OpamFilename.to_string file) (chrono ());
    Some cache.cached_opams
  with e ->
    log "Got an error while loading the cache: %s" (Printexc.to_string e);
    OpamFilename.remove file;
    None

let save_state ~update t =
  let chrono = OpamGlobals.timer () in
  let file = OpamPath.state_cache t.root in
  OpamFilename.remove file;
  if update then (
    OpamGlobals.msg
      "Updating the cache of metadata (%s) ...\n"
      (OpamFilename.prettify file);
  ) else
    OpamGlobals.msg
      "Creating a cache of metadata in %s ...\n"
      (OpamFilename.prettify file);
  let oc = open_out_bin (OpamFilename.to_string file) in
  output_string oc OpamVersion.magic;
  Marshal.to_channel oc { cached_opams = t.opams } [Marshal.No_sharing];
  close_out oc;
  log "%s written in %.3fs" (OpamFilename.prettify file) (chrono ())

let remove_state_cache () =
  let root = OpamPath.root () in
  let file = OpamPath.state_cache root in
  OpamFilename.remove file

let reinstall_system_compiler t =
  log "reinstall-system-compiler";
  let continue =
    confirm "Your system compiler has been upgraded. Do you want to upgrade \
             your OPAM installation?" in

  if continue then (

    (* Update system.comp *)
    create_system_compiler_description t.root (OpamCompiler.Version.system ());

    (* Reinstall all system compiler switches *)
    OpamSwitch.Map.iter (fun s a ->
      if a = OpamCompiler.system then (
        OpamGlobals.msg "\n=o=o=o= Upgrading %s =o=o=o=\n" (OpamSwitch.to_string s);
        !switch_reinstall_hook s
      )
    ) t.aliases

  ) else
    OpamGlobals.exit 1

let upgrade_to_1_1_hook =
  ref (fun () -> assert false)

let load_state ?(save_cache=true) call_site =
  log "LOAD-STATE(%s)" call_site;
  let chrono = OpamGlobals.timer () in
  !upgrade_to_1_1_hook ();
  let root = OpamPath.root () in

  let config_p = OpamPath.config root in
  let config =
    let config = OpamFile.Config.read config_p in
    if OpamFile.Config.opam_version config <> OpamVersion.current then (
      (* opam has been updated, so refresh the configuration file and
         clean-up the cache. *)
      let config = OpamFile.Config.with_current_opam_version config in
      OpamFile.Config.write config_p config;
      remove_state_cache ();
      config
    ) else
      config in

  let opams =
    let file = OpamPath.state_cache root in
    if OpamFilename.exists file then
      marshal_from_file file
    else
      None in
  let cached = opams <> None in
  let partial = false in

  let switch = match !OpamGlobals.switch with
    | `Command_line s
    | `Env s   -> OpamSwitch.of_string s
    | `Not_set -> OpamFile.Config.switch config in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let compilers =
    let files = OpamFilename.rec_files (OpamPath.compilers_dir root) in
    let files =
      List.fold_left (fun acc file ->
        if OpamFilename.exists file then file :: acc
        else acc
      ) [] files in
    let comp = OpamMisc.filter_map OpamCompiler.of_filename files in
    OpamCompiler.Set.of_list comp in
  let switch, compiler =
    try switch, OpamSwitch.Map.find switch aliases
    with Not_found ->
      log "%S does not contain the compiler name associated to the switch %s"
        (OpamFilename.to_string (OpamPath.aliases root))
        (OpamSwitch.to_string switch);
      match !OpamGlobals.switch with
      | `Command_line s
      | `Env s   -> OpamSwitch.not_installed (OpamSwitch.of_string s)
      | `Not_set ->
        if OpamSwitch.Map.cardinal aliases > 0 then (
          let new_switch, new_compiler = OpamSwitch.Map.choose aliases in
          OpamGlobals.error "The current switch (%s) is an unknown compiler \
                             switch. Switching back to %s ..."
            (OpamSwitch.to_string switch)
            (OpamSwitch.to_string new_switch);
          let config = OpamFile.Config.with_switch config new_switch in
          OpamFile.Config.write config_p config;
          new_switch, new_compiler;
        ) else
          OpamGlobals.error_and_exit
            "The current switch (%s) is an unknown compiler switch."
            (OpamSwitch.to_string switch) in
  let compiler_version = lazy (
    let comp_f = OpamPath.compiler_comp root compiler in
    (* XXX: useful for upgrade to 1.1 *)
    if compiler = OpamCompiler.system && not (OpamFilename.exists comp_f) then
      create_system_compiler_description root (OpamCompiler.Version.system ());
    if not (OpamFilename.exists comp_f) then
      OpamCompiler.unknown compiler
    else
      OpamFile.Comp.version (OpamFile.Comp.read comp_f)
  ) in
  let repositories = read_repositories root config in
  let package_index =
    OpamFile.Package_index.safe_read (OpamPath.package_index root) in
  let compiler_index =
    OpamFile.Compiler_index.safe_read (OpamPath.compiler_index root) in
  let opams = match opams with
    | None   ->
      let packages =
        OpamPackage.Set.union
          (OpamFile.Installed.safe_read (OpamPath.Switch.installed root switch))
          (OpamPackage.Set.of_list (OpamPackage.Map.keys package_index)) in
      OpamPackage.Set.fold (fun nv map ->
          try
            if OpamPackage.is_pinned nv then map else
              let file =
                let f1 = OpamPath.opam root nv in
                if OpamFilename.exists f1 then f1 else
                  let repo, prefix = OpamPackage.Map.find nv package_index in
                  let repo = OpamRepositoryName.Map.find repo repositories in
                  OpamPath.Repository.opam repo prefix nv in
              let opam = OpamFile.OPAM.read file in
              OpamPackage.Map.add nv opam map
          with
          | Not_found ->
            OpamGlobals.warning "Cannot find an OPAM file for %s, skipping."
              (OpamPackage.to_string nv);
            map
          | Parsing.Parse_error | OpamSystem.Internal_error _ ->
            OpamGlobals.warning "Errors while parsing %s OPAM file, skipping."
              (OpamPackage.to_string nv);
            map
        ) packages OpamPackage.Map.empty
    | Some o -> o in
  let pinned =
    OpamFile.Pinned.safe_read (OpamPath.Switch.pinned root switch) in
  let installed =
    OpamFile.Installed.safe_read (OpamPath.Switch.installed root switch) in
  let installed_roots =
    OpamFile.Installed_roots.safe_read (OpamPath.Switch.installed_roots root switch) in
  let packages =
    OpamPackage.Name.Map.fold
      (fun name _ -> OpamPackage.Set.add (OpamPackage.pinned name))
      pinned
      (OpamPackage.Set.of_list (OpamPackage.Map.keys opams)) in
  let reinstall =
    OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall root switch) in
  let system =
    (compiler = OpamCompiler.system) in
  let available_packages_stub = lazy OpamPackage.Set.empty in
  let t = {
    partial; root; switch; compiler; compiler_version; repositories; opams;
    packages; installed; installed_roots; reinstall;
    config; aliases; pinned; compilers;
    package_index; compiler_index;
    available_packages = available_packages_stub
  } in
  let t = { t with available_packages = lazy (available_packages t system) } in
  print_state t;
  if save_cache && not cached then
    save_state ~update:false t;
  let load_time = chrono () in
  log "State %s loaded in %.3fs" call_site load_time;
  (* Check whether the system compiler has been updated *)
  if system_needs_upgrade t then (
    reinstall_system_compiler t;
    OpamGlobals.exit 0
  ) else
    t

(* install ~/.opam/switches/<switch>/config/global-conf.config *)
let install_global_config root switch =
  log "install_global_config switch=%s" (OpamSwitch.to_string switch);

  (* .config *)
  let vars =
    let map f l = List.rev_map (fun (s,p) -> OpamVariable.of_string s, S (f p)) l in
    let id x = x in

    map OpamFilename.Dir.to_string
      [
        ("root", root);
        ("prefix", OpamPath.Switch.root root switch);
        ("lib", OpamPath.Switch.lib_dir root switch);
        ("bin", OpamPath.Switch.bin root switch);
        ("sbin", OpamPath.Switch.sbin root switch);
        ("doc", OpamPath.Switch.doc_dir root switch);
        ("stublibs", OpamPath.Switch.stublibs root switch);
        ("toplevel", OpamPath.Switch.toplevel root switch);
        ("man", OpamPath.Switch.man_dir root switch);
        ("share", OpamPath.Switch.share_dir root switch);
        ("etc", OpamPath.Switch.etc_dir root switch);
      ]
    @ map id [
      ("user" , try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name with _ -> "user");
      ("group", try (Unix.getgrgid (Unix.getgid ())).Unix.gr_name with _ -> "group");
      ("make" , !OpamGlobals.makecmd ());
      ("os"   , OpamGlobals.os_string ());
    ] in

  let config = OpamFile.Dot_config.create vars in
  OpamFile.Dot_config.write
    (OpamPath.Switch.config root switch OpamPackage.Name.global_config)
    config

let fix_descriptions_hook =
  ref (fun ?save_cache:_ _ ~verbose:_ -> assert false)

(* Upgrade to the new file overlay *)
let upgrade_to_1_1 () =
  let root  = OpamPath.root () in
  let opam  = root / "opam" in
  let opam_tmp = root / "opam_tmp" in
  let descr = root / "descr" in
  let compilers = root / "compilers" in
  let repo_index = root / "repo" // "index" in
  if OpamFilename.exists_dir opam || OpamFilename.exists repo_index then (

    OpamGlobals.header_msg
      "Upgrading to OPAM 1.1 %s"
      (OpamGlobals.colorise `red "[DO NOT INTERRUPT THE PROCESS]");
    OpamGlobals.msg
      "\n\
      \   In case something goes wrong, you can run that upgrade\n\
      \   process again by doing:\n\
       \n\
      \       mkdir %s/opam && opam list\n\
       \n\
       ** Processing **\n"
      (OpamFilename.prettify_dir (OpamPath.root ()));

    if OpamFilename.exists_dir opam then
      OpamFilename.move_dir ~src:opam ~dst:opam_tmp;
    OpamFilename.rmdir descr;
    if OpamFilename.exists_dir (OpamPath.packages_dir root) then
      OpamFilename.rmdir (OpamPath.packages_dir root);

    (* Remove the cache. *)
    if OpamFilename.exists (OpamPath.state_cache root) then
      OpamFilename.remove (OpamPath.state_cache root);

    (* Remove the index files *)
    OpamFilename.remove (OpamPath.root () / "repo" // "index");
    OpamFilename.remove (OpamPath.root () / "repo" // "index.packages");
    OpamFilename.remove (OpamPath.root () / "repo" // "index.compilers");

    (* fix the base config files *)
    let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
    OpamSwitch.Map.iter (fun switch _ ->
        install_global_config root switch
      ) aliases;

    OpamFilename.with_tmp_dir (fun dir ->
        (* Fix system.comp *)
        let system_comp = root / "compilers" // "system.comp" in
        let tmp_file = OpamFilename.create dir (OpamFilename.basename system_comp) in
        if OpamFilename.exists system_comp then (
          log "backing up %s to %s"
            (OpamFilename.to_string system_comp)
            (OpamFilename.to_string tmp_file);
          OpamFilename.move ~src:system_comp ~dst:tmp_file;
        );

        OpamFilename.rmdir compilers;
        let system_comp = OpamPath.compiler_comp root OpamCompiler.system in
        if OpamFilename.exists tmp_file then (
          log "restoring %s" (OpamFilename.to_string tmp_file);
          OpamFilename.mkdir (OpamFilename.dirname system_comp);
          OpamFilename.move ~src:tmp_file ~dst:system_comp;
        ) else
          create_system_compiler_description root (OpamCompiler.Version.system ())
      );
    (* Remove pinned cache *)
    OpamSwitch.Map.iter (fun switch _ ->
        let pinned_cache = OpamPath.Switch.root root switch / "pinned.cache" in
        if OpamFilename.exists_dir pinned_cache then (
          OpamGlobals.msg
            "Removing the cache of pinned packages for the switch %s ...\n"
            (OpamSwitch.to_string switch);
          OpamFilename.rmdir pinned_cache;
        )
      ) aliases;

    (* Fix all the descriptions *)
    let t = load_state ~save_cache:false "update-to-1.1." in
    !fix_descriptions_hook t ~verbose:false;

    (* Fix the pinned packages *)
    OpamSwitch.Map.iter (fun switch _ ->
        let pinned = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned root switch) in
        OpamPackage.Name.Map.iter (fun name _ ->
            let t = { t with switch } in
            if is_pinned t name then (
              let nv = OpamPackage.pinned name in
              OpamFilename.rmdir (OpamPath.Switch.Overlay.package root switch nv);
              add_pinned_overlay t name;
            )
          ) pinned
      ) aliases;

    (* Workaround to add back packages without repositories *)
    List.iter (fun file ->
        let nv =
          file
          |> OpamFilename.chop_extension
          |> OpamFilename.basename
          |> OpamFilename.Base.to_string
          |> OpamPackage.of_string in
        let dst = OpamPath.opam root nv in
        if not (OpamFilename.exists dst) then OpamFilename.copy ~src:file ~dst
      ) (OpamFilename.files opam_tmp);
    OpamFilename.rmdir opam_tmp;

    OpamGlobals.header_msg
      "Upgrade complete. Now continuing with \"%s\""
      (String.concat " " (Array.to_list Sys.argv));
    OpamGlobals.msg "\n";
  )

let () =
  upgrade_to_1_1_hook := upgrade_to_1_1

let rebuild_state_cache () =
  remove_state_cache ();
  let t = load_state ~save_cache:false "rebuild-cache" in
  save_state ~update:true t

let switch_eval_sh = "switch_eval.sh"
let complete_sh    = "complete.sh"
let complete_zsh   = "complete.zsh"
let variables_sh   = "variables.sh"
let variables_csh  = "variables.csh"
let init_sh        = "init.sh"
let init_zsh       = "init.zsh"
let init_csh       = "init.csh"
let init_fish      = "init.fish"
let init_file = function
  | `sh   -> init_sh
  | `csh  -> init_csh
  | `zsh  -> init_zsh
  | `bash -> init_sh
  | `fish -> init_fish

let source t ?(interactive_only=false) f =
  let file f = OpamFilename.to_string (OpamPath.init t.root // f) in
  let s =
    Printf.sprintf ". %s > /dev/null 2> /dev/null || true\n" (file f)
  in
  if interactive_only then
    Printf.sprintf "if tty -s >/dev/null 2>&1; then\n  %sfi\n" s
  else s

let expand_env t (env: env_updates) : env =
  let fenv = env_filter t OpamVariable.Map.empty in
  List.rev_map (fun (ident, symbol, string) ->
    let string = OpamFilter.substitute_string fenv string in
    let read_env () =
      let prefix = OpamFilename.Dir.to_string t.root in
      try OpamMisc.reset_env_value ~prefix (OpamMisc.getenv ident)
      with _ -> [] in
    let cons ~head a b =
      let c = List.filter ((<>)"") b in
      match b with
      | []      -> if head then [ ""; a ] else [ a; "" ]
      | "" :: _ -> "" :: a :: c
      | _       ->
        match List.rev b with
        | "" :: _ -> (a :: c) @ [""]
        | _       -> a :: c in
    match symbol with
    | "="  -> (ident, string)
    | "+=" -> (ident, String.concat ":" (string :: read_env ()))
    | "=+" -> (ident, String.concat ":" (read_env () @ [string]))
    | ":=" -> (ident, String.concat ":" (cons ~head:true string (read_env())))
    | "=:" -> (ident, String.concat ":" (cons ~head:false string (read_env())))
    | _    -> failwith (Printf.sprintf "expand_env: %s is an unknown symbol" symbol)
  ) env

let add_to_env t (env: env) (updates: env_updates) =
  let env =
    List.filter (fun (k,_) -> List.for_all (fun (u,_,_) -> u <> k) updates) env in
  env @ expand_env t updates

let env_updates ~opamswitch t =
  let comp = compiler_comp t t.compiler in

  let add_to_path = OpamPath.Switch.bin t.root t.switch in
  let new_path = "PATH", "+=", OpamFilename.Dir.to_string add_to_path in
  let perl5 = OpamPackage.Name.of_string "perl5" in
  let add_to_perl5lib =  OpamPath.Switch.lib t.root t.switch perl5 in
  let new_perl5lib = "PERL5LIB", "+=", OpamFilename.Dir.to_string add_to_perl5lib in
  let toplevel_dir =
    "OCAML_TOPLEVEL_PATH", "=",
    OpamFilename.Dir.to_string (OpamPath.Switch.toplevel t.root t.switch) in
  let man_path =
    "MANPATH", "=:",
    OpamFilename.Dir.to_string (OpamPath.Switch.man_dir t.root t.switch) in
  let comp_env = OpamFile.Comp.env comp in
  let switch =
    if not opamswitch then []
    else match !OpamGlobals.switch with
      | `Command_line s -> [ "OPAMSWITCH", "=", s ]
      | `Env _
      | `Not_set -> [] in
  let root =
    if !OpamGlobals.root_dir <> OpamGlobals.default_opam_dir then
      [ "OPAMROOT", "=", !OpamGlobals.root_dir ]
    else
      [] in

  new_path :: man_path :: toplevel_dir :: new_perl5lib :: (switch @ root @ comp_env)

(* This function is used by 'opam config env' and 'opam switch' to
   display the environment variables. We have to make sure that
   OPAMSWITCH is always the one being reported in '~/.opam/config'
   otherwise we can have very weird results (as the inability to switch
   between compilers).

   Note: when we do the later command with --switch=SWITCH, this mean
   we really want to get the environment for this switch. *)
let get_opam_env t =
  let t = match !OpamGlobals.switch with
    | `Command_line _
    | `Not_set -> t
    | `Env _   -> { t with switch = OpamFile.Config.switch t.config } in
  add_to_env t [] (env_updates ~opamswitch:true t)

let get_full_env t =
  let env0 = OpamMisc.env () in
  add_to_env t env0 (env_updates ~opamswitch:true t)

let mem_pattern_in_string ~pattern ~string =
  let pattern = Re.compile (Re.str pattern) in
  Re.execp pattern string

let ocamlinit () =
  try
    let file = Filename.concat (OpamMisc.getenv "HOME") ".ocamlinit" in
    Some (OpamFilename.of_string file)
  with _ ->
    None

let ocamlinit_needs_update () =
  match ocamlinit () with
  | None      -> true
  | Some file ->
    if OpamFilename.exists file then (
      let body = OpamFilename.read file in
      let pattern = "OCAML_TOPLEVEL_PATH" in
      not (mem_pattern_in_string ~pattern ~string:body)
    ) else
      true

let update_ocamlinit () =
  if ocamlinit_needs_update () then (
    match ocamlinit () with
    | None      -> ()
    | Some file ->
      let body =
        if not (OpamFilename.exists file) then ""
        else OpamFilename.read file in
      if body = "" then
        OpamGlobals.msg "  Generating ~/.ocamlinit.\n"
      else
        OpamGlobals.msg "  Updating ~/.ocamlinit.\n";
      try
        let header =
          "(* Added by OPAM. *)\n\
           let () =\n\
          \  try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")\n\
          \  with Not_found -> ()\n\
           ;;\n\n" in
        let oc = open_out_bin (OpamFilename.to_string file) in
        output_string oc (header ^ body);
        close_out oc;
      with _ ->
        OpamSystem.internal_error "Cannot write ~/.ocamlinit."
  ) else
    OpamGlobals.msg "  ~/.ocamlinit is already up-to-date.\n"

let string_of_env_update t shell updates =
  let fenv = env_filter t OpamVariable.Map.empty in
  let sh  (k,v) = Printf.sprintf "%s=%s; export %s;\n" k v k in
  let csh (k,v) = Printf.sprintf "setenv %s %S;\n" k v in
  let export = match shell with
    | `zsh
    | `sh  -> sh
    | `csh -> csh in
  let aux (ident, symbol, string) =
    let string = OpamFilter.substitute_string fenv string in
    let key, value = match symbol with
      | "="  -> (ident, string)
      | "+="
      | ":=" -> (ident, Printf.sprintf "%s:$%s" string ident)
      | "=:"
      | "=+" -> (ident, Printf.sprintf "$%s:%s" ident string)
      | _    -> failwith (Printf.sprintf "%s is not a valid env symbol" symbol) in
    export (key, value) in
  String.concat "" (List.rev_map aux updates)

let init_script t ~switch_eval ~complete (variables_sh, switch_eval_sh, complete_sh)=
  let variables =
    Some (source t variables_sh) in
  let switch_eval =
    if switch_eval then
      Some (source t ~interactive_only:true switch_eval_sh)
    else
      None in
  let complete =
    if complete then
      Some (source t ~interactive_only:true complete_sh)
    else
      None in
  let buf = Buffer.create 128 in
  let append name = function
    | None   -> ()
    | Some c ->
      Printf.bprintf buf "# %s\n%s\n" name c in
  append "Load the environment variables" variables;
  append "Load the auto-complete scripts" complete;
  append "Load the opam-switch-eval script" switch_eval;
  Buffer.contents buf

let update_init_scripts t ~global =
  let init_scripts =
    match global with
    | None   -> []
    | Some g ->
      let scripts = [
        init_sh , (variables_sh , switch_eval_sh, complete_sh);
        init_zsh, (variables_sh , switch_eval_sh, complete_zsh);
        init_csh, (variables_csh, switch_eval_sh, complete_sh);
      ] in
      let aux (init, scripts) =
        init,
        init_script t ~switch_eval:g.switch_eval ~complete:g.complete scripts in
      List.map aux scripts in
  let scripts = [
    (complete_sh   , OpamScript.complete);
    (complete_zsh  , OpamScript.complete_zsh);
    (switch_eval_sh, OpamScript.switch_eval);
    (variables_sh  , string_of_env_update t `sh  (env_updates ~opamswitch:false t));
    (variables_csh , string_of_env_update t `csh (env_updates ~opamswitch:false t));
  ] @
                init_scripts
  in
  let overwrite = [
    init_sh;
    init_csh;
    init_zsh;
    variables_sh;
    variables_csh;
  ] in
  let updated = ref false in
  let write (name, body) =
    let file = OpamPath.init t.root // name in
    let needs_update =
      if OpamFilename.exists file
      && List.mem name overwrite then
        let current = OpamFilename.read file in
        body <> current
      else
        not (OpamFilename.exists file) in
    if needs_update then (
      updated := true;
      try OpamFilename.write file body
      with _ -> ()
    ) in
  List.iter write scripts;
  match global with
  | None   -> ()
  | Some o ->
    List.iter
      (fun init_file ->
        let pretty_init_file =
          OpamFilename.prettify (OpamPath.init t.root // init_file) in
        if !updated then
          OpamGlobals.msg
            "  Updating %s\n    auto-completion : [%b]\n    opam-switch-eval: [%b]\n"
            pretty_init_file
            o.complete
            o.switch_eval
        else
          OpamGlobals.msg "  %s is already up-to-date.\n" pretty_init_file)
      [ init_sh; init_zsh; init_csh ]

let status_of_init_file t init_sh =
  let init_sh = OpamPath.init t.root // init_sh in
  if OpamFilename.exists init_sh then (
    let string = OpamFilename.read init_sh in
    let aux pattern = mem_pattern_in_string ~pattern ~string in
    if OpamFilename.exists init_sh then
      let complete_sh = aux complete_sh in
      let complete_zsh = aux complete_zsh in
      let switch_eval_sh = aux switch_eval_sh in
      Some (complete_sh, complete_zsh, switch_eval_sh)
    else
      None
  ) else
    None

let dot_profile_needs_update t dot_profile =
  if OpamFilename.exists dot_profile then (
    let body = OpamFilename.read dot_profile in
    let pattern1 = "opam config" in
    let pattern2 = OpamFilename.to_string (OpamPath.init t.root // "init") in
    let pattern3 = OpamMisc.remove_prefix ~prefix:!OpamGlobals.root_dir pattern2 in
    if mem_pattern_in_string ~pattern:pattern1 ~string:body then
      `no
    else if mem_pattern_in_string ~pattern:pattern2 ~string:body then
      `no
    else if mem_pattern_in_string ~pattern:pattern3 ~string:body then
      `otherroot
    else
      `yes
  ) else
    `yes

let update_dot_profile t dot_profile shell =
  let pretty_dot_profile = OpamFilename.prettify dot_profile in
  match dot_profile_needs_update t dot_profile with
  | `no        -> OpamGlobals.msg "  %s is already up-to-date.\n" pretty_dot_profile
  | `otherroot ->
    OpamGlobals.msg
      "  %s is already configured for another OPAM root.\n"
      pretty_dot_profile
  | `yes       ->
    let init_file = init_file shell in
    let body =
      if OpamFilename.exists dot_profile then
        OpamFilename.read dot_profile
      else
        "" in
    OpamGlobals.msg "  Updating %s.\n" pretty_dot_profile;
    let body =
      Printf.sprintf
        "%s\n\n\
         # OPAM configuration\n\
         %s"
        (OpamMisc.strip body) (source t init_file) in
    OpamFilename.write dot_profile body

let update_setup t user global =
  begin match user with
    | Some { ocamlinit = false; dot_profile = None }
    | None   -> ()
    | Some l ->
      OpamGlobals.msg "User configuration:\n";
      if l.ocamlinit then update_ocamlinit ();
      match l.dot_profile with
      | None   -> ()
      | Some f -> update_dot_profile t f l.shell;
  end;
  begin match global with
    | None   -> ()
    | Some _ ->
      OpamGlobals.msg "Global configuration:\n";
      update_init_scripts t ~global
  end

let display_setup t shell dot_profile =
  let print (k,v) = OpamGlobals.msg "  %-25s - %s\n" k v in
  let not_set = "not set" in
  let ok      = "string is already present so file unchanged" in
  let error   = "error" in
  let user_setup =
    let ocamlinit_status =
      if ocamlinit_needs_update () then not_set else ok in
    let dot_profile_status =
      match dot_profile_needs_update t dot_profile with
      | `no        -> ok
      | `yes       -> not_set
      | `otherroot -> error in
    [ ("~/.ocamlinit"                   , ocamlinit_status);
      (OpamFilename.prettify dot_profile, dot_profile_status); ]
  in
  let init_file = init_file shell in
  let pretty_init_file = OpamFilename.prettify (OpamPath.init t.root // init_file) in
  let global_setup =
    match status_of_init_file t init_file with
    | None -> [pretty_init_file, not_set ]
    | Some(complete_sh, complete_zsh, switch_eval_sh) ->
      let completion =
        if not complete_sh
        && not complete_zsh then
          not_set
        else ok in
      let switch_eval =
        if switch_eval_sh then
          ok
        else
          not_set in
      [ ("init-script"     , Printf.sprintf "%s" pretty_init_file);
        ("auto-completion" , completion);
        ("opam-switch-eval", switch_eval);
      ]
  in
  OpamGlobals.msg "User configuration:\n";
  List.iter print user_setup;
  OpamGlobals.msg "Global configuration:\n";
  List.iter print global_setup

let eval_string () =
  let root =
    if !OpamGlobals.root_dir <> OpamGlobals.default_opam_dir then
      Printf.sprintf " --root=%s" !OpamGlobals.root_dir
    else
      "" in
  Printf.sprintf "eval `opam config env%s`\n" root

let up_to_date_env t =
  let changes =
    List.filter
      (fun (s, v) -> Some v <> try Some (OpamMisc.getenv s) with _ -> None)
      (get_opam_env t) in
  changes = []

let print_env_warning_at_init t user =
  if up_to_date_env t then ()
  else
    let profile_string = match user.dot_profile with
      | None -> ""
      | Some f ->
        Printf.sprintf
          "2. To correctly configure OPAM for subsequent use, add the following\n\
           line to your profile file (for instance %s):\n\
           \n\
          \      %s\n"
          (OpamFilename.prettify f)
          (source t (init_file user.shell))
    in
    let ocamlinit_string =
      if not user.ocamlinit then "" else
        "3. To avoid issues related to non-system installations of `ocamlfind`\n\
        \  add the following lines to ~/.ocamlinit (create it if necessary):\n\
         \n\
        \      let () =\n\
        \        try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")\n\
        \        with Not_found -> ()\n\
        \      ;;\n\n"
    in
    let line =
      "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n"
    in
    OpamGlobals.msg
      "\n%s\n\
       1. To configure OPAM in the current shell session, you need to run:\n\
       \n\
      \      %s\n\
       %s%s%s\n"
      line (eval_string ()) profile_string ocamlinit_string line

let print_env_warning_at_switch t =
  if up_to_date_env t then ()
  else
    OpamGlobals.msg
      "# To complete the configuration of OPAM, you need to run:\n%s"
      (eval_string ())

let update_setup_interactive t shell dot_profile =
  let update dot_profile =
    let user = Some { shell; ocamlinit = true; dot_profile = Some dot_profile } in
    let global = Some { complete = true ; switch_eval = true } in
    OpamGlobals.msg "\n";
    update_setup t user global;
    true in

  OpamGlobals.msg "\n";

  match read
      "In normal operation, OPAM only alters files within ~/.opam.\n\
       \n\
       During this initialisation, you can allow OPAM to add information to two\n\
       other files for best results. You can also make these additions manually\n\
       if you wish.\n\
       \n\
       If you agree, OPAM will modify:\n\n\
      \  - %s (or a file you specify) to set the right environment\n\
      \    variables and to load the auto-completion scripts for your shell (%s)\n\
      \    on startup. Specifically, it checks for and appends the following line:\n\
      \n\
      \    . ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true\n\
       \n\
      \  - ~/.ocamlinit to ensure that non-system installations of `ocamlfind`\n\
      \    (i.e. those installed by OPAM) will work correctly when running the\n\
      \    OCaml toplevel. It does this by adding $OCAML_TOPLEVEL_PATH to the list\n\
      \    of include directories.\n\
      \n\
       If you choose to not configure your system now, you can either configure\n\
       OPAM manually (instructions will be displayed) or launch the automatic setup\n\
       later by running:\n\
      \n\
       \   `opam config setup -a`.\n\
       \n\
      \n\
       Do you want OPAM to modify %s and ~/.ocamlinit?\n\
       (default is 'no', use 'f' to name a file other than %s)\n\
      \    [N/y/f]"
      (OpamFilename.prettify dot_profile)
      (string_of_shell shell)
      (OpamFilename.prettify dot_profile)
      (OpamFilename.prettify dot_profile)
  with
  | Some ("y" | "Y" | "yes"  | "YES" ) -> update dot_profile
  | Some ("f" | "F" | "file" | "FILE") ->
    begin match read "  Enter the name of the file to update:" with
      | None   ->
        OpamGlobals.msg "-- No filename: skipping the auto-configuration step --\n";
        false
      | Some f -> update (OpamFilename.of_string f)
    end
  | _ -> false

(* Add the given packages to the set of package to reinstall. If [all]
   is set, this is done for ALL the switches (useful when a package
   change upstream for instance). If not, only the reinstall state of the
   current switch is changed. *)
let add_to_reinstall t ~all packages =
  log "add-to-reinstall all:%b packages:%s" all (OpamPackage.Set.to_string packages);
  let packages = OpamPackage.Set.fold (fun nv set ->
      try
        let nv =
          if not (OpamPackage.is_pinned nv) then nv
          else OpamPackage.pinned (OpamPackage.name nv) in
        OpamPackage.Set.add nv set
      with Not_found ->
        set
    ) packages OpamPackage.Set.empty in
  let aux switch =
    let installed =
      OpamFile.Installed.safe_read (OpamPath.Switch.installed t.root switch) in
    let reinstall =
      OpamPackage.Set.union
        (OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall t.root switch))
        packages in
    let reinstall =
      OpamPackage.Set.filter (fun nv ->
        OpamPackage.Set.mem nv installed
      ) reinstall in
    let file = OpamPath.Switch.reinstall t.root switch in
    if not (OpamPackage.Set.is_empty reinstall) then
      OpamFile.Reinstall.write file reinstall
    else
      OpamFilename.remove file in
  if all
  then OpamSwitch.Map.iter (fun switch _ -> aux switch) t.aliases
  else aux t.switch

let add_switch root switch compiler =
  log "add_switch switch=%s compiler=%s"
    (OpamSwitch.to_string switch) (OpamCompiler.to_string compiler);
  let aliases_f = OpamPath.aliases root in
  let aliases = OpamFile.Aliases.safe_read aliases_f in
  if not (OpamSwitch.Map.mem switch aliases) then begin
    OpamFile.Aliases.write aliases_f (OpamSwitch.Map.add switch compiler aliases);
  end

(* - compiles and install $opam/compiler/[ocaml_version].comp in $opam/[switch]
   - update $opam/switch
   - update $opam/config *)
let install_compiler t ~quiet switch compiler =
  log "install_compiler switch=%s compiler=%s"
    (OpamSwitch.to_string switch)
    (OpamCompiler.to_string compiler);

  let comp_f = OpamPath.compiler_comp t.root compiler in
  if not (OpamFilename.exists comp_f) then (
    OpamGlobals.msg "Cannot find %s: %s is not a valid compiler name.\n"
      (OpamFilename.to_string comp_f)
      (OpamCompiler.to_string compiler);
    OpamGlobals.exit 1;
  );

  let switch_dir = OpamPath.Switch.root t.root switch in

  (* Do some clean-up if necessary *)
  if not (is_switch_installed t switch)
  && OpamFilename.exists_dir switch_dir then
    OpamFilename.rmdir switch_dir;

  if OpamFilename.exists_dir switch_dir then (
    OpamGlobals.msg "The compiler %s is already installed.\n"
      (OpamSwitch.to_string switch);
    OpamGlobals.exit 0;
  );

  (* Create base directories *)
  OpamFilename.mkdir switch_dir;
  OpamFilename.mkdir (OpamPath.Switch.lib_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.stublibs t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.toplevel t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.build_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.bin t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.sbin t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.doc_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.man_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.install_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.config_dir t.root switch);
  List.iter (fun num ->
    OpamFilename.mkdir (OpamPath.Switch.man_dir ~num t.root switch)
  ) ["1";"1M";"2";"3";"4";"5";"6";"7";"9"];

  install_global_config t.root switch;

  let comp = OpamFile.Comp.read comp_f in
  begin try
      if not (OpamFile.Comp.preinstalled comp) then begin

        OpamGlobals.verbose := not quiet;

        (* Install the compiler *)
        let comp_src = match OpamFile.Comp.src comp with
          | Some f -> f
          | None   ->
            OpamGlobals.error_and_exit
              "No source for compiler %s"
              (OpamCompiler.to_string compiler) in
        let build_dir = OpamPath.Switch.build_ocaml t.root switch in
        let kind = guess_repository_kind (OpamFile.Comp.kind comp) comp_src in
        if kind = `local
        && Sys.file_exists (fst comp_src)
        && Sys.is_directory (fst comp_src) then
          OpamFilename.link_dir
            ~src:(OpamFilename.Dir.of_string (fst comp_src)) ~dst:build_dir
        else OpamFilename.with_tmp_dir (fun download_dir ->
            let result =
              OpamRepository.pull_url kind (OpamPackage.of_string "compiler.get")
                download_dir None comp_src in
            match result with
            | Not_available u -> OpamGlobals.error_and_exit "%s is not available." u
            | Up_to_date r
            | Result r        -> OpamFilename.extract_generic_file r build_dir
          );
        let patches = OpamFile.Comp.patches comp in
        let patches = List.map (fun f ->
            OpamFilename.download ~overwrite:true f build_dir
          ) patches in
        List.iter (fun f -> OpamFilename.patch f build_dir) patches;
        if OpamFile.Comp.configure comp @ OpamFile.Comp.make comp <> [] then begin
          OpamFilename.exec build_dir
            [ ( "./configure" :: OpamFile.Comp.configure comp )
              @ [ "-prefix";  OpamFilename.Dir.to_string switch_dir ]
            (*-bindir %s/bin -libdir %s/lib -mandir %s/man*)
            (* NOTE In case it exists 2 '-prefix', in general the script
               ./configure will only consider the last one, others will be
               discarded. *)
            ; ( !OpamGlobals.makecmd () :: OpamFile.Comp.make comp )
            ; [ !OpamGlobals.makecmd () ; "install" ]
            ]
        end else begin
          let t = { t with switch } in
          let env = env_filter t OpamVariable.Map.empty in
          let builds = OpamFilter.commands env (OpamFile.Comp.build comp) in
          OpamFilename.exec build_dir builds
        end;
      end;

      (* Update ~/.opam/aliases *)
      add_switch t.root switch compiler

    with e ->
      if not !OpamGlobals.debug then
        OpamFilename.rmdir switch_dir;
      raise e
  end

(* write the new version in the configuration file *)
let update_switch_config t switch =
  let config = OpamFile.Config.with_switch t.config switch in
  OpamFile.Config.write (OpamPath.config t.root) config;
  update_init_scripts { t with switch }  ~global:None

(* Dev packages *)

let update_dev_package t nv =
  log "update-dev-package %s" (OpamPackage.to_string nv);
  let nv = real_package t nv in
  let needs_update = OpamPackage.Set.singleton nv in
  let skip = OpamPackage.Set.empty in
  match url t nv with
  | None     -> skip
  | Some url ->
    let remote_url = OpamFile.URL.url url in
    match guess_repository_kind (OpamFile.URL.kind url) remote_url with
    | ` http -> skip
    | kind   ->
      log "updating %s:%s"
        (string_of_address remote_url) (string_of_repository_kind kind);
        let dirname = dev_package t nv in
        let checksum = OpamFile.URL.checksum url in
        let r = OpamRepository.pull_url kind nv dirname checksum remote_url in
        match r with
        | Not_available u -> OpamGlobals.error "%s is not available anymore!" u; skip
        | Up_to_date _    -> skip
        | Result _        -> needs_update

let update_dev_packages t =
  log "update-dev-packages";
  let updates packages =
    let packages = OpamPackage.Set.filter (is_dev_package t) packages in
    let packages = OpamPackage.Set.inter packages t.installed in
    let packages = OpamPackage.Set.elements packages in
    OpamPackage.Parallel.map_reduce_l (2 * jobs t) packages
      ~map:(update_dev_package t)
      ~merge:OpamPackage.Set.union
      ~init:OpamPackage.Set.empty in

  let switch_dev_packages = keys (switch_dev_packages t) in
  let global_dev_packages =
    let all = keys (global_dev_packages t) in
    OpamPackage.Set.diff all switch_dev_packages in

  let global_updates = updates global_dev_packages in
  add_to_reinstall t ~all:true global_updates;

  let switch_updates = updates switch_dev_packages in
  add_to_reinstall t ~all:false switch_updates;

  OpamPackage.Set.union global_updates switch_updates

(* Try to download $name.$version+opam.tar.gz *)
let download_archive t nv =
  log "get_archive %s" (OpamPackage.to_string nv);
  let dst = OpamPath.archive t.root nv in
  if OpamFilename.exists dst then Some dst
  else
    let repo, _ = OpamPackage.Map.find nv t.package_index in
    let repo = find_repository t repo in
    match OpamRepository.pull_archive repo nv with
    | Not_available _ -> None
    | Up_to_date f
    | Result f        -> OpamFilename.copy ~src:f ~dst; Some dst

(* Download a package from its upstream source, using 'cache_dir' as cache
   directory. *)
let download_upstream t nv dirname =
  let nv = real_package t nv in
  match url t nv with
  | None   -> None
  | Some u ->
    let url = OpamFile.URL.url u in
    let kind = guess_repository_kind (OpamFile.URL.kind u) url in
    let checksum = OpamFile.URL.checksum u in
    match OpamRepository.pull_url kind nv dirname checksum url with
    | Not_available u -> OpamGlobals.error_and_exit "%s is not available" u
    | Result f
    | Up_to_date f    -> Some f

let check f =
  let root = OpamPath.root () in
  let with_switch_lock a f =
    OpamFilename.with_flock (OpamPath.Switch.lock root a) f in
  let error () =
    OpamGlobals.error_and_exit
      "Please run 'opam init' first to initialize the state of OPAM."
      (OpamFilename.Dir.to_string root) in

  if not (OpamFilename.exists_dir root)
  || not (OpamFilename.exists (OpamPath.config root)) then
    error ()

  else match f with

    | Global_lock f ->
      (* Take the global lock *)
      OpamFilename.with_flock (OpamPath.lock root) (fun () ->
          (* clean the log directory *)
          OpamFilename.cleandir (OpamPath.log root);
          (* Take all the switch locks *)
          let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
          let f =
            OpamSwitch.Map.fold (fun a _ f ->
                if OpamFilename.exists_dir (OpamPath.Switch.root root a)
                then with_switch_lock a (fun () -> f ())
                else f
              ) aliases f in
          let t = load_state "global-lock" in
          global_consistency_checks t;
          f ()
        ) ()

    | Read_lock f ->
      (* Simply check that OPAM is correctly initialized *)
      if OpamFilename.exists_dir root then
        f ()
      else
        error ()

    | Switch_lock f ->
      (* Take a switch lock (and check that the global lock is free). *)
      let switch =
        OpamFilename.with_flock
          (OpamPath.lock root)
          (fun () -> match !OpamGlobals.switch with
            | `Command_line s
            | `Env s   -> OpamSwitch.of_string s
            | `Not_set ->
              OpamFile.Config.switch (OpamFile.Config.read (OpamPath.config root)))
          () in
      (* XXX: We can have a small race just here ... *)
      let t = load_state "switch-lock" in
      switch_consistency_checks t;
      with_switch_lock switch f ()

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
  OpamLocal.register ()

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

type state = {
  partial: bool;
  root: OpamPath.t;
  switch: switch;
  compiler: compiler;
  compiler_version: compiler_version;
  opams: OpamFile.OPAM.t package_map;
  descrs: OpamFile.Descr.t lazy_t package_map;
  repositories: OpamFile.Repo_config.t repository_name_map;
  prefixes: OpamFile.Prefix.t repository_name_map;
  packages: package_set;
  available_packages: package_set Lazy.t;
  aliases: OpamFile.Aliases.t;
  compilers: compiler_set;
  pinned: OpamFile.Pinned.t;
  installed: OpamFile.Installed.t;
  installed_roots: OpamFile.Installed_roots.t;
  reinstall: OpamFile.Reinstall.t;
  config: OpamFile.Config.t;
  package_index: repository_name package_map;
  compiler_index: repository_name compiler_map;
}

let universe t action = {
  u_packages  = t.packages;
  u_action    = action;
  u_installed = t.installed;
  u_available = Lazy.force t.available_packages;
  u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends t.opams;
  u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts t.opams;
  u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts t.opams;
  u_installed_roots = t.installed_roots;
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
  log "ROOT      : %s" (OpamFilename.Dir.to_string (OpamPath.root t.root));
  log "SWITCH    : %s" (OpamSwitch.to_string t.switch);
  log "COMPILER  : %s" (OpamCompiler.to_string t.compiler);
  log "COMPILERS : %s" (OpamCompiler.Set.to_string t.compilers);
  log "REPOS     : %s" (string_of_repositories t.repositories);
  log "PACKAGES  : %s" packages;
  log "INSTALLED : %s" (OpamPackage.Set.to_string t.installed);
  log "ROOTS     : %s" (OpamPackage.Set.to_string t.installed_roots);
  log "REINSTALL : %s" (OpamPackage.Set.to_string t.reinstall)

let opam t nv =
  try OpamPackage.Map.find nv t.opams
  with Not_found ->
    OpamPackage.unknown (OpamPackage.name nv) (Some (OpamPackage.version nv))

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


(* Sort repositories by priority *)
let sorted_repositories t =
  let repositories = OpamRepositoryName.Map.values t.repositories in
  List.sort OpamRepository.compare repositories

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

let find_repository_opt t repo_name =
  try Some (OpamRepositoryName.Map.find repo_name t.repositories)
  with Not_found -> None

let package_repository_state t nv =
  try
    let repo_name = OpamPackage.Map.find nv t.package_index in
    let repo = find_repository t repo_name in
    let prefix = OpamRepositoryName.Map.find repo_name t.prefixes in
    let prefix = OpamRepository.find_prefix prefix nv in
    let state = OpamRepository.package_state repo prefix nv in
    Some state
  with Not_found ->
    None

let package_index repositories repo_index =
  log "package-index";
  let package_maps = ref [] in
  let get_packages repo_name =
    if List.mem_assoc repo_name !package_maps then
      List.assoc repo_name !package_maps
    else (
      let repo = find_repository_aux repo_name repositories in
      let _, packages = OpamRepository.packages repo in
      package_maps := (repo_name, packages) :: !package_maps;
      packages
    ) in
  OpamPackage.Name.Map.fold (fun n repos map ->
    List.fold_left (fun map repo_name ->
      let packages = get_packages repo_name in
      let packages =
        OpamPackage.Set.filter (fun nv ->
          OpamPackage.name nv = n
        ) packages in
      OpamPackage.Set.fold (fun nv map ->
        if OpamPackage.Map.mem nv map then map
        else OpamPackage.Map.add nv repo_name map
      ) packages map
    ) map repos
  ) repo_index OpamPackage.Map.empty

let package_state_index t =
  log "package-state-index";
  OpamPackage.Map.fold (fun nv _ map ->
    match package_repository_state t nv with
    | None   -> map
    | Some s -> OpamPackage.Map.add nv s map
  ) t.package_index OpamPackage.Map.empty

let compiler_repository_state t compiler =
  try
    let repo_name = OpamCompiler.Map.find compiler t.compiler_index in
    let repo = find_repository t repo_name in
    let state = OpamRepository.compiler_state repo compiler in
    Some state
  with Not_found ->
    None

let compiler_index repositories =
  log "compiler-index";
  let repositories = List.sort OpamRepository.compare repositories in
  List.fold_left (fun map repo ->
    let comps = OpamRepository.compilers repo in
    OpamCompiler.Map.fold (fun comp _ map ->
      if OpamCompiler.Map.mem comp map then map
      else OpamCompiler.Map.add comp repo.repo_name map
    ) comps map
  ) OpamCompiler.Map.empty repositories

let compiler_state_index t =
  log "compiler-state-index";
  OpamCompiler.Map.fold (fun comp _ map ->
    match compiler_repository_state t comp with
    | None   -> map
    | Some s -> OpamCompiler.Map.add comp s map
  ) t.compiler_index OpamCompiler.Map.empty

let is_pinned_aux pinned n =
  OpamPackage.Name.Map.mem n pinned

let pinned_package_aux installed pinned packages n =
  match OpamPackage.Name.Map.find n pinned with
  | Version v -> OpamPackage.create n v
  | _         ->
    if mem_installed_package_by_name_aux installed n then
      find_installed_package_by_name_aux installed n
    else (
      (* We arbitrary select only the latest version; the solver will
         see this package only, which means that it will use the
         correspondng build instructions, but the location will be the
         one pointed out by the pinned path. *)
      let versions = OpamPackage.versions_of_name packages n in
      OpamPackage.create n (OpamPackage.Version.Set.max_elt versions)
    )

let pinned_package t n =
  pinned_package_aux t.installed t.pinned t.packages n

let is_pinned t n = is_pinned_aux t.pinned n

let pinned_path t name =
  if OpamPackage.Name.Map.mem name t.pinned then
    match OpamPackage.Name.Map.find name t.pinned with
    | Local d
    | Darcs d
    | Git d -> Some d
    | _     -> None
  else
    None

(* is the current package locally pinned *)
let is_locally_pinned t name =
  if OpamPackage.Name.Map.mem name t.pinned then
    match OpamPackage.Name.Map.find name t.pinned with
    | Local _ | Darcs _ | Git _ -> true
    | _ -> false
  else
    false

let jobs t =
  match !OpamGlobals.jobs with
  | None   -> OpamFile.Config.jobs t.config
  | Some j -> j

(* List the packages which does fullfil the compiler constraints *)
let available_packages
    system opams installed package_index compiler_version pinned packages =
  let filter nv =
    if OpamPackage.Map.mem nv opams then (
      let opam = OpamPackage.Map.find nv opams in
      let available () =
        OpamPackage.Set.mem nv installed
        || OpamPackage.Map.mem nv package_index in
      let consistent_ocaml_version () =
        let atom (r,v) =
          match OpamCompiler.Version.to_string v with
          | "system" ->
            begin match r with
              | `Eq  -> system
              | `Neq -> not system
              | _    -> OpamSystem.internal_error
                          "%s is not a valid constraint for the system compiler \
                           (only '=' and '!' are valid)."
                          (OpamFormula.string_of_relop r)
            end
          | _ ->
            OpamCompiler.Version.compare compiler_version r v in
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
      let consistent_pinned_version () =
        let name = OpamPackage.name nv in
        not (is_pinned_aux pinned name)
        || pinned_package_aux installed pinned packages name = nv
      in
      available ()
      && consistent_ocaml_version ()
      && consistent_pinned_version ()
      && consistent_os ()
    ) else
      false in
  OpamPackage.Set.filter filter packages

let base_packages =
  List.map OpamPackage.Name.of_string [ "base-unix"; "base-bigarray"; "base-threads" ]

let create_system_compiler_description root = function
  | None         -> ()
  | Some version ->
    log "create-system-compiler-description %s"
      (OpamCompiler.Version.to_string version);
    let comp = OpamPath.compiler root OpamCompiler.system in
    OpamFilename.remove comp;
    let f =
      OpamFile.Comp.create_preinstalled
        OpamCompiler.system version
        (if not !OpamGlobals.no_base_packages then base_packages else [])
        [ ("CAML_LD_LIBRARY_PATH", "=",
          "%{lib}%/stublibs"
          ^ ":" ^
          (match Lazy.force OpamSystem.system_ocamlc_where with
           | Some d -> Filename.concat d "stublibs"
           | None   -> assert false))
        ] in
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
        (OpamCompiler.Version.to_string t.compiler_version)
    );
    false
  | Some v -> t.compiler_version <> v

let read_repositories root config =
  let names = OpamFile.Config.repositories config in
  List.fold_left (fun map repo_name ->
    let repo = OpamFile.Repo_config.read
        (OpamPath.Repository.raw_config root repo_name) in
    OpamRepositoryName.Map.add repo_name repo map
  ) OpamRepositoryName.Map.empty names

let read_prefixes repositories: string name_map repository_name_map =
  OpamRepositoryName.Map.map (fun repo ->
    OpamRepository.read_prefix repo
  ) repositories

(* Only used during init: load only repository-related information *)
let load_repository_state call_site =
  log "LOAD-REPO-STATE(%s)" call_site;
  let root = OpamPath.default () in
  let config_p = OpamPath.config root in
  let config = OpamFile.Config.read config_p in
  let repositories = read_repositories root config in
  let prefixes = read_prefixes repositories in
  let switch = match !OpamGlobals.switch with
    | `Command_line s
    | `Env s   -> OpamSwitch.of_string s
    | `Not_set -> OpamFile.Config.switch config in

  let partial = true in

  (* evertything else is empty *)
  let aliases = OpamSwitch.Map.empty in
  let compilers = OpamCompiler.Set.empty in
  let compiler = OpamCompiler.of_string "none" in
  let compiler_version = OpamCompiler.Version.of_string "none" in
  let opams = OpamPackage.Map.empty in
  let descrs = OpamPackage.Map.empty in
  let packages = OpamPackage.Set.empty in
  let available_packages = lazy OpamPackage.Set.empty in
  let installed = OpamPackage.Set.empty in
  let installed_roots = OpamPackage.Set.empty in
  let reinstall = OpamPackage.Set.empty in
  let package_index = OpamPackage.Map.empty in
  let compiler_index = OpamCompiler.Map.empty in
  let pinned = OpamPackage.Name.Map.empty in
  {
    partial; root; switch; compiler; compiler_version; repositories; opams; descrs;
    packages; available_packages; installed; installed_roots; reinstall;
    config; aliases; pinned; prefixes; compilers; package_index; compiler_index;
  }

(* load partial state to be able to read env variables *)
let load_env_state call_site =
  log "LOAD-ENV-STATE(%s)" call_site;
  let root = OpamPath.default () in
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
  let prefixes = OpamRepositoryName.Map.empty in
  let compiler_version = OpamCompiler.Version.of_string "none" in
  let opams = OpamPackage.Map.empty in
  let descrs = OpamPackage.Map.empty in
  let packages = OpamPackage.Set.empty in
  let available_packages = lazy OpamPackage.Set.empty in
  let installed = OpamPackage.Set.empty in
  let installed_roots = OpamPackage.Set.empty in
  let reinstall = OpamPackage.Set.empty in
  let package_index = OpamPackage.Map.empty in
  let compiler_index = OpamCompiler.Map.empty in
  let pinned = OpamPackage.Name.Map.empty in
  {
    partial; root; switch; compiler; compiler_version; repositories; opams; descrs;
    packages; available_packages; installed; installed_roots; reinstall;
    config; aliases; pinned; prefixes; compilers; package_index; compiler_index;
  }

let get_compiler_packages t comp =
  let comp = compiler t comp in
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
        (OpamPackage.Name.to_string |> OpamGlobals.error "Package %s not found")
        pkg_not;
      OpamGlobals.exit 1
    );

    pkg_available
  )

let compiler_installed t comp =
  OpamSwitch.Map.exists (fun _ c -> c = comp) t.aliases

let switch_installed t switch =
  OpamSwitch.Map.mem switch t.aliases

let check_base_packages t =
  let base_packages = get_compiler_packages t t.compiler in
  let missing_packages =
    List.filter
      (fun (name,_) -> not (mem_installed_package_by_name t name))
      base_packages in
  if missing_packages <> [] then (
    let names = List.map (fst |> OpamPackage.Name.to_string) missing_packages in
    OpamGlobals.warning "Some of the compiler base packages are not installed. \
                         You should run:\n\n    $ opam install %s\n"
      (String.concat " " names)
  )

let all_installed t =
  OpamSwitch.Map.fold (fun switch _ accu ->
    let installed_f = OpamPath.Switch.installed t.root switch in
    let installed = OpamFile.Installed.safe_read installed_f in
    OpamPackage.Set.union installed accu
  ) t.aliases OpamPackage.Set.empty

let installed_versions t name =
  OpamSwitch.Map.fold (fun switch _ map ->
    let installed =
      OpamFile.Installed.safe_read (OpamPath.Switch.installed t.root switch) in
    let pinned =
      OpamFile.Pinned.safe_read (OpamPath.Switch.pinned t.root switch) in
    if mem_installed_package_by_name_aux installed name then
      let nv = find_installed_package_by_name_aux installed name in
      let nv =
        if is_locally_pinned { t with pinned } name then
          OpamPackage.create name (OpamPackage.Version.of_string "(pinned)")
        else if is_pinned { t with pinned } name then
          let version = OpamPackage.Version.to_string (OpamPackage.version nv) in
          let version = OpamPackage.Version.of_string (version ^ "(pinned)") in
          OpamPackage.create name version
        else
          nv
      in
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
let clean dir name =
  if OpamFilename.exists_dir dir then (
    OpamGlobals.error "%s exists although %s is not installed. Removing it."
      (OpamFilename.Dir.to_string dir) name;
    OpamFilename.rmdir dir
  )

let global_consistency_checks t =
  let clean_repo repo nv =
    let tmp_dir = OpamPath.Repository.tmp_dir repo nv in
    clean tmp_dir (OpamPackage.to_string nv) in
  let all_installed = all_installed t in
  OpamRepositoryName.Map.iter (fun _ repo ->
    let tmp_dir = OpamPath.Repository.tmp repo in
    let available =
      let dirs = OpamFilename.sub_dirs tmp_dir in
      let pkgs = OpamMisc.filter_map OpamPackage.of_dirname dirs in
      OpamPackage.Set.of_list pkgs in
    let not_installed = OpamPackage.Set.diff available all_installed in
    OpamPackage.Set.iter (clean_repo repo) not_installed
  ) t.repositories

let switch_consistency_checks t =
  let pin_cache = OpamPath.Switch.pinned_cache t.root t.switch in
  let clean_pin name =
    let name = OpamPackage.Name.to_string name in
    let pin_dir = pin_cache / name in
    clean pin_dir name in
  let available =
    let dirs = OpamFilename.rec_dirs pin_cache in
    let pkgs = List.rev_map (
        OpamFilename.basename_dir
        |> OpamFilename.Base.to_string
        |> OpamPackage.Name.of_string
      ) dirs in
    OpamPackage.Name.Set.of_list pkgs in
  let installed = OpamPackage.names_of_packages t.installed in
  let not_installed = OpamPackage.Name.Set.diff available installed in
  OpamPackage.Name.Set.iter clean_pin not_installed

let loads = ref []
let saves = ref []

let print_stats () =
  List.iter (Printf.printf "load-state: %.2fs\n") !loads;
  List.iter (Printf.printf "save-state: %.2fs\n") !saves

type cache = {
  cached_opams: (package * OpamFile.OPAM.t) list;
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
  close_in ic;
  if not (expected_size = current_size) then (
    OpamGlobals.error "The local-state cache is corrupted, removing it.";
    OpamSystem.internal_error "Corrupted cache";
  )

let marshal_from_file file =
  try
    check_marshaled_file file;
    let ic = open_in_bin (OpamFilename.to_string file) in
    let () =
      let magic_len = String.length OpamVersion.magic in
      really_input ic (String.create magic_len) 0 magic_len in
    let (cache: cache) = Marshal.from_channel ic in
    close_in ic;
    let opams = OpamPackage.Map.of_list cache.cached_opams in
    Some opams
  with _ ->
    OpamFilename.remove file;
    None

let save_state ~update t =
  let t0 = Unix.gettimeofday () in
  let file = OpamPath.state_cache t.root in
  OpamFilename.remove file;
  if update then (
    OpamGlobals.msg
      "Updating the cache of metadata (%s).\n"
      (OpamFilename.prettify file);
  ) else
    OpamGlobals.msg
      "Creating a cache of metadata in %s.\n"
      (OpamFilename.prettify file);
  let oc = open_out_bin (OpamFilename.to_string file) in
  output_string oc OpamVersion.magic;
  let cached_opams = OpamPackage.Map.bindings t.opams in
  Marshal.to_channel oc { cached_opams } [];
  close_out oc;
  let t1 = Unix.gettimeofday () in
  saves := (t1 -. t0) :: !saves

let remove_state_cache () =
  let root = OpamPath.default () in
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

let load_state ?(save_cache=true) call_site =
  log "LOAD-STATE(%s)" call_site;
  let t0 = Unix.gettimeofday () in
  let root = OpamPath.default () in

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
  let compiler_version =
    let comp_f = OpamPath.compiler root compiler in
    if not (OpamFilename.exists comp_f) then
      OpamCompiler.unknown compiler;
    OpamFile.Comp.version (OpamFile.Comp.read comp_f) in
  let package_files fn =
    OpamPackage.Set.fold (fun nv map ->
      try
        let file = fn root nv in
        OpamPackage.Map.add nv file map
      with _ ->
        map
    ) (OpamPackage.list (OpamPath.opam_dir root)) OpamPackage.Map.empty in
  let opams = match opams with
    | None   ->
      package_files (fun root nv -> OpamFile.OPAM.read (OpamPath.opam root nv))
    | Some o -> o in
  let descrs =
    package_files (fun root nv ->
        lazy (OpamFile.Descr.safe_read (OpamPath.descr root nv))
      ) in
  let repositories = read_repositories root config in
  let prefixes = read_prefixes repositories in
  let package_index =
    let f = OpamPath.package_index root in
    let refresh () =
      (* XXX: upgrade from 1.0.0 to 1.0.1 *)
      let repo_index = OpamFile.Repo_index.safe_read (OpamPath.repo_index root) in
      let index = package_index repositories repo_index in
      OpamFile.Package_index.write f (Some index);
      index in
    if OpamFilename.exists f then
      match OpamFile.Package_index.read f with
      | None       -> refresh ()
      | Some index -> index
    else
      refresh () in
  let compiler_index =
    let f = OpamPath.compiler_index root in
    if OpamFilename.exists f then OpamFile.Compiler_index.read f
    else
      (* XXX: upgrade from 1.0.0 to 1.0.1 *)
      let index = compiler_index (OpamRepositoryName.Map.values repositories) in
      OpamFile.Compiler_index.write f index;
      index in
  let pinned =
    OpamFile.Pinned.safe_read (OpamPath.Switch.pinned root switch) in
  let installed =
    OpamFile.Installed.safe_read (OpamPath.Switch.installed root switch) in
  let installed_roots =
    OpamFile.Installed_roots.safe_read (OpamPath.Switch.installed_roots root switch) in
  let reinstall =
    OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall root switch) in
  let packages =
    OpamPackage.Set.of_list (OpamPackage.Map.keys opams) in
  let system =
    (compiler = OpamCompiler.system) in
  let available_packages =
    lazy (
      available_packages
        system opams installed package_index compiler_version pinned packages
    ) in
  let t = {
    partial; root; switch; compiler; compiler_version; repositories; opams; descrs;
    packages; available_packages; installed; installed_roots; reinstall;
    config; aliases; pinned; prefixes; compilers; package_index; compiler_index;
  } in
  print_state t;
  if save_cache && not cached then
    save_state ~update:false t;
  let t1 = Unix.gettimeofday () in
  loads :=  (t1 -. t0) :: !loads;
  (* Check whether the system compiler has been updated *)
  if system_needs_upgrade t then (
    reinstall_system_compiler t;
    OpamGlobals.exit 0
  ) else
    t

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
let init_file = function
  | `sh   -> init_sh
  | `csh  -> init_csh
  | `zsh  -> init_zsh
  | `bash -> init_sh

let source t ?(interactive_only=false) f =
  let file f = OpamFilename.to_string (OpamPath.init t.root // f) in
  let s =
    Printf.sprintf ". %s > /dev/null 2> /dev/null || true\n" (file f)
  in
  if interactive_only then
    Printf.sprintf "if tty -s >/dev/null 2>&1; then\n  %sfi\n" s
  else s

exception Variable_not_defined

(* Return the contents of a fully qualified variable *)
let contents_of_variable t v =
  let name = OpamVariable.Full.package v in
  let var = OpamVariable.Full.variable v in
  let var_str = OpamVariable.to_string var in
  let read_var name =
    let c = dot_config t name in
    try match OpamVariable.Full.section v with
      | None   -> OpamFile.Dot_config.variable c var
      | Some s -> OpamFile.Dot_config.Section.variable c s var
    with Not_found ->
      OpamGlobals.error "%s is not defined" (OpamVariable.Full.to_string v);
      raise Variable_not_defined in
  if name = OpamPackage.Name.default then (
    try S (OpamMisc.getenv var_str)
    with Not_found ->
      if var_str = "ocaml-version" then
        S (OpamCompiler.Version.to_string t.compiler_version)
      else if var_str = "preinstalled" then
        B (OpamFile.Comp.preinstalled (compiler t t.compiler))
      else if var_str = "switch" then
        S (OpamSwitch.to_string t.switch)
      else if var_str = "jobs" then
        S (string_of_int (jobs t))
      else
        read_var name
  ) else (
    let process_one name =
      let exists = find_packages_by_name t name <> None in
      let name_str = OpamPackage.Name.to_string name in
      if not exists then None
      else
        try
          let var_hook = Printf.sprintf "OPAM_%s_%s" name_str var_str in
          match OpamMisc.getenv var_hook with
          | "true"  -> Some (B true)
          | "false" -> Some (B false)
          | s       -> Some (S s)
        with Not_found ->
          let installed = mem_installed_package_by_name t name in
          let no_section = OpamVariable.Full.section v = None in
          if var = OpamVariable.enable && installed && no_section then
            Some (S "enable")
          else if var = OpamVariable.enable && not installed && no_section then
            Some (S "disable")
          else if var = OpamVariable.installed && no_section then
            Some (B installed)
          else if var = OpamVariable.installed || var = OpamVariable.enable then (
            OpamGlobals.error
              "Syntax error: invalid section argument in '%s'.\nUse '%s:%s' instead."
              (OpamVariable.Full.to_string v)
              name_str
              (OpamVariable.to_string var);
            raise Variable_not_defined
          ) else if not installed then
            None
          else
            Some (read_var name) in
    match process_one name with
    | Some r -> r
    | None   ->
      let name_str = OpamPackage.Name.to_string name in
      let names = OpamMisc.split name_str '+' in
      begin match names with
        | [name] -> OpamPackage.unknown (OpamPackage.Name.of_string name) None
        | _      -> ()
      end;
      let names = List.rev_map OpamPackage.Name.of_string names in
      let results =
        List.rev_map (fun name ->
          match process_one name with
          | None   ->
            OpamGlobals.error
              "Package %s is not installed"
              (OpamPackage.Name.to_string name);
            raise Variable_not_defined
          | Some r -> r
        ) names in
      let rec compose x y = match x,y with
        | S "enable" , S "enable"  -> S "enable"
        | S "disable", S "enable"
        | S "enable" , S "disable"
        | S "disable", S "disable" -> S "disable"
        | B b1       , B b2        -> B (b1 && b2)
        | S b, r     | r, S b      ->
          if b = "true" then compose (B true) r
          else if b = "false" then compose (B false) r
          else
            OpamGlobals.error_and_exit
              "Cannot compose %s and %s"
              (OpamVariable.string_of_variable_contents x)
              (OpamVariable.string_of_variable_contents y) in
      match results with
      | [] | [_] -> assert false
      | h::t     -> List.fold_left compose h t
  )

let substitute_ident t i =
  let v = OpamVariable.Full.of_string i in
  contents_of_variable t v

(* Substitute the file contents *)
let substitute_file t f =
  let f = OpamFilename.of_basename f in
  let src = OpamFilename.add_extension f "in" in
  let contents = OpamFile.Subst.read src in
  let newcontents = OpamFile.Subst.replace contents (contents_of_variable t) in
  OpamFile.Subst.write f newcontents

(* Substitue the string contents *)
let substitute_string t s =
  OpamFile.Subst.replace_string s (contents_of_variable t)

exception Filter_type_error

let filter_type_error f actual expected =
  OpamGlobals.error
    "\'%s\' has type %s, but a filter element of type %s was expected."
    (string_of_filter f) actual expected;
  raise Filter_type_error

let string_of_variable_contents ident = function
  | S s -> s
  | B _ -> filter_type_error (FIdent ident) "bool" "string"

let bool_of_variable_contents ident = function
  | B b -> b
  | S _ -> filter_type_error (FIdent ident) "string" "bool"

let eval_string t = function
  | FString s -> substitute_string t s
  | FIdent s  -> string_of_variable_contents s (substitute_ident t s)
  | f         -> filter_type_error f "bool" "string"

let rec eval_bool t = function
  | FBool b    -> b
  | FString s  -> substitute_string t s = "true"
  | FIdent s   -> bool_of_variable_contents s (substitute_ident t s)
  | FOp(e,s,f) ->
    (* We are supposed to compare version strings *)
    let s = match s with
      | Eq  -> (fun a b -> Debian.Version.compare a b =  0)
      | Neq -> (fun a b -> Debian.Version.compare a b <> 0)
      | Ge  -> (fun a b -> Debian.Version.compare a b >= 0)
      | Le  -> (fun a b -> Debian.Version.compare a b <= 0)
      | Gt  -> (fun a b -> Debian.Version.compare a b >  0)
      | Lt  -> (fun a b -> Debian.Version.compare a b <  0) in
    s (eval_string t e) (eval_string t f)
  | FOr(e,f)  -> eval_bool t e || eval_bool t f
  | FAnd(e,f) -> eval_bool t e && eval_bool t f
  | FNot e    -> not (eval_bool t e)

let eval_filter t = function
  | None   -> true
  | Some f ->
    try eval_bool t f
    with _ -> false

let filter_arg t (a,f) =
  if eval_filter t f then
    try match a with
      | CString s -> Some (substitute_string t s)
      | CIdent i  -> Some (string_of_variable_contents i (substitute_ident t i))
    with _ ->
      None
  else
    None

let filter_command t (l, f) =
  if eval_filter t f then
    match OpamMisc.filter_map (filter_arg t) l with
    | [] -> None
    | l  -> Some l
  else
    None

let filter_commands t l =
  OpamMisc.filter_map (filter_command t) l

let expand_env t (env: env_updates) : env =
  List.rev_map (fun (ident, symbol, string) ->
    let string = substitute_string t string in
    let read_env () =
      let prefix = OpamFilename.Dir.to_string t.root in
      try OpamMisc.reset_env_value ~prefix (OpamMisc.getenv ident)
      with _ -> [] in
    match symbol with
    | "="  -> (ident, string)
    | "+=" -> (ident, String.concat ":" (string :: read_env ()))
    | "=+" -> (ident, String.concat ":" (read_env () @ [string]))
    | ":=" -> (ident, string ^":"^ (String.concat ":" (read_env())))
    | "=:" -> (ident, (String.concat ":" (read_env())) ^":"^ string)
    | _    -> failwith (Printf.sprintf "expand_env: %s is an unknown symbol" symbol)
  ) env

let add_to_env t (env: env) (updates: env_updates) =
  let env =
    List.filter (fun (k,_) -> List.for_all (fun (u,_,_) -> u <> k) updates) env in
  env @ expand_env t updates

let env_updates t =
  let comp = compiler t t.compiler in

  let add_to_path = OpamPath.Switch.bin t.root t.switch in
  let new_path = "PATH", "+=", OpamFilename.Dir.to_string add_to_path in
  let toplevel_dir =
    "OCAML_TOPLEVEL_PATH", "=",
    OpamFilename.Dir.to_string (OpamPath.Switch.toplevel t.root t.switch) in
  let man_path =
    "MANPATH", ":=",
    OpamFilename.Dir.to_string (OpamPath.Switch.man_dir t.root t.switch) in
  let comp_env = OpamFile.Comp.env comp in
  let switch = match !OpamGlobals.switch with
    | `Command_line s -> [ "OPAMSWITCH", "=", s ]
    | `Env _
    | `Not_set -> [] in
  let root =
    if !OpamGlobals.root_dir <> OpamGlobals.default_opam_dir then
      [ "OPAMROOT", "=", !OpamGlobals.root_dir ]
    else
      [] in

  new_path :: man_path :: toplevel_dir :: (switch @ root @ comp_env)

(* This function is used by 'opam config env' and 'opam switch' to
   display the environment variables. We have to make sure that
   OPAMSWITCH is always the one being reported in '~/.opa/config'
   otherwise we can have very weird results (as the inability to switch
   between compilers).

   Note: when we do the later command with --switch=SWITCH, this mean
   we really want to get the environment for this switch. *)
let get_opam_env t =
  let t = match !OpamGlobals.switch with
    | `Command_line _
    | `Not_set -> t
    | `Env _   -> { t with switch = OpamFile.Config.switch t.config } in
  add_to_env t [] (env_updates t)

let get_full_env t =
  let env0 = OpamMisc.env () in
  add_to_env t env0 (env_updates t)

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
  let sh  (k,v) = Printf.sprintf "%s=%s; export %s;\n" k v k in
  let csh (k,v) = Printf.sprintf "setenv %s %S;\n" k v in
  let export = match shell with
    | `zsh
    | `sh  -> sh
    | `csh -> csh in
  let aux (ident, symbol, string) =
    let string = substitute_string t string in
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
    (variables_sh  , string_of_env_update t `sh  (env_updates t));
    (variables_csh , string_of_env_update t `csh (env_updates t));
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
      "  %s is already configured for an other OPAM root.\n"
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
  let print (k,v) = OpamGlobals.msg "  %-25s %35s\n" k v in
  let not_set = "[NOT SET]" in
  let ok      = "[OK]" in
  let error   = "[ERROR]" in
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
      [ ("init-script"     , Printf.sprintf "[%s]" pretty_init_file);
        ("auto-completion" , completion);
        ("opam-switch-eval", switch_eval);
      ]
  in
  OpamGlobals.msg "User configuration:\n";
  List.iter print user_setup;
  OpamGlobals.msg "Global configuration:\n";
  List.iter print global_setup

let print_env_warning t user =
  match
    List.filter
      (fun (s, v) ->
        Some v <> try Some (OpamMisc.getenv s) with _ -> None)
      (get_opam_env t)
  with
  | [] -> () (* every variables are correctly set *)
  | _  ->
    let eval_string =
      let root =
        if !OpamGlobals.root_dir <> OpamGlobals.default_opam_dir then
          Printf.sprintf " --root=%s" !OpamGlobals.root_dir
        else
          "" in
      Printf.sprintf "eval `opam config env%s`\n" root in
    let profile_string = match user with
      | None   -> ""
      | Some u -> match u.dot_profile with
        | None -> ""
        | Some f ->
          let dot_profile =
            Printf.sprintf " (for instance %s)" (OpamFilename.prettify f) in
          Printf.sprintf
            "2. To manually configure OPAM for subsequent usages, add the following \n\
            \   line to your profile file%s:\n\
             \n\
            \      %s\n"
            dot_profile
            (source t (init_file u.shell)) in
    let line =
      "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n"
    in
    OpamGlobals.msg
      "\n%s\n\
       1. To configure OPAM in the current shell session, you need to run:\n\
       \n\
      \      %s\n\
       %s%s\n"
      line eval_string profile_string line

let update_setup_interactive t shell dot_profile =
  let update dot_profile =
    let user = Some { shell; ocamlinit = true; dot_profile = Some dot_profile } in
    let global = Some { complete = true ; switch_eval = true } in
    OpamGlobals.msg "\n";
    update_setup t user global;
    true in

  OpamGlobals.msg "\n";

  match read
      "OPAM can configure your computer for optimal use.\n\
      \n\
       In order to do so, it will modify: \n\
      \  - %s to set the right environment variables and to load the \n\
      \    auto-completion scripts for your shell (%s) on startup.\n\
      \  - ~/.ocamlinit to make non-root installations of `ocamlfind` (such as the \n\
      \    ones done by OPAM) work when running the OCaml toplevel (eg. by adding \n\
      \    $OCAML_TOPLEVEL_PATH to the list of include directories).\n\
       \n\
       If you choose to not configure your system now, you can either configure \n\
       OPAM manually (more instructions on this later) or launch the automatic setup \n\
       later by running:\n\
       \n\
      \    `opam config setup -a`.\n\
       \n\
       Do you want OPAM to modify your %s? (default is 'no', use 'f' to \n\
       modify another file instead)\n\
      \    [N/y/f]"
      (OpamFilename.prettify dot_profile)
      (string_of_shell shell)
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

(* install ~/.opam/<switch>/config/conf-ocaml.config *)
let install_conf_ocaml_config root switch =
  log "install_conf_ocaml_config switch=%s" (OpamSwitch.to_string switch);

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
        ("doc", OpamPath.Switch.doc_dir root switch);
        ("stublibs", OpamPath.Switch.stublibs root switch);
        ("toplevel", OpamPath.Switch.toplevel root switch);
        ("man", OpamPath.Switch.man_dir root switch);
        ("share", OpamPath.Switch.share_dir root switch);
      ]
    @ map id [
      ("user" , try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name with _ -> "user");
      ("group", try (Unix.getgrgid (Unix.getgid ())).Unix.gr_name with _ -> "group");
      ("make" , !OpamGlobals.makecmd ());
      ("os"   , OpamGlobals.os_string ());
    ] in

  let config = OpamFile.Dot_config.create vars in
  OpamFile.Dot_config.write
    (OpamPath.Switch.config root switch OpamPackage.Name.default)
    config

(* - compiles and install $opam/compiler/[ocaml_version].comp in $opam/[switch]
   - update $opam/switch
   - update $opam/config *)
let install_compiler t ~quiet switch compiler =
  log "install_compiler switch=%s compiler=%s"
    (OpamSwitch.to_string switch)
    (OpamCompiler.to_string compiler);

  let comp_f = OpamPath.compiler t.root compiler in
  if not (OpamFilename.exists comp_f) then (
    OpamGlobals.msg "Cannot find %s: %s is not a valid compiler name.\n"
      (OpamFilename.to_string comp_f)
      (OpamCompiler.to_string compiler);
    OpamGlobals.exit 0;
  );

  let switch_dir = OpamPath.Switch.root t.root switch in

  (* Do some clean-up if necessary *)
  if not (switch_installed t switch)
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
  OpamFilename.mkdir (OpamPath.Switch.doc_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.man_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.install_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.config_dir t.root switch);
  List.iter (fun num ->
    OpamFilename.mkdir (OpamPath.Switch.man_dir ~num t.root switch)
  ) ["1";"1M";"2";"3";"4";"5";"6";"7";"9"];

  install_conf_ocaml_config t.root switch;

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
        let comp_src_raw = OpamFilename.to_string comp_src in
        if Sys.file_exists comp_src_raw && Sys.is_directory comp_src_raw then
          OpamFilename.link_dir
            ~src:(OpamFilename.Dir.of_string comp_src_raw) ~dst:build_dir
        else if Sys.file_exists comp_src_raw then
          OpamFilename.extract comp_src build_dir
        else OpamFilename.with_tmp_dir (fun download_dir ->
            let file = OpamFilename.download ~overwrite:true comp_src download_dir in
            OpamFilename.extract file build_dir;
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
          let builds = filter_commands t (OpamFile.Comp.build comp) in
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

let update_pinned_package t n =
  if OpamPackage.Name.Map.mem n t.pinned then
    let pin = OpamPackage.Name.Map.find n t.pinned in
    match kind_of_pin_option pin with
    | (`git|`darcs|`local as k) ->
      let path = OpamFilename.raw_dir (path_of_pin_option pin) in
      let dst = OpamPath.Switch.pinned_dir t.root t.switch n in
      let module B = (val OpamRepository.find_backend k: OpamRepository.BACKEND) in
      B.pull_dir dst path
    | _ ->
      OpamGlobals.error_and_exit
        "Cannot update the pinned package %s: wrong backend."
        (OpamPackage.Name.to_string n)
  else
    OpamGlobals.error_and_exit "%s is not pinned."
      (OpamPackage.Name.to_string n)

let check f =
  let root = OpamPath.default () in
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
      if OpamFilename.exists_dir (OpamPath.root root) then
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

module Types = struct
  type t = state = {
    partial: bool;
    root: OpamPath.t;
    switch: switch;
    compiler: compiler;
    compiler_version: compiler_version;
    opams: OpamFile.OPAM.t package_map;
    descrs: OpamFile.Descr.t lazy_t package_map;
    repositories: OpamFile.Repo_config.t repository_name_map;
    prefixes: OpamFile.Prefix.t repository_name_map;
    packages: package_set;
    available_packages: package_set Lazy.t;
    aliases: OpamFile.Aliases.t;
    compilers: compiler_set;
    pinned: OpamFile.Pinned.t;
    installed: OpamFile.Installed.t;
    installed_roots: OpamFile.Installed_roots.t;
    reinstall: OpamFile.Reinstall.t;
    config: OpamFile.Config.t;
    package_index: repository_name package_map;
    compiler_index: repository_name compiler_map;
  }
end

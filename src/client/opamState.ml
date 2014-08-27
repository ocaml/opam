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
open OpamMisc.OP
open OpamFilename.OP
open OpamPackage.Set.Op

let log fmt = OpamGlobals.log "STATE" fmt
let slog = OpamGlobals.slog

let () =
  OpamHTTP.register ();
  OpamGit.register ();
  OpamDarcs.register();
  OpamLocal.register ();
  OpamHg.register ()

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

let string_of_repositories r =
  OpamMisc.string_of_list
    OpamRepositoryName.to_string
    (OpamRepositoryName.Map.keys r)

let print_state t =
  let packages p =
    if OpamPackage.Set.cardinal p <= 20 then
      OpamPackage.Set.to_string p
    else
      Printf.sprintf "%d packages" (OpamPackage.Set.cardinal p) in
  log "ROOT      : %a" (slog OpamFilename.Dir.to_string) t.root;
  log "SWITCH    : %a" (slog OpamSwitch.to_string) t.switch;
  log "COMPILER  : %a" (slog OpamCompiler.to_string) t.compiler;
  log "COMPILERS : %a" (slog OpamCompiler.Set.to_string) t.compilers;
  log "REPOS     : %a" (slog string_of_repositories) t.repositories;
  log "PACKAGES  : %a" (slog packages) t.packages;
  log "INSTALLED : %a" (slog OpamPackage.Set.to_string) t.installed;
  log "ROOTS     : %a" (slog OpamPackage.Set.to_string) t.installed_roots;
  log "REINSTALL : %a" (slog OpamPackage.Set.to_string) t.reinstall

let compiler_comp t c =
  OpamFile.Comp.read (OpamPath.compiler_comp t.root c)

let is_name_installed_aux installed name =
  OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = name) installed

let is_name_installed t name =
  is_name_installed_aux t.installed name

let find_installed_package_by_name_aux installed name =
  OpamPackage.Set.find (fun nv -> OpamPackage.name nv = name) installed

let find_installed_package_by_name t name =
  find_installed_package_by_name_aux t.installed name

let find_packages_by_name t name =
  let r = OpamPackage.packages_of_name t.packages name in
  if OpamPackage.Set.is_empty r then None
  else Some r

let installed_map t =
  OpamPackage.Name.Map.map OpamPackage.Version.Set.choose_one
    (OpamPackage.to_map t.installed)

let dot_config t name =
  let f =
    if name = OpamPackage.Name.global_config then
      OpamPath.Switch.global_config t.root t.switch
    else
      OpamPath.Switch.config t.root t.switch name
  in
  OpamFile.Dot_config.safe_read f

let is_package_installed t nv =
  OpamPackage.Set.mem nv t.installed

let jobs t =
  match !OpamGlobals.jobs with
  | None   -> OpamFile.Config.jobs t.config
  | Some j -> j

let dl_jobs t =
  match !OpamGlobals.dl_jobs with
  | None   -> OpamFile.Config.dl_jobs t.config
  | Some j -> j

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
      @ OpamRepository.url_checksum url
      @ OpamFilename.checksum_dir files
      @ OpamFilename.checksum archive
    | `partial true ->
      OpamRepository.url_checksum url
      @ OpamFilename.checksum_dir files
      @ OpamFilename.checksum archive
    | `partial false ->
      OpamRepository.url_checksum url
      @ OpamFilename.checksum_dir files

let all_installed t =
  OpamSwitch.Map.fold (fun switch _ accu ->
    let installed_f = OpamPath.Switch.installed t.root switch in
    let installed = OpamFile.Installed.safe_read installed_f in
    installed ++ accu
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

let locally_pinned_package t n =
  let option = OpamPackage.Name.Map.find n t.pinned in
  let path = string_of_pin_option option in
  let kind = kind_of_pin_option option in
  match repository_kind_of_pin_kind kind with
  | None    -> OpamSystem.internal_error "locally pinned"
  | Some kind -> (address_of_string path, kind)

let url_of_locally_pinned_package t n =
  let path, kind = locally_pinned_package t n in
  OpamFile.URL.create kind path

(* Returns the directory holding the original metadata of the package.
   This is a low-level function, you generally want to handle different
   locations, like overlays for pinned packages *)
let package_repo_dir root repositories package_index nv =
  if OpamFilename.exists (OpamPath.opam root nv) then
    OpamPath.packages root nv
  else
  let repo_name, prefix = OpamPackage.Map.find nv package_index in
  let repo = OpamRepositoryName.Map.find repo_name repositories in
  OpamPath.Repository.packages repo prefix nv

(* Copies package definition from the repository to the overlay *)
let add_pinned_overlay ?(template=false) ?version t name =
  let open OpamFile in
  let module Ov = OpamPath.Switch.Overlay in
  let pkg_overlay f = f t.root t.switch name in
  let get_orig_meta rv =
    let orig = package_repo_dir t.root t.repositories t.package_index rv in
    let files = OpamFilename.rec_files orig in
    let opam_f = orig // "opam" in
    let url_f = orig // "url" in
    let files = List.filter (fun f -> f <> opam_f && f <> url_f) files in
    let opam = OPAM.read opam_f in
    let url =
      try Some (URL.read url_f) with e -> OpamMisc.fatal e; None in
    opam, url, orig, files
  in
  try match OpamPackage.Name.Map.find name t.pinned with
    | Version v ->
      let opam, url, root, files = get_orig_meta (OpamPackage.create name v) in
      List.iter (fun f -> OpamFilename.copy_in ~root f (pkg_overlay Ov.package))
        files;
      OPAM.write (pkg_overlay Ov.opam) (OPAM.with_version opam v);
      OpamMisc.Option.iter (URL.write (pkg_overlay Ov.url)) url
    | _ ->
      let rv =
        match version with
        | Some v -> OpamPackage.create name v
        | None -> (* Lookup in package_index to ignore pinned versions *)
          let versions =
            OpamPackage.Map.fold (fun nv _ acc ->
              if OpamPackage.name nv = name then
                OpamPackage.Set.add nv acc
              else acc)
              t.package_index OpamPackage.Set.empty
          in
          OpamPackage.max_version versions name in
      let v = OpamPackage.version rv in
      let opam, _url, root, files = get_orig_meta rv in
      let url = url_of_locally_pinned_package t name in
      List.iter (fun f -> OpamFilename.copy_in ~root f (pkg_overlay Ov.package))
        files;
      OPAM.write (pkg_overlay Ov.opam) (OPAM.with_version opam v);
      URL.write (pkg_overlay Ov.url) url
  with Not_found -> (* No original meta *)
    let url = url_of_locally_pinned_package t name in
    let version =
      OpamPackage.Version.of_string (if template then "0.1" else "0") in
    let nv = OpamPackage.create name version in
    let opam = if template then OPAM.template nv else OPAM.create nv in
    OPAM.write (pkg_overlay Ov.opam) opam;
    URL.write (pkg_overlay Ov.url) url

let overlay_of_name t name =
  let f = OpamPath.Switch.Overlay.opam t.root t.switch name in
  if not (OpamFilename.exists f) then
    (log "overlay missing for %s !" (OpamPackage.Name.to_string name);
     add_pinned_overlay t name);
  f

let version_of_pin t name = function
  | Version v -> v
  | _ ->
    let overlay = overlay_of_name t name in
    let opam = OpamFile.OPAM.read overlay in
    if OpamFile.OPAM.version_opt opam = None then
      let nv =
        try OpamPackage.max_version t.packages name
        with Not_found ->
          OpamPackage.create name (OpamPackage.Version.of_string "0") in
      OpamGlobals.warning "Setting missing version in %s to %s"
        (OpamFilename.to_string overlay)
        (OpamPackage.Version.to_string @@ OpamPackage.version nv);
      OpamFile.OPAM.(write overlay (with_version opam (OpamPackage.version nv)));
      OpamPackage.version nv
    else
      OpamFile.OPAM.version opam

let pinned t name =
  let v = version_of_pin t name (OpamPackage.Name.Map.find name t.pinned) in
  OpamPackage.create name v

let pinned_opt t name = try Some (pinned t name) with Not_found -> None

let is_locally_pinned t name =
  try match OpamPackage.Name.Map.find name t.pinned with
    | Version _ -> false
    | _ -> true
  with Not_found -> false

let opam_opt t nv =
  let name = OpamPackage.name nv in
  let base () =
    try Some (OpamPackage.Map.find nv t.opams)
    with Not_found ->
      if OpamPackage.Set.mem nv t.installed then
        (OpamGlobals.warning "no package description left for %s"
           (OpamPackage.to_string nv);
         Some (OpamFile.OPAM.create nv))
      else
        None
  in
  let overlay = OpamPath.Switch.Overlay.opam t.root t.switch name in
  if OpamFilename.exists overlay then
    let o = OpamFile.OPAM.read overlay in
    if OpamFile.OPAM.version o = OpamPackage.version nv then Some o
    else if OpamPackage.Map.mem nv t.opams then
      (log "Looking for %s which is pinned to %s (not using overlay)"
         (OpamPackage.to_string nv) (OpamPackage.Version.to_string (OpamFile.OPAM.version o));
       base ())
    else
      (log "Opam file for %s not found: using the overlay even if it's for %s"
         (OpamPackage.to_string nv) (OpamPackage.Version.to_string (OpamFile.OPAM.version o));
       Some (OpamFile.OPAM.with_version o (OpamPackage.version nv)))
  else
    base ()

let opam t nv =
  match opam_opt t nv with
  | None    -> OpamPackage.unknown (OpamPackage.name nv) (Some (OpamPackage.version nv))
  | Some nv -> nv

let locate_meta overlay global repo exists t nv =
  let name = OpamPackage.name nv in
  if Some nv = pinned_opt t name &&
     OpamFilename.exists_dir
       (OpamPath.Switch.Overlay.package t.root t.switch name)
  then
    let meta = overlay t.root t.switch name in
    if exists meta then Some meta else None
  else
  let meta = global t.root nv in
  if exists meta then Some meta else
  try
    let r, prefix = OpamPackage.Map.find nv t.package_index in
    let r = find_repository_exn t r in
    let meta = repo r prefix nv in
    if exists meta then Some meta else None
  with Not_found ->
    None

let descr_file =
  locate_meta
    OpamPath.Switch.Overlay.descr OpamPath.descr OpamPath.Repository.descr
    OpamFilename.exists

let url_file =
  locate_meta
    OpamPath.Switch.Overlay.url OpamPath.url OpamPath.Repository.url
    OpamFilename.exists

let files =
  locate_meta
    OpamPath.Switch.Overlay.files OpamPath.files OpamPath.Repository.files
    OpamFilename.exists_dir

let install_metadata t nv =
  if is_pinned t (OpamPackage.name nv) ||
     OpamFilename.exists (OpamPath.opam t.root nv) then ()
  else
    let opam = opam t nv in
    OpamFile.OPAM.write (OpamPath.opam t.root nv) opam;
    let onsome f = function None -> () | Some x -> f x in
    onsome (fun f ->
        OpamFile.Descr.write (OpamPath.descr t.root nv) (OpamFile.Descr.read f))
      (descr_file t nv);
    onsome (fun f ->
        OpamFile.URL.write (OpamPath.url t.root nv) (OpamFile.URL.read f))
      (url_file t nv);
    onsome (fun d ->
        OpamFilename.copy_dir ~src:d ~dst:(OpamPath.files t.root nv))
      (files t nv)

let remove_metadata t packages =
  let all_installed = all_installed t in
  let packages = packages -- all_installed in
  OpamPackage.Set.iter (fun nv ->
      let dir = OpamPath.packages t.root nv in
      OpamFilename.rmdir dir;
      let parent = OpamFilename.dirname_dir dir in
      if OpamFilename.dir_is_empty parent then OpamFilename.rmdir parent;
      let archive = OpamPath.archive t.root nv in
      OpamFilename.remove archive;
    ) packages

(* Returns [opam, descr_file, files_dir]. We don't consider [url] since
   this is for pinned packages. if [root], don't look for a subdir [opam]
   to find [files] and [descr]. *)
let local_opam ?(root=false) ?(version_override=true) nv dir =
  let has_dir d = if OpamFilename.exists_dir d then Some d else None in
  let has_file f = if OpamFilename.exists f then Some f else None in
  let opam_dir, descr, files_dir =
    if root then dir, has_file (dir // "descr"), has_dir (dir / "files") else
    match has_dir (dir / "opam") with
    | Some dir -> dir, has_file (dir // "descr"), has_dir (dir / "files")
    | None -> dir, None, None
  in
  let opam =
    match has_file (opam_dir // "opam") with
    | None -> None
    | Some local_opam ->
      try
        let opam = OpamFile.OPAM.read local_opam in
        let opam = OpamFile.OPAM.with_name opam (OpamPackage.name nv) in
        let version_override =
          version_override || OpamFile.OPAM.version_opt opam = None
        in
        if version_override then
          Some (OpamFile.OPAM.with_version opam (OpamPackage.version nv))
        else Some opam
      with e ->
        OpamMisc.fatal e;
        OpamGlobals.error "opam file for %s contains errors, ignoring"
          (OpamPackage.to_string nv);
        None
  in
  opam, descr, files_dir

let remove_overlay t name =
  OpamFilename.rmdir (OpamPath.Switch.Overlay.package t.root t.switch name)

let has_url_overlay t name =
  OpamFilename.exists (OpamPath.Switch.Overlay.url t.root t.switch name)

let dev_package t nv =
  if has_url_overlay t (OpamPackage.name nv) &&
     pinned_opt t (OpamPackage.name nv) = Some nv
  then OpamPath.Switch.dev_package t.root t.switch (OpamPackage.name nv)
  else OpamPath.dev_package t.root nv

let pinned_packages t =
  OpamPackage.Name.Map.fold
    (fun name _ acc ->
       try OpamPackage.Set.add (pinned t name) acc
       with e ->
         OpamMisc.fatal e;
         OpamGlobals.error "Ignoring invalid pinned package %s"
           (OpamPackage.Name.to_string name);
         acc)
    t.pinned OpamPackage.Set.empty

let descr_opt t nv =
  OpamMisc.Option.map OpamFile.Descr.read
    (descr_file t nv)

let descr t nv =
  match descr_file t nv with
  | None -> OpamFile.Descr.empty
  | Some f -> OpamFile.Descr.read f

let url t nv =
  OpamMisc.Option.map OpamFile.URL.read
    (url_file t nv)

(* For documentation *)
let global_variable_names = [
  "ocaml-version",        "The version of the currently used OCaml compiler";
  "opam-version",         "The currently running OPAM version";
  "compiler",             "The name of the current OCaml compiler (may be more \
                           specific than the version, eg: \"4.01.0+fp\"";
  "preinstalled",         "Whether the compiler was preinstalled on the system, \
                           or installed by OPAM";
  "switch",               "The local name (alias) of the current switch";
  "jobs",                 "The number of parallel jobs set up in OPAM \
                           configuration";
  "ocaml-native",         "Whether the OCaml native compilers are available";
  "ocaml-tools-opt",      "Whether the .opt versions of the OCaml tools are \
                           available";
  "ocaml-native-dynlink", "Whether native dynlink is available on this \
                           installation";
  "arch",                 "The current arch, as returned by \"uname -m\"";
]
let package_variable_names = [
  "name",      "Name of the package";
  "version",   "Version of the package";
  "depends",   "Resolved direct dependencies of the package";
  "installed", "Whether the package is installed";
  "enable",    "Takes the value \"enable\" or \"disable\" depending on whether \
                the package is installed";
  "pinned",    "Whether the package is pinned";
  "bin",       "Binary directory for this package";
  "sbin",      "System binary directory for this package";
  "lib",       "Library directory for this package";
  "man",       "Man directory for this package";
  "doc",       "Doc directory for this package";
  "share",     "Share directory for this package";
  "etc",       "Etc directory for this package";
  "build",     "Directory where the package was built";
  "hash",      "Hash of the package archive";
]

let string str = Some (S str)
let bool b = Some (B b)
let int i = string (string_of_int i)
let is_global_conf v =
  OpamVariable.Full.package v = OpamPackage.Name.global_config

(* Read the env variables *)
let get_env_var v =
  let var_str = OpamVariable.to_string (OpamVariable.Full.variable v) in
  let var_hook =
    if is_global_conf v then
      OpamMisc.string_map (function '-' -> '_' | c -> c) var_str
    else
      Printf.sprintf "%s_%s"
        (OpamPackage.Name.to_string (OpamVariable.Full.package v))
        var_str
  in
  try match OpamMisc.getenv ("OPAMVAR_" ^ var_hook) with
    | "true"  | "1" -> bool true
    | "false" | "0" -> bool false
    | s             -> string s
  with Not_found -> None

(* filter handling *)
let resolve_variable t ?opam local_variables v =
  let dirname dir = string (OpamFilename.Dir.to_string dir) in
  let read_var v =
    let var = OpamVariable.Full.variable v in
    let c = dot_config t (OpamVariable.Full.package v) in
    try OpamFile.Dot_config.variable c var
    with Not_found -> None in
  let get_local_var v =
    if not (is_global_conf v) then None else
    let var = OpamVariable.Full.variable v in
    try Some (OpamVariable.Map.find var local_variables)
    with Not_found -> None
  in
  let get_global_var v =
    if not (is_global_conf v) then None else
    match OpamVariable.to_string (OpamVariable.Full.variable v) with
    | "ocaml-version" -> string (OpamCompiler.Version.to_string
                                   (Lazy.force t.compiler_version))
    | "opam-version"  -> string (OpamVersion.to_string OpamVersion.current)
    | "compiler" -> string (OpamCompiler.to_string t.compiler)
    | "preinstalled"  -> bool (OpamFile.Comp.preinstalled
                                 (compiler_comp t t.compiler))
    | "switch"        -> string (OpamSwitch.to_string t.switch)
    | "jobs"          -> int (jobs t)
    | "ocaml-native"  -> bool (Lazy.force OpamSystem.ocaml_native_available)
    | "ocaml-tools-opt" -> bool (Lazy.force OpamSystem.ocaml_opt_available)
    | "ocaml-native-dynlink" ->
      bool (OpamSystem.ocaml_natdynlink_available
              (OpamFilename.Dir.to_string (OpamPath.Switch.lib_dir t.root t.switch)))
    | "arch"          -> string (OpamGlobals.arch ())
    | _               -> None
  in
  let get_package_var opam v =
    if is_global_conf v then None else
    let var_str = OpamVariable.to_string (OpamVariable.Full.variable v) in
    let name = OpamVariable.Full.package v in
    let opam = (* ensure opam, if not None, corresponds to name *)
      match opam with
      | Some o when OpamFile.OPAM.name o = name -> opam
      | _ when is_name_installed t name ->
        opam_opt t (find_installed_package_by_name t name)
      | _ -> None
    in
    let get_nv opam = OpamPackage.create name (OpamFile.OPAM.version opam) in
    match var_str, opam with
    | "enable",    Some _    -> string "enable"
    | "enable",    None      -> string "disable"
    | "installed", Some _    -> bool true
    | "installed", None      -> bool false
    | "pinned",    _         -> bool    (OpamPackage.Name.Map.mem name t.pinned)
    | _,           None      -> None
    | "bin",       _         -> dirname (OpamPath.Switch.bin     t.root t.switch)
    | "sbin",      _         -> dirname (OpamPath.Switch.sbin    t.root t.switch)
    | "lib",       _         -> dirname (OpamPath.Switch.lib     t.root t.switch name)
    | "man",       _         -> dirname (OpamPath.Switch.man_dir t.root t.switch)
    | "doc",       _         -> dirname (OpamPath.Switch.doc     t.root t.switch name)
    | "share",     _         -> dirname (OpamPath.Switch.share   t.root t.switch name)
    | "etc",       _         -> dirname (OpamPath.Switch.etc     t.root t.switch name)
    | "name",      _         -> string  (OpamPackage.Name.to_string name)
    | "build",     Some opam ->
      dirname (OpamPath.Switch.build t.root t.switch (get_nv opam))
    | "version",   Some opam ->
      let ver = OpamFile.OPAM.version opam in
      string (OpamPackage.Version.to_string ver)
    | "depends",   Some opam ->
      let deps =
        OpamFormula.atoms (filter_deps (OpamFile.OPAM.depends opam)) @
        OpamFormula.atoms (filter_deps (OpamFile.OPAM.depopts opam))
      in
      let installed_deps =
        OpamMisc.filter_map
          (fun (n,cstr) ->
             try
               let nv =
                 OpamPackage.Set.find (fun nv -> OpamPackage.name nv = n) t.installed in
               let version = OpamPackage.version nv in
               match cstr with
               | None -> Some nv
               | Some (op,v) when OpamFormula.eval_relop op version v -> Some nv
               | Some _ -> None
             with Not_found -> None)
          deps
      in
      string (String.concat " " (List.map OpamPackage.to_string installed_deps))
    | "hash",      Some opam ->
      (try
         let nv = get_nv opam in
         let f = OpamPath.archive t.root nv in
         if OpamFilename.exists f then string (OpamFilename.digest f)
         else string ""
       with Not_found -> string "")
    | _,           _         -> None
  in
  let make_package_local v =
    (* [var] within the opam file of [pkg] is tried as [pkg:var] *)
    match is_global_conf v, opam with
    | true, Some opam ->
      OpamVariable.Full.create (OpamFile.OPAM.name opam) (OpamVariable.Full.variable v)
    | _ -> v
  in
  let skip _ = None in
  let v' = make_package_local v in
  let contents =
    List.fold_left
      (function None -> (fun (f,v) -> f v) | r -> (fun _ -> r))
      None
      [
        get_local_var, v;
        get_env_var, v;
        (if v' <> v then get_env_var else skip), v';
        read_var, v;
        (if v' <> v then read_var else skip), v';
        get_global_var, v;
        get_package_var opam, v';
      ]
  in
  contents

let eval_filter t ?opam local_variables =
  OpamFilter.eval_opt (resolve_variable t ?opam local_variables)

let filter_commands t ?opam local_variables =
  OpamFilter.commands (resolve_variable t ?opam local_variables)

let substitute_file t ?opam local_variables =
  OpamFilter.substitute_file (resolve_variable t ?opam local_variables)

let substitute_string t ?opam local_variables =
  OpamFilter.substitute_string (resolve_variable t ?opam local_variables)

let contents_of_variable t ?opam local_variables =
  OpamFilter.contents_of_variable (resolve_variable t ?opam local_variables)

let contents_of_variable_exn t ?opam local_variables =
  OpamFilter.contents_of_variable_exn
    (resolve_variable t ?opam local_variables)

let redirect t repo =
  if repo.repo_kind <> `http then None else
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

let copy_files t nv dst =
  match files t nv with
  | None     -> ()
  | Some src -> OpamFilename.copy_files ~src ~dst

let consistent_ocaml_version t opam =
  let atom (r,v) =
    match OpamCompiler.Version.to_string v with
    | "system" ->
      begin match r with
        | `Eq  -> t.compiler = OpamCompiler.system
        | `Neq -> t.compiler <> OpamCompiler.system
        | _    -> OpamSystem.internal_error
                    "%s is not a valid constraint for the system compiler \
                     (only '=' and '!=' are valid)."
                    (OpamFormula.string_of_relop r)
      end
    | _ -> OpamCompiler.Version.eval_relop r
             (Lazy.force t.compiler_version) v
  in
  match OpamFile.OPAM.ocaml_version opam with
  | None   -> true
  | Some c -> OpamFormula.eval atom c

let consistent_os opam =
  match OpamFile.OPAM.os opam with
  | Empty -> true
  | f ->
    let atom (b, os) =
      let ($) = if b then (=) else (<>) in
      os $ OpamGlobals.os_string () in
    OpamFormula.eval atom f

let consistent_available_field t opam =
  let env opam = resolve_variable t ~opam OpamVariable.Map.empty in
  OpamFilter.eval (env opam) (OpamFile.OPAM.available opam)

(* List the packages which do fulfil the compiler and OS constraints *)
let available_packages t =
  let filter nv =
    match opam_opt t nv with
    | None -> false
    | Some opam ->
      let has_repository () =
        OpamPackage.Map.mem nv t.package_index ||
        OpamPackage.Name.Map.mem (OpamPackage.name nv) t.pinned in
      consistent_ocaml_version t opam
      && consistent_os opam
      && consistent_available_field t opam
      && has_repository ()
  in
  let pinned = pinned_packages t in
  let pinned_names =
    OpamPackage.Name.Map.fold
      (fun n _ s -> OpamPackage.Name.Set.add n s)
      t.pinned OpamPackage.Name.Set.empty in
  let packages =
    pinned ++
    OpamPackage.Set.filter
      (fun nv -> not (OpamPackage.Name.Set.mem (OpamPackage.name nv) pinned_names))
      t.packages in
  OpamPackage.Set.filter filter packages

(* Display a meaningful error for a package that doesn't exist *)
let unknown_package t (name, _ as atom) =
  if OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = name) t.packages
  then
    Printf.sprintf "No package matches %s."
      (OpamFormula.short_string_of_atom atom)
  else
    Printf.sprintf "No package named %s found."
      (OpamPackage.Name.to_string name)

(* Display a meaningful error for an unavailable package *)
let unavailable_reason t (name, _ as atom) =
  let reasons () =
    let candidates =
      OpamPackage.Set.filter (OpamFormula.check atom) t.packages in
    let nv = OpamPackage.max_version candidates name in
    let r =
      let opam = opam t nv in
      (if consistent_ocaml_version t opam then [] else [
          Printf.sprintf "it requires OCaml %s"
            (OpamFormula.string_of_formula
               (fun (op,v) ->
                  OpamFormula.string_of_relop op ^ " " ^
                  OpamCompiler.Version.to_string v)
               (match OpamFile.OPAM.ocaml_version opam with
                | Some v -> v
                | None -> assert false))
        ]) @
      (if consistent_os opam then [] else [
          Printf.sprintf "your OS doesn't match %s"
            (OpamFormula.string_of_formula
               (fun (b,s) -> (if b then "= " else "!= ") ^ s)
               (OpamFile.OPAM.os opam))
        ]) @
      (if consistent_available_field t opam then [] else [
          Printf.sprintf "your system doesn't comply with %s"
            (string_of_filter (OpamFile.OPAM.available opam))
        ]) in
    match r with
    | [] -> raise Not_found
    | [r] -> " because " ^ r ^ "."
    | rs -> " because:\n" ^ String.concat "\n" (List.map ((^) "    - ") rs) in
  try
    Printf.sprintf
      "%s is not available%s"
      (OpamFormula.short_string_of_atom atom)
      (reasons ())
  with Not_found -> try
    let pin = OpamPackage.Name.Map.find name t.pinned in
    Printf.sprintf
      "%s is not available because the package is pinned to %s."
      (OpamFormula.short_string_of_atom atom)
      (string_of_pin_option pin)
  with Not_found ->
    unknown_package t atom

let base_packages =
  List.map OpamPackage.Name.of_string [ "base-unix"; "base-bigarray"; "base-threads" ]

let create_system_compiler_description root = function
  | None         -> ()
  | Some version ->
    log "create-system-compiler-description %a"
      (slog OpamCompiler.Version.to_string) version;
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
            "%{lib}%/stublibs" ^ String.make 1 OpamSystem.path_sep ^
            Filename.concat dir "stublibs" ] in
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
        (OpamPackage.Name.to_string @> OpamGlobals.error "Package %s not found")
        pkg_not;
      OpamGlobals.exit 1
    );

    pkg_available
  )

let is_compiler_installed t comp =
  OpamSwitch.Map.exists (fun _ c -> c = comp) t.aliases

let is_switch_installed t switch =
  OpamSwitch.Map.mem switch t.aliases

let universe t action =
  let opams = (* Read overlays of pinned packages *)
    OpamPackage.Name.Map.fold (fun name pin map ->
        let v = version_of_pin t name pin in
        let overlay = OpamPath.Switch.Overlay.opam t.root t.switch name in
        if OpamFilename.exists overlay then
          OpamPackage.Map.add
            (OpamPackage.create name v) (OpamFile.OPAM.read overlay) map
        else map)
      t.pinned t.opams
  in
  {
    u_packages  = t.installed ++ t.packages;
    u_action    = action;
    u_installed = t.installed;
    u_available = Lazy.force t.available_packages;
    u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends opams;
    u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts opams;
    u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams;
    u_installed_roots = t.installed_roots;
    u_pinned    = pinned_packages t;
  }

let check_base_packages t =
  let base_packages = get_compiler_packages t t.compiler in
  let missing_packages =
    List.filter
      (fun (name,_) -> not (is_name_installed t name))
      base_packages in
  if missing_packages <> [] then (
    let names = List.map (fst @> OpamPackage.Name.to_string) missing_packages in
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
    log "%a exists although %a is not installed. Removing it."
      (slog OpamFilename.Dir.to_string) dir
      (slog OpamPackage.to_string) nv;
    OpamFilename.rmdir dir
  )

let clean_file file nv =
  if OpamFilename.exists file then (
    log "%a exists although %a is not installed. Removing it."
      (slog OpamFilename.to_string) file
      (slog OpamPackage.to_string) nv;
    OpamFilename.remove file
  )

let global_dev_packages t =
  let dir = OpamPath.dev_packages_dir t.root in
  let dirs = OpamFilename.dirs dir in
  List.fold_left (fun map dir ->
      match OpamPackage.of_dirname dir with
      | None     ->
        OpamGlobals.note "Removing %s" (OpamFilename.Dir.to_string dir);
        OpamFilename.rmdir dir;
        map
      | Some nv  ->
        OpamPackage.Map.add nv dir map
    ) OpamPackage.Map.empty dirs

let switch_dev_packages t =
  List.fold_left (fun map dir ->
      try
        let name =
          OpamPackage.Name.of_string @@ OpamFilename.Base.to_string @@
          OpamFilename.basename_dir dir in
        OpamPackage.Map.add (pinned t name) dir map
      with Failure _ | Not_found ->
        OpamGlobals.note "Removing %s" (OpamFilename.Dir.to_string dir);
        OpamFilename.rmdir dir;
        map
    )
    OpamPackage.Map.empty
    (OpamFilename.dirs (OpamPath.Switch.dev_packages_dir t.root t.switch))

let keys map =
  OpamPackage.Map.fold (fun nv _ set ->
      OpamPackage.Set.add nv set
    ) map OpamPackage.Set.empty

let is_dev_package t nv =
  match url t nv with
  | None     -> false
  | Some url ->
    match OpamFile.URL.kind url with
    | `http  -> false
    | _      -> true

let dev_packages t =
  let global = global_dev_packages t in
  let switch = switch_dev_packages t in
  let all = keys global ++ keys switch in
  OpamPackage.Set.filter (is_dev_package t) all

(* Check that the dev packages are installed -- if not, just remove
   the temporary files. *)
let global_consistency_checks t =
  let pkgdir = OpamPath.dev_packages_dir t.root in
  let pkgdirs = OpamFilename.dirs pkgdir in
  let stale_pkgdirs =
    List.filter (fun dir ->
        match OpamPackage.of_dirname dir with
        | None -> true
        | Some nv ->
          not (OpamPackage.Set.mem nv t.installed) &&
          try OpamPackage.Name.Map.find (OpamPackage.name nv) t.pinned <>
              Version (OpamPackage.version nv)
          with Not_found -> true)
      pkgdirs
  in
  List.iter (fun d ->
      log "Stale dev directory %s, removing" (OpamFilename.Dir.to_string d);
      OpamFilename.rmdir d)
    stale_pkgdirs;
  List.iter (fun f ->
      log "Stale file %s found, Removing\n" (OpamFilename.to_string f);
      OpamFilename.remove f)
    (OpamFilename.files pkgdir);
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases t.root) in
  if OpamSwitch.Map.exists (fun _ c -> c =  OpamCompiler.system) aliases then
    let comp_f = OpamPath.compiler_comp t.root OpamCompiler.system in
    if not (OpamFilename.exists comp_f) then (
      OpamGlobals.msg "Regenerating the system compiler description.\n";
      create_system_compiler_description t.root (OpamCompiler.Version.system ());
    )

let switch_consistency_checks t =
  let cleanup_dir title dir filter =
    let dirs = OpamFilename.dirs dir in
    let stale_dirs =
      List.filter (fun d ->
          try
            let name =
              OpamPackage.Name.of_string
                (OpamFilename.Base.to_string (OpamFilename.basename_dir d)) in
            filter name
          with Failure _ -> true)
        dirs
    in
    List.iter (fun d ->
        log "Stale %s directory %s, removing" title (OpamFilename.Dir.to_string d);
        OpamFilename.rmdir d)
      stale_dirs;
    List.iter (fun f ->
        OpamGlobals.error "Removing %s.\n" (OpamFilename.to_string f);
        OpamFilename.remove f)
      (OpamFilename.files dir)
  in
  cleanup_dir "dev"
    (OpamPath.Switch.dev_packages_dir t.root t.switch)
    (fun name ->
       not (is_name_installed t name) && not (is_pinned t name));
  cleanup_dir "overlay"
    (OpamPath.Switch.Overlay.dir t.root t.switch)
    (fun name ->
       not (is_pinned t name) &&
       (not (is_name_installed t name) ||
        (* Don't cleanup installed packages which don't have any other metadata *)
        not (OpamPackage.Map.exists (fun nv _ -> OpamPackage.name nv = name) t.package_index)))

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
    log "Loaded %a in %.3fs" (slog OpamFilename.to_string) file (chrono ());
    Some cache.cached_opams
  with e ->
    OpamMisc.fatal e;
    log "Got an error while loading the cache: %a" (slog Printexc.to_string) e;
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
  log "%a written in %.3fs" (slog OpamFilename.prettify) file (chrono ())

let remove_state_cache () =
  let root = OpamPath.root () in
  let file = OpamPath.state_cache root in
  OpamFilename.remove file

let reinstall_system_compiler t =
  log "reinstall-system-compiler";
  let continue =
    OpamGlobals.confirm "Your system compiler has been upgraded. Do you want to upgrade \
             your OPAM installation?" in

  if continue then (

    (* Update system.comp *)
    create_system_compiler_description t.root (OpamCompiler.Version.system ());

    (* Reinstall all system compiler switches *)
    OpamSwitch.Map.iter (fun s a ->
      if a = OpamCompiler.system then (
        OpamGlobals.header_msg "Upgrading %s" (OpamSwitch.to_string s);
        !switch_reinstall_hook s
      )
    ) t.aliases

  ) else
    OpamGlobals.exit 1

let upgrade_to_1_1_hook =
  ref (fun () -> assert false)

let upgrade_to_1_2_hook =
  ref (fun () -> assert false)

let load_state ?(save_cache=true) call_site =
  log "LOAD-STATE(%s)" call_site;
  let chrono = OpamGlobals.timer () in
  !upgrade_to_1_1_hook ();

  let root = OpamPath.root () in

  let config_p = OpamPath.config root in
  let config =
    let config = OpamFile.Config.read config_p in
    let config_version = OpamFile.Config.opam_version config in
    if config_version <> OpamVersion.current then (
      (* opam has been updated, so refresh the configuration file and
         clean-up the cache. *)
      if OpamVersion.compare config_version (OpamVersion.of_string "1.2") < 0 then
        !upgrade_to_1_2_hook ();
      let config = OpamFile.Config.with_current_opam_version config in
      OpamFile.Config.write config_p config;
      remove_state_cache ();
      config
    ) else
      config in
  let solver_prefs =
    let config_crit =
      !OpamGlobals.solver_preferences @ OpamFile.Config.criteria config in
    let f kind =
      kind, try List.assoc kind config_crit
      with Not_found ->
        match OpamCudf.check_cudf_version () with
        | `Latest -> OpamGlobals.default_preferences kind
        | `Compat -> OpamGlobals.compat_preferences kind
    in
    [f `Default; f `Upgrade; f `Fixup]
  in
  OpamGlobals.solver_preferences := solver_prefs;
  OpamGlobals.external_solver :=
    OpamMisc.Option.Op.(
      !OpamGlobals.external_solver ++
      OpamFile.Config.solver config);

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
      log "%a does not contain the compiler name associated to the switch %a"
        (slog @@ OpamFilename.to_string @* OpamPath.aliases) root
        (slog OpamSwitch.to_string) switch;
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
  let installed =
    OpamFile.Installed.safe_read (OpamPath.Switch.installed root switch) in
  let pinned =
    OpamFile.Pinned.safe_read (OpamPath.Switch.pinned root switch) in
  let installed_roots =
    OpamFile.Installed_roots.safe_read (OpamPath.Switch.installed_roots root switch) in
  let opams = match opams with
    | None   ->
      let packages =
        OpamPackage.Set.of_list (OpamPackage.Map.keys package_index) ++
        installed in
      OpamPackage.Set.fold (fun nv map ->
          try
            let file =
              package_repo_dir root repositories package_index nv // "opam" in
            try
              let opam = OpamFile.OPAM.read file in
              OpamPackage.Map.add nv opam map
            with OpamFormat.Bad_format _
               | Lexer_error _ -> map (* Error printed, continue *)
          with
          | Not_found ->
            if not (OpamPackage.Name.Map.mem (OpamPackage.name nv) pinned) then
              OpamGlobals.warning "Cannot find an OPAM file for %s, skipping."
                (OpamPackage.to_string nv);
            map
          | Parsing.Parse_error | OpamSystem.Internal_error _ ->
            OpamGlobals.warning "Errors while parsing %s OPAM file, skipping."
              (OpamPackage.to_string nv);
            map
        ) packages OpamPackage.Map.empty
    | Some o -> o in
  let packages = OpamPackage.Set.of_list (OpamPackage.Map.keys opams) in
  let reinstall =
    OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall root switch) in
  let available_packages_stub = lazy OpamPackage.Set.empty in
  let t = {
    partial; root; switch; compiler; compiler_version; repositories; opams;
    packages; installed; installed_roots; reinstall;
    config; aliases; pinned; compilers;
    package_index; compiler_index;
    available_packages = available_packages_stub
  } in
  let packages = pinned_packages t ++ packages in
  let t = { t with packages } in
  let available_packages = lazy (available_packages t) in
  let t = { t with available_packages } in
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
  log "install_global_config switch=%a" (slog OpamSwitch.to_string) switch;

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
      ("user" , try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name with Not_found -> "user");
      ("group", try (Unix.getgrgid (Unix.getgid ())).Unix.gr_name with Not_found -> "group");
      ("make" , !OpamGlobals.makecmd ());
      ("os"   , OpamGlobals.os_string ());
    ] in

  let config = OpamFile.Dot_config.create vars in
  OpamFile.Dot_config.write
    (OpamPath.Switch.global_config root switch)
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
    let cwd = OpamFilename.cwd () in
    let () = OpamSystem.chdir OpamGlobals.home in

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

    OpamFilename.with_tmp_dir (fun tmp_dir ->
        let keep_compilers =
          OpamCompiler.Set.of_list (OpamSwitch.Map.values aliases) in
        (* Fix system.comp *)
        let backups =
          OpamCompiler.Set.fold (fun compname backups ->
              let comp =
                root / "compilers" // (OpamCompiler.to_string compname ^ ".comp") in
              if OpamFilename.exists comp then (
                let tmp_file =
                  OpamFilename.create tmp_dir (OpamFilename.basename comp) in
                log "backing up %a to %a"
                  (slog OpamFilename.to_string) comp
                  (slog OpamFilename.to_string) tmp_file;
                OpamFilename.move ~src:comp ~dst:tmp_file;
                (compname,tmp_file) :: backups
              )
              else backups
            ) keep_compilers [] in

        OpamFilename.rmdir compilers;

        List.iter (fun (compname,tmp_file) ->
            log "restoring %a" (slog OpamFilename.to_string) tmp_file;
            let comp = OpamPath.compiler_comp root compname in
            OpamFilename.mkdir (OpamFilename.dirname comp);
            OpamFilename.move ~src:tmp_file ~dst:comp
          ) backups;
        if not (OpamFilename.exists (OpamPath.compiler_comp root OpamCompiler.system))
        then create_system_compiler_description root (OpamCompiler.Version.system ())
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
              OpamFilename.rmdir (OpamPath.Switch.Overlay.package root switch name);
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

    let () =
      try OpamSystem.chdir (OpamFilename.Dir.to_string cwd)
      with OpamSystem.Internal_error _ -> () in

    OpamGlobals.header_msg
      "Upgrade complete. Now continuing with \"%s\""
      (String.concat " " (Array.to_list Sys.argv));
    OpamGlobals.msg "\n";
  )

let upgrade_to_1_2 () =
  log "Upgrade pinned packages format to 1.2";
  let root  = OpamPath.root () in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let remove_pinned_suffix d =
    let s = OpamFilename.Dir.to_string d in
    if Filename.check_suffix s ".pinned" then
      OpamFilename.move_dir ~src:d
        ~dst:(OpamFilename.Dir.of_string (Filename.chop_suffix s ".pinned"))
  in
  let packages = lazy (
    OpamPackage.Set.of_list
      (OpamPackage.Map.keys
         (OpamFile.Package_index.safe_read (OpamPath.package_index root)))
  ) in
  OpamSwitch.Map.iter (fun switch _ ->
    let pinned_version name =
      try
        let f = OpamPath.Switch.Overlay.opam root switch name in
        match OpamFile.OPAM.version_opt (OpamFile.OPAM.read f) with
        | None -> raise Not_found
        | Some v -> v
      with e ->
        OpamMisc.fatal e;
        try OpamPackage.version (OpamPackage.max_version (Lazy.force packages) name)
        with Not_found -> OpamPackage.Version.of_string "0" in
    let fix_version nv =
      let obsolete_pinned_v = OpamPackage.Version.of_string "pinned" in
      if OpamPackage.version nv = obsolete_pinned_v then
        let name = OpamPackage.name nv in
        OpamPackage.create name (pinned_version name)
      else nv in
    List.iter remove_pinned_suffix
      (OpamFilename.dirs (OpamPath.Switch.dev_packages_dir root switch));
    List.iter remove_pinned_suffix
      (OpamFilename.dirs (OpamPath.Switch.Overlay.dir root switch));
    let installed_f = OpamPath.Switch.installed root switch in
    let installed = OpamFile.Installed.safe_read installed_f in
    OpamFile.Installed.write installed_f
      (OpamPackage.Set.map fix_version installed);
    let installed_roots_f = OpamPath.Switch.installed_roots root switch in
    let installed_roots = OpamFile.Installed_roots.safe_read installed_roots_f in
    OpamFile.Installed_roots.write installed_roots_f
      (OpamPackage.Set.map fix_version installed_roots);
    (* Move .config files *)
    List.iter (fun f ->
        let name =
          OpamPackage.Name.of_string @@
          OpamFilename.Base.to_string @@
          OpamFilename.basename @@
          OpamFilename.chop_extension f in
        if name <> OpamPackage.Name.global_config then
          let dst = OpamPath.Switch.config root switch name in
          OpamFilename.mkdir (OpamFilename.dirname dst);
          OpamFilename.move ~src:f ~dst
      )
      (OpamFilename.files (OpamPath.Switch.config_dir root switch))
  ) aliases

let () =
  upgrade_to_1_1_hook := upgrade_to_1_1;
  upgrade_to_1_2_hook := upgrade_to_1_2

let rebuild_state_cache () =
  remove_state_cache ();
  let t = load_state ~save_cache:false "rebuild-cache" in
  save_state ~update:true t

let switch_eval_sh = "switch_eval.sh"
let complete_sh    = "complete.sh"
let complete_zsh   = "complete.zsh"
let variables_sh   = "variables.sh"
let variables_csh  = "variables.csh"
let variables_fish = "variables.fish"
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

let source t ~shell ?(interactive_only=false) f =
  let file f = OpamFilename.to_string (OpamPath.init t.root // f) in
  let s =
    match shell with
    | `csh ->
      Printf.sprintf "source %s >& /dev/null || true\n" (file f)
    | `fish ->
      Printf.sprintf ". %s > /dev/null 2> /dev/null or true\n" (file f)
    | _ ->
      Printf.sprintf ". %s > /dev/null 2> /dev/null || true\n" (file f)
  in
  if interactive_only then
    match shell with
    | `csh ->
      Printf.sprintf "if (tty -s >&/dev/null) then\n  %sendif\n" s
    | `fish ->
      Printf.sprintf "if tty -s >/dev/null 2>&1\n %send\n" s
    | _ ->
      Printf.sprintf "if tty -s >/dev/null 2>&1; then\n  %sfi\n" s
  else s

let expand_env t ?opam (env: env_updates) : env =
  let fenv = resolve_variable t ?opam OpamVariable.Map.empty in
  List.rev_map (fun (ident, symbol, string) ->
    let string = OpamFilter.substitute_string fenv string in
    let prefix = OpamFilename.Dir.to_string t.root in
    let read_env () =
      try OpamMisc.reset_env_value ~prefix OpamSystem.path_sep (OpamMisc.getenv ident)
      with Not_found -> [] in
    let update_env a =
      let before, after =
        OpamMisc.cut_env_value
          ~prefix OpamSystem.path_sep (OpamMisc.getenv ident)
      in
      List.rev_append before (a::after)
    in
    let cons ~head a b =
      let c = List.filter ((<>)"") b in
      match b with
      | []      -> if head then [ ""; a ] else [ a; "" ]
      | "" :: _ -> "" :: a :: c
      | _       ->
        match List.rev b with
        | "" :: _ -> (a :: c) @ [""]
        | _       -> a :: c in
    let c = String.make 1 OpamSystem.path_sep in
    match symbol with
    | "="  -> (ident, string)
    | "+=" -> (ident, String.concat c (string :: read_env ()))
    | "=+" -> (ident, String.concat c (read_env () @ [string]))
    | ":=" -> (ident, String.concat c (cons ~head:true string (read_env())))
    | "=:" -> (ident, String.concat c (cons ~head:false string (read_env())))
    | "=+=" -> (ident, String.concat c (update_env string))
    | _    -> failwith (Printf.sprintf "expand_env: %s is an unknown symbol" symbol)
  ) env

let add_to_env t ?opam (env: env) (updates: env_updates) =
  let env =
    List.filter (fun (k,_) -> List.for_all (fun (u,_,_) -> u <> k) updates) env in
  env @ expand_env t ?opam updates

let env_updates ~opamswitch t =
  let comp = compiler_comp t t.compiler in

  let add_to_path = OpamPath.Switch.bin t.root t.switch in
  let new_path = "PATH", "=+=", OpamFilename.Dir.to_string add_to_path in
  let perl5 = OpamPackage.Name.of_string "perl5" in
  let add_to_perl5lib =  OpamPath.Switch.lib t.root t.switch perl5 in
  let new_perl5lib = "PERL5LIB", "+=", OpamFilename.Dir.to_string add_to_perl5lib in
  let toplevel_dir =
    "OCAML_TOPLEVEL_PATH", "=",
    OpamFilename.Dir.to_string (OpamPath.Switch.toplevel t.root t.switch) in
  let man_path =
    match OpamGlobals.os () with
    | OpamGlobals.OpenBSD | OpamGlobals.NetBSD ->
      [] (* MANPATH is a global override on those, so disabled for now *)
    | _ ->
      ["MANPATH", "=:",
       OpamFilename.Dir.to_string (OpamPath.Switch.man_dir t.root t.switch)] in
  let utf8 =
    match OpamGlobals.os () with
    | OpamGlobals.Darwin -> ["OPAMUTF8MSGS", "=", "1"]
    | _ -> []
  in
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

  new_path :: toplevel_dir :: new_perl5lib ::
  (man_path @ switch @ root @ utf8 @ comp_env)

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

let get_full_env ?opam t =
  let env0 = OpamMisc.env () in
  add_to_env t ?opam env0 (env_updates ~opamswitch:true t)

let mem_pattern_in_string ~pattern ~string =
  let pattern = Re.compile (Re.str pattern) in
  Re.execp pattern string

let ocamlinit () =
  try
    let file = Filename.concat (OpamMisc.getenv "HOME") ".ocamlinit" in
    Some (OpamFilename.of_string file)
  with Not_found ->
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
      with e ->
        OpamMisc.fatal e;
        OpamSystem.internal_error "Cannot write ~/.ocamlinit."
  ) else
    OpamGlobals.msg "  ~/.ocamlinit is already up-to-date.\n"

let string_of_env_update t shell updates =
  let fenv = resolve_variable t OpamVariable.Map.empty in
  let sh   (k,v) = Printf.sprintf "%s=%s; export %s;\n" k v k in
  let csh  (k,v) = Printf.sprintf "setenv %s %S;\n" k v in
  let fish (k,v) = Printf.sprintf "set -gx %s %s\n" k v in
  let export = match shell with
    | `zsh
    | `sh  -> sh
    | `fish -> fish
    | `csh -> csh in
  let aux (ident, symbol, string) =
    let string = OpamFilter.substitute_string fenv string in
    let key, value = match symbol with
      | "="  -> (ident, string)
      | "+="
      | ":=" -> (ident, Printf.sprintf "%s:$%s" string ident)
      | "=:"
      | "=+" -> (ident, Printf.sprintf "$%s:%s" ident string)
      | "=+=" -> (ident, Printf.sprintf "%s:$%s" string ident)
      | _    -> failwith (Printf.sprintf "%s is not a valid env symbol" symbol) in
    export (key, value) in
  String.concat "" (List.rev_map aux updates)

let init_script t ~switch_eval ~complete ~shell (variables_sh, switch_eval_sh, complete_sh)=
  let variables =
    Some (source t ~shell variables_sh) in
  let switch_eval =
    if switch_eval then
      Some (source t ~shell ~interactive_only:true switch_eval_sh)
    else
      None in
  let complete =
    if complete then
      Some (source t ~shell ~interactive_only:true complete_sh)
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
        `sh,   init_sh ,  (variables_sh  , switch_eval_sh, complete_sh);
        `zsh,  init_zsh,  (variables_sh  , switch_eval_sh, complete_zsh);
        `csh,  init_csh,  (variables_csh , switch_eval_sh, complete_sh);
        `fish, init_fish, (variables_fish, switch_eval_sh, complete_sh);
      ] in
      let aux (shell, init, scripts) =
        init,
        init_script t ~shell ~switch_eval:g.switch_eval ~complete:g.complete scripts in
      List.map aux scripts in
  let scripts = [
    (complete_sh   , OpamScript.complete);
    (complete_zsh  , OpamScript.complete_zsh);
    (switch_eval_sh, OpamScript.switch_eval);
    (variables_sh  , string_of_env_update t `sh   (env_updates ~opamswitch:false t));
    (variables_csh , string_of_env_update t `csh  (env_updates ~opamswitch:false t));
    (variables_fish, string_of_env_update t `fish (env_updates ~opamswitch:false t));
  ] @
                init_scripts
  in
  let overwrite = [
    init_sh;
    init_csh;
    init_fish;
    init_zsh;
    variables_sh;
    variables_csh;
    variables_fish;
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
      with e -> OpamMisc.fatal e
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
      [ init_sh; init_zsh; init_csh; init_fish ]

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
        (OpamMisc.strip body) (source t ~shell init_file) in
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
  match OpamMisc.guess_shell_compat () with
  | `fish ->
    Printf.sprintf "eval (opam config env%s)\n" root
  | _ ->
    Printf.sprintf "eval `opam config env%s`\n" root

let up_to_date_env t =
  let changes =
    List.filter
      (fun (s, v) -> Some v <> try Some (OpamMisc.getenv s) with Not_found -> None)
      (get_opam_env t) in
  changes = []

let print_env_warning_at_init t user =
  if up_to_date_env t then ()
  else
    let profile_string = match user.dot_profile with
      | None -> ""
      | Some f ->
        Printf.sprintf
          "%s To correctly configure OPAM for subsequent use, add the following\n\
          \   line to your profile file (for instance %s):\n\
           \n\
          \      %s\n"
          (OpamGlobals.colorise `yellow "2.")
          (OpamFilename.prettify f)
          (source t ~shell:user.shell (init_file user.shell))
    in
    let ocamlinit_string =
      if not user.ocamlinit then "" else
        OpamGlobals.colorise `yellow "3." ^
        " To avoid issues related to non-system installations of `ocamlfind`\n\
        \   add the following lines to ~/.ocamlinit (create it if necessary):\n\
         \n\
        \      let () =\n\
        \        try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")\n\
        \        with Not_found -> ()\n\
        \      ;;\n\n"
    in
    let line =
      OpamGlobals.colorise `cyan
        "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
    in
    OpamGlobals.msg
      "\n%s\n\n\
       %s To configure OPAM in the current shell session, you need to run:\n\
       \n\
      \      %s\n\
       %s%s%s\n\n"
      line
      (OpamGlobals.colorise `yellow "1.")
      (eval_string ()) profile_string ocamlinit_string line

let print_env_warning_at_switch t =
  if up_to_date_env t then ()
  else
    OpamGlobals.msg
      "# To complete the configuration of OPAM, you need to run:\n%s"
      (eval_string ())

let update_setup_interactive t shell dot_profile =
  let update dot_profile =
    let modify_user_conf = dot_profile <> None in
    let user = Some { shell; ocamlinit = modify_user_conf; dot_profile } in
    let complete, switch_eval = match shell with
      | `fish | `csh -> false, false
      | _     -> true, true in
    let global = Some { complete ; switch_eval } in
    OpamGlobals.msg "\n";
    update_setup t user global;
    modify_user_conf in

  OpamGlobals.msg "\n";

  match OpamGlobals.read
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
      \    %s\n\
      \n\
      \  - %s to ensure that non-system installations of `ocamlfind`\n\
      \    (i.e. those installed by OPAM) will work correctly when running the\n\
      \    OCaml toplevel. It does this by adding $OCAML_TOPLEVEL_PATH to the list\n\
      \    of include directories.\n\
      \n\
       If you choose to not configure your system now, you can either configure\n\
       OPAM manually (instructions will be displayed) or launch the automatic setup\n\
       later by running:\n\
      \n\
       \   opam config setup -a\n\
       \n\
      \n\
       Do you want OPAM to modify %s and ~/.ocamlinit?\n\
       (default is 'no', use 'f' to name a file other than %s)\n\
      \    [N/y/f]"
      (OpamGlobals.colorise `cyan @@ OpamFilename.prettify dot_profile)
      (OpamGlobals.colorise `bold @@ string_of_shell shell)
      (source t ~shell (init_file shell))
      (OpamGlobals.colorise `cyan @@ "~/.ocamlinit")
      (OpamFilename.prettify dot_profile)
      (OpamFilename.prettify dot_profile)
  with
  | Some ("y" | "Y" | "yes"  | "YES" ) -> update (Some dot_profile)
  | Some ("f" | "F" | "file" | "FILE") ->
    begin match OpamGlobals.read "  Enter the name of the file to update:" with
      | None   ->
        OpamGlobals.msg "-- No filename: skipping the auto-configuration step --\n";
        false
      | Some f -> update (Some (OpamFilename.of_string f))
    end
  | _ -> update None

(* Add the given packages to the set of package to reinstall. If [all]
   is set, this is done for ALL the switches (useful when a package
   change upstream for instance). If not, only the reinstall state of the
   current switch is changed. *)
let add_to_reinstall t ~all packages =
  log "add-to-reinstall all:%b packages:%a" all
    (slog OpamPackage.Set.to_string) packages;
  let aux switch =
    let installed =
      OpamFile.Installed.safe_read (OpamPath.Switch.installed t.root switch) in
    let reinstall =
      OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall t.root switch) ++
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
  log "add_switch switch=%a compiler=%a"
    (slog OpamSwitch.to_string) switch
    (slog OpamCompiler.to_string) compiler;
  let aliases_f = OpamPath.aliases root in
  let aliases = OpamFile.Aliases.safe_read aliases_f in
  if not (OpamSwitch.Map.mem switch aliases) then begin
    OpamFile.Aliases.write aliases_f (OpamSwitch.Map.add switch compiler aliases);
  end

(* - compiles and install $opam/compiler/[ocaml_version].comp in $opam/[switch]
   - update $opam/switch
   - update $opam/config *)
let install_compiler t ~quiet switch compiler =
  log "install_compiler switch=%a compiler=%a"
    (slog OpamSwitch.to_string) switch
    (slog OpamCompiler.to_string) compiler;

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
        let kind = OpamFile.Comp.kind comp in
        if kind = `local
        && Sys.file_exists (fst comp_src)
        && Sys.is_directory (fst comp_src) then
          OpamFilename.link_dir
            ~src:(OpamFilename.Dir.of_string (fst comp_src)) ~dst:build_dir
        else OpamFilename.with_tmp_dir (fun download_dir ->
            let result =
              OpamRepository.pull_url kind (OpamPackage.of_string "compiler.get")
                download_dir None [comp_src] in
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
          let env = resolve_variable t OpamVariable.Map.empty in
          let builds = OpamFilter.commands env (OpamFile.Comp.build comp) in
          OpamFilename.exec build_dir builds
        end;
        if not !OpamGlobals.keep_build_dir then OpamFilename.rmdir build_dir
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

let has_empty_opam t nv =
  let f = OpamPath.Switch.Overlay.opam t.root t.switch (OpamPackage.name nv) in
  not (OpamFilename.exists f) ||
  OpamFile.OPAM.read f = OpamFile.OPAM.create nv

(* XXX split update_dev taking nv and update_pin taking name ? *)
let update_dev_package t nv =
  log "update-dev-package %a" (slog OpamPackage.to_string) nv;
  let name = OpamPackage.name nv in
  let nv, pinned =
    try pinned t name, true
    with Not_found -> nv, false in
  let skip = OpamPackage.Set.empty in
  match url t nv with
  | None     -> skip
  | Some url ->
    let remote_url = OpamFile.URL.url url in
    let mirrors = remote_url :: OpamFile.URL.mirrors url in
    let kind = OpamFile.URL.kind url in
    let srcdir = dev_package t nv in
    let fetch () =
      log "updating %a:%a"
        (slog string_of_address) remote_url
        (slog string_of_repository_kind) kind;
      let checksum = OpamFile.URL.checksum url in
      let r = OpamRepository.pull_url kind nv srcdir checksum mirrors in
      match r with
      | Not_available u ->
        OpamGlobals.error "Upstream %s of %s is unavailable" u (OpamPackage.to_string nv);
        skip
      | Up_to_date _    -> skip
      | Result _        -> OpamPackage.Set.singleton nv
    in
    if not pinned then
      if kind = `http then skip else fetch ()
    else
    (* XXX need to also consider updating metadata for version-pinned packages ? *)
    let overlay = OpamPath.Switch.Overlay.package t.root t.switch name in
    let version = OpamPackage.version nv in
    let pinning_kind =
      kind_of_pin_option (OpamPackage.Name.Map.find name t.pinned) in
    (* Four versions of the metadata: from the old and new versions
       of the package, from the current overlay, and also the original one
       from the repo *)
    let hash_meta (opam, descr, files_dir) =
      (match opam with None -> [] | Some o ->
        ["opam", `Opam o]) @
      (match descr with None -> [] | Some d ->
        ["descr", `Digest (OpamFilename.digest d)]) @
      (match files_dir with None -> [] | Some files_dir ->
        List.map (fun f ->
            OpamFilename.remove_prefix (OpamFilename.dirname_dir files_dir) f,
            `Digest (OpamFilename.digest f))
          (OpamFilename.rec_files files_dir))
    in
    let old_meta = (* Version previously present in the source *)
      if pinning_kind = `version then [] else
      hash_meta @@ local_opam ~version_override:false nv srcdir
    in
    let was_single_opam_file = OpamFilename.exists (srcdir // "opam") in
    let just_opam = List.filter (function (_, `Opam _) -> true | _ -> false) in
    let user_meta, user_version = (* Installed version (overlay) *)
      let opam,_,_ as files =
        local_opam ~root:true ~version_override:false nv overlay in
      hash_meta files,
      OpamMisc.Option.map OpamFile.OPAM.version opam
    in
    let repo_meta = (* Version from the repo *)
      let v = OpamMisc.Option.default version user_version in
      let nv = OpamPackage.create name v in
      try
        let dir = package_repo_dir t.root t.repositories t.package_index nv in
        hash_meta @@ local_opam ~root:true nv dir
      with Not_found ->
        hash_meta @@
        (Some (OpamFile.OPAM.create nv),
         None, None)
    in
    (* Do the update *)
    let result = fetch () in
    let new_meta = (* New version from the source *)
      hash_meta @@ local_opam ~version_override:false nv srcdir
    in
    let user_meta, old_meta, repo_meta =
      if OpamFilename.exists (srcdir // "opam") then
        (* Single opam file in the project src (ie not a directory):
           don't override other files, restrict to 'opam' *)
        just_opam user_meta, just_opam old_meta, just_opam repo_meta
      else user_meta, old_meta, repo_meta
    in
    let rec diff a b = match a,b with
      | (f1,h1)::r1, (f2,h2)::r2 ->
        if f1 < f2 then `Removed f1 :: diff r1 b
        else if f1 > f2 then `Added f2 :: diff a r2
        else if h1 = h2 then diff r1 r2
        else `Changed f1 :: diff r1 r2
      | l, [] -> List.map (fun (f,_) -> `Removed f) l
      | [], l -> List.map (fun (f,_) -> `Added f) l
    in
    let diff_to_string = function
      | `Removed f -> Printf.sprintf "%S was removed" f
      | `Added f -> Printf.sprintf "%S was added" f
      | `Changed f -> Printf.sprintf "The contents of %S changed" f
    in
    let install_meta dir rm_hash hash =
      let root =
        let d = dir / "opam" in
        if OpamFilename.exists_dir d then d else dir in
      List.iter (fun (f, _) -> OpamFilename.remove (overlay // f)) rm_hash;
      List.iter (fun (f,kind) -> match kind with
          | `Opam o -> OpamFile.OPAM.write (overlay // f) o
          | `Digest _ -> OpamFilename.copy_in ~root (root // f) overlay)
        hash
    in
    (* Metadata from the package changed *)
    if result <> skip && new_meta <> [] &&
       new_meta <> old_meta && new_meta <> user_meta
    then
      if old_meta = user_meta || repo_meta = user_meta ||
         user_meta = ["opam", `Opam (OpamFile.OPAM.create nv)] ||
         was_single_opam_file && old_meta = just_opam user_meta
      then
        (* No manual changes *)
        (OpamGlobals.msg "Installing new package description for %s from %s\n"
           (OpamPackage.to_string nv)
           (Filename.concat (string_of_address remote_url) "opam");
         install_meta srcdir user_meta new_meta)
      else if
        OpamGlobals.msg
          "Conflicting update of the metadata of %s from %s:\n  - %s\n"
          (OpamPackage.to_string nv) (string_of_address remote_url)
          (String.concat "\n  - "
             (List.map diff_to_string (diff user_meta new_meta)));
        OpamGlobals.confirm "\nOverride files in %s\n\
                            \  (there will be a backup) ?"
          (OpamFilename.Dir.to_string overlay)
      then (
        let bak =
          OpamPath.backup_dir t.root / (OpamPackage.to_string nv ^ ".bak") in
        OpamFilename.mkdir (OpamPath.backup_dir t.root);
        OpamFilename.rmdir bak;
        OpamFilename.copy_dir ~src:overlay ~dst:bak;
        OpamGlobals.msg "User metadata backed up in %s\n"
          (OpamFilename.Dir.to_string bak);
        install_meta srcdir user_meta new_meta;
      );
    result

let update_dev_packages t packages =
  log "update-dev-packages";
  let packages = OpamPackage.Set.elements packages in
  let updates =
    OpamPackage.Parallel.map_reduce_l 1 packages
      ~map:(fun nv -> update_dev_package t nv)
      ~merge:OpamPackage.Set.union
      ~init:OpamPackage.Set.empty in

  let global =
    OpamPackage.Set.of_list (OpamPackage.Map.keys (global_dev_packages t)) in
  add_to_reinstall t ~all:true (updates %% global);
  add_to_reinstall t ~all:false (updates -- global);
  updates

(* Try to download $name.$version+opam.tar.gz *)
let download_archive t nv =
  log "get_archive %a" (slog OpamPackage.to_string) nv;
  let dst = OpamPath.archive t.root nv in
  if OpamFilename.exists dst then Some dst else
  try
    let repo, _ = OpamPackage.Map.find nv t.package_index in
    let repo = find_repository t repo in
    match OpamRepository.pull_archive repo nv with
    | Not_available _ -> None
    | Up_to_date f
    | Result f        -> OpamFilename.copy ~src:f ~dst; Some dst
  with Not_found ->
    None

(* Download a package from its upstream source, using 'cache_dir' as cache
   directory. *)
let download_upstream t nv dirname =
  match url t nv with
  | None   -> None
  | Some u ->
    let remote_url = OpamFile.URL.url u in
    let mirrors = remote_url :: OpamFile.URL.mirrors u in
    let kind = OpamFile.URL.kind u in
    let checksum = OpamFile.URL.checksum u in
    match OpamRepository.pull_url kind nv dirname checksum mirrors with
    | Not_available u -> OpamGlobals.error_and_exit "%s is not available" u
    | Result f
    | Up_to_date f    -> Some f

let check f =
  let root = OpamPath.root () in

  if not (OpamFilename.exists_dir root)
  || not (OpamFilename.exists (OpamPath.config root)) then
    OpamGlobals.error_and_exit
      "Please run 'opam init' first to initialize the state of OPAM."
      (OpamFilename.Dir.to_string root);

  match f with

    | Global_lock f ->
      (* Take the global lock *)
      OpamFilename.with_flock (OpamPath.lock root) (fun () ->
          (* clean the log directory *)
          OpamFilename.cleandir (OpamPath.log root);
          (* XXX pass t to f so that it doesn't have to reload it ? *)
          let t = load_state "global-lock" in
          global_consistency_checks t;
          f ()
        ) ()

    | Read_lock f ->
      (* Global read lock *)
      OpamFilename.with_flock ~read:true (OpamPath.lock root) f ()

    | Switch_lock f ->
      (* Take a switch lock (and a global read lock). *)
      OpamFilename.with_flock ~read:true (OpamPath.lock root) (fun () ->
          let switch = match !OpamGlobals.switch with
            | `Command_line s | `Env s -> OpamSwitch.of_string s
            | `Not_set ->
              OpamFile.Config.switch
                (OpamFile.Config.read (OpamPath.config root))
          in
          let t = load_state "switch-lock" in
          switch_consistency_checks t;
          OpamFilename.with_flock (OpamPath.Switch.lock root switch) f ()
        ) ()

    | Global_with_switch_cont_lock f ->
      (* Take the global lock *)
      let global_lock = OpamPath.lock root in
      let switch, cont =
        OpamFilename.with_flock global_lock (fun () ->
            (* clean the log directory *)
            OpamFilename.cleandir (OpamPath.log root);
            let t = load_state "global-lock" in
            global_consistency_checks t;
            f ()
          ) ()
      in
      (* Could be safer to first get the next lock, but there seems to be no
         guarantee with flock that we can properly turn a write lock to a read
         lock without unlocking first. *)
      OpamFilename.with_flock ~read:true global_lock (fun () ->
          OpamFilename.with_flock (OpamPath.Switch.lock root switch) cont ()
        ) ()

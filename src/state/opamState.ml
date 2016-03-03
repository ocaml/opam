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
open OpamTypesBase
open OpamStd.Op
open OpamProcess.Job.Op
open OpamFilename.Op
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "STATE" fmt
let slog = OpamConsole.slog

let switch_reinstall_hook = ref (fun _ -> assert false)

module Types = struct
  type t = {
    partial: bool;
    (** General *)
    root: OpamPath.t;
    config: OpamFile.Config.t;
    aliases: OpamFile.Aliases.t;
    (** Repositories *)
    repositories: OpamFile.Repo_config.t repository_name_map;
    compilers: compiler_set;
    package_index: OpamFile.Package_index.t;
    compiler_index: OpamFile.Compiler_index.t;
    (** Current switch *)
    switch: switch;
    (** [compiler] and [compiler_version] are obsolete, to be replaced by
        [compiler_packages] *)
    compiler: compiler;
    compiler_version: compiler_version lazy_t;
    compiler_packages: package_set;
    switch_config: OpamFile.Dot_config.t;
    opams: OpamFile.OPAM.t package_map;
    packages: package_set;
    available_packages: package_set Lazy.t;
    pinned: pin_option name_map;
    installed: package_set;
    installed_roots: package_set;
    reinstall: package_set;
  }
end

type state = Types.t
open Types

let string_of_repositories r =
  OpamStd.List.to_string
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
  OpamPackage.packages_of_name t.packages name

let packages_of_atoms t atoms =
  let check_atoms nv =
    let name = OpamPackage.name nv in
    let atoms = List.filter (fun (n,_) -> n = name) atoms in
    atoms <> [] && List.for_all (fun a -> OpamFormula.check a nv) atoms in
  (* All packages satisfying [atoms] *)
  OpamPackage.Set.filter check_atoms t.packages ++
  OpamPackage.Set.filter check_atoms t.installed

let installed_map t =
  OpamPackage.Name.Map.map OpamPackage.Version.Set.choose_one
    (OpamPackage.to_map t.installed)

let global_config t =
  t.switch_config

let dot_config t name =
  OpamFile.Dot_config.safe_read
    (OpamPath.Switch.config t.root t.switch t.switch_config name)

let is_package_installed t nv =
  OpamPackage.Set.mem nv t.installed

let jobs _ = Lazy.force OpamStateConfig.(!r.jobs)

let dl_jobs _ = OpamStateConfig.(!r.dl_jobs)

let sorted_repositories t =
  OpamRepository.sort t.repositories

let mem_repository t repo_name =
  OpamRepositoryName.Map.mem repo_name t.repositories

let find_repository_aux repo_name repositories =
  try OpamRepositoryName.Map.find repo_name repositories
  with Not_found ->
    OpamConsole.error_and_exit
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
      OpamRepository.url_checksum url
      @ OpamFilename.checksum_dir files
      @ OpamFilename.checksum archive
    | `partial false ->
      OpamRepository.url_checksum url
      @ OpamFilename.checksum_dir files

let all_installed t =
  OpamSwitch.Map.fold (fun switch _ acc ->
      (OpamFile.State.safe_read (OpamPath.Switch.state t.root switch))
      .OpamFile.State.installed
      ++ acc
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
  let exists_archive = OpamFilename.exists (OpamRepositoryPath.archive repo nv) in
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
  match OpamPackage.Name.Map.find n t.pinned with
  | Version _ -> OpamSystem.internal_error "locally pinned"
  | Source url -> url

let url_of_locally_pinned_package t n =
  OpamFile.URL.create (locally_pinned_package t n)

(* Returns the directory holding the original metadata of the package.
   This is a low-level function, you generally want to handle different
   locations, like overlays for pinned packages *)
let package_repo_dir root repositories package_index nv =
  if OpamFilename.exists (OpamPath.opam root nv) then
    OpamPath.packages root nv
  else
  let repo_name, prefix = OpamPackage.Map.find nv package_index in
  let repo = OpamRepositoryName.Map.find repo_name repositories in
  OpamRepositoryPath.packages repo prefix nv

(* Copies package definition from the repository to the overlay *)
let add_pinned_overlay ?(template=false) ?version t name =
  let open OpamFile in
  let module Ov = OpamPath.Switch.Overlay in
  log "Add pinning overlay for %a (template:%b, version:%a)"
    (slog OpamPackage.Name.to_string) name template
    OpamStd.Option.Op.(
      slog @@ fun v -> (v >>| OpamPackage.Version.to_string) +! "none"
    ) version;
  let pkg_overlay f = f t.root t.switch name in
  let get_orig_meta rv =
    let orig = package_repo_dir t.root t.repositories t.package_index rv in
    let files = OpamFilename.rec_files orig in
    let opam_f = orig // "opam" in
    let url_f = orig // "url" in
    let files = List.filter (fun f -> f <> opam_f && f <> url_f) files in
    let opam = OPAM.read opam_f in
    let url =
      try Some (URL.read url_f)
      with e ->
        OpamStd.Exn.fatal e;
        OPAM.url opam
    in
    opam, url, orig, files
  in
  try match OpamPackage.Name.Map.find name t.pinned with
    | Version v ->
      let opam, url, root, files = get_orig_meta (OpamPackage.create name v) in
      List.iter (fun f -> OpamFilename.copy_in ~root f (pkg_overlay Ov.package))
        files;
      OPAM.write (pkg_overlay Ov.opam)
        (OPAM.with_version (OPAM.with_url_opt opam None) v);
      OpamStd.Option.iter (URL.write (pkg_overlay Ov.url)) url
    | _ ->
      let nv = OpamStd.Option.map (OpamPackage.create name) version in
      let rv = (* repo version *)
        match nv with
        (* Lookup in package_index to ignore pinned versions *)
        | Some nv when OpamPackage.Map.mem nv t.package_index -> nv
        | _ ->
          let versions =
            OpamPackage.Map.fold (fun nv _ acc ->
                if OpamPackage.name nv = name then
                  OpamPackage.Set.add nv acc
                else acc)
              t.package_index OpamPackage.Set.empty
          in
          OpamPackage.max_version versions name (* raises Not_found *)
      in
      let v = OpamStd.Option.default (OpamPackage.version rv) version in
      let opam, _url, root, files = get_orig_meta rv in
      let url = url_of_locally_pinned_package t name in
      List.iter (fun f -> OpamFilename.copy_in ~root f (pkg_overlay Ov.package))
        files;
      OPAM.write (pkg_overlay Ov.opam)
        (OPAM.with_version (OPAM.with_url_opt opam None) v);
      URL.write (pkg_overlay Ov.url) url
  with Not_found -> (* No original meta *)
    let url = url_of_locally_pinned_package t name in
    let version =
      OpamStd.Option.default
        (OpamPackage.Version.of_string
           (if template then "0.1" else "~unknown"))
        version
    in
    let nv = OpamPackage.create name version in
    let opam = if template then OPAM.template nv else OPAM.create nv in
    OPAM.write (pkg_overlay (if template then Ov.tmp_opam else Ov.opam)) opam;
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
      OpamConsole.warning "Setting missing version in %s to %s"
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

let get_package t name =
  try pinned t name with Not_found ->
  try find_installed_package_by_name t name with Not_found ->
  try OpamPackage.max_version (Lazy.force t.available_packages) name
  with Not_found ->
    OpamPackage.max_version t.packages name

let is_locally_pinned t name =
  try match OpamPackage.Name.Map.find name t.pinned with
    | Version _ -> false
    | _ -> true
  with Not_found -> false

let read_opam dir =
  let opam_file = dir // "opam" in
  let url_file = dir // "url" in
  let descr_file = dir // "descr" in
  let opam =
    try Some (OpamFile.OPAM.read opam_file) with
    | OpamSystem.Internal_error _ | Not_found -> None
    | Parsing.Parse_error | OpamFormat.Bad_format _ | Lexer_error _ ->
      OpamConsole.warning "Errors while parsing %s, skipping."
        (OpamFilename.to_string opam_file);
      None
  in
  match opam with
  | Some opam ->
    let opam =
      if OpamFilename.exists url_file then
        try OpamFile.OPAM.with_url opam (OpamFile.URL.read url_file)
        with e ->
          OpamStd.Exn.fatal e;
          OpamConsole.warning "Errors while parsing %s, skipping."
            (OpamFilename.to_string url_file);
          opam
      else opam
    in
    let opam =
      if OpamFilename.exists descr_file then
        try OpamFile.OPAM.with_descr opam (OpamFile.Descr.read descr_file)
        with e ->
          OpamStd.Exn.fatal e;
          OpamConsole.warning "Errors while parsing %s, skipping."
            (OpamFilename.to_string descr_file);
          opam
      else opam
    in
    Some opam
  | None -> None

let opam_opt t nv =
  let name = OpamPackage.name nv in
  let base () =
    try Some (OpamPackage.Map.find nv t.opams)
    with Not_found ->
      if OpamPackage.Set.mem nv t.installed then
        (OpamConsole.warning "no package description left for %s"
           (OpamPackage.to_string nv);
         Some (OpamFile.OPAM.create nv))
      else
        None
  in
  let overlay_dir = OpamPath.Switch.Overlay.package t.root t.switch name in
  let overlay = OpamPath.Switch.Overlay.opam t.root t.switch name in
  if OpamFilename.exists overlay then
    match read_opam overlay_dir with
    | Some o ->
      if OpamFile.OPAM.version o = OpamPackage.version nv then Some o
      else if OpamPackage.Map.mem nv t.opams then
        (log "Looking for %s which is pinned to %s (not using overlay)"
           (OpamPackage.to_string nv) (OpamPackage.Version.to_string (OpamFile.OPAM.version o));
         base ())
      else
        (log "Opam file for %s not found: using the overlay even if it's for %s"
           (OpamPackage.to_string nv) (OpamPackage.Version.to_string (OpamFile.OPAM.version o));
         Some (OpamFile.OPAM.with_version o (OpamPackage.version nv)))
    | None -> base ()
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
    OpamPath.Switch.Overlay.descr OpamPath.descr OpamRepositoryPath.descr
    OpamFilename.exists

let url_file =
  locate_meta
    OpamPath.Switch.Overlay.url OpamPath.url OpamRepositoryPath.url
    OpamFilename.exists

let files =
  locate_meta
    OpamPath.Switch.Overlay.files OpamPath.files OpamRepositoryPath.files
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

let find_opam_file_in_source name dir =
  List.fold_left (function
      | None -> fun f -> if OpamFilename.exists f then Some f else None
      | some -> fun _ -> some)
    None
    [ dir / (OpamPackage.Name.to_string name ^ ".opam") // "opam";
      dir // (OpamPackage.Name.to_string name ^ ".opam");
      dir / "opam" // "opam";
      dir // "opam" ]

(* Returns [opam, descr_file, files_dir]. We don't consider [url] since
   this is for pinned packages. if [root], don't look for a subdir [opam]
   to find [files] and [descr]. *)
let local_opam ?(root=false) ?fixed_version ?(check=false) ?copy_invalid_to
    name dir =
  let has_dir d = if OpamFilename.exists_dir d then Some d else None in
  let has_file f = if OpamFilename.exists f then Some f else None in
  let metadir =
    if root then dir else
    match has_dir (dir / (OpamPackage.Name.to_string name ^ ".opam")) with
    | Some d -> d
    | None -> dir / "opam"
  in
  let opam_file =
    if root then has_file (dir // "opam")
    else find_opam_file_in_source name dir
  in
  let opam_opt = match opam_file with
    | None -> None
    | Some local_opam ->
      let warns, opam_opt = OpamFile.OPAM.validate_file local_opam in
      if check && warns <> [] then
        (OpamConsole.warning
           "%s opam file from upstream of %s (fix with 'opam pin edit'):"
           (if opam_opt = None then "Fatal errors, not using"
            else "Failed checks in")
           (OpamConsole.colorise `bold (OpamPackage.Name.to_string name));
         OpamConsole.errmsg "%s\n"
           (OpamFile.OPAM.warns_to_string warns));
      (match opam_opt, copy_invalid_to with
       | None, Some dst ->
         if not check then
           OpamConsole.warning
             "Errors in opam file from %s upstream, ignored (fix with \
              'opam pin edit')"
             (OpamPackage.Name.to_string name);
         OpamFilename.copy ~src:local_opam ~dst:dst
       | _ -> ());
      OpamStd.Option.map
        (fun opam ->
           let opam = OpamFile.OPAM.with_name opam name in
           let opam = match fixed_version with
             | None -> opam
             | Some v -> OpamFile.OPAM.with_version opam v
           in
           opam)
        opam_opt
  in
  opam_opt, has_file (metadir // "descr"), has_dir (metadir / "files")


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
         OpamStd.Exn.fatal e;
         if OpamFormatConfig.(!r.strict) then
           OpamConsole.error_and_exit "Invalid pinned package %s"
             (OpamPackage.Name.to_string name)
         else
           OpamConsole.error "Ignoring invalid pinned package %s"
             (OpamPackage.Name.to_string name);
         acc)
    t.pinned OpamPackage.Set.empty

let descr_opt t nv =
  OpamStd.Option.Op.(opam_opt t nv >>= OpamFile.OPAM.descr)

let descr t nv =
  OpamStd.Option.Op.(descr_opt t nv +! OpamFile.Descr.empty)

let url t nv =
  OpamStd.Option.Op.(opam_opt t nv >>= OpamFile.OPAM.url)

let global_dev_packages t =
  let dir = OpamPath.dev_packages_dir t.root in
  let dirs = OpamFilename.dirs dir in
  List.fold_left (fun map dir ->
      match OpamPackage.of_dirname dir with
      | None     ->
        OpamConsole.note "Removing %s" (OpamFilename.Dir.to_string dir);
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
        OpamConsole.note "Removing %s" (OpamFilename.Dir.to_string dir);
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
  | Some url -> (OpamFile.URL.url url).OpamUrl.backend <> `http

let dev_packages t =
  let global = global_dev_packages t in
  let switch = switch_dev_packages t in
  let all = keys global ++ keys switch in
  OpamPackage.Set.filter (is_dev_package t) all

(* For documentation *)
let global_variable_names = [
  "ocaml-version",        "The version of the currently used OCaml compiler";
  "opam-version",         "The currently running OPAM version";
  "compiler",             "The name of the current OCaml compiler (may be more \
                           specific than the version, eg: \"4.01.0+fp\", or \
                           \"system\")";
  "preinstalled",         "Whether the compiler was preinstalled on the system, \
                           or installed by OPAM";
  "switch",               "The local name (alias) of the current switch";
  "jobs",                 "The number of parallel jobs set up in OPAM \
                           configuration";
  "ocaml-native",         "Whether the OCaml native compilers are available";
  "ocaml-native-tools",   "Whether the native \".opt\" version of the OCaml \
                           toolchain is available";
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

(* Read the env variables *)
let get_env_var v =
  let var_str = OpamVariable.to_string (OpamVariable.Full.variable v) in
  let var_str = OpamStd.String.map (function '-' -> '_' | c -> c) var_str in
  let var_hook =
    match OpamVariable.Full.package v with
    | Some n ->
      Printf.sprintf "%s_%s" (OpamPackage.Name.to_string n) var_str
    | None -> var_str
  in
  try match OpamStd.Env.get ("OPAMVAR_" ^ var_hook) with
    | "true"  | "1" -> bool true
    | "false" | "0" -> bool false
    | s             -> string s
  with Not_found -> None

(* filter handling *)
let rec resolve_variable t ?opam:opam_arg local_variables v =
  let dirname dir = string (OpamFilename.Dir.to_string dir) in
  let pkgname = OpamStd.Option.map OpamFile.OPAM.name opam_arg in
  let read_var v =
    let get c =
      try OpamFile.Dot_config.variable c (OpamVariable.Full.variable v)
      with Not_found -> None
    in
    match OpamVariable.Full.scope v with
    | OpamVariable.Full.Global -> get (global_config t)
    | OpamVariable.Full.Package n -> get (dot_config t n)
    | OpamVariable.Full.Self ->
      OpamStd.Option.Op.(pkgname >>| dot_config t >>= get)
  in
  let get_local_var v =
    match OpamVariable.Full.package v with
    | Some _ -> None
    | None ->
      let var = OpamVariable.Full.variable v in
      try match OpamVariable.Map.find var local_variables with
        | None -> raise Exit (* Variable explicitly undefined *)
        | some -> some
      with Not_found -> None
  in
  let get_features_var opam v =
    let to_str opam =
      OpamPackage.to_string @@
      OpamPackage.create (OpamFile.OPAM.name opam) (OpamFile.OPAM.version opam)
    in
    let features = OpamFile.OPAM.features opam in
    try
      let v, _descr, filt = List.find (fun (id,_,_) -> id = v) features in
      let local_variables = (* Avoid recursion *)
        OpamVariable.Map.add v None local_variables in
      try Some (OpamFilter.eval (resolve_variable t ~opam local_variables) filt)
      with Failure _ ->
        OpamConsole.warning "Feature %s of %s didn't resolve%s"
          (OpamVariable.to_string v) (to_str opam)
          (match opam_arg with None -> "" | Some o ->
            Printf.sprintf " (referred to from %s)" (to_str o));
        None
    with Not_found -> None
  in
  let get_global_var v =
    if not (OpamVariable.Full.is_global v) then None else
    let preinstalled_comp = OpamFile.Comp.preinstalled (compiler_comp t t.compiler) in
    match OpamVariable.to_string (OpamVariable.Full.variable v) with
    | "ocaml-version" -> string (OpamCompiler.Version.to_string
                                   (Lazy.force t.compiler_version))
    | "opam-version"  -> string (OpamVersion.to_string OpamVersion.current)
    | "compiler" -> string (OpamCompiler.to_string t.compiler)
    | "preinstalled"  -> bool (OpamFile.Comp.preinstalled
                                 (compiler_comp t t.compiler))
    | "switch"        -> string (OpamSwitch.to_string t.switch)
    | "jobs"          -> int (jobs t)
    | "ocaml-native"  ->
      if preinstalled_comp then
        bool (Lazy.force OpamOCaml.ocaml_native_available)
      else
        bool (OpamFilename.exists
                (OpamPath.Switch.bin t.root t.switch t.switch_config
                 // "ocamlopt"))
    | "ocaml-native-tools" ->
      if preinstalled_comp then
        bool (Lazy.force OpamOCaml.ocaml_opt_available)
      else
        bool (OpamFilename.exists
                (OpamPath.Switch.bin t.root t.switch t.switch_config
                 // "ocamlc.opt"))
    | "ocaml-native-dynlink" ->
      if preinstalled_comp then
        bool (Lazy.force OpamOCaml.ocaml_natdynlink_available)
      else
        bool (OpamFilename.exists
                (OpamPath.Switch.lib_dir t.root t.switch t.switch_config
                 / "ocaml" // "dynlink.cmxa"))
    | "arch"          -> string (OpamStd.Sys.arch ())
    | _               -> None
  in
  let get_package_var v =
    if OpamVariable.Full.is_global v then None else
    let var_str = OpamVariable.to_string (OpamVariable.Full.variable v) in
    let name =
      match OpamVariable.Full.scope v with
      | OpamVariable.Full.Global -> assert false
      | OpamVariable.Full.Package n -> n
      | OpamVariable.Full.Self ->
        match pkgname with Some n -> n | None -> raise Exit
    in
    let opam = (* ensure opam, if not None, corresponds to name *)
      match opam_arg with
      | Some o when OpamFile.OPAM.name o = name -> opam_arg
      | _ ->
        try opam_opt t (find_installed_package_by_name t name)
        with Not_found -> None
    in
    let feat = match opam with
      | Some o -> get_features_var o (OpamVariable.Full.variable v)
      | None -> None in
    if feat <> None then feat else
    let get_nv opam = OpamPackage.create name (OpamFile.OPAM.version opam) in
    match var_str, opam with
    | "installed", Some _    -> bool (is_name_installed t name)
    | "installed", None      -> bool false
    | "pinned",    _         -> bool    (OpamPackage.Name.Map.mem name t.pinned)
    | "name",      _         ->
      if OpamPackage.has_name t.packages name
      then string (OpamPackage.Name.to_string name)
      else None
    | _,           None      -> None
    | "bin",       _         -> dirname (OpamPath.Switch.bin     t.root t.switch t.switch_config)
    | "sbin",      _         -> dirname (OpamPath.Switch.sbin    t.root t.switch t.switch_config)
    | "lib",       _         -> dirname (OpamPath.Switch.lib     t.root t.switch t.switch_config name)
    | "man",       _         -> dirname (OpamPath.Switch.man_dir t.root t.switch t.switch_config)
    | "doc",       _         -> dirname (OpamPath.Switch.doc     t.root t.switch t.switch_config name)
    | "share",     _         -> dirname (OpamPath.Switch.share   t.root t.switch t.switch_config name)
    | "etc",       _         -> dirname (OpamPath.Switch.etc     t.root t.switch t.switch_config name)
    | "build",     Some opam ->
      dirname (OpamPath.Switch.build t.root t.switch (get_nv opam))
    | "version",   Some opam ->
      let ver = OpamFile.OPAM.version opam in
      string (OpamPackage.Version.to_string ver)
    | "depends",   Some opam ->
      let dev = is_dev_package t (get_nv opam)  in
      let deps =
        OpamFormula.atoms (OpamStateConfig.filter_deps ~dev (OpamFile.OPAM.depends opam)) @
        OpamFormula.atoms (OpamStateConfig.filter_deps ~dev (OpamFile.OPAM.depopts opam))
      in
      let installed_deps =
        OpamStd.List.filter_map
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
      string (OpamStd.List.concat_map " "  OpamPackage.to_string installed_deps)
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
    match OpamVariable.Full.is_global v, pkgname with
    | true, Some name ->
      OpamVariable.Full.create name (OpamVariable.Full.variable v)
    | _ -> v
  in
  let skip _ = None in
  let v' = make_package_local v in
  let contents =
    try
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
          get_package_var, v';
        ]
    with Exit -> None
  in
  contents

let filter_env ?opam ?(local_variables=OpamVariable.Map.empty) t =
  resolve_variable t ?opam local_variables

let redirect t repo =
  if repo.repo_url.OpamUrl.backend <> `http then None else
  let redirect =
    repo
    |> OpamRepositoryPath.repo
    |> OpamFile.Repo.safe_read
    |> OpamFile.Repo.redirect
  in
  let redirect = List.fold_left (fun acc (redirect, filter) ->
      if OpamFilter.opt_eval_to_bool (filter_env t) filter
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
      os $ OpamStd.Sys.os_string () in
    OpamFormula.eval atom f

let consistent_available_field t opam =
  OpamFilter.eval_to_bool ~default:false (filter_env ~opam t)
    (OpamFile.OPAM.available opam)

let quick_var_lookup v =
  if OpamVariable.Full.is_global v then (
    let var = OpamVariable.Full.variable v in
    let root = OpamStateConfig.(!r.root_dir) in
    let switch = OpamStateConfig.(!r.current_switch) in
    let config_f = OpamPath.Switch.global_config root switch in
    let config = OpamFile.Dot_config.read config_f in
    match get_env_var v with
    | Some _ as c -> c
    | None ->
      if OpamVariable.to_string var = "switch" then
        Some (S (OpamSwitch.to_string switch))
      else
        OpamFile.Dot_config.variable config var
  ) else
    None

let contents_of_variable t v =
  log "config-variable";
  match quick_var_lookup v with
  | Some c -> c
  | None   ->
    let env = filter_env (Lazy.force t) in
    let default = S "#undefined" in
    OpamFilter.ident_value env ~default (OpamFilter.ident_of_var v)

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
            (OpamFilter.to_string (OpamFile.OPAM.available opam))
        ]) in
    match r with
    | [] -> raise Not_found
    | [r] -> " because " ^ r ^ "."
    | rs -> " because:\n" ^ OpamStd.Format.itemize ~bullet:"    - " (fun x -> x) rs in
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
      (match pin with
       | Version v ->
         Printf.sprintf "version %s" (OpamPackage.Version.to_string v)
       | _ ->
         Printf.sprintf "%s, version %s" (string_of_pin_option pin)
           (OpamPackage.Version.to_string (version_of_pin t name pin)))
  with Not_found ->
    unknown_package t atom

let static_base_packages =
  List.map OpamPackage.Name.of_string [ "base-unix"; "base-bigarray"; "base-threads" ]

let create_system_compiler_description root =
  match Lazy.force OpamOCaml.system_compiler with
  | None         -> ()
  | Some current ->
    log "create-system-compiler-description %a"
      (slog OpamCompiler.to_string) current;
    match Lazy.force OpamOCaml.system_ocamlc_where with
    | None     -> log "System compiler found but didn't answer to -where ??"
    | Some libdir ->
      let name = OpamCompiler.system in
      (* XXX legacy name: should be [current], but the change would have lots of
         implications *)
      let version = OpamCompiler.version current in
      let comp = OpamPath.compiler_comp root name in
      OpamFilename.remove comp;
      let f =
        OpamFile.Comp.create_preinstalled name version
          (if OpamStateConfig.(!r.no_base_packages) then []
           else static_base_packages)
          [ "CAML_LD_LIBRARY_PATH", Eq,
            "%{lib}%/stublibs" ^ String.make 1 (OpamStd.Sys.path_sep ()) ^
            Filename.concat libdir "stublibs", None ] in
      OpamFile.Comp.write comp f

let system_needs_upgrade_displayed = ref false
let system_needs_upgrade t =
  t.compiler = OpamCompiler.system
  && match Lazy.force OpamOCaml.system_compiler with
  | None   ->
    if not !system_needs_upgrade_displayed then (
      system_needs_upgrade_displayed := true;
      OpamConsole.error
        "You current switch uses the system compiler, but no OCaml compiler \
         has been found in the current path.\n\
         You should either:\n\
        \   (i) reinstall OCaml on your system; or\n\
        \  (ii) use 'opam switch <version>' to use a local OCaml compiler."
    );
    false
  | Some c ->
    OpamFilename.exists (OpamPath.compiler_comp t.root t.compiler)
    (* XXX name of the compiler is replaced with 'system' for legacy reasons,
       so we'll skim over differences in suffix-versions! *)
    && Lazy.force t.compiler_version <> OpamCompiler.version c

let read_repositories root config =
  let names = OpamFile.Config.repositories config in
  List.fold_left (fun map repo_name ->
    let repo = OpamFile.Repo_config.read
        (OpamRepositoryPath.raw_config root repo_name) in
    OpamRepositoryName.Map.add repo_name repo map
  ) OpamRepositoryName.Map.empty names

module Cache = struct
  type t = {
    cached_opams: (package * OpamFile.OPAM.t) list;
  }

  let check_marshaled_file file =
    let ic = open_in_bin (OpamFilename.to_string file) in
    let this_magic = OpamVersion.magic () in
    let magic_len = String.length this_magic in
    let file_magic =
      let b = Bytes.create magic_len in
      really_input ic b 0 magic_len;
      Bytes.to_string b in
    if file_magic <> this_magic then (
      close_in ic;
      OpamConsole.note
        "Clearing cache (wrong magic string %s, expected %s)."
        file_magic this_magic;
      None
    ) else
    let header = Bytes.create Marshal.header_size in
    really_input ic header 0 Marshal.header_size;
    let expected_size = magic_len + Marshal.total_size header 0 in
    let current_size = in_channel_length ic in
    if expected_size <> current_size then (
      close_in ic;
      OpamConsole.note "Clearing cache (wrong length %d, expected %d)."
        current_size expected_size;
      None
    ) else (
      seek_in ic magic_len;
      Some ic
    )

  let marshal_from_file file =
    let chrono = OpamConsole.timer () in
    match check_marshaled_file file with
    | Some ic ->
      let (cache: t) = Marshal.from_channel ic in
      close_in ic;
      log "Loaded %a in %.3fs" (slog OpamFilename.to_string) file (chrono ());
      Some (OpamPackage.Map.of_list cache.cached_opams)
    | None ->
      log "Invalid cache, removing";
      OpamFilename.remove file;
      None

  let load root =
    let file = OpamPath.state_cache root in
    if OpamFilename.exists file
    then marshal_from_file file
    else None

  let save t =
    let chrono = OpamConsole.timer () in
    let file = OpamPath.state_cache t.root in
    assert (OpamPackage.Map.is_empty t.package_index || not (OpamPackage.Map.is_empty t.opams));
    OpamFilename.remove file;
    log "Writing the cache of metadata to %s ...\n"
      (OpamFilename.prettify file);
    let oc = open_out_bin (OpamFilename.to_string file) in
    output_string oc (OpamVersion.magic ());
    Marshal.to_channel oc { cached_opams = OpamPackage.Map.bindings t.opams }
      [Marshal.No_sharing];
    close_out oc;
    log "%a written in %.3fs" (slog OpamFilename.prettify) file (chrono ())

  let remove () =
    let root = OpamStateConfig.(!r.root_dir) in
    let file = OpamPath.state_cache root in
    OpamFilename.remove file

end

let empty = {
  partial = true;
  root = OpamFilename.Dir.of_string "";
  switch = OpamSwitch.system;
  compiler = OpamCompiler.system;
  compiler_version = lazy (OpamCompiler.Version.of_string "none");
  compiler_packages = OpamPackage.Set.empty;
  switch_config = OpamFile.Dot_config.empty;
  opams = OpamPackage.Map.empty;
  repositories = OpamRepositoryName.Map.empty;
  packages = OpamPackage.Set.empty;
  available_packages = lazy OpamPackage.Set.empty;
  aliases = OpamFile.Aliases.empty;
  compilers = OpamCompiler.Set.empty;
  pinned = OpamPackage.Name.Map.empty;
  installed = OpamPackage.Set.empty;
  installed_roots = OpamPackage.Set.empty;
  reinstall = OpamPackage.Set.empty;
  config = OpamFile.Config.empty;
  package_index = OpamPackage.Map.empty;
  compiler_index = OpamCompiler.Map.empty;
}

let upgrade_to_1_1_hook =
  ref (fun () -> assert false)

let upgrade_to_1_2 () =
  if OpamCoreConfig.(!r.safe_mode) then
    OpamConsole.error_and_exit "Safe mode: not upgrading from opamroot <1.2";
  log "Upgrade pinned packages format to 1.2";
  let root  = OpamStateConfig.(!r.root_dir) in
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
        OpamStd.Exn.fatal e;
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
    let switch_prefix = OpamPath.Switch.root root switch in
    let installed_f = OpamFilename.Op.(switch_prefix // "installed") in
    let installed = OpamFile.PkgList.safe_read installed_f in
    OpamFile.PkgList.write installed_f
      (OpamPackage.Set.map fix_version installed);
    let installed_roots_f =
      OpamFilename.Op.(switch_prefix // "installed.roots")
    in
    let installed_roots = OpamFile.PkgList.safe_read installed_roots_f in
    OpamFile.PkgList.write installed_roots_f
      (OpamPackage.Set.map fix_version installed_roots);
    (* Move .config files *)
    List.iter (fun f ->
        let name =
          OpamFilename.Base.to_string @@
          OpamFilename.basename @@
          OpamFilename.chop_extension f in
        if name <> "global-config" then
          let dst =
            OpamPath.Switch.Default.config root switch (OpamPackage.Name.of_string name)
          in
          OpamFilename.mkdir (OpamFilename.dirname dst);
          OpamFilename.move ~src:f ~dst
      )
      (OpamFilename.files (OpamPath.Switch.config_dir root switch))
  ) aliases

let upgrade_to_1_3 () =
  if OpamCoreConfig.(!r.safe_mode) then
    OpamConsole.error_and_exit "Safe mode: not upgrading from opamroot <1.3";
  log "Upgrade switch state files format to 1.3";
  let root  = OpamStateConfig.(!r.root_dir) in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  OpamSwitch.Map.iter (fun switch c ->
      let switch_dir = OpamPath.Switch.root root switch in
      let open OpamFilename.Op in
      let installed_f = switch_dir // "installed" in
      let installed_roots_f = switch_dir // "installed.roots" in
      let pinned_f = switch_dir // "pinned" in
      let installed = OpamFile.PkgList.safe_read installed_f in
      let installed_roots = OpamFile.PkgList.safe_read installed_roots_f in
      let pinned = OpamFile.Pinned_legacy.safe_read pinned_f in
      let compiler =
        let comp = OpamFile.Comp.read (OpamPath.compiler_comp root c) in
        let atoms = OpamFormula.atoms (OpamFile.Comp.packages comp) in
        List.fold_left (fun acc (name,_) ->
            let nv =
              try
                match OpamPackage.Name.Map.find name pinned with
                | Version v -> OpamPackage.create name v
                | Source _ ->
                  let overlay = OpamPath.Switch.Overlay.opam root switch name in
                  let opam = OpamFile.OPAM.read overlay in
                  match OpamFile.OPAM.version_opt opam with
                  | Some v -> OpamPackage.create name v
                  | None -> raise Not_found
              with Not_found ->
              try OpamPackage.max_version installed name with Not_found ->
                OpamPackage.create name
                  (OpamPackage.Version.of_string "~unknown")
            in
            OpamPackage.Set.add nv acc)
          OpamPackage.Set.empty atoms
      in
      OpamFile.State.write
        (OpamPath.Switch.state root switch)
        { OpamFile.State.
          installed; installed_roots; pinned; compiler };
      OpamFilename.remove installed_f;
      OpamFilename.remove installed_roots_f;
      OpamFilename.remove pinned_f)
    aliases

(* Loads the global config file *)
let load_config root =
  let config = match OpamStateConfig.load root with
    | Some c -> c
    | None -> OpamFile.Config.empty
  in
  let config_version = OpamFile.Config.opam_version config in
  if config_version <> OpamVersion.current_nopatch then (
    if OpamVersion.(compare current config_version) < 0 &&
       not OpamFormatConfig.(!r.skip_version_checks) then
      OpamConsole.error_and_exit
        "%s reports a newer OPAM version, aborting."
        (OpamFilename.Dir.to_string (OpamStateConfig.(!r.root_dir)));
    (* opam has been updated, so refresh the configuration file and
       clean-up the cache. *)
    let v1_2 = OpamVersion.of_string "1.2" in
    let v1_3_dev2 = OpamVersion.of_string "1.3~dev2" in
    if OpamVersion.compare config_version v1_3_dev2 < 0 then
      let config = OpamFile.Config.with_opam_version config v1_3_dev2 in
      if OpamVersion.compare config_version v1_2 < 0 then upgrade_to_1_2 ();
      upgrade_to_1_3 ();
      OpamStateConfig.write root config;
      Cache.remove ();
      config
    else
      config;
  ) else
    config

let load_global_state () =
  log "LOAD-GLOBAL-STATE";
  let root = OpamStateConfig.(!r.root_dir) in
  let config = load_config root in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  { empty with root; config; aliases }

let load_switch_config t switch =
  let f = OpamPath.Switch.global_config t.root switch in
  if OpamFilename.exists f then OpamFile.Dot_config.read f
  else
    (OpamConsole.error "No global config file found for switch %s. \
                        Switch broken ?"
       (OpamSwitch.to_string t.switch);
     OpamFile.Dot_config.empty)

let with_switch switch t =
  let compiler =
    try OpamSwitch.Map.find switch t.aliases
    with Not_found ->
      OpamConsole.error_and_exit
        "The current switch (%s) is an unknown compiler switch."
        (OpamSwitch.to_string switch) in
  let switch_config = load_switch_config t switch in
  { t with switch; compiler; switch_config; }

(* load partial state to be able to read env variables *)
let load_env_state call_site switch =
  let t = load_global_state () in
  log "LOAD-ENV-STATE(%s)" call_site;
  with_switch switch t

let base_package_names t = OpamPackage.names_of_packages t.compiler_packages

let base_packages t = t.compiler_packages

let is_compiler_installed t comp =
  OpamSwitch.Map.exists (fun _ c -> c = comp) t.aliases

let is_switch_installed t switch =
  OpamSwitch.Map.mem switch t.aliases

let switch_state t =
  { OpamFile.State.
    installed = t.installed;
    installed_roots = t.installed_roots;
    compiler = t.compiler_packages;
    pinned = t.pinned; }

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
  let base = base_packages t in
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
    u_dev       = dev_packages t;
    u_base      = base;
    u_attrs     = [];
    u_test      = OpamStateConfig.(!r.build_test);
    u_doc       = OpamStateConfig.(!r.build_doc);
  }

let string_of_cnf string_of_atom cnf =
  let string_of_clause c =
    Printf.sprintf "%s" (OpamStd.List.concat_map " | " string_of_atom (List.rev c)) in
  Printf.sprintf "%s" (OpamStd.List.concat_map " , " string_of_clause (List.rev cnf))

let string_of_conjunction string_of_atom c =
  Printf.sprintf "%s" (OpamStd.List.concat_map " , " string_of_atom (List.rev c))

let dump_state t oc =
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
  let depends   = OpamPackage.Map.map OpamFile.OPAM.depends opams in
  let depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts opams in
  let conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams in
  let maintainers = OpamPackage.Map.map OpamFile.OPAM.maintainer opams in
  let base = base_packages t in
  let filter _ = true in

  let aux package =
    Printf.fprintf oc "package: %s\n" (OpamPackage.name_to_string package);
    Printf.fprintf oc "version: %s\n" (OpamPackage.version_to_string package);
    (try
      let m = OpamPackage.Map.find package maintainers in
      Printf.fprintf oc "maintainer: %s\n"
        (string_of_conjunction (fun a -> a) m);
    with Not_found -> () );

    if OpamPackage.Set.mem package base then
      Printf.fprintf oc "base: true\n";

    (try
      let d = OpamPackage.Map.find package depends in
      let formula = (OpamFormula.formula_of_extended ~filter d) in
      match OpamFormula.to_cnf formula with
      |[] -> ()
      |[[]] -> ()
      |dd -> Printf.fprintf oc "depends: %s\n"
              (string_of_cnf OpamFormula.string_of_atom dd)
    with Not_found -> () );

    (try
      let d = OpamPackage.Map.find package depopts in
      let formula = (OpamFormula.formula_of_extended ~filter d) in
      match OpamFormula.to_cnf formula with
      |[] -> ()
      |[[]] -> ()
      |dd -> Printf.fprintf oc "recommends: %s\n"
              (string_of_cnf OpamFormula.string_of_atom dd)
    with Not_found -> () );

    (try
      let c = OpamPackage.Map.find package conflicts in
      let n = OpamPackage.name package in
      match (n,None)::(OpamFormula.to_conjunction c) with
      |[] -> ()
      |cc -> Printf.fprintf oc "conflicts: %s\n"
              (string_of_conjunction OpamFormula.string_of_atom cc);
    with Not_found -> () );
    Printf.fprintf oc "\n";
  in
  OpamPackage.Set.iter aux (Lazy.force t.available_packages)

let installed_versions t name =
  OpamSwitch.Map.fold (fun switch _ map ->
    let installed =
      (OpamFile.State.safe_read (OpamPath.Switch.state t.root switch))
      .OpamFile.State.installed
    in
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

let installed_timestamp t name =
  let instfile = OpamPath.Switch.install t.root t.switch name in
  (Unix.stat (OpamFilename.to_string instfile)).Unix.st_mtime

(* Checks:
   * correct opam version
   * only installed packages have something in $repo/tmp
   * only installed packages have something in $opam/pinned.cache *)
(* unused ?
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
*)

(* Check that the dev packages are installed -- if not, just remove
   the temporary files. *)
let global_consistency_checks t =
  let pkgdir = OpamPath.dev_packages_dir t.root in
  let pkgdirs = OpamFilename.dirs pkgdir in
  let all_installed = all_installed t in
  let stale_pkgdirs =
    List.filter (fun dir ->
        match OpamPackage.of_dirname dir with
        | None -> true
        | Some nv -> not (OpamPackage.Set.mem nv all_installed))
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
      OpamConsole.msg "Regenerating the system compiler description.\n";
      create_system_compiler_description t.root;
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
        OpamConsole.error "Removing %s.\n" (OpamFilename.to_string f);
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
       try
         let opam =
           OpamFile.OPAM.read
             (OpamPath.Switch.Overlay.opam t.root t.switch name)
         in
         let nv =
           OpamPackage.create
             (OpamFile.OPAM.name opam) (OpamFile.OPAM.version opam)
         in
         not (OpamPackage.Set.mem nv t.installed) ||
         OpamPackage.Map.mem nv t.opams (* this package has upstream metadata *)
       with e -> OpamStd.Exn.fatal e; true)

let reinstall_system_compiler t =
  log "reinstall-system-compiler";
  let continue =
    OpamConsole.confirm
      "Your system compiler has been changed. Do you want to upgrade \
       your OPAM installation ?" in

  if continue then (

    (* Update system.comp *)
    create_system_compiler_description t.root;

    (* Reinstall all system compiler switches *)
    OpamSwitch.Map.iter (fun s a ->
      if a = OpamCompiler.system then (
        OpamConsole.header_msg "Upgrading %s" (OpamSwitch.to_string s);
        !switch_reinstall_hook s
      )
    ) t.aliases

  ) else
    OpamStd.Sys.exit 1

let load_repository_state ?(save_cache=true) () =
  let t = load_global_state () in
  log "LOAD-REPOSITORY-STATE";

  let opams = Cache.load t.root in
  let cached = opams <> None in
  if cached then log "Cache found";
  let compilers =
    let files = OpamFilename.rec_files (OpamPath.compilers_dir t.root) in
    let comp = OpamStd.List.filter_map OpamCompiler.of_filename files in
    OpamCompiler.Set.of_list comp
  in
  let repositories = read_repositories t.root t.config in
  let package_index =
    OpamFile.Package_index.safe_read (OpamPath.package_index t.root) in
  let compiler_index =
    OpamFile.Compiler_index.safe_read (OpamPath.compiler_index t.root) in
  let load_opam_file nv =
    let dir = package_repo_dir t.root repositories package_index nv in
    read_opam dir
  in
  let opams =
    match opams with Some o -> o | None ->
      let packages =
        OpamPackage.Set.of_list (OpamPackage.Map.keys package_index)
      in
      OpamPackage.Set.fold (fun nv map ->
          match load_opam_file nv with
          | Some o -> OpamPackage.Map.add nv o map
          | None -> map
        ) packages OpamPackage.Map.empty
  in
  let t =
    { t with repositories; compilers; package_index; compiler_index; opams }
  in
  if save_cache && not cached then Cache.save t;
  t

let load_state ?save_cache call_site switch =
  let chrono = OpamConsole.timer () in
  !upgrade_to_1_1_hook ();

  let t = load_repository_state ?save_cache () in
  log "LOAD-STATE(%s)" call_site;

  let partial = false in

  let switch, compiler =
    try switch, OpamSwitch.Map.find switch t.aliases
    with Not_found ->
      log "%a does not contain the compiler name associated to the switch %a"
        (slog @@ OpamFilename.to_string @* OpamPath.aliases) t.root
        (slog OpamSwitch.to_string) switch;
      if OpamCoreConfig.(!r.safe_mode) then
        OpamConsole.error_and_exit "Safe mode: invalid switch selected";
      match OpamStateConfig.(!r.switch_from) with
      | `Command_line | `Env -> OpamSwitch.not_installed switch
      | `Default ->
        OpamConsole.error "Current switch set to %S, which is unknown."
          (OpamSwitch.to_string switch);
        (try
           let new_switch, _ = OpamSwitch.Map.choose t.aliases in
           let config = OpamFile.Config.with_switch t.config new_switch in
           OpamStateConfig.write t.root config;
           OpamConsole.errmsg "Swiched back to %s"
             (OpamSwitch.to_string new_switch);
         with Not_found -> ());
        OpamStd.Sys.exit 10
  in
  let switch_config =
    load_switch_config t switch
  in
  let compiler_version = lazy (
    let comp_f = OpamPath.compiler_comp t.root compiler in
    (* XXX: useful for upgrade to 1.1 *)
    if not (OpamFilename.exists comp_f) then
      if compiler = OpamCompiler.system then
        create_system_compiler_description t.root
      else
        OpamConsole.error_and_exit "%S is not a valid compiler."
          (OpamCompiler.to_string compiler);
    OpamFile.Comp.version (OpamFile.Comp.read comp_f)
  ) in
  let { OpamFile.State. installed; installed_roots; pinned;
        compiler = compiler_packages; } =
    OpamFile.State.safe_read (OpamPath.Switch.state t.root switch)
  in
  let opams =
    (* Add installed packages without repository (from ~/.opam/packages) *)
    OpamPackage.Set.fold (fun nv opams ->
        if OpamPackage.Map.mem nv opams then opams else
        try
          OpamStd.Option.Op.(
            (read_opam (OpamPath.packages t.root nv) >>| fun opam ->
             OpamPackage.Map.add nv opam opams)
            +! opams)
        with
        | OpamFormat.Bad_format _ | Lexer_error _
        | Parsing.Parse_error | OpamSystem.Internal_error _ -> opams
      )
      installed t.opams
  in
  let packages = OpamPackage.Set.of_list (OpamPackage.Map.keys t.opams) in
  let reinstall =
    OpamFile.PkgList.safe_read (OpamPath.Switch.reinstall t.root switch)
  in
  let t = {
    t with partial; switch; compiler; compiler_version; compiler_packages;
           switch_config;
           installed; pinned; installed_roots; opams; packages; reinstall
  } in
  let t = { t with packages = pinned_packages t ++ packages } in
  let t = { t with available_packages = lazy (available_packages t) } in
  print_state t;
  let load_time = chrono () in
  log "State %s loaded in %.3fs" call_site load_time;
  (* Check whether the system compiler has been updated *)
  if system_needs_upgrade t then (
    reinstall_system_compiler t;
    if OpamConsole.confirm "\nSystem update successful. Go on with %S ?"
        (String.concat " " (Array.to_list Sys.argv))
    then t
    else OpamStd.Sys.exit 0
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
        ("lib", OpamPath.Switch.Default.lib_dir root switch);
        ("bin", OpamPath.Switch.Default.bin root switch);
        ("sbin", OpamPath.Switch.Default.sbin root switch);
        ("doc", OpamPath.Switch.Default.doc_dir root switch);
        ("stublibs", OpamPath.Switch.Default.stublibs root switch);
        ("toplevel", OpamPath.Switch.Default.toplevel root switch);
        ("man", OpamPath.Switch.Default.man_dir root switch);
        ("share", OpamPath.Switch.Default.share_dir root switch);
        ("etc", OpamPath.Switch.Default.etc_dir root switch);
      ]
    @ map id [
      ("user" ,
       try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name
       with Not_found -> "user");
      ("group",
       try (Unix.getgrgid (Unix.getgid ())).Unix.gr_name
       with Not_found -> "group");
      ("make" , OpamStateConfig.(Lazy.force !r.makecmd));
      ("os"   , OpamStd.Sys.os_string ());
    ] in

  let config = OpamFile.Dot_config.create vars in
  OpamFile.Dot_config.write
    (OpamPath.Switch.global_config root switch)
    config;
  config

let fix_descriptions_hook =
  ref (fun ?save_cache:_ ?verbose:_ _ -> assert false)

(* Upgrade to the new file overlay *)
let upgrade_to_1_1 () =
  let root  = OpamStateConfig.(!r.root_dir) in
  let opam  = root / "opam" in
  let opam_tmp = root / "opam_tmp" in
  let descr = root / "descr" in
  let compilers = root / "compilers" in
  let repo_index = root / "repo" // "index" in
  if OpamFilename.exists_dir opam || OpamFilename.exists repo_index then (
    if OpamCoreConfig.(!r.safe_mode) then
      OpamConsole.error_and_exit "Safe mode: not upgrading from opamroot <1.1";
    OpamSystem.in_dir (OpamStd.Sys.home ()) @@ fun () ->

    OpamConsole.header_msg
      "Upgrading to OPAM 1.1 %s"
      (OpamConsole.colorise `red "[DO NOT INTERRUPT THE PROCESS]");
    OpamConsole.formatted_msg
      "\n\
      \   In case something goes wrong, you can run that upgrade process again \
       by doing:\n\
       \n\
      \       mkdir %s/opam && opam list\n\
       \n\
       ** Processing **\n"
      (OpamFilename.prettify_dir (OpamStateConfig.(!r.root_dir)));

    if OpamFilename.exists_dir opam then
      OpamFilename.move_dir ~src:opam ~dst:opam_tmp;
    OpamFilename.rmdir descr;
    if OpamFilename.exists_dir (OpamPath.packages_dir root) then
      OpamFilename.rmdir (OpamPath.packages_dir root);

    (* Remove the cache. *)
    if OpamFilename.exists (OpamPath.state_cache root) then
      OpamFilename.remove (OpamPath.state_cache root);

    (* Remove the index files *)
    OpamFilename.remove (OpamStateConfig.(!r.root_dir) / "repo" // "index");
    OpamFilename.remove (OpamStateConfig.(!r.root_dir) / "repo" // "index.packages");
    OpamFilename.remove (OpamStateConfig.(!r.root_dir) / "repo" // "index.compilers");

    (* fix the base config files *)
    let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
    OpamSwitch.Map.iter (fun switch _ ->
        ignore (install_global_config root switch)
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
        then create_system_compiler_description root
      );
    (* Remove pinned cache *)
    OpamSwitch.Map.iter (fun switch _ ->
        let pinned_cache = OpamPath.Switch.root root switch / "pinned.cache" in
        if OpamFilename.exists_dir pinned_cache then (
          OpamConsole.msg
            "Removing the cache of pinned packages for the switch %s ...\n"
            (OpamSwitch.to_string switch);
          OpamFilename.rmdir pinned_cache;
        )
      ) aliases;

    (* Fix all the descriptions *)
    let t = load_state ~save_cache:false "update-to-1.1."
        OpamStateConfig.(!r.current_switch)in
    !fix_descriptions_hook ~verbose:false t;

    (* Fix the pinned packages *)
    OpamSwitch.Map.iter (fun switch _ ->
        let pinned =
          OpamFile.Pinned_legacy.safe_read
            OpamFilename.Op.(OpamPath.Switch.root root switch // "pinned")
        in
        OpamPackage.Name.Map.iter (fun name _ ->
            let t = with_switch switch t in
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

    OpamConsole.header_msg
      "Upgrade complete. Now continuing with \"%s\""
      (String.concat " " (Array.to_list Sys.argv));
    OpamConsole.msg "\n";
  )

let () = upgrade_to_1_1_hook := upgrade_to_1_1

let switch_eval_sh = "switch_eval.sh"
let complete_sh    = "complete.sh"
let complete_zsh   = "complete.zsh"
let prompt_sh      = "prompt.sh"
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
      Printf.sprintf "source %s > /dev/null 2> /dev/null; or true\n" (file f)
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

let expand_env t (env: env_update list) : env =
  List.rev_map (fun (ident, op, string, comment) ->
    let prefix = OpamFilename.Dir.to_string t.root in
    let read_env () =
      try OpamStd.Env.reset_value ~prefix (OpamStd.Sys.path_sep ())
            (OpamStd.Env.get ident)
      with Not_found -> [] in
    let update_env a =
      let before, after =
        OpamStd.Env.cut_value
          ~prefix (OpamStd.Sys.path_sep ()) (OpamStd.Env.get ident)
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
    let c = String.make 1 (OpamStd.Sys.path_sep ()) in
    match op with
    | Eq  -> ident, string, comment
    | PlusEq -> ident, String.concat c (string :: read_env ()), comment
    | EqPlus -> ident, String.concat c (read_env () @ [string]), comment
    | EqPlusEq -> ident, String.concat c (update_env string), comment
    | ColonEq ->
      ident, String.concat c (cons ~head:true string (read_env())), comment
    | EqColon ->
      ident, String.concat c (cons ~head:false string (read_env())), comment
  ) env

let add_to_env t (env: env) (updates: env_update list) =
  let env =
    List.filter (fun (k,_,_) -> List.for_all (fun (u,_,_,_) -> u <> k) updates)
      env
  in
  env @ expand_env t updates

let compute_env_updates t =
  (* Todo: put these back into their packages !
  let perl5 = OpamPackage.Name.of_string "perl5" in
  let add_to_perl5lib =  OpamPath.Switch.lib t.root t.switch t.switch_config perl5 in
  let new_perl5lib = "PERL5LIB", "+=", OpamFilename.Dir.to_string add_to_perl5lib in
  let toplevel_dir =
    "OCAML_TOPLEVEL_PATH", "=",
    OpamFilename.Dir.to_string (OpamPath.Switch.toplevel t.root t.switch t.switch_config) in
*)
  let fenv ?opam v =
    try resolve_variable t ?opam OpamVariable.Map.empty v
    with Not_found ->
      log "Undefined variable: %s" (OpamVariable.Full.to_string v);
      None
  in
  let man_path =
    let open OpamStd.Sys in
    match os () with
    | OpenBSD | NetBSD | FreeBSD ->
      [] (* MANPATH is a global override on those, so disabled for now *)
    | _ ->
      ["MANPATH", EqColon,
       OpamFilename.Dir.to_string
         (OpamPath.Switch.man_dir t.root t.switch t.switch_config),
      Some "Current opam switch man dir"]
  in
  let pkg_env = (* XXX: Does this need a (costly) topological sort ? *)
    OpamPackage.Set.fold (fun nv acc ->
        let opam = opam t nv in
        List.map (fun (name,op,str,cmt) ->
            let s = OpamFilter.expand_string (fenv ~opam) str in
            name, op, s, cmt)
          (OpamFile.OPAM.env opam)
        @ acc)
      t.installed []
  in
  let comp_env =
    List.map (fun (name,op,str,cmt) ->
        name, op, OpamFilter.expand_string (fenv ?opam:None) str, cmt)
      (OpamFile.Comp.env (compiler_comp t t.compiler)) in
  man_path @ comp_env @ pkg_env

let env_updates ~opamswitch ?(force_path=false) t =
  let update =
    let fn = OpamPath.Switch.environment t.root t.switch in
    if OpamFilename.exists fn then
      OpamFile.Environment.read fn
    else
      let update = compute_env_updates t in
      OpamFile.Environment.write fn update;
      update
  in
  let add_to_path = OpamPath.Switch.bin t.root t.switch t.switch_config in
  let new_path =
    "PATH",
    (if force_path then PlusEq else EqPlusEq),
    OpamFilename.Dir.to_string add_to_path,
    Some "Current opam switch binary dir" in
  let root =
    let current = t.root in
    let default = OpamStateConfig.(default.root_dir) in
    let current_string = OpamFilename.Dir.to_string current in
    let env = OpamStd.Env.getopt "OPAMROOT" in
    if current <> default || (env <> None && env <> Some current_string)
    then [ "OPAMROOT", Eq, current_string, None ]
    else []
  in
  let switch =
    if opamswitch then
      [ "OPAMSWITCH", Eq, OpamSwitch.to_string t.switch, None ]
    else [] in
  new_path :: root @ switch @ update

(* This function is used by 'opam config env' and 'opam switch' to
   display the environment variables. We have to make sure that
   OPAMSWITCH is always the one being reported in '~/.opam/config'
   otherwise we can have very weird results (as the inability to switch
   between compilers).

   Note: when we do the later command with --switch=SWITCH, this mean
   we really want to get the environment for this switch. *)
let get_opam_env ~force_path t =
  let opamswitch = OpamStateConfig.(!r.switch_from <> `Default) in
  add_to_env t [] (env_updates ~opamswitch ~force_path t)

let get_full_env ?(opamswitch=true) ~force_path t =
  let env0 = List.map (fun (v,va) -> v,va,None) (OpamStd.Env.list ()) in
  add_to_env t env0 (env_updates ~opamswitch ~force_path t)

let mem_pattern_in_string ~pattern ~string =
  let pattern = Re.compile (Re.str pattern) in
  Re.execp pattern string

let ocamlinit () =
  try
    let file = Filename.concat (OpamStd.Env.get "HOME") ".ocamlinit" in
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
        OpamConsole.msg "  Generating ~/.ocamlinit.\n"
      else
        OpamConsole.msg "  Updating ~/.ocamlinit.\n";
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
        OpamStd.Exn.fatal e;
        OpamSystem.internal_error "Cannot write ~/.ocamlinit."
  ) else
    OpamConsole.msg "  ~/.ocamlinit is already up-to-date.\n"

let string_of_env_update t shell updates =
  let fenv = resolve_variable t OpamVariable.Map.empty in
  let make_comment comment_opt =
    OpamStd.Option.to_string (Printf.sprintf "# %s\n") comment_opt
  in
  let sh   (k,v,comment) =
    Printf.sprintf "%s%s=%S; export %s;\n"
      (make_comment comment) k v k in
  let csh  (k,v,comment) =
    Printf.sprintf "%sif ( ! ${?%s} ) setenv %s \"\"\nsetenv %s %S\n"
      (make_comment comment) k k k v in
  let fish (k,v,comment) =
    (* Fish converts some colon-separated vars to arrays, which have to be treated differently.
     * Opam only changes PATH and MANPATH but we handle CDPATH for completeness. *)
    let fish_array_vars = ["PATH"; "MANPATH"; "CDPATH"] in
    let fish_array_derefs = List.map (fun s -> "$" ^ s) fish_array_vars in
    if not (List.mem k fish_array_vars) then
      (* Regular string variables *)
      Printf.sprintf "%sset -gx %s %S;\n"
        (make_comment comment) k v
    else
      (* The MANPATH and CDPATH have default "values" if they are unset and we
       * must be sure that we preserve these defaults when "appending" to them.
       * This because Fish has trouble dealing with the case where we want to
       * have a colon at the start or at the end of the string that gets exported.
       *  - MANPATH: ""  (default system manpages)
       *  - CDPATH:  "." (current directory) *)
      let init_array = match k with
        | "PATH"    -> "" (* PATH is always set *)
        | "MANPATH" -> "if [ 0 -eq (count $MANPATH) ]; set -gx MANPATH \"\"; end;\n"
        | "CDPATH"  -> "if [ 0 -eq (count $CDPATH) ]; set -gx CDPATH \".\"; end;\n"
        | _         -> assert false in
      (* Opam assumes that `v` is a string with colons in the middle so we have
       * to convert that to an array assignment that fish understands.
       * We also have to pay attention so we don't quote array expansions - that
       * would replace some colons by spaces in the exported string *)
      let vs = OpamStd.String.split_delim v ':' in
      let to_arr_element v =
        if List.mem v fish_array_derefs then v else Printf.sprintf "%S" v in
      let set_array =
        Printf.sprintf "%sset -gx %s %s;\n"
          (make_comment comment)
          k (OpamStd.List.concat_map " " to_arr_element vs) in
      (init_array ^ set_array) in
  let export = match shell with
    | `zsh | `sh  -> sh
    | `fish -> fish
    | `csh -> csh in
  let aux (ident, symbol, string, comment) =
    let string = OpamFilter.expand_string fenv string in
    let key, value = match symbol with
      | Eq  -> ident, string
      | PlusEq | ColonEq -> ident, Printf.sprintf "%s:$%s" string ident
      | EqColon | EqPlus ->
        ident, (match shell with `csh -> Printf.sprintf "${%s}:%s" ident string
                               | _ -> Printf.sprintf "$%s:%s" ident string)
      | EqPlusEq -> ident, Printf.sprintf "%s:$%s" string ident
    in
    export (key, value, comment) in
  OpamStd.List.concat_map "" aux updates

let init_script t ~switch_eval ~complete ~shell (variables_sh, switch_eval_sh, complete_sh)=
  let variables =
    Some (source t ~shell variables_sh) in
  let switch_eval =
    if switch_eval then
      OpamStd.Option.map (source t ~shell ~interactive_only:true)
        switch_eval_sh
    else
      None in
  let complete =
    if complete then
      OpamStd.Option.map (source t ~shell ~interactive_only:true) complete_sh
    else
      None in
  let prompt =
    Some (source t ~shell prompt_sh) in
  let buf = Buffer.create 128 in
  let append name = function
    | None   -> ()
    | Some c ->
      Printf.bprintf buf "# %s\n%s\n" name c in
  append "Load the environment variables" variables;
  append "Load the auto-complete scripts" complete;
  append "Load the opam-switch-eval script" switch_eval;
  append "Load the prompt script" prompt;
  Buffer.contents buf

let update_init_scripts t ~global =
  let init_scripts =
    match global with
    | None   -> []
    | Some g ->
      let scripts = [
        `sh,   init_sh ,  (variables_sh  , Some switch_eval_sh, Some complete_sh);
        `zsh,  init_zsh,  (variables_sh  , Some switch_eval_sh, Some complete_zsh);
        `csh,  init_csh,  (variables_csh , None, None);
        `fish, init_fish, (variables_fish, None, None);
      ] in
      let aux (shell, init, scripts) =
        init,
        init_script t ~shell ~switch_eval:g.switch_eval ~complete:g.complete scripts in
      List.map aux scripts in
  let scripts = [
    (complete_sh   , OpamScript.complete);
    (complete_zsh  , OpamScript.complete_zsh);
    (switch_eval_sh, OpamScript.switch_eval);
    (prompt_sh     , OpamScript.prompt);
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
      with e -> OpamStd.Exn.fatal e
    ) in
  List.iter write scripts;
  if global <> None then
    List.iter
      (fun init_file ->
         let pretty_init_file =
           OpamFilename.prettify (OpamPath.init t.root // init_file) in
         if !updated then OpamConsole.msg "  Updating %s\n" pretty_init_file
         else OpamConsole.msg "  %s is already up-to-date.\n" pretty_init_file)
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
      let prompt_sh = aux prompt_sh in
      Some (complete_sh, complete_zsh, switch_eval_sh, prompt_sh)
    else
      None
  ) else
    None

let dot_profile_needs_update t dot_profile =
  if OpamFilename.exists dot_profile then (
    let body = OpamFilename.read dot_profile in
    let pattern1 = "opam config" in
    let pattern2 = OpamFilename.to_string (OpamPath.init t.root // "init") in
    let pattern3 =
      OpamStd.String.remove_prefix
        ~prefix:OpamStateConfig.(OpamFilename.Dir.to_string !r.root_dir)
        pattern2 in
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
  | `no        -> OpamConsole.msg "  %s is already up-to-date.\n" pretty_dot_profile
  | `otherroot ->
    OpamConsole.msg
      "  %s is already configured for another OPAM root.\n"
      pretty_dot_profile
  | `yes       ->
    let init_file = init_file shell in
    let body =
      if OpamFilename.exists dot_profile then
        OpamFilename.read dot_profile
      else
        "" in
    OpamConsole.msg "  Updating %s.\n" pretty_dot_profile;
    let body =
      Printf.sprintf
        "%s\n\n\
         # OPAM configuration\n\
         %s"
        (OpamStd.String.strip body) (source t ~shell init_file) in
    OpamFilename.write dot_profile body

let update_setup t user global =
  begin match user with
    | Some { ocamlinit = false; dot_profile = None; _ }
    | None   -> ()
    | Some l ->
      OpamConsole.msg "User configuration:\n";
      if l.ocamlinit then update_ocamlinit ();
      match l.dot_profile with
      | None   -> ()
      | Some f -> update_dot_profile t f l.shell;
  end;
  begin match global with
    | None   -> ()
    | Some _ ->
      OpamConsole.msg "Global configuration:\n";
      update_init_scripts t ~global
  end

let display_setup t shell dot_profile =
  let print (k,v) = OpamConsole.msg "  %-25s - %s\n" k v in
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
    | Some(complete_sh, complete_zsh, switch_eval_sh, prompt_sh) ->
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
      let prompt =
        if prompt_sh then
          ok
        else
          not_set in
      [ ("init-script"     , Printf.sprintf "%s" pretty_init_file);
        ("auto-completion" , completion);
        ("prompt"          , prompt);
        ("opam-switch-eval", switch_eval);
      ]
  in
  OpamConsole.msg "User configuration:\n";
  List.iter print user_setup;
  OpamConsole.msg "Global configuration:\n";
  List.iter print global_setup

let eval_string t =
  let root =
    let opamroot_cur = OpamFilename.Dir.to_string t.root in
    let opamroot_env =
      OpamStd.Option.Op.(
        OpamStd.Env.getopt "OPAMROOT" +!
        OpamFilename.Dir.to_string OpamStateConfig.(default.root_dir)
      ) in
    if opamroot_cur <> opamroot_env then
      Printf.sprintf " --root=%s" opamroot_cur
    else
      "" in
  let switch =
    try
      let sw_cur = OpamSwitch.to_string t.switch in
      let sw_env = OpamStd.Env.get "OPAMSWITCH" in
      if sw_cur <> sw_env then Printf.sprintf " --switch=%s" sw_cur
      else ""
    with Not_found -> ""
  in
  match OpamStd.Sys.guess_shell_compat () with
  | `fish ->
    Printf.sprintf "eval (opam config env%s%s)" root switch
  | _ ->
    Printf.sprintf "eval `opam config env%s%s`" root switch

let up_to_date_env t =
  let changes =
    List.filter
      (fun (s, v, _) -> Some v <>
                        try Some (OpamStd.Env.get s) with Not_found -> None)
      (get_opam_env ~force_path:false t) in
  log "Not up-to-date env variables: [%s]"
    (String.concat " " (List.map (fun (v, _, _) -> v) changes));
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
          (OpamConsole.colorise `yellow "2.")
          (OpamFilename.prettify f)
          (source t ~shell:user.shell (init_file user.shell))
    in
    let ocamlinit_string =
      if not user.ocamlinit then "" else
        OpamConsole.colorise `yellow "3." ^
        " To avoid issues related to non-system installations of `ocamlfind`\n\
        \   add the following lines to ~/.ocamlinit (create it if necessary):\n\
         \n\
        \      let () =\n\
        \        try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")\n\
        \        with Not_found -> ()\n\
        \      ;;\n\n"
    in
    let line =
      OpamConsole.colorise `cyan
        "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
    in
    OpamConsole.msg
      "\n%s\n\n\
       %s To configure OPAM in the current shell session, you need to run:\n\
       \n\
      \      %s\n\
       \n\
       %s%s%s\n\n"
      line
      (OpamConsole.colorise `yellow "1.")
      (eval_string t) profile_string ocamlinit_string line

let is_switch_globally_set t =
  OpamFile.Config.switch t.config = t.switch

let check_and_print_env_warning t =
  if (is_switch_globally_set t ||
      OpamStateConfig.(!r.switch_from <> `Command_line)) &&
     not (up_to_date_env t) then
    OpamConsole.formatted_msg
      "# Run %s to update the current shell environment\n"
      (OpamConsole.colorise `bold (eval_string t))

let update_setup_interactive t shell dot_profile =
  let update dot_profile =
    let modify_user_conf = dot_profile <> None in
    let user = Some { shell; ocamlinit = modify_user_conf; dot_profile } in
    OpamConsole.msg "\n";
    update_setup t user (Some {complete=true; switch_eval=true});
    modify_user_conf in

  OpamConsole.msg "\n";

  match OpamConsole.read
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
      (OpamConsole.colorise `cyan @@ OpamFilename.prettify dot_profile)
      (OpamConsole.colorise `bold @@ string_of_shell shell)
      (source t ~shell (init_file shell))
      (OpamConsole.colorise `cyan @@ "~/.ocamlinit")
      (OpamFilename.prettify dot_profile)
      (OpamFilename.prettify dot_profile)
  with
  | Some ("y" | "Y" | "yes"  | "YES" ) -> update (Some dot_profile)
  | Some ("f" | "F" | "file" | "FILE") ->
    begin match OpamConsole.read "  Enter the name of the file to update:" with
      | None   ->
        OpamConsole.msg "-- No filename: skipping the auto-configuration step --\n";
        false
      | Some f -> update (Some (OpamFilename.of_string f))
    end
  | _ -> update None

(* Add the given packages to the set of package to reinstall. If [all]
   is set, this is done for ALL the switches (useful when a package
   changed upstream for instance). If not, only the reinstall state of the
   current switch is changed. *)
let add_to_reinstall t ~all_unpinned packages =
  log "add-to-reinstall all:%b packages:%a" all_unpinned
    (slog OpamPackage.Set.to_string) packages;
  let aux switch =
    let { OpamFile.State.installed; pinned; _ } =
      OpamFile.State.safe_read (OpamPath.Switch.state t.root switch)
    in
    let packages =
      if all_unpinned then
        OpamPackage.Set.filter
          (fun nv -> not OpamPackage.(Name.Map.mem (name nv) pinned))
          packages
      else packages
    in
    let reinstall =
      OpamFile.PkgList.safe_read (OpamPath.Switch.reinstall t.root switch) ++
      packages in
    let reinstall =
      OpamPackage.Set.filter (fun nv ->
        OpamPackage.Set.mem nv installed
      ) reinstall in
    let file = OpamPath.Switch.reinstall t.root switch in
    if not (OpamPackage.Set.is_empty reinstall) then
      OpamFile.PkgList.write file reinstall
    else
      OpamFilename.remove file in
  if all_unpinned
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
let install_compiler t ~quiet:_ switch compiler =
  log "install_compiler switch=%a compiler=%a"
    (slog OpamSwitch.to_string) switch
    (slog OpamCompiler.to_string) compiler;

  let comp_f = OpamPath.compiler_comp t.root compiler in
  if not (OpamFilename.exists comp_f) then (
    OpamConsole.msg "Cannot find %s: %s is not a valid compiler name.\n"
      (OpamFilename.to_string comp_f)
      (OpamCompiler.to_string compiler);
    OpamStd.Sys.exit 1;
  );

  let switch_dir = OpamPath.Switch.root t.root switch in

  (* Do some clean-up if necessary *)
  if not (is_switch_installed t switch)
  && OpamFilename.exists_dir switch_dir then
    OpamFilename.rmdir switch_dir;

  try
    (* Create base directories *)
    OpamFilename.mkdir switch_dir;
    OpamFilename.mkdir (OpamPath.Switch.Default.lib_dir t.root switch);
    OpamFilename.mkdir (OpamPath.Switch.Default.stublibs t.root switch);
    OpamFilename.mkdir (OpamPath.Switch.Default.toplevel t.root switch);
    OpamFilename.mkdir (OpamPath.Switch.build_dir t.root switch);
    OpamFilename.mkdir (OpamPath.Switch.Default.bin t.root switch);
    OpamFilename.mkdir (OpamPath.Switch.Default.sbin t.root switch);
    OpamFilename.mkdir (OpamPath.Switch.Default.doc_dir t.root switch);
    OpamFilename.mkdir (OpamPath.Switch.Default.man_dir t.root switch);
    OpamFilename.mkdir (OpamPath.Switch.install_dir t.root switch);
    OpamFilename.mkdir (OpamPath.Switch.config_dir t.root switch);
    List.iter (fun num ->
        OpamFilename.mkdir (OpamPath.Switch.Default.man_dir ~num t.root switch)
      ) ["1";"1M";"2";"3";"4";"5";"6";"7";"9"];

    let switch_config = install_global_config t.root switch in

    let comp = OpamFile.Comp.read comp_f in
    if not (OpamFile.Comp.preinstalled comp) &&
       OpamFile.Comp.src comp <> None
    then begin

      (* Install the compiler *)
      let comp_url = match OpamFile.Comp.src comp with
        | Some f -> f
        | None   ->
          OpamConsole.error_and_exit
            "No source for compiler %s"
            (OpamCompiler.to_string compiler) in
      let build_dir = OpamPath.Switch.build_ocaml t.root switch in
      let comp_name = OpamCompiler.to_string (OpamFile.Comp.name comp) in
      OpamConsole.header_msg "Installing compiler %s" comp_name;
      (match comp_url.OpamUrl.backend, OpamUrl.local_dir comp_url with
       | `rsync, Some dir -> OpamFilename.link_dir ~src:dir ~dst:build_dir
       | _ ->
         OpamProcess.Job.run @@
         OpamFilename.with_tmp_dir_job (fun download_dir ->
             let fake_pkg =
               match repository_and_prefix_of_compiler t compiler with
               | None -> OpamPackage.of_string "compiler.get"
               | Some (repo,_) ->
                 OpamPackage.of_string (OpamRepositoryName.to_string
                                          repo.repo_name ^ ".comp")
             in
             let text =
               OpamProcess.make_command_text ~color:`magenta
                 comp_name (OpamUrl.string_of_backend comp_url.OpamUrl.backend)
             in
             OpamProcess.Job.with_text text @@
             OpamRepository.pull_url fake_pkg download_dir None [comp_url]
             @@+ function
             | Not_available u ->
               OpamConsole.error_and_exit "%s is not available." u
             | Up_to_date r | Result r ->
               Done (OpamFilename.extract_generic_file r build_dir)
           ));
      let patches = OpamFile.Comp.patches comp in
      let patch_command url =
        let text =
          OpamProcess.make_command_text ~color:`magenta
            comp_name ~args:[OpamUrl.basename url] "download"
        in
        OpamProcess.Job.with_text text @@
        OpamDownload.download ~overwrite:true url build_dir
      in
      let patches =
        OpamParallel.map
          ~jobs:(dl_jobs t)
          ~command:patch_command
          patches
      in
      List.iter (fun f -> OpamFilename.patch f build_dir) patches;
      OpamConsole.msg "Now compiling OCaml. This may take a while, \
                       please bear with us...\n";
      let commands =
        if OpamFile.Comp.configure comp @ OpamFile.Comp.make comp <> [] then
          [ ( "./configure" :: OpamFile.Comp.configure comp )
            @ [ "-prefix";  OpamFilename.Dir.to_string switch_dir ]
          (*-bindir %s/bin -libdir %s/lib -mandir %s/man*)
          (* NOTE In case it exists 2 '-prefix', in general the script
             ./configure will only consider the last one, others will be
             discarded. *)
          ; (OpamStateConfig.(Lazy.force !r.makecmd)::OpamFile.Comp.make comp)
          ; [OpamStateConfig.(Lazy.force !r.makecmd); "install" ]
          ]
        else
        let t = { t with switch; compiler; switch_config } in
        let env = resolve_variable t OpamVariable.Map.empty in
        OpamFilter.commands env (OpamFile.Comp.build comp)
      in
      let commands =
        OpamStd.List.filter_map (function
            | [] -> None
            | cmd::args ->
              let text =
                OpamProcess.make_command_text ~color:`magenta comp_name
                  ~args cmd
              in
              Some (OpamSystem.make_command
                      ~text
                      ~dir:(OpamFilename.Dir.to_string build_dir)
                      ~verbose:(OpamConsole.verbose ())
                      cmd args))
          commands
      in
      match
        OpamProcess.Job.run (OpamProcess.Job.of_list commands)
      with
      | None ->
        OpamConsole.msg "Done.\n";
        if not OpamStateConfig.(!r.keep_build_dir) then OpamFilename.rmdir build_dir
      | Some (cmd,err) ->
        OpamConsole.error_and_exit "Compiler build failed at %S:\n%s"
          (OpamProcess.string_of_command cmd)
          (OpamProcess.string_of_result err)
    end;

    (* Update ~/.opam/aliases *)
    add_switch t.root switch compiler

  with e ->
    if not (OpamConsole.debug ()) then
      OpamFilename.rmdir switch_dir;
    raise e

let write_switch_state t =
  if not OpamStateConfig.(!r.dryrun) then
    let f = OpamPath.Switch.state t.root t.switch in
    OpamFile.Environment.write
      (OpamPath.Switch.environment t.root t.switch)
      (compute_env_updates t);
    OpamFile.State.write f (switch_state t)

(* write the new version in the configuration file *)
let update_switch_config t switch =
  let config = OpamFile.Config.with_switch t.config switch in
  OpamStateConfig.write t.root config;
  let t = load_state "switch-config" switch in
  update_init_scripts t ~global:None;
  t

(* Dev packages *)

let fetch_dev_package url srcdir nv =
  let remote_url = OpamFile.URL.url url in
  let mirrors = remote_url :: OpamFile.URL.mirrors url in
  let checksum = OpamFile.URL.checksum url in
  log "updating %a" (slog OpamUrl.to_string) remote_url;
  let text =
    OpamProcess.make_command_text
      (OpamPackage.Name.to_string (OpamPackage.name nv))
      (OpamUrl.string_of_backend remote_url.OpamUrl.backend) in
  OpamProcess.Job.with_text text @@
  OpamRepository.pull_url nv srcdir checksum mirrors
  @@| function
  | Not_available _ ->
    (* OpamConsole.error "Upstream %s of %s is unavailable" u *)
    (*   (OpamPackage.to_string nv); *)
    false
  | Up_to_date _    -> false
  | Result _        -> true

let update_pinned_package t ?fixed_version name =
  let overlay = OpamPath.Switch.Overlay.package t.root t.switch name in
  let url_f = OpamPath.Switch.Overlay.url t.root t.switch name in
  if not (OpamFilename.exists url_f) then Done false else
  let url = OpamFile.URL.read url_f in
  let srcdir = OpamPath.Switch.dev_package t.root t.switch name in
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
      hash_meta @@ local_opam ?fixed_version name srcdir
  in
  let old_opam_file =
    try Some (List.find OpamFilename.exists
                [srcdir // "opam"; srcdir / "opam" // "opam"])
    with Not_found -> None
  in
  let was_single_opam_file = (old_opam_file = Some (srcdir // "opam")) in
  let old_opam_digest =
    OpamStd.Option.map OpamFilename.digest old_opam_file
  in
  let just_opam = List.filter (function (_, `Opam _) -> true | _ -> false) in
  let user_meta, empty_user_meta, user_version =
    (* Installed version (overlay) *)
    let opam,_,_ as files = local_opam ~root:true ?fixed_version name overlay in
    hash_meta files,
    (match opam with Some o -> OpamFile.OPAM.(empty = with_name_opt (with_version_opt o None) None)
                   | None -> true),
    OpamStd.Option.map OpamFile.OPAM.version opam
  in
  let repo_meta = (* Version from the repo *)
    let nv =
      let packages =
        OpamPackage.Map.filter (fun nv _ -> OpamPackage.name nv = name)
          t.package_index
      in
      match user_version with
      | None ->
        (try Some (fst (OpamPackage.Map.max_binding packages)) with
         | Not_found -> None)
      | Some v ->
        let nv = OpamPackage.create name v in
        (* get the latest version below v *)
        match OpamPackage.Map.split nv packages with
        | _, Some _, _ -> Some nv
        | below, None, _ when not (OpamPackage.Map.is_empty below) ->
          Some (fst (OpamPackage.Map.max_binding below))
        | _, None, above when not (OpamPackage.Map.is_empty above) ->
          Some (fst (OpamPackage.Map.min_binding above))
        | _ -> None
    in
    let meta = match nv with
      | None ->
        Some (OpamFile.OPAM.with_name OpamFile.OPAM.empty name), None, None
      | Some nv ->
        let dir = package_repo_dir t.root t.repositories t.package_index nv in
        let opam, descr, files = local_opam ~root:true ?fixed_version name dir in
        OpamStd.Option.map
          (fun o -> OpamFile.OPAM.with_version_opt o user_version) opam,
        descr, files
    in
    hash_meta @@ meta
  in
  let fake_nv = OpamPackage.create name (OpamPackage.Version.of_string "") in
  (* Do the update *)
  fetch_dev_package url srcdir fake_nv @@+ fun result ->
  let check = (* only on upstream changes *)
    try
      old_opam_digest <> Some (
        OpamFilename.digest
          (List.find OpamFilename.exists
             [srcdir // "opam"; srcdir / "opam" // "opam"]))
    with Not_found -> false
  in
  let new_meta = (* New version from the source *)
    hash_meta @@
    local_opam ?fixed_version
      ~check
      ~copy_invalid_to:(OpamPath.Switch.Overlay.tmp_opam t.root t.switch name)
      name srcdir
  in
  let user_meta, old_meta, repo_meta =
    match find_opam_file_in_source name srcdir with
    | Some f when OpamFilename.dirname f = srcdir ->
      (* Single opam file directly at the project root:
         don't override other files, restrict to 'opam' *)
      just_opam user_meta, just_opam old_meta, just_opam repo_meta
    | _ -> user_meta, old_meta, repo_meta
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
      let d = dir / (OpamPackage.Name.to_string name ^ ".opam") in
      if OpamFilename.exists_dir d then d else
      let d = dir / "opam" in
      if OpamFilename.exists_dir d then d else
        dir
    in
    List.iter (fun (f, _) -> OpamFilename.remove (overlay // f)) rm_hash;
    List.iter (fun (f,kind) -> match kind with
        | `Opam o ->
          let vo =
            OpamStd.Option.Op.(OpamFile.OPAM.version_opt o ++ user_version)
          in
          OpamFile.OPAM.write (overlay // f)
            (OpamFile.OPAM.with_version_opt o vo)
        | `Digest _ -> OpamFilename.copy_in ~root (root // f) overlay)
      hash
  in
  (* Metadata from the package changed *)
  if result && new_meta <> [] &&
     new_meta <> old_meta && new_meta <> user_meta
  then
    if old_meta = user_meta || repo_meta = user_meta || empty_user_meta ||
       was_single_opam_file && old_meta = just_opam user_meta
    then
      (* No manual changes *)
      (OpamConsole.formatted_msg
         "[%s] Installing new package description from %s\n"
         (OpamConsole.colorise `green (OpamPackage.Name.to_string name))
         (OpamUrl.to_string (OpamFile.URL.url url));
       OpamFilename.remove
         (OpamPath.Switch.Overlay.tmp_opam t.root t.switch name);
       install_meta srcdir user_meta new_meta)
    else if
      OpamConsole.formatted_msg
        "[%s] Conflicting update of the metadata from %s:\n%s"
        (OpamConsole.colorise `green (OpamPackage.Name.to_string name))
        (OpamUrl.to_string (OpamFile.URL.url url))
        (OpamStd.Format.itemize diff_to_string (diff user_meta new_meta));
      OpamConsole.confirm "\nOverride files in %s (there will be a backup) ?"
        (OpamFilename.Dir.to_string overlay)
    then (
      let bak =
        OpamPath.backup_dir t.root / (OpamPackage.Name.to_string name ^ ".bak") in
      OpamFilename.mkdir (OpamPath.backup_dir t.root);
      OpamFilename.rmdir bak;
      OpamFilename.copy_dir ~src:overlay ~dst:bak;
      OpamConsole.formatted_msg "User metadata backed up in %s\n"
        (OpamFilename.Dir.to_string bak);
      install_meta srcdir user_meta new_meta;
    );
  Done result

let update_dev_package t nv =
  log "update-dev-package %a" (slog OpamPackage.to_string) nv;
  let name = OpamPackage.name nv in
  if is_pinned t name then update_pinned_package t name else
  match url t nv with
  | None     -> Done false
  | Some url ->
    let srcdir = dev_package t nv in
    if (OpamFile.URL.url url).OpamUrl.backend = `http
    then Done false
    else fetch_dev_package url srcdir nv

let update_dev_packages t packages =
  log "update-dev-packages";
  let command nv =
    OpamProcess.Job.ignore_errors ~default:OpamPackage.Set.empty @@
    update_dev_package t nv @@| function
    | true -> OpamPackage.Set.singleton nv
    | false -> OpamPackage.Set.empty
  in
  let updates =
    OpamParallel.reduce ~jobs:(dl_jobs t)
      ~command
      ~merge:OpamPackage.Set.union
      ~nil:OpamPackage.Set.empty
      (OpamPackage.Set.elements packages)
  in
  let pinned =
    OpamPackage.Set.filter (fun nv -> is_pinned t (OpamPackage.name nv))
      packages
  in
  add_to_reinstall t ~all_unpinned:true (updates -- pinned);
  add_to_reinstall t ~all_unpinned:false (updates %% pinned);
  updates

let update_pinned_packages t names =
  log "update-pinned-packages";
  let command name =
    OpamProcess.Job.ignore_errors ~default:OpamPackage.Name.Set.empty @@
    update_pinned_package t name @@| function
    | true -> OpamPackage.Name.Set.singleton name
    | false -> OpamPackage.Name.Set.empty
  in
  let updates =
    OpamParallel.reduce ~jobs:(dl_jobs t)
      ~command
      ~merge:OpamPackage.Name.Set.union
      ~nil:OpamPackage.Name.Set.empty
      (OpamPackage.Name.Set.elements names)
  in
  let updates =
    OpamPackage.Name.Set.fold (fun name acc ->
        OpamPackage.Set.add (pinned t name) acc)
      updates OpamPackage.Set.empty
  in
  add_to_reinstall t ~all_unpinned:false updates;
  updates

(* Try to download $name.$version+opam.tar.gz *)
let download_archive t nv =
  log "get_archive %a" (slog OpamPackage.to_string) nv;
  let dst = OpamPath.archive t.root nv in
  try
    let repo, _ = OpamPackage.Map.find nv t.package_index in
    let repo = find_repository t repo in
    let text =
      OpamProcess.make_command_text
        (OpamPackage.name_to_string nv)
        ~args:[OpamRepositoryName.to_string repo.repo_name]
        "from"
    in
    OpamProcess.Job.with_text text @@
    OpamRepository.pull_archive repo nv
    @@+ function
    | Not_available _ ->
      if OpamCoreConfig.(!r.verbose_level) >= 2 then
        OpamConsole.msg "%s Repo archive not found\n" text;
      Done None
    | Up_to_date f ->
      OpamConsole.msg "[%s] Archive in cache\n"
        (OpamConsole.colorise `green (OpamPackage.name_to_string nv));
      OpamFilename.copy ~src:f ~dst; Done (Some dst)
    | Result f ->
      OpamFilename.copy ~src:f ~dst; Done (Some dst)
  with Not_found ->
    Done None

(* Download a package from its upstream source, using 'cache_dir' as cache
   directory. *)
let download_upstream t nv dirname =
  match url t nv with
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
    @@| fun x -> Some x

let check f =
  let root = OpamStateConfig.(!r.root_dir) in

  if not (OpamFilename.exists_dir root)
  || not (OpamFilename.exists (OpamPath.config root)) then
    OpamConsole.error_and_exit
      "No OPAM root found at %s.\n\
       Please run 'opam init' to initialize the state of OPAM, or \
       specify '--root'.\n\
       See 'opam init --help' for details."
      (OpamFilename.Dir.to_string root);

  match f with

    | Global_lock f ->
      (* Take the global lock *)
      OpamFilename.with_flock (OpamPath.lock root) (fun () ->
          (* clean the log directory *)
          OpamFilename.cleandir (OpamPath.log root);
          (* XXX pass t to f so that it doesn't have to reload it ? *)
          let t = load_state "global-lock"
              OpamStateConfig.(!r.current_switch) in
          (* Really the switch state shouldn't be loaded here;
             global_consistency_checks doesn't use it *)
          global_consistency_checks t;
          f ()
        ) ()

    | Read_lock f ->
      (* Global read lock *)
      OpamFilename.with_flock ~read:true (OpamPath.lock root) f ()

    | Switch_lock (switchf, f) ->
      (* Take a switch lock (and a global read lock). *)
      OpamFilename.with_flock ~read:true (OpamPath.lock root) (fun () ->
          let switch = switchf () in
          let t = load_state "switch-lock" switch in
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
            let t = load_state "global-lock"
                OpamStateConfig.(!r.current_switch) (* same remark *) in
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

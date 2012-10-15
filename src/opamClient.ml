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
  OpamGlobals.log "CLIENT" fmt

let () =
  OpamCurl.register ();
  OpamGit.register ();
  OpamRsync.register ()

type lock =
  | Read_lock of (unit -> unit)
  | Global_lock of (unit -> unit)
  | Alias_lock of (unit -> unit)

let check f =
  let root = OpamPath.default () in
  let with_alias_lock a f = OpamFilename.with_flock (OpamPath.Alias.lock root a) f in
  if OpamFilename.exists_dir root then
    match f with

    | Global_lock f ->
      (* Take the global lock *)
      OpamFilename.with_flock (OpamPath.lock root) (fun () ->
        (* Take all the alias locks *)
        let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
        let f = OpamAlias.Map.fold (fun a _ f -> with_alias_lock a (fun () -> f ())) aliases f in
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

    | Alias_lock f ->
      (* Take an alias lock (and check that the global lock is free). *)
      let alias =
        OpamFilename.with_flock
          (OpamPath.lock root)
          (fun () -> match !OpamGlobals.alias with
          | None   -> OpamFile.Config.alias (OpamFile.Config.read (OpamPath.config root))
          | Some a -> OpamAlias.of_string a)
          () in
      (* XXX: We can have a small race just here ... *)
      with_alias_lock alias f ()

type state = {
  root: OpamPath.t;
  alias: alias;
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

let string_of_repositories r =
  OpamMisc.string_of_list
    OpamRepositoryName.to_string
    (OpamRepositoryName.Map.keys r)

let print_state t =
  log "ROOT      : %s" (OpamFilename.Dir.to_string (OpamPath.root t.root));
  log "ALIAS     : %s" (OpamAlias.to_string t.alias);
  log "COMPILER  : %s" (OpamCompiler.to_string t.compiler);
  log "REPOS     : %s" (string_of_repositories t.repositories);
  log "AVAILABLE : %s" (OpamPackage.Set.to_string t.packages);
  log "INSTALLED : %s" (OpamPackage.Set.to_string t.installed);
  log "REINSTALL : %s" (OpamPackage.Set.to_string t.reinstall)

let packages_in_dir dir =
  log "packages in dir %s" (OpamFilename.Dir.to_string dir);
  if OpamFilename.exists_dir dir then (
    let files = OpamFilename.list_files dir in
    let files = List.filter (fun f -> OpamFilename.check_suffix f ".opam") files in
    List.fold_left (fun set file ->
      match OpamPackage.of_filename file with
      | None    ->
          log "%s is not a valid package filename!" (OpamFilename.to_string file);
          set
      | Some nv -> OpamPackage.Set.add nv set
    ) OpamPackage.Set.empty files
  ) else
    OpamPackage.Set.empty

let compilers_in_dir dir =
  log "compilers in dir %s" (OpamFilename.Dir.to_string dir);
  if OpamFilename.exists_dir dir then (
    let files = OpamFilename.list_files dir in
    let files = List.filter (fun f -> OpamFilename.check_suffix f ".comp") files in
    let l =
      List.map
        (OpamFilename.chop_extension
         |> OpamFilename.basename
         |> OpamFilename.Base.to_string
         |> OpamCompiler.of_string)
        files in
    OpamCompiler.Set.of_list l
  ) else
    OpamCompiler.Set.empty

let compiler_of_alias t alias =
  try Some (OpamAlias.Map.find alias t.aliases)
  with Not_found -> None

let config t =
  OpamFile.Config.read (OpamPath.config t.root)

let available_compilers t =
  compilers_in_dir (OpamPath.compilers_dir t.root)

let available_repositories t =
  OpamFile.Config.repositories t.config

let versions_of_packages nvset =
  OpamPackage.Set.fold
    (fun nv vset -> OpamPackage.Version.Set.add (OpamPackage.version nv) vset)
    nvset
    OpamPackage.Version.Set.empty

let available_versions t n =
  versions_of_packages
    (OpamPackage.Set.filter
       (fun nv -> OpamPackage.name nv = n)
       t.packages)

let find_opam t nv =
  OpamPackage.Map.find nv t.opams

let available_archives t =
  OpamFilename.Set.of_list (OpamFilename.list_files (OpamPath.archives_dir t.root))

let compiler_description t v =
  OpamFile.Comp.safe_read (OpamPath.compiler t.root v)

type r = OpamPath.Repository.r

let repository_packages r =
  let dir = OpamPath.Repository.packages_dir r in
  if OpamFilename.exists_dir dir then (
    let all = OpamFilename.list_dirs dir in
    let basenames = List.map OpamFilename.basename_dir all in
    OpamPackage.Set.of_list
      (OpamMisc.filter_map
         (OpamFilename.Base.to_string |> OpamPackage.of_string_opt)
         basenames)
  ) else
    OpamPackage.Set.empty

let repository_versions r n =
  versions_of_packages
    (OpamPackage.Set.filter
       (fun nv -> OpamPackage.name nv = n)
       (repository_packages r))

let repository_archives r =
  let d = OpamPath.Repository.archives_dir r in
  if OpamFilename.exists_dir d then
    OpamFilename.Set.of_list (OpamFilename.list_files d)
  else
    OpamFilename.Set.empty

let repository_compilers r =
  compilers_in_dir (OpamPath.Repository.compilers_dir r)

let repository_files r nv =
  let l =
    if OpamFilename.exists_dir (OpamPath.Repository.files r nv) then
      OpamFilename.list_files (OpamPath.Repository.files r nv)
    else
      [] in
  OpamFilename.Set.of_list l

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

let find_package_by_name t name =
  let r = OpamPackage.Set.filter (fun nv -> OpamPackage.name nv = name) t.packages in
  if OpamPackage.Set.is_empty r then
    None
  else
    Some r

let dot_config t nv =
  OpamFile.Dot_config.safe_read (OpamPath.Alias.config t.root t.alias nv)

let reinstall t =
  OpamFile.Reinstall.safe_read (OpamPath.Alias.reinstall t.root t.alias)

let mem_repository t name =
  OpamRepositoryName.Map.exists (fun n _ -> n = name) t.repositories

let find_repo_by_name t name =
  OpamRepositoryName.Map.find name t.repositories

let find_repo_aux repositories root repo_index nv =
  log "find_repo %s" (OpamPackage.to_string nv);
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

let find_repo t nv =
  find_repo_aux t.repositories t.root t.repo_index nv

let mem_repo t nv =
  find_repo t nv <> None

let with_repo t nv fn =
  match find_repo t nv with
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
      find_repo_aux repositories root repo_index nv <> None in
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
  log "root path is %s" (OpamFilename.Dir.to_string root);

  let config = OpamFile.Config.read (OpamPath.config root) in
  let alias = match !OpamGlobals.alias with
    | None   -> OpamFile.Config.alias config
    | Some a -> OpamAlias.of_string a in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let compiler =
    try OpamAlias.Map.find alias aliases
    with Not_found ->
      OpamGlobals.error_and_exit "%S does not contain the compiler name associated to the alias %s"
        (OpamFilename.to_string (OpamPath.aliases root))
        (OpamAlias.to_string alias) in
  let compiler_version =
    let comp = OpamFile.Comp.read (OpamPath.compiler root compiler) in
    OpamFile.Comp.version comp in
  let opams =
    OpamPackage.Set.fold (fun nv map ->
      let opam = OpamFile.OPAM.read (OpamPath.opam root nv) in
      OpamPackage.Map.add nv opam map
    ) (packages_in_dir (OpamPath.opam_dir root)) OpamPackage.Map.empty in
  let repositories =
    List.fold_left (fun map repo ->
      let repo_p = OpamPath.Repository.create root repo in
      let config = OpamFile.Repo_config.read (OpamPath.Repository.config repo_p) in
      OpamRepositoryName.Map.add repo config map
    ) OpamRepositoryName.Map.empty (OpamFile.Config.repositories config) in
  let repo_index = OpamFile.Repo_index.safe_read (OpamPath.repo_index root) in
  let pinned = OpamFile.Pinned.safe_read (OpamPath.Alias.pinned root alias) in
  let installed = OpamFile.Installed.safe_read (OpamPath.Alias.installed root alias) in
  let reinstall = OpamFile.Reinstall.safe_read (OpamPath.Alias.reinstall root alias) in
  let packages = packages_in_dir (OpamPath.opam_dir root) in
  let available_packages =
    lazy (available_packages root opams repositories repo_index compiler_version config pinned packages) in
  let t = {
    root; alias; compiler; compiler_version; repositories; opams;
    packages; available_packages; installed; reinstall;
    repo_index; config; aliases; pinned;
  } in
  print_state t;
  t

let print_updated t updated pinned_updated =
  let new_packages =
    OpamPackage.Set.filter (fun nv -> not (OpamPackage.Set.mem nv t.installed)) updated in
  let updated_packages =
    let aux set = OpamPackage.Set.filter (fun nv -> OpamPackage.Set.mem nv t.installed) set in
    OpamPackage.Set.union (aux updated) (aux pinned_updated) in
  if not (OpamPackage.Set.is_empty new_packages) then (
    if OpamPackage.Set.cardinal new_packages = 1 then
      OpamGlobals.msg "The following NEW package is available:\n"
    else
      OpamGlobals.msg "The following NEW packages are available:\n";
    OpamPackage.Set.iter (fun nv ->
      OpamGlobals.msg " - %s\n" (OpamPackage.to_string nv)
    ) new_packages;
  );
  if not (OpamPackage.Set.is_empty updated_packages) then (
    if OpamPackage.Set.cardinal updated_packages = 1 then
      OpamGlobals.msg "The following package needs to be upgraded:\n"
    else
      OpamGlobals.msg "The following packages need to be upgraded:\n";
    OpamPackage.Set.iter (fun nv ->
      if OpamPackage.Set.mem nv pinned_updated then
        OpamGlobals.msg " - %s\n" (OpamPackage.Name.to_string (OpamPackage.name nv))
      else
        OpamGlobals.msg " - %s\n" (OpamPackage.to_string nv)
    ) updated_packages
  )

let print_compilers t compilers repo =
  let repo_p = OpamPath.Repository.create t.root repo.repo_name in
  let repo_compilers = OpamRepository.compilers repo_p in
  let (--) = OpamCompiler.Set.diff in
  let new_compilers = repo_compilers -- compilers in
  if not (OpamCompiler.Set.is_empty new_compilers) then
    OpamGlobals.msg "New compiler descriptions available:\n";
  OpamCompiler.Set.iter (fun v ->
    OpamGlobals.msg " - %s\n" (OpamCompiler.to_string v)
  ) new_compilers;
  let all_compilers =
    OpamRepositoryName.Map.fold (fun repo _ set ->
      let repo_p = OpamPath.Repository.create t.root repo in
      let repo_compilers = OpamRepository.compilers repo_p in
      OpamCompiler.Set.union repo_compilers set;
    ) t.repositories OpamCompiler.Set.empty in
  let del_compilers = compilers -- all_compilers -- (OpamCompiler.Set.singleton OpamCompiler.default) in
  if not (OpamCompiler.Set.is_empty del_compilers) then
    OpamGlobals.msg "Some compilers are not available anymore:\n";
  OpamCompiler.Set.iter (fun v ->
    OpamGlobals.msg " - %s\n" (OpamCompiler.to_string v)
  ) del_compilers

(* install ~/.opam/<alias>/config/conf-ocaml.config *)
let install_conf_ocaml_config root alias =
  let name = OpamPackage.Name.of_string OpamGlobals.default_package in
  (* .config *)
  let vars =
    let map f l = List.map (fun (s,p) -> OpamVariable.of_string s, S (f p)) l in
    let id x = x in

    map OpamFilename.Dir.to_string
      [
        ("prefix", OpamPath.Alias.root root alias);
        ("lib", OpamPath.Alias.lib_dir root alias);
        ("bin", OpamPath.Alias.bin root alias);
        ("doc", OpamPath.Alias.doc_dir root alias);
        ("stublibs", OpamPath.Alias.stublibs root alias);
        ("toplevel", OpamPath.Alias.toplevel root alias);
        ("man", OpamPath.Alias.man_dir root alias);
      ]
    @ map id [
      ("user" , try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name with _ -> "user");
      ("group", try (Unix.getgrgid (Unix.getgid ())).Unix.gr_name with _ -> "group");
      ("make" , Lazy.force !OpamGlobals.makecmd);
      ("os"   , Lazy.force OpamGlobals.os_string);
    ] in

  let config = OpamFile.Dot_config.create vars in
  OpamFile.Dot_config.write (OpamPath.Alias.config root alias name) config

(* install the package conf-ocaml.<alias> *)
let install_conf_ocaml root alias =
  log "installing conf-ocaml";
  let name = OpamPackage.Name.of_string OpamGlobals.default_package in
  let version = OpamPackage.Version.of_string (OpamAlias.to_string alias) in
  let nv = OpamPackage.create name version in
  (* .opam *)
  let opam = OpamFile.OPAM.create nv in
  OpamFile.OPAM.write (OpamPath.opam root nv) opam;
  (* description *)
  let descr = OpamFile.Descr.create "Compiler configuration flags" in
  OpamFile.Descr.write (OpamPath.descr root nv) descr;
  install_conf_ocaml_config root alias;
  (* installed *)
  let installed_p = OpamPath.Alias.installed root alias in
  let installed = OpamFile.Installed.safe_read installed_p in
  let installed = OpamPackage.Set.add nv installed in
  OpamFile.Installed.write installed_p installed;
  (* stublibs *)
  let stublibs = OpamPath.Alias.stublibs root alias in
  OpamFilename.mkdir stublibs;
  (* toplevel dir *)
  let toplevel = OpamPath.Alias.toplevel root alias in
  OpamFilename.mkdir toplevel

let uninstall_conf_ocaml root alias =
  let name = OpamPackage.Name.of_string OpamGlobals.default_package in
  let version = OpamPackage.Version.of_string (OpamAlias.to_string alias) in
  let nv = OpamPackage.create name version in
  OpamFilename.remove (OpamPath.opam root nv);
  OpamFilename.remove (OpamPath.descr root nv)

let reinstall_conf_ocaml root alias =
  uninstall_conf_ocaml root alias;
  install_conf_ocaml root alias

let compare_repo t r1 r2 =
  OpamRepository.compare
    (OpamRepositoryName.Map.find r1 t.repositories)
    (OpamRepositoryName.Map.find r2 t.repositories)

let sorted_repositories  t =
  let repos = OpamRepositoryName.Map.values t.repositories in
  List.sort OpamRepository.compare repos

let update_repo_index t =

  (* Update repo_index *)
  let repositories = sorted_repositories t in

  (* Add new repositories *)
  let repo_index =
    List.fold_left (fun repo_index r ->
      let p = OpamPath.Repository.create t.root r.repo_name in
      let available = OpamRepository.packages p in
      log "repo=%s packages=%s"
        (OpamRepositoryName.to_string r.repo_name)
        (OpamPackage.Set.to_string available);
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
      match List.filter (mem_repository t) repo_s with
      | []     ->repo_index
      | repo_s -> OpamPackage.Name.Map.add n repo_s repo_index
    ) repo_index OpamPackage.Name.Map.empty in

  (* Write ~/.opam/repo/index *)
  OpamFile.Repo_index.write (OpamPath.repo_index t.root) repo_index;

  (* suppress previous links, but keep metadata of installed packages
     (because you need them to uninstall the package) *)
  let all_installed =
    OpamAlias.Map.fold (fun alias _ accu ->
      let installed_f = OpamPath.Alias.installed t.root alias in
      let installed = OpamFile.Installed.safe_read installed_f in
      OpamPackage.Set.union installed accu
    ) t.aliases OpamPackage.Set.empty in
  OpamPackage.Set.iter (fun nv ->
    if not (OpamPackage.Set.mem nv all_installed) then (
      let opam_g = OpamPath.opam t.root nv in
      let descr_g = OpamPath.descr t.root nv in
      let archive_g = OpamPath.archive t.root nv in
      OpamFilename.remove opam_g;
      OpamFilename.remove descr_g;
      OpamFilename.remove archive_g;
    );
  ) t.packages;

  reinstall_conf_ocaml t.root t.alias;

  (* Create symbolic links from $repo dirs to main dir *)
  OpamPackage.Name.Map.iter (fun n repo_s ->
    let all_versions = ref OpamPackage.Version.Set.empty in
    List.iter (fun r ->
      let repo = find_repo_by_name t r in
      let repo_p = OpamPath.Repository.create t.root repo.repo_name in
      let available_versions = OpamRepository.versions repo_p n in
      OpamPackage.Version.Set.iter (fun v ->
        if not (OpamPackage.Version.Set.mem v !all_versions) then (
          all_versions := OpamPackage.Version.Set.add v !all_versions;
          let nv = OpamPackage.create n v in
          let opam_g = OpamPath.opam t.root nv in
          let descr_g = OpamPath.descr t.root nv in
          let archive_g = OpamPath.archive t.root nv in
          let opam_r = OpamPath.Repository.opam repo_p nv in
          let descr_r = OpamPath.Repository.descr repo_p nv in
          let archive_r = OpamPath.Repository.archive repo_p nv in
          (* clean-up previous versions *)
          OpamFilename.remove opam_g;
          OpamFilename.remove descr_g;
          OpamFilename.remove archive_g;
          (* update global files *)
          if OpamFilename.exists opam_r then (
            OpamFilename.link opam_r opam_g;
            if OpamFilename.exists descr_r then
              OpamFilename.link descr_r descr_g;
            if OpamFilename.exists archive_r then
              OpamFilename.link archive_r archive_g;
          )
        )
      ) available_versions
    ) repo_s
  ) repo_index

let base_packages = List.map OpamPackage.Name.of_string [ "base-unix"; "base-bigarray"; "base-threads" ]

let create_default_compiler_description root = function
  | None         -> ()
  | Some version ->
    let f =
      OpamFile.Comp.create_preinstalled
        OpamCompiler.default version
        (if !OpamGlobals.base_packages then base_packages else [])
        [ ("CAML_LD_LIBRARY_PATH", "=",
           "%{lib}%/stublibs"
           ^ ":" ^
             (match Lazy.force OpamSystem.system_ocamlc_where with
             | Some d -> Filename.concat d "stublibs"
             | None   -> assert false))
        ] in
    let comp = OpamPath.compiler root OpamCompiler.default in
    OpamFile.Comp.write comp f

(* sync the repositories, display the new compilers, and create
   compiler description file links *)
(* XXX: the compiler things should splitted out, but the handling of
   compiler description files is a bit had-hoc *)
let update_repositories t ~show_compilers repositories =
  log "update_repositories %s" (string_of_repositories repositories);

  let compilers = available_compilers t in

  (* first update all the given repositories *)
  OpamRepositoryName.Map.iter (fun _ repo ->
    OpamRepository.update repo
  ) repositories;

  (* Display the new compilers available *)
  OpamRepositoryName.Map.iter (fun _ repo ->
    if show_compilers then
      print_compilers t compilers repo
  ) repositories;

  (* Delete compiler descritions which are not installed *)
  OpamCompiler.Set.iter (fun comp ->
    if comp <> OpamCompiler.default
    && OpamAlias.Map.for_all (fun _ c -> comp <> c) t.aliases then (
      let comp_f = OpamPath.compiler t.root comp in
      OpamFilename.remove comp_f;
    )
  ) (available_compilers t);

  (* Link existing compiler description files, following the
     repository priorities *)
  List.iter (fun repo ->
    let repo_p = OpamPath.Repository.create t.root repo.repo_name in
    let comps = OpamRepository.compilers repo_p in
    let comp_dir = OpamPath.compilers_dir t.root in
    OpamCompiler.Set.iter (fun o ->
      let comp_g = OpamPath.compiler t.root o in
      let comp_f = OpamPath.Repository.compiler repo_p o in
      if not (OpamFilename.exists comp_g) && OpamFilename.exists comp_f then
        OpamFilename.link_in comp_f comp_dir
    ) comps
  ) (sorted_repositories t)

let update_pinned_package t nv pin =
  let kind = kind_of_pin_option pin in
  let path = OpamFilename.raw_dir (path_of_pin_option pin) in
  let module B = (val OpamRepository.find_backend kind: OpamRepository.BACKEND) in
  let build = OpamPath.Alias.build t.root t.alias nv in
  match B.download_dir nv ~dst:build path with
  | Up_to_date _    -> None
  | Result _
  | Not_available -> Some nv

(* Update the package contents, display the new packages and update reinstall *)
let update_packages t ~show_packages repositories =
  log "update_packages %s" (string_of_repositories repositories);
  (* Update the pinned packages *)
  let pinned_updated =
    OpamPackage.Set.of_list (
      OpamMisc.filter_map
        (function
          | n, (Path p | Git p as k) ->
            if mem_installed_package_by_name t n then
              let nv = find_installed_package_by_name t n in
              OpamGlobals.msg "Synchronizing with %s ...\n" (OpamFilename.Dir.to_string p);
              update_pinned_package t nv k
            else
              None
          | _ -> None)
        (OpamPackage.Name.Map.bindings t.pinned)) in

  (* then update $opam/repo/index *)
  update_repo_index t;
  let t = load_state () in
  let updated =
    OpamPackage.Name.Map.fold (fun n repo_s accu ->
      (* we do not try to update pinned packages *)
      if OpamPackage.Name.Map.mem n t.pinned then
        accu
      else (
        let all_versions = ref OpamPackage.Version.Set.empty in
        List.fold_left (fun accu r ->
          let repo_p = OpamPath.Repository.create t.root r in
          let available_versions = OpamRepository.versions repo_p n in
          let new_versions = OpamPackage.Version.Set.diff available_versions !all_versions in
          log "repo=%s n=%s new_versions= %s"
            (OpamRepositoryName.to_string r)
            (OpamPackage.Name.to_string n)
            (OpamPackage.Version.Set.to_string new_versions);
          if not (OpamPackage.Version.Set.is_empty new_versions) then (
            all_versions := OpamPackage.Version.Set.union !all_versions new_versions;
            let all_updated = OpamFile.Updated.safe_read (OpamPath.Repository.updated repo_p) in
            let updated =
              OpamPackage.Set.filter (fun nv ->
                OpamPackage.name nv = n && OpamPackage.Version.Set.mem (OpamPackage.version nv) new_versions
              ) all_updated in
            if OpamRepositoryName.Map.exists (fun n _ -> n = r) repositories then
              OpamPackage.Set.union updated accu
            else
              accu
          ) else
            accu
        ) accu repo_s
      )
    ) t.repo_index OpamPackage.Set.empty in
  if show_packages then
    print_updated t updated pinned_updated;

  let updated = OpamPackage.Set.union pinned_updated updated in
  (* update $opam/$oversion/reinstall *)
  OpamAlias.Map.iter (fun alias _ ->
    let installed = OpamFile.Installed.safe_read (OpamPath.Alias.installed t.root alias) in
    let reinstall = OpamFile.Reinstall.safe_read (OpamPath.Alias.reinstall t.root alias) in
    let reinstall =
      OpamPackage.Set.fold (fun nv reinstall ->
        if OpamPackage.Set.mem nv installed then
          OpamPackage.Set.add nv reinstall
        else
          reinstall
      ) updated reinstall in
    if not (OpamPackage.Set.is_empty reinstall) then
      OpamFile.Reinstall.write (OpamPath.Alias.reinstall t.root alias) reinstall
  ) t.aliases;

  (* Check all the dependencies exist *)
  let t = load_state () in
  let has_error = ref false in
  OpamPackage.Set.iter (fun nv ->
    let opam = find_opam t nv in
    let name = OpamFile.OPAM.name opam in
    let version = OpamFile.OPAM.version opam in
    if nv <> OpamPackage.create name version then
      (OpamGlobals.error
        "The file %s is not consistent with the package %s (%s)"
        (OpamFilename.to_string (OpamPath.opam t.root nv))
        (OpamPackage.Name.to_string name)
        (OpamPackage.Version.to_string version);
      has_error := true);
    let map_b b = OpamFormula.fold_left (fun accu (n,_) -> (b, n) :: accu) [] in
    let depends = map_b true (OpamFile.OPAM.depends opam) in
    let depopts = map_b false (OpamFile.OPAM.depopts opam) in
    List.iter (fun (mandatory, d) ->
      match find_package_by_name t d with
        | None   ->
          if mandatory then
            OpamGlobals.warning
              "Package %s depends on the unknown package %s"
              (OpamPackage.to_string nv) (OpamPackage.Name.to_string d)
          else
            OpamGlobals.warning
              "Package %s depends optionally on the unknown package %s"
              (OpamPackage.to_string nv) (OpamPackage.Name.to_string d)
        | Some _ -> ()
    ) (depends @ depopts)
  ) (Lazy.force t.available_packages);
  if !has_error then
    OpamGlobals.exit 1

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
      OpamGlobals.error_and_exit "%s is not defined" (OpamVariable.Full.to_string v) in
  if OpamPackage.Name.to_string name = OpamGlobals.default_package then (
    try S (Sys.getenv var_str)
    with Not_found ->
      if var_str = "ocaml-version" then (
        let comp_str = OpamCompiler.Version.to_string t.compiler_version in
        if comp_str = OpamGlobals.default_alias then
          match OpamFile.Config.system_version t.config with
          | None   -> S "<none>"
          | Some v -> S (OpamCompiler.Version.to_string v)
        else
          S comp_str
      ) else if var_str = "preinstalled" then (
        B (OpamFile.Comp.preinstalled (compiler_description t t.compiler))
      ) else
          read_var name
  ) else (
    let process_one name =
      let name_str = OpamPackage.Name.to_string name in
      try Some (S (Sys.getenv (name_str ^"_"^ var_str)))
      with Not_found ->
        let installed = mem_installed_package_by_name t name in
        if var = OpamVariable.enable && installed then
          Some (S "enable")
        else if var = OpamVariable.enable && not installed then
          Some (S "disable")
        else if var = OpamVariable.installed then
          Some (B installed)
        else if not installed then
          None
        else
          Some (read_var name) in
    match process_one name with
    | Some r -> r
    | None   ->
      let name_str = OpamPackage.Name.to_string name in
      let names = OpamMisc.split name_str '+' in
      if List.length names = 1 then
        OpamGlobals.error_and_exit "Package %s is not installed" name_str;
      let names = List.map OpamPackage.Name.of_string names in
      let results =
        List.map (fun name ->
          match process_one name with
          | None   -> OpamGlobals.error_and_exit "Package %s is not installed" (OpamPackage.Name.to_string name)
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

let rec substitute_filter t = function
  | FBool b    -> FBool b
  | FString s  -> FString (substitute_string t s)
  | FOp(e,s,f) ->
    let e = substitute_filter t e in
    let f = substitute_filter t f in
    FOp(e, s, f)
  | FAnd (e,f) ->
    let e = substitute_filter t e in
    let f = substitute_filter t f in
    FAnd(e,f)
  | FOr(e,f) ->
    let e = substitute_filter t e in
    let f = substitute_filter t f in
    FOr(e,f)

let substitute_arg t (a, f) =
  let a = substitute_string t a in
  let f = match f with
    | None   -> None
    | Some f -> Some (substitute_filter t f) in
  (a, f)

let substitute_command t (l, f) =
  let l = List.map (substitute_arg t) l in
  let f = match f with
    | None   -> None
    | Some f -> Some (substitute_filter t f) in
  (l, f)

let substitute_commands t c =
  List.map (substitute_command t) c

let rec eval_filter t = function
  | FBool b    -> string_of_bool b
  | FString s  -> substitute_string t s
  | FOp(e,s,f) ->
    (* We are supposed to compare version strings *)
    let s = match s with
      | Eq  -> (=)
      | Neq -> (<>)
      | Ge  -> (fun a b -> Debian.Version.compare a b >= 0)
      | Le  -> (fun a b -> Debian.Version.compare a b <= 0)
      | Gt  -> (fun a b -> Debian.Version.compare a b >  0)
      | Lt  -> (fun a b -> Debian.Version.compare a b <  0) in
    let e = eval_filter t e in
    let f = eval_filter t f in
    if s e f then "true" else "false"
  | FOr(e,f)  ->
    if eval_filter t e = "true"
    || eval_filter t f = "true"
    then "true" else "false"
  | FAnd(e,f) ->
    if eval_filter t e = "true"
    && eval_filter t f = "true"
    then "true" else "false"

let eval_filter t = function
  | None   -> true
  | Some f -> eval_filter t f = "true"

let filter_arg t (a,f) =
  if eval_filter t f then
    Some a
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

let empty_env = {
  add_to_env  = [];
  add_to_path = OpamFilename.raw_dir "";
  new_env     = []
}

let print_env env =
  if env <> empty_env then
    List.iter (fun (k,v) ->
      OpamGlobals.msg "%s=%s; export %s;\n" k v k;
    ) env.new_env

let expand_env t env =
  List.map (fun (ident, symbol, string) ->
    let string = substitute_string t string in
    let read_env () =
      let prefix = OpamFilename.Dir.to_string t.root in
      try OpamMisc.reset_env_value ~prefix (Sys.getenv ident)
      with _ -> [] in
    match symbol with
    | "="  -> (ident, string)
    | "+=" -> (ident, String.concat ":" (string :: read_env ()))
    | "=+" -> (ident, String.concat ":" (read_env () @ [string]))
    | ":=" -> (ident, string ^":"^ (String.concat ":" (read_env())))
    | "=:" -> (ident, (String.concat ":" (read_env())) ^":"^ string)
    | _    -> failwith (Printf.sprintf "expand_env: %s is an unknown symbol" symbol)
  ) env

let update_env t env e =
  let expanded = expand_env t e in
  { env with
    add_to_env = expanded @ env.add_to_env;
    new_env    = expanded @ env.new_env }

let get_env t =
  let comp = compiler_description t t.compiler in

  let add_to_path = OpamPath.Alias.bin t.root t.alias in
  let new_path = "PATH", "+=", OpamFilename.Dir.to_string add_to_path in

  let add_to_env = OpamFile.Comp.env comp in
  let toplevel_dir =
    "OCAML_TOPLEVEL_PATH", "=", OpamFilename.Dir.to_string (OpamPath.Alias.toplevel t.root t.alias) in
  let man_path =
    "MANPATH", ":=", OpamFilename.Dir.to_string (OpamPath.Alias.man_dir t.root t.alias) in
  let new_env = new_path :: man_path :: toplevel_dir :: add_to_env in

  let add_to_env = expand_env t add_to_env in
  let new_env = expand_env t new_env in

  { add_to_env; add_to_path; new_env }

let print_env_warning ?(add_profile = false) t =
  match
    List.filter
      (fun (s, v) ->
        Some v <> try Some (Unix.getenv s) with _ -> None)
      (get_env t).new_env
  with
    | [] -> () (* every variables are correctly set *)
    | l ->
      let which_opam =
        if add_profile then
          "which opam && "
        else
          "" in
      let add_profile =
        if add_profile then
          "\nand add this in your ~/.profile"
        else
          "" in
      let opam_root =
        (if !OpamGlobals.root_dir = OpamGlobals.default_opam_dir then
            ""
         else
            Printf.sprintf " --root %s" !OpamGlobals.root_dir) in
      let variables = String.concat ", " (List.map (fun (s, _) -> "$" ^ s) l) in
      OpamGlobals.msg "\nTo update %s; you can now run:
            \n\    $ %seval `opam%s config -env`\n%s\n"
        variables
        which_opam
        opam_root
        add_profile

let add_alias root alias compiler =
  log "adding alias %s %s" (OpamAlias.to_string alias) (OpamCompiler.to_string compiler);
  let aliases_f = OpamPath.aliases root in
  let aliases = OpamFile.Aliases.safe_read aliases_f in
  if not (OpamAlias.Map.mem alias aliases) then begin
    (* Install the initial package and reload the global state *)
    install_conf_ocaml root alias;
    (* Update the list of aliases *)
    OpamFile.Aliases.write aliases_f (OpamAlias.Map.add alias compiler aliases);
  end

(* - compiles and install $opam/compiler/[ocaml_version].comp in $opam/[alias]
   - update $opam/alias
   - update $opam/config *)
let init_ocaml t quiet alias compiler =
  log "init_ocaml alias=%s compiler=%s"
    (OpamAlias.to_string alias)
    (OpamCompiler.to_string compiler);

  let comp_f = OpamPath.compiler t.root compiler in
  if not (OpamFilename.exists comp_f) then (
    OpamGlobals.msg "Cannot find %s: %s is not a valid compiler name.\n"
      (OpamFilename.to_string comp_f)
      (OpamCompiler.to_string compiler);
    OpamGlobals.exit 0;
  );

  let alias_dir = OpamPath.Alias.root t.root alias in
  if OpamFilename.exists_dir alias_dir then (
    OpamGlobals.msg "The compiler %s is already installed.\n" (OpamAlias.to_string alias);
    OpamGlobals.exit 0;
  );

  OpamFilename.mkdir alias_dir;
  OpamFilename.mkdir (OpamPath.Alias.lib_dir t.root alias);
  OpamFilename.mkdir (OpamPath.Alias.build_dir t.root alias);
  OpamFilename.mkdir (OpamPath.Alias.bin t.root alias);
  OpamFilename.mkdir (OpamPath.Alias.doc_dir t.root alias);
  OpamFilename.mkdir (OpamPath.Alias.man_dir t.root alias);
  OpamFilename.mkdir (OpamPath.Alias.install_dir t.root alias);
  OpamFilename.mkdir (OpamPath.Alias.config_dir t.root alias);
  List.iter (fun num ->
    OpamFilename.mkdir (OpamPath.Alias.man_dir ~num t.root alias)
  ) ["1";"1M";"2";"3";"4";"5";"6";"7";"9"];

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
      let build_dir = OpamPath.Alias.build_ocaml t.root alias in
      let comp_src_raw = OpamFilename.to_string comp_src in
      if Sys.file_exists comp_src_raw && Sys.is_directory comp_src_raw then
        OpamFilename.link_dir (OpamFilename.Dir.of_string comp_src_raw) build_dir
      else if Sys.file_exists comp_src_raw then
        OpamFilename.extract comp_src build_dir
      else OpamFilename.with_tmp_dir (fun download_dir ->
        let file = OpamFilename.download comp_src download_dir in
        OpamFilename.extract file build_dir;
      );
      let patches = OpamFile.Comp.patches comp in
      let patches = List.map (fun f -> OpamFilename.download f build_dir) patches in
      List.iter (fun f -> OpamFilename.patch f build_dir) patches;
      install_conf_ocaml_config t.root alias;
      if OpamFile.Comp.configure comp @ OpamFile.Comp.make comp <> [] then begin
        OpamFilename.exec build_dir
          [ ( "./configure" :: OpamFile.Comp.configure comp )
            @ [ "-prefix";  OpamFilename.Dir.to_string alias_dir ]
          (*-bindir %s/bin -libdir %s/lib -mandir %s/man*)
          (* NOTE In case it exists 2 '-prefix', in general the script
             ./configure will only consider the last one, others will be
             discarded. *)
          ; ( Lazy.force !OpamGlobals.makecmd :: OpamFile.Comp.make comp )
          ; [ Lazy.force !OpamGlobals.makecmd ; "install" ]
          ]
      end else begin
        let t = { t with alias } in
        let builds =
          List.map (List.map (substitute_string t)) (OpamFile.Comp.build comp) in
        OpamFilename.exec build_dir builds
      end;
    end;

    (* write the new version in the configuration file *)
    let config = OpamFile.Config.with_alias t.config alias in
    OpamFile.Config.write (OpamPath.config t.root) config;
    add_alias t.root alias compiler

  with e ->
    if not !OpamGlobals.debug then
      OpamFilename.rmdir alias_dir;
    raise e
  end

let indent_left s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    s ^ String.make nb ' '

let indent_right s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    String.make nb ' ' ^ s

let sub_at n s =
  if String.length s <= n then
    s
  else
    String.sub s 0 n

let s_not_installed = "--"

let unknown_package name version =
  match version with
  | None   -> OpamGlobals.error_and_exit "%S is not a valid package.\n" (OpamPackage.Name.to_string name)
  | Some v -> OpamGlobals.error_and_exit "The package %S has no version %s." (OpamPackage.Name.to_string name) (OpamPackage.Version.to_string v)

let unavailable_package name version =
  match version with
  | None   -> OpamGlobals.error_and_exit "%S is not available for your compiler.\n" (OpamPackage.Name.to_string name)
  | Some v -> OpamGlobals.error_and_exit "Version %s of %S is incompatible with your compiler." (OpamPackage.Version.to_string v) (OpamPackage.Name.to_string name)

let list ~print_short ~installed_only ?(name_only = true) ?(case_sensitive = false) res =
  log "list";
  let t = load_state () in
  let res =
    OpamMisc.filter_map (fun re ->
      try Some (Re.compile (let re = Re_glob.globx re in
                            if case_sensitive then re else Re.no_case re))
      with Re_glob.Parse_error ->
        OpamGlobals.error "\"%s\" is not a valid package descriptor" re;
        None
    ) res in
  let exact_match str =
    List.exists (fun re -> OpamMisc.exact_match re str) res in
  let partial_match str =
    List.exists (fun re -> Re.execp re str) res in
  let packages = Lazy.force t.available_packages in
  let names =
    OpamPackage.Set.fold
      (fun nv set -> OpamPackage.Name.Set.add (OpamPackage.name nv) set)
      packages
      OpamPackage.Name.Set.empty in
  let names =
    OpamPackage.Name.Set.fold (fun name map ->
      let has_name nv = OpamPackage.name nv = name in
      let version, nv =
        if OpamPackage.Set.exists has_name t.installed then
          let nv = OpamPackage.Set.find has_name t.installed in
          Some (OpamPackage.version nv), nv
        else (
          let nv = OpamPackage.Set.max_elt (OpamPackage.Set.filter has_name packages) in
          None, nv
        ) in
      let descr_f = OpamFile.Descr.safe_read (OpamPath.descr t.root nv) in
      let synopsis = OpamFile.Descr.synopsis descr_f in
      let descr = OpamFile.Descr.full descr_f in
      OpamPackage.Name.Map.add name (version, synopsis, descr) map
    ) names OpamPackage.Name.Map.empty in

  let max_n, max_v =
    OpamPackage.Name.Map.fold (fun name (version, _, _) (max_n, max_v) ->
      let max_n = max max_n (String.length (OpamPackage.Name.to_string name)) in
      let v_str = match version with
        | None   -> s_not_installed
        | Some v -> OpamPackage.Version.to_string v in
      let max_v = max max_v (String.length v_str) in
      max_n, max_v
    ) names (0,0) in

  (* Filter the list of packages, depending on user predicates *)
  let names =
    OpamPackage.Name.Map.filter (fun name (version, synopsis, descr) ->
      (* installp *) (not installed_only || version <> None)
      (* allp     *) && (res = []
      (* namep    *)  || name_only && exact_match (OpamPackage.Name.to_string name)
      (* descrp   *)  || not name_only
                      && (partial_match (OpamPackage.Name.to_string name) || partial_match synopsis || partial_match descr))
    ) names in
  if not print_short && OpamPackage.Name.Map.cardinal names > 0 then (
    let kind = if installed_only then "Installed" else "Available" in
    OpamGlobals.msg "%s packages for %s:\n" kind (OpamAlias.to_string t.alias);
  );
  OpamPackage.Name.Map.iter (
    if print_short then
      fun name _ -> OpamGlobals.msg "%s " (OpamPackage.Name.to_string name)
    else
      fun name (version, synopsis, _) ->
        let name = OpamPackage.Name.to_string name in
        let version = match version with
          | None   -> s_not_installed
          | Some v -> OpamPackage.Version.to_string v in
        OpamGlobals.msg "%s  %s  %s\n"
          (indent_left name max_n)
          (indent_right version max_v)
          (sub_at 100 synopsis)
  ) names

let info package =
  log "info %s" (OpamPackage.Name.to_string package);
  let t = load_state () in

  (* Compute the installed versions, for each alias *)
  let installed =
    OpamAlias.Map.fold (fun alias _ map ->
      let installed = OpamFile.Installed.safe_read (OpamPath.Alias.installed t.root alias) in
      if mem_installed_package_by_name_aux installed package then
        let nv = find_installed_package_by_name_aux installed package in
        if OpamPackage.Map.mem nv map then
          let aliases = OpamPackage.Map.find nv map in
          let map = OpamPackage.Map.remove nv map in
          OpamPackage.Map.add nv (alias :: aliases) map
        else
          OpamPackage.Map.add nv [alias] map
      else
        map
    ) t.aliases OpamPackage.Map.empty in

  let installed_str =
    let one (nv, aliases) =
      Printf.sprintf "%s [%s]"
        (OpamPackage.to_string nv)
        (String.concat " " (List.map OpamAlias.to_string aliases)) in
    String.concat ", " (List.map one (OpamPackage.Map.bindings installed)) in

  (* All the version of the package *)
  let versions = available_versions t package in
  if OpamPackage.Version.Set.is_empty versions then
    unknown_package package None;
  let versions =
    OpamPackage.Version.Set.filter (fun v ->
      OpamPackage.Map.for_all (fun nv _ -> OpamPackage.version nv <> v) installed
    ) versions in

  let installed_version = match OpamPackage.Map.cardinal installed with
    | 0 -> []
    | _ -> [ "installed-version", installed_str ] in

  let available_versions =
    match List.map OpamPackage.Version.to_string (OpamPackage.Version.Set.elements versions) with
    | []  -> []
    | [v] -> [ "available-version" , v ]
    | l   -> [ "available-versions", String.concat ", " l ] in

  let libraries, syntax = match OpamPackage.Map.cardinal installed with
    | 0 -> [], []
    | _ ->
      let fold f =
        let m =
          OpamPackage.Map.fold (fun nv _ set ->
            let opam = find_opam t nv in
            let incr = OpamVariable.Section.Set.of_list (f opam) in
            OpamVariable.Section.Set.union set incr
          ) installed OpamVariable.Section.Set.empty in
        OpamVariable.Section.Set.elements m in
      let libraries = match fold OpamFile.OPAM.libraries with
        | [] -> []
        | l  -> [ "libraries", String.concat ", " (List.map OpamVariable.Section.to_string l) ] in
      let syntax = match fold OpamFile.OPAM.syntax with
        | [] -> []
        | l  -> [ "syntax", String.concat ", " (List.map OpamVariable.Section.to_string l) ] in
      libraries, syntax in

  List.iter
    (fun (tit, desc) -> OpamGlobals.msg "%20s: %s\n" tit desc)
    ( [ "package", OpamPackage.Name.to_string package ]
     @ installed_version
     @ available_versions
     @ libraries
     @ syntax
     @ let descr = match OpamPackage.Map.cardinal installed with
         | 0 -> OpamFile.Descr.empty
         | _ ->
           let nv, _ = OpamPackage.Map.max_binding installed in
           OpamFile.Descr.safe_read (OpamPath.descr t.root nv) in
       [ "description", OpamFile.Descr.full descr ]
    )

let proceed_toinstall t nv =
  let build_dir = OpamPath.Alias.build t.root t.alias nv in
  if OpamFilename.exists_dir build_dir then OpamFilename.in_dir build_dir (fun () ->

    OpamGlobals.msg "Installing %s ...\n" (OpamPackage.to_string nv);
    let t = load_state () in
    let name = OpamPackage.name nv in
    let opam_f = OpamPath.opam t.root nv in
    let opam = find_opam t nv in
    let config_f = OpamPath.Alias.build_config t.root t.alias nv in
    let config = OpamFile.Dot_config.safe_read config_f in
    let install_f = OpamPath.Alias.build_install t.root t.alias nv in
    let install = OpamFile.Dot_install.safe_read install_f in

    (* check that libraries and syntax extensions specified in .opam and
       .config are in sync *)
    let check kind config_sections opam_sections =
      List.iter (fun cs ->
        if not (List.mem cs opam_sections) then
          OpamGlobals.error_and_exit "The %s %s does not appear in %s"
            kind (OpamVariable.Section.to_string cs) (OpamFilename.to_string opam_f)
      ) config_sections;
      List.iter (fun os ->
        if not (List.mem os config_sections) then
          OpamGlobals.error_and_exit "The %s %s does not appear in %s"
            kind (OpamVariable.Section.to_string os) (OpamFilename.to_string config_f)
      ) opam_sections in
    if not (OpamFilename.exists config_f)
    && (OpamFile.OPAM.libraries opam <> [] || OpamFile.OPAM.syntax opam <> []) then
      OpamGlobals.error_and_exit
        "%s does not exist but %s defines some libraries and syntax extensions"
        (OpamFilename.to_string config_f)
        (OpamFilename.to_string opam_f);
    check "library"
      (OpamFile.Dot_config.Library.available config)
      (OpamFile.OPAM.libraries opam);
    check "syntax"
      (OpamFile.Dot_config.Syntax.available config)
      (OpamFile.OPAM.syntax opam);

    (* check that depends (in .opam) and requires (in .config) fields
       are in almost in sync *)
    (* NOTES: the check is partial as we don't know which clause is valid
       in depends (XXX there is surely a way to get it from the solver) *)
    let local_sections = OpamFile.Dot_config.Section.available config in
    let libraries_in_opam =
      OpamFormula.fold_left (fun accu (n,_) ->
        let nv = find_installed_package_by_name t n in
        let opam = find_opam t nv in
        let libs = OpamFile.OPAM.libraries opam in
        let syntax = OpamFile.OPAM.syntax opam in
        List.fold_right OpamVariable.Section.Set.add (libs @ syntax) accu
      ) OpamVariable.Section.Set.empty (OpamFile.OPAM.depends opam) in
    let libraries_in_config =
      List.fold_left (fun accu s ->
        List.fold_left (fun accu r ->
          OpamVariable.Section.Set.add r accu
        ) accu (OpamFile.Dot_config.Section.requires config s)
      ) OpamVariable.Section.Set.empty local_sections in
    OpamVariable.Section.Set.iter (fun s ->
      if not (List.mem s local_sections)
      && not (OpamVariable.Section.Set.mem s libraries_in_opam) then
        let config_f = OpamFilename.to_string (OpamPath.Alias.build_config t.root t.alias nv) in
        let opam_f = OpamFilename.to_string (OpamPath.opam t.root nv) in
        let local_sections = List.map OpamVariable.Section.to_string local_sections in
        let opam_sections = List.map OpamVariable.Section.to_string (OpamVariable.Section.Set.elements libraries_in_opam) in
        OpamGlobals.error_and_exit
          "%s appears as a library dependency in %s, but:\n\
             - %s defines the libraries {%s}\n\
             - Packages in %s defines the libraries {%s}"
          (OpamVariable.Section.to_string s) config_f
          config_f (String.concat ", " local_sections)
          opam_f (String.concat ", " opam_sections)
    ) libraries_in_config;

    (* .install *)
    OpamFile.Dot_install.write (OpamPath.Alias.install t.root t.alias name) install;

    (* .config *)
    OpamFile.Dot_config.write (OpamPath.Alias.config t.root t.alias name) config;

    (* lib *)
    let warnings = ref [] in
    let check f dst =
      if not f.optional && not (OpamFilename.exists f.c) then (
        warnings := (f.c, dst) :: !warnings
      );
      OpamFilename.exists f.c in
    let lib = OpamPath.Alias.lib t.root t.alias name in
    List.iter (fun f ->
      if check f lib then
        OpamFilename.copy_in f.c lib
    ) (OpamFile.Dot_install.lib install);

    (* toplevel *)
    let toplevel = OpamPath.Alias.toplevel t.root t.alias in
    List.iter (fun f ->
      if check f toplevel then
        OpamFilename.copy_in f.c toplevel
    ) (OpamFile.Dot_install.toplevel install);

    (* bin *)
    List.iter (fun (src, dst) ->
      let dst = OpamPath.Alias.bin t.root t.alias // OpamFilename.Base.to_string dst in
      (* WARNING [dst] could be a symbolic link (in this case, it will be removed). *)
      if check src  (OpamPath.Alias.bin t.root t.alias) then
        OpamFilename.copy src.c dst;
    ) (OpamFile.Dot_install.bin install);

    (* misc *)
    List.iter
      (fun (src, dst) ->
        if OpamFilename.exists dst && OpamMisc.confirm "Overwriting %s ?" (OpamFilename.to_string dst) then
          OpamFilename.copy src.c dst
        else begin
          OpamGlobals.msg "Installing %s to %s.\n" (OpamFilename.to_string src.c) (OpamFilename.to_string dst);
          if OpamMisc.confirm "Continue ?" then
            OpamFilename.copy src.c dst
        end
      ) (OpamFile.Dot_install.misc install);

    if !warnings <> [] then (
      let print (f, dst) = Printf.sprintf " - %s in %s" (OpamFilename.to_string f) (OpamFilename.Dir.to_string dst) in
      OpamGlobals.error
        "Error while installing the following files:\n%s"
        (String.concat "\n" (List.map print !warnings));
      OpamGlobals.exit 2;
    )
  )

let pinned_path t nv =
  let name = OpamPackage.name nv in
  if OpamPackage.Name.Map.mem name t.pinned then
    match OpamPackage.Name.Map.find name t.pinned with
    | Path _
    | Git _ as k -> Some k
    | _          -> None
  else
    None

let get_archive t nv =
  let aux repo_p repo =
    OpamRepository.download repo nv;
    let src = OpamPath.Repository.archive repo_p nv in
    let dst = OpamPath.archive t.root nv in
    if OpamFilename.exists src then (
      OpamFilename.link src dst;
      Some dst
    ) else
      None in
  with_repo t nv aux

let extract_package t nv =
  log "extract_package: %s" (OpamPackage.to_string nv);
  let p_build = OpamPath.Alias.build t.root t.alias nv in
  match pinned_path t nv with
  | Some (Git p| Path p as pin) ->
    OpamGlobals.msg "Synchronizing pinned package ...\n";
    ignore (update_pinned_package t nv pin);
    OpamFilename.mkdir p_build;
    let _files = with_repo t nv (fun repo _ ->
      OpamFilename.in_dir p_build (fun () -> OpamRepository.copy_files repo nv)
    ) in
    ()
  | _ ->
    OpamFilename.rmdir p_build;
    match get_archive t nv with
    | None         -> ()
    | Some archive ->
      OpamGlobals.msg "Extracting %s ...\n" (OpamFilename.to_string archive);
      OpamFilename.extract archive p_build

let proceed_todelete ~rm_build t nv =
  log "deleting %s" (OpamPackage.to_string nv);
  OpamGlobals.msg "Uninstalling %s ...\n" (OpamPackage.to_string nv);
  let name = OpamPackage.name nv in

  (* Run the remove script *)
  let opam_f = OpamPath.opam t.root nv in
  if OpamFilename.exists opam_f then (
    let opam = find_opam t nv in
    let remove = substitute_commands t (OpamFile.OPAM.remove opam) in
    match filter_commands t remove with
    | []     -> ()
    | remove ->
      let p_build = OpamPath.Alias.build t.root t.alias nv in
      (* We try to run the remove scripts in the folder where it was extracted
         If it does not exist, we try to download and extract the archive again,
         if that fails, we don't really care. *)
      if not (OpamFilename.exists_dir p_build) && mem_repo t nv then (
        try extract_package t nv
        with _ -> OpamFilename.mkdir p_build;
      );
      try OpamFilename.exec ~add_to_path:[OpamPath.Alias.bin t.root t.alias] p_build remove
      with _ -> ();
  );

  (* Remove the libraries *)
  OpamFilename.rmdir (OpamPath.Alias.lib t.root t.alias name);

  (* Remove the documentation *)
  OpamFilename.rmdir (OpamPath.Alias.doc t.root t.alias name);
  (* XXX: remove the man pages *)

  (* Remove build/<package> if requested *)
  if rm_build then
    OpamFilename.rmdir (OpamPath.Alias.build t.root t.alias nv);

  (* Clean-up the repositories *)
  log "Cleaning-up the repositories";
  let repos =
    try OpamPackage.Name.Map.find (OpamPackage.name nv) t.repo_index
    with _ -> [] in
  List.iter (fun r ->
    let repo_p = OpamPath.Repository.create t.root r in
    let tmp_dir = OpamPath.Repository.tmp_dir repo_p nv in
    OpamFilename.rmdir tmp_dir
  ) repos;

  (* Remove the binaries *)
  log "Removing the binaries";
  let install = OpamFile.Dot_install.safe_read (OpamPath.Alias.install t.root t.alias name) in
  List.iter (fun (_,dst) ->
    let dst = OpamPath.Alias.bin t.root t.alias // (OpamFilename.Base.to_string dst) in
    OpamFilename.remove dst
  ) (OpamFile.Dot_install.bin install);

  (* Remove the misc files *)
  log "Removing the misc files";
  List.iter (fun (_,dst) ->
    if OpamFilename.exists dst then begin
      OpamGlobals.msg "Removing %s." (OpamFilename.to_string dst);
      if OpamMisc.confirm "Continue ?" then
        OpamFilename.remove dst
    end
  ) (OpamFile.Dot_install.misc install);

  (* Remove .config and .install *)
  log "Removing config and install files";
  OpamFilename.remove (OpamPath.Alias.install t.root t.alias name);
  OpamFilename.remove (OpamPath.Alias.config t.root t.alias name)

(* In case of error, simply return the error traces, and let the
   repo in a state that the user can explore.
   Do not try to recover yet. *)
let proceed_tochange t nv_old nv =
  OpamGlobals.msg "\n=-=-= %s =-=-=\n" (OpamPackage.to_string nv);

  (* First, uninstall any previous version *)
  (match nv_old with
  | Some nv_old -> proceed_todelete ~rm_build:true t nv_old
  | None        -> ());

  let opam = find_opam t nv in

  (* Get the env variables set up in the compiler description file *)
  let env0 = get_env t in
  let env = update_env t env0 (OpamFile.OPAM.build_env opam) in

  (* Prepare the package for the build. *)

  extract_package t nv;

  let p_build = OpamPath.Alias.build t.root t.alias nv in

  if not (OpamFilename.exists_dir p_build) then
    OpamFilename.mkdir p_build;

  (* Substitute the configuration files. We should be in the right
     directory to get the correct absolute path for the
     substitution files (see [substitute_file] and
     [OpamFilename.of_basename]. *)
  OpamFilename.in_dir p_build (fun () ->
    List.iter (substitute_file t) (OpamFile.OPAM.substs opam)
  );

  (* Generate an environnement file *)
  let env_f = OpamPath.Alias.build_env t.root t.alias nv in
  OpamFile.Env.write env_f env.new_env;

  (* Apply the patches *)
  let patches = OpamFile.OPAM.patches opam in
  List.iter (fun (base, filter) ->
    let root = OpamPath.Alias.build t.root t.alias nv in
    let patch = root // OpamFilename.Base.to_string base in
    if eval_filter t filter then (
      OpamGlobals.msg "Applying %s\n" (OpamFilename.Base.to_string base);
      OpamFilename.patch patch p_build)
  ) patches;

  (* Call the build script and copy the output files *)
  let commands = substitute_commands t (OpamFile.OPAM.build opam) in
  let commands = filter_commands t commands in
  let commands_s = List.map (fun cmd -> String.concat " " cmd)  commands in
  if commands_s <> [] then
    OpamGlobals.msg "Build commands:\n  %s\n" (String.concat "\n  " commands_s)
  else
    OpamGlobals.msg "Nothing to do.\n";
  try
    OpamFilename.exec
      ~add_to_env:env.add_to_env
      ~add_to_path:[env.add_to_path]
      p_build
      commands;
    proceed_toinstall t nv;
  with e ->
    (* We keep the build dir to help debugging *)
    proceed_todelete ~rm_build:false t nv;
    begin match nv_old with
    | None        ->
      OpamGlobals.error
        "The compilation of %s failed in %s."
        (OpamPackage.to_string nv)
        (OpamFilename.Dir.to_string p_build)
    | Some nv_old ->
      OpamGlobals.error
        "The recompilation of %s failed in %s."
        (OpamPackage.to_string nv)
        (OpamFilename.Dir.to_string p_build)
    end;
    raise e

(* We need to clean-up things before recompiling. *)
let proceed_torecompile t nv =
  proceed_tochange t (Some nv) nv

type version_constraint =
  | V_any of name * OpamPackage.Version.Set.t (* available versions *) * version option (* installed version *)
  | V_eq  of name * version

let string_of_version_constraint = function
  | V_any (n,s,i) ->
      Printf.sprintf
        "{name=%s available=%s installed=%s}"
        (OpamPackage.Name.to_string n)
        (OpamPackage.Version.Set.to_string s)
        (match i with None -> "<none>" | Some v -> OpamPackage.Version.to_string v)
  | V_eq (n,v) ->
      Printf.sprintf "{name=%s version=%s}" (OpamPackage.Name.to_string n) (OpamPackage.Version.to_string v)

let string_of_version_constraints =
  OpamMisc.string_of_list string_of_version_constraint

let string_of_version_constraints =
  OpamMisc.string_of_list string_of_version_constraint

let name_of_version_constraint = function
  | V_any (n,_,_) -> n
  | V_eq (n,_)    -> n

let nv_of_version_constraint = function
  | V_eq (n, v)
  | V_any (n, _, Some v) -> OpamPackage.create n v
  | V_any (n, vs, None)  -> OpamPackage.create n (OpamPackage.Version.Set.choose vs)

type solver_result =
  | Nothing_to_do
  | OK
  | Aborted
  | No_solution

let error_if_no_solution = function
  | No_solution -> OpamGlobals.exit 3
  | _           -> ()

let sum stats =
  stats.s_install + stats.s_reinstall + stats.s_remove + stats.s_upgrade + stats.s_downgrade

let debpkg_of_nv t action nv =
  let opam = find_opam t nv in
  let installed =
    OpamPackage.Set.mem nv t.installed &&
    match action with
    | `upgrade reinstall -> not (OpamPackage.Set.mem nv reinstall)
    | _                  -> true in
  OpamFile.OPAM.to_package opam installed

module Heuristic = struct

  let vpkg_of_n op name =
    (OpamPackage.Name.to_string name, None), op

  let vpkg_of_n_op op name v =
    vpkg_of_n (Some (op, OpamPackage.Version.to_string v)) name

  let vpkg_of_nv_eq = vpkg_of_n_op "="
  let vpkg_of_nv_ge = vpkg_of_n_op ">="
  let vpkg_of_nv_le = vpkg_of_n_op "<="
  let vpkg_of_nv_any = vpkg_of_n None

  (* Choose any available version *)
  let v_any _ _ =
    vpkg_of_nv_any

  (* Choose the max version *)
  let v_max _ set n =
    vpkg_of_nv_eq n (OpamPackage.Version.Set.max_elt set)

  (* Choose the installed version (if any); if the package is not installed,
     pick the max version *)
  let v_eq v set n =
    match v with
    | None   -> vpkg_of_nv_eq n (OpamPackage.Version.Set.max_elt set)
    | Some v -> vpkg_of_nv_eq n v

  (* Choose at least the installed version (if any); if the package is not
     installed, pick the max version *)
  let v_ge v set n =
    match v with
    | None   -> vpkg_of_nv_eq n (OpamPackage.Version.Set.max_elt set)
    | Some v -> vpkg_of_nv_ge n v

  let get t packages f_h =
    let available = Lazy.force t.available_packages in
    let available_map = OpamPackage.to_map available in
    let packages =
      OpamPackage.Set.filter
        (fun nv -> OpamPackage.Set.mem nv available)
        packages in
    let map = OpamPackage.to_map packages in
    OpamPackage.Name.Map.mapi
      (fun n vs -> f_h (Some (OpamPackage.Version.Set.choose_one vs)) (OpamPackage.Name.Map.find n available_map) n)
      map

  let get_installed t f_h =
    get t t.installed f_h

  let get_comp_packages t ocaml_version f_h =
    let comp_f = OpamPath.compiler t.root ocaml_version in
    let comp = OpamFile.Comp.read comp_f in
    let available = OpamPackage.to_map (Lazy.force t.available_packages) in

    let pkg_available, pkg_not =
      List.partition
        (fun (n, _) -> OpamPackage.Name.Map.mem n available)
        (OpamFormula.atoms (OpamFile.Comp.packages comp)) in

    (* check that all packages in [comp] are in [available]
       except for "base-..."
       (depending if "-no-base-packages" is set or not) *)
    let pkg_not = List.rev_map (function (n, _) -> n) pkg_not in
    let pkg_not =
      if !OpamGlobals.base_packages then
        pkg_not
      else
        List.filter (fun n -> not (List.mem n base_packages)) pkg_not in
    if pkg_not <> [] then (
      List.iter (OpamPackage.Name.to_string |> OpamGlobals.error "Package %s not found") pkg_not;
      OpamGlobals.exit 2
    );

    List.rev_map (function
      | n, None       -> f_h None (OpamPackage.Name.Map.find n available) n
      | n, Some (r,v) -> (OpamPackage.Name.to_string n, None), Some (r, OpamPackage.Version.to_string v)
    )  pkg_available

  (* Take a list of version constraints and an heuristic, and return a list of
     packages constraints satisfying the constraints *)
  let apply f_heuristic constraints =
    List.map
      (function
        | V_any (n, set, v) -> f_heuristic v set n
        | V_eq (n, v)       -> vpkg_of_nv_eq n v)
      constraints

  (* transform a name into:
     - <name, installed version> package
     - <$n,$v> package when name = $n.$v *)
  let nv_of_names t names =
    let available = OpamPackage.to_map (Lazy.force t.available_packages) in
    let installed = OpamPackage.to_map t.installed in
    List.map
      (fun name ->
        if OpamPackage.Name.Map.mem name installed && not (OpamPackage.Name.Map.mem name available) then
          V_eq (name, OpamPackage.Version.Set.choose_one (OpamPackage.Name.Map.find name installed))
        else if OpamPackage.Name.Map.mem name available then begin
          let set = OpamPackage.Name.Map.find name available in
          if OpamPackage.Name.Map.mem name installed then
            let version = OpamPackage.Version.Set.choose_one (OpamPackage.Name.Map.find name installed) in
            V_any (name, set, Some version)
          else
            V_any (name, set, None)
        end else
          (* perhaps the package is unavailable for this compiler *)
          let get_available = available_versions t in
          let all_versions = get_available name in
          if not (OpamPackage.Version.Set.is_empty all_versions) then
            unavailable_package name None
          else
            (* consider 'name' to be 'name.version' *)
            let nv =
              try OpamPackage.of_string (OpamPackage.Name.to_string name)
              with Not_found -> unknown_package name None in
            let sname = OpamPackage.name nv in
            let sversion = OpamPackage.version nv in
            log "The raw name %S not found, looking for package %s version %s"
                (OpamPackage.Name.to_string name) (OpamPackage.Name.to_string sname) (OpamPackage.Version.to_string sversion);
            if OpamPackage.Name.Map.mem sname available
               && OpamPackage.Version.Set.mem sversion (OpamPackage.Name.Map.find sname available) then
              V_eq (sname, sversion)
            else
              let all_versions = get_available sname in
              if OpamPackage.Version.Set.mem sversion all_versions then
                unavailable_package sname (Some sversion)
              else
                unknown_package sname (Some sversion)
      )
      (OpamPackage.Name.Set.elements names)

  (* Apply a solution *)
  let apply_solution ?(force = false) t sol =
    if OpamSolver.solution_is_empty sol then
      (* The current state satisfies the request contraints *)
      Nothing_to_do
    else (
      let stats = OpamSolver.stats sol in
      OpamGlobals.msg "The following actions will be performed:\n";
      OpamSolver.print_solution sol;
      OpamGlobals.msg "%s\n" (OpamSolver.string_of_stats stats);

      let continue =
        if force || sum stats <= 1 then
          true
        else
          OpamMisc.confirm "Do you want to continue ?" in

      if continue then (

        let installed = ref t.installed in
        (* This function should be called by the parent process only, as it modifies
           the global state of OPAM *)
        let write_installed () =
          OpamFile.Installed.write (OpamPath.Alias.installed t.root t.alias) !installed in

        (* Delete the requested packages in the parent process *)
        (* In case of errors, we try to keep the list of installed packages up-to-date *)
        List.iter
          (fun nv ->
            if OpamPackage.Set.mem nv !installed then begin
              try
                proceed_todelete ~rm_build:true t nv;
                installed := OpamPackage.Set.remove nv !installed;
                write_installed ()
              with _ ->
                ()
            end)
          sol.to_remove;

        (* Installation and recompilation are done by child processes *)
        let child n =
          let t = load_state () in
          match OpamSolver.action n with
          | To_change (o, nv) -> proceed_tochange t o nv
          | To_recompile nv   -> proceed_torecompile t nv
          | To_delete _       -> assert false in

        let pre _ = () in

        (* Update the installed file in the parent process *)
        let post n = match OpamSolver.action n with
          | To_delete _    -> assert false
          | To_recompile _ -> ()
          | To_change (None, nv) ->
              installed := OpamPackage.Set.add nv !installed;
              write_installed ()
          | To_change (Some o, nv)   ->
              installed := OpamPackage.Set.add nv (OpamPackage.Set.remove o !installed);
              write_installed () in

        (* Try to recover from errors.
           XXX: this is higly experimental. *)
        let can_try_to_recover_from_error l =
          List.exists (function (n,_) ->
            match OpamSolver.action n with
            | To_change(Some _,_)
            | To_recompile _ -> true
            | _ -> false
          ) l in

        let recover_from_error (n, _) = match OpamSolver.action n with
          | To_change (Some o, _) ->
              begin try
                proceed_toinstall t o;
                installed := OpamPackage.Set.add o !installed;
                write_installed ()
              with _ ->
                  ()
              end
          | To_change (None, _)   -> ()
          | To_recompile nv       ->
              (* this case is quite tricky. We have to remove all the packages
                 depending in nv, as they will be broken if nv is uninstalled. *)
              let universe =
                OpamSolver.U
                  (OpamPackage.Set.fold
                     (fun nv l -> (debpkg_of_nv t `remove nv) :: l)
                     (Lazy.force t.available_packages) []) in
              let depends =
                OpamSolver.get_forward_dependencies ~depopts:true universe
                  (OpamSolver.P [debpkg_of_nv t `remove nv]) in
              let depends = OpamPackage.Set.of_list (List.rev_map OpamPackage.of_dpkg depends) in
              let depends = OpamPackage.Set.filter (fun nv -> OpamPackage.Set.mem nv t.installed) depends in
              OpamPackage.Set.iter (proceed_todelete ~rm_build:true t) depends;
              installed := OpamPackage.Set.diff !installed depends;
              write_installed ();
          | To_delete nv            -> assert false in

        let display_error (n, error) =
          let f action nv =
            OpamGlobals.error "[ERROR] while %s %s" action (OpamPackage.to_string nv);
            match error with
            | OpamParallel.Process_error r  -> OpamProcess.display_error_message r
            | OpamParallel.Internal_error s -> OpamGlobals.error "  %s" s in
          match OpamSolver.action n with
          | To_change (Some o, nv) ->
              if OpamPackage.Version.compare (OpamPackage.version o) (OpamPackage.version nv) < 0 then
                f "upgrading to" nv
              else
                f "downgrading to" nv
          | To_change (None, nv)   -> f "installing" nv
          | To_recompile nv        -> f "recompiling" nv
          | To_delete nv           -> f "removing" nv in

        let string_of_errors errors =
          let actions = List.map OpamSolver.action (List.map fst errors) in
          let packages =
            List.map (function
              | To_change (_,nv)
              | To_recompile nv
              | To_delete nv -> nv
            ) actions in
          match packages with
          | []  -> assert false
          | [h] -> OpamPackage.to_string h
          | l   -> OpamPackage.Set.to_string (OpamPackage.Set.of_list l) in

        let cores = OpamFile.Config.cores t.config in
        try
          OpamSolver.PA_graph.Parallel.iter cores sol.to_add ~pre ~child ~post;
          OK
        with OpamSolver.PA_graph.Parallel.Errors (errors, remaining) ->
          OpamGlobals.msg "\n";
          if remaining <> [] then (
            OpamGlobals.error
              "Due to some errors while processing %s, the following actions will NOT be proceeded:"
              (string_of_errors errors);
            List.iter (fun n -> OpamGlobals.error "%s" (OpamSolver.string_of_action (OpamSolver.action n))) remaining;
          );
          if can_try_to_recover_from_error errors then (
            OpamGlobals.msg "\nRecovering from errors:\n";
            List.iter recover_from_error errors;
          );
          List.iter display_error errors;
          OpamGlobals.exit 2
      ) else
        Aborted
    )

  let find_solution action_k t l_request =
    let available = Lazy.force t.available_packages in
    let l_pkg = OpamPackage.Set.fold (fun nv l -> debpkg_of_nv t action_k nv :: l) available [] in
    let conflicts = ref (fun _ -> "") in
    let rec aux = function
      | []                   -> Conflicts !conflicts
      | request :: l_request ->
          match OpamSolver.resolve (OpamSolver.U l_pkg) request t.installed with
          | Conflicts cs  ->
              log "heuristic with no solution";
              conflicts := cs;
              aux l_request
          | Success sol -> Success sol in
    aux l_request

  let new_variables e =
    let open OpamMisc in
    let e = List.filter (fun (_,s,_) -> s="=") e in
    let e = List.map (fun (v,_,_) -> v) e in
    List.fold_right StringSet.add e StringSet.empty

  let variable_warnings = ref false
  let print_variable_warnings () =
    let variables = ref [] in
    if not !variable_warnings then (
      let t = load_state () in
      let warn w =
        let is_defined s =
          try let _ = Sys.getenv s in true
          with Not_found -> false in
        if is_defined w then
          variables := w :: !variables in

      (* 1. Warn about OCAMLFIND variables if it is installed *)
      let ocamlfind_vars = [
        "OCAMLFIND_DESTDIR";
        "OCAMLFIND_CONF";
        "OCAMLFIND_METADIR";
        "OCAMLFIND_COMMANDS";
        "OCAMLFIND_LDCONF";
      ] in
      if OpamPackage.Set.exists (fun nv -> OpamPackage.Name.to_string (OpamPackage.name nv) = "ocamlfind") t.installed then
        List.iter warn ocamlfind_vars;
      (* 2. Warn about variables possibly set by other compilers *)
      let new_variables version =
        let comp_f = OpamPath.compiler t.root version in
        let env = OpamFile.Comp.env (OpamFile.Comp.read comp_f) in
        new_variables env in
      let vars = ref OpamMisc.StringSet.empty in
      OpamAlias.Map.iter (fun _ version ->
        vars := OpamMisc.StringSet.union !vars (new_variables version)
      ) t.aliases;
      vars := OpamMisc.StringSet.diff !vars (new_variables t.compiler);
      OpamMisc.StringSet.iter warn !vars;
      if !variables <> [] then (
        OpamGlobals.msg "The following variables are set in your environment, \
                     you should better unset it if you want OPAM to work \
                     correctly.\n";
        List.iter (OpamGlobals.msg " - %s\n") !variables;
        if not (OpamMisc.confirm "Do you want to continue ?") then
          OpamGlobals.exit 0;
      );
      variable_warnings := true;
    )

  let resolve ?(force=false) action_k t l_request =
    match find_solution action_k t l_request with
    | Conflicts cs ->
        OpamGlobals.msg "No solution has been found:\n%s\n" (cs ());
        No_solution
    | Success sol ->
      print_variable_warnings ();
      apply_solution ~force t sol

end

let dry_upgrade () =
  log "dry-upgrade";
  let t = load_state () in
  let reinstall = OpamPackage.Set.inter t.reinstall t.installed in
  let solution = Heuristic.find_solution (`upgrade reinstall) t
    (List.map (fun to_upgrade ->
      { wish_install = [];
        wish_remove  = [];
        wish_upgrade = OpamPackage.Name.Map.values (Heuristic.get_installed t to_upgrade) })
       [ Heuristic.v_max; Heuristic.v_ge ]) in
  match solution with
  | Conflicts _ -> None
  | Success sol -> Some (OpamSolver.stats sol)

let upgrade names =
  log "upgrade %s" (OpamPackage.Name.Set.to_string names);
  let t = load_state () in
  let reinstall = OpamPackage.Set.inter t.reinstall t.installed in
  let to_not_reinstall = ref OpamPackage.Set.empty in
  let solution_found = ref No_solution in
  if OpamPackage.Name.Set.is_empty names then (
    let solution = Heuristic.resolve (`upgrade reinstall) t
      (List.map (fun to_upgrade ->
        { wish_install = [];
          wish_remove  = [];
          wish_upgrade = OpamPackage.Name.Map.values (Heuristic.get_installed t to_upgrade) })
         [ Heuristic.v_max; Heuristic.v_ge ]) in
    solution_found := solution;
  ) else (
    let names = Heuristic.nv_of_names t names in
    let partial_reinstall = OpamPackage.Set.of_list (List.map nv_of_version_constraint names) in
    to_not_reinstall := OpamPackage.Set.diff reinstall partial_reinstall;
    let solution = Heuristic.resolve (`upgrade partial_reinstall) t
      (List.map (fun (to_upgrade, to_keep) ->
        let wish_install = Heuristic.get_installed t to_keep in
        let wish_install =
          (* Remove the packages in [names] *)
          OpamPackage.Name.Map.filter
            (fun n _ -> List.for_all (fun vc -> name_of_version_constraint vc <> n) names)
            wish_install in
        let wish_install = OpamPackage.Name.Map.values wish_install in
        let wish_upgrade = Heuristic.apply to_upgrade names in
        { wish_install;
          wish_remove  = [];
          wish_upgrade })
         [ (Heuristic.v_max, Heuristic.v_eq);
           (Heuristic.v_max, Heuristic.v_ge);
           (Heuristic.v_max, Heuristic.v_any);
           (Heuristic.v_ge , Heuristic.v_eq);
           (Heuristic.v_ge , Heuristic.v_ge);
           (Heuristic.v_ge , Heuristic.v_any); ]
      ) in
    solution_found := solution;
  );
  let t = load_state () in
  begin match !solution_found with
    | OK            -> ()
    | Nothing_to_do -> OpamGlobals.msg "Already up-to-date.\n"
    | Aborted
    | No_solution   -> to_not_reinstall := reinstall
  end;
  let reinstall = OpamPackage.Set.inter t.installed !to_not_reinstall in
  let reinstall_f = OpamPath.Alias.reinstall t.root t.alias in
  if OpamPackage.Set.is_empty reinstall then
    OpamFilename.remove reinstall_f
  else
    OpamFile.Reinstall.write reinstall_f reinstall;
  error_if_no_solution !solution_found

let check_opam_version () =
  let t = load_state () in
  let n = OpamPackage.Name.of_string "opam" in
  match find_package_by_name t n with
  | None   -> ()
  | Some _ ->
    let max_version = OpamPackage.Version.Set.max_elt (available_versions t n) in
    let max_version = OpamVersion.of_string (OpamPackage.Version.to_string max_version) in
    if OpamVersion.compare max_version OpamVersion.current > 0 then (
      if OpamMisc.confirm "Your version of opam (%s) is not up-to-date. Do you want to upgrade to version %s ?"
        (OpamVersion.to_string OpamVersion.current)
        (OpamVersion.to_string max_version)
      then
        upgrade (OpamPackage.Name.Set.singleton n)
    )

let update repos =
  log "update %s" (OpamMisc.string_of_list OpamRepositoryName.to_string repos);
  let t = load_state () in
  let repositories =
    if repos = [] then
      t.repositories
    else
      let aux r _ = List.mem r repos in
      OpamRepositoryName.Map.filter aux t.repositories in
  if not (OpamRepositoryName.Map.is_empty repositories) then (
    update_repositories t ~show_compilers:true repositories;
    update_packages t ~show_packages:true repositories;
  );
  match dry_upgrade () with
  | None   -> OpamGlobals.msg "Already up-to-date.\n"
  | Some _ ->
    check_opam_version ();
    (* we re-run dry_upgrade, as some packages might have been
       upgraded by the precedent function *)
    match dry_upgrade () with
    | None       -> OpamGlobals.msg "Already up-to-date.\n"
    | Some stats ->
      if sum stats > 0 then (
        OpamGlobals.msg "%s\n" (OpamSolver.string_of_stats stats);
        OpamGlobals.msg "You can now run 'opam upgrade' to upgrade your system.\n"
      ) else
        OpamGlobals.msg "Already up-to-date.\n"

let init repo compiler cores =
  log "init %s" (OpamRepository.to_string repo);
  let root = OpamPath.default () in
  let config_f = OpamPath.config root in
  if OpamFilename.exists config_f then
    OpamGlobals.error_and_exit "%s already exist" (OpamFilename.to_string config_f)
  else try
    let repo_p = OpamPath.Repository.create root repo.repo_name in
    (* Create (possibly empty) configuration files *)
    let alias = match compiler with
      | None   -> OpamAlias.default
      | Some c -> OpamAlias.of_string (OpamCompiler.to_string c) in
    let compiler = match compiler with
      | None   -> OpamCompiler.default
      | Some c -> c in

    (* Create ~/.opam/compilers/system.comp *)
    let system_version = OpamCompiler.Version.current () in
    create_default_compiler_description root system_version;

    (* Create ~/.opam/config *)
    let config = OpamFile.Config.create OpamVersion.current alias system_version [repo.repo_name] cores in
    OpamFile.Config.write config_f config;

    (* Create ~/.opam/aliases *)
    OpamFile.Aliases.write (OpamPath.aliases root) (OpamAlias.Map.add alias compiler OpamAlias.Map.empty);

    (* Init repository *)
    OpamFile.Repo_index.write (OpamPath.repo_index root) OpamPackage.Name.Map.empty;
    OpamFile.Repo_config.write (OpamPath.Repository.config repo_p) repo;
    OpamRepository.init repo;

    (* Init global dirs *)
    OpamFilename.mkdir (OpamPath.opam_dir root);
    OpamFilename.mkdir (OpamPath.descr_dir root);
    OpamFilename.mkdir (OpamPath.archives_dir root);
    OpamFilename.mkdir (OpamPath.compilers_dir root);

    (* Finally, load the partial state, and call the update functions to finish the initialisation *)
    let t = load_state () in
    update_repositories t ~show_compilers:false t.repositories;
    let t = load_state () in
    let alias = OpamAlias.of_string (OpamCompiler.to_string compiler) in
    let quiet = (compiler = OpamCompiler.default) in
    init_ocaml t quiet alias compiler;
    update_packages t ~show_packages:false t.repositories;
    let t = load_state () in
    let wish_install = Heuristic.get_comp_packages t compiler Heuristic.v_any in
    let _solution = Heuristic.resolve ~force:true `init t
      [ { wish_install
        ; wish_remove = []
        ; wish_upgrade = [] } ] in

    print_env_warning ~add_profile:true t

  with e ->
    if not !OpamGlobals.debug then
      OpamFilename.rmdir (OpamPath.root root);
    raise e

let install names =
  log "install %s" (OpamPackage.Name.Set.to_string names);
  let t = load_state () in
  let names = Heuristic.nv_of_names t names in
  let nvs = List.map nv_of_version_constraint names in

  let pkg_skip, pkg_new =
    List.partition (fun nv ->
      let name = OpamPackage.name nv in
      OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = name) t.installed
    ) nvs in

  (* Display a message if at least one package is already installed *)
  List.iter
    (fun nv ->
      let nv = find_installed_package_by_name t (OpamPackage.name nv) in
      OpamGlobals.msg
        "Package %s is already installed (current version is %s)\n"
        (OpamPackage.Name.to_string (OpamPackage.name nv))
        (OpamPackage.Version.to_string (OpamPackage.version nv)))
    pkg_skip;

  if pkg_new <> [] then (

    (* Display a warning if at least one package contains
       dependencies to some unknown packages *)
    let available = OpamPackage.to_map (Lazy.force t.available_packages) in
    List.iter
      (fun nv ->
        let opam = find_opam t nv in
        let f_warn (n, _) =
          if not (OpamPackage.Name.Map.mem n available) then
            OpamGlobals.warning "unknown package %S" (OpamPackage.Name.to_string n)
        in
        List.iter (OpamFormula.iter f_warn) [
          OpamFile.OPAM.depends opam;
          OpamFile.OPAM.depopts opam;
          OpamFile.OPAM.conflicts opam;
        ]
      ) pkg_new;

    let name_new = List.map OpamPackage.name pkg_new in
    List.iter (fun n -> log "new: %s" (OpamPackage.Name.to_string n)) name_new;

    let universe =
      OpamSolver.U (OpamPackage.Set.fold
                      (fun nv l -> (debpkg_of_nv t `install nv) :: l)
                      (Lazy.force t.available_packages) []) in
    let depends =
      OpamSolver.get_backward_dependencies ~depopts:true universe
        (OpamSolver.P (List.rev_map (fun nv -> debpkg_of_nv t `install nv) pkg_new)) in
    let depends = OpamPackage.Set.of_list (List.rev_map OpamPackage.of_dpkg depends) in
    log "depends=%s" (OpamPackage.Set.to_string depends);
    let depends =
      OpamPackage.Set.filter (fun nv ->
        let name = OpamPackage.name nv in
        OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = name) t.installed
      ) depends in

    let name_might_change = List.map OpamPackage.name (OpamPackage.Set.elements depends) in

    (* A gross approximation of the collection of packages which migh
       be upgraded/downloaded by the installation process *)
    let pkg_might_change f_h =
      let pkgs = Heuristic.get_installed t f_h in
      let pkgs = OpamPackage.Name.Map.filter (fun n _ -> List.mem n name_might_change) pkgs in
      OpamPackage.Name.Map.values pkgs in

    (* The collection of packages which should change very rarely (so the NOT is a bit misleading
       as it may happen if some packages indirectly List.map name_of_version_constraint pkg_new
       add new package constraints) *)
    let pkg_not_change f_h =
      let pkgs = Heuristic.get_installed t f_h in
      let pkgs = OpamPackage.Name.Map.filter (fun n _ -> not (List.mem n name_new)) pkgs in
      let pkgs = OpamPackage.Name.Map.filter (fun n _ -> not (List.mem n name_might_change)) pkgs in
      OpamPackage.Name.Map.values pkgs in

    let pkg_new =
      List.filter
        (fun v_cstr -> List.mem (name_of_version_constraint v_cstr) name_new)
        names in
    let solution = Heuristic.resolve `install t
      (List.map
         (fun (f_new, f_might, f_not) ->
           { wish_install =
               Heuristic.apply f_new pkg_new
               @ (pkg_might_change f_might)
               @ (pkg_not_change f_not)
           ; wish_remove  = []
           ; wish_upgrade = [] })
         (let open Heuristic in
          [ v_max, v_eq , v_eq
          ; v_max, v_ge , v_eq
          ; v_max, v_any, v_eq
          ; v_any, v_eq , v_eq
          ; v_any, v_ge , v_eq
          ; v_any, v_any, v_eq
          ; v_any, v_any, v_any ])
      ) in
    error_if_no_solution solution
  )

let remove names =
  log "remove %s" (OpamPackage.Name.Set.to_string names);
  let default_package = OpamPackage.Name.of_string OpamGlobals.default_package in
  if OpamPackage.Name.Set.mem default_package names then
    OpamGlobals.msg "Package %s can not be removed.\n" OpamGlobals.default_package;
  let names = OpamPackage.Name.Set.filter (fun n -> n <> default_package) names in
  let t = load_state () in
  let wish_remove = Heuristic.nv_of_names t names in
  let wish_remove, not_installed, does_not_exist =
    let aux (wish_remove, not_installed, does_not_exist) c nv =
      let name = OpamPackage.name nv in
      if not (OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = name) t.installed) then
        (wish_remove, OpamPackage.Name.Set.add name not_installed, does_not_exist)
      else if not (OpamPackage.Set.mem nv (Lazy.force t.available_packages)) then
        (wish_remove, not_installed, nv :: does_not_exist)
      else
        (c :: wish_remove, not_installed, does_not_exist) in
    List.fold_left
      (fun accu c ->
        match c with
        | V_eq (n, v)
        | V_any (n, _, Some v) -> aux accu c (OpamPackage.create n v)
        | V_any (n, _, None)   ->
            match find_package_by_name t n with
            | None    -> accu
            | Some vs ->  OpamPackage.Set.fold (fun v accu -> aux accu c v) vs accu
      ) ([], OpamPackage.Name.Set.empty, []) wish_remove in

  if does_not_exist <> [] then (
    List.iter (proceed_todelete ~rm_build:true t) does_not_exist;
    let installed_f = OpamPath.Alias.installed t.root t.alias in
    let installed = OpamFile.Installed.read installed_f in
    let installed = OpamPackage.Set.filter (fun nv -> not (List.mem nv does_not_exist)) installed in
    OpamFile.Installed.write installed_f installed;
  );

  if not (OpamPackage.Name.Set.is_empty not_installed) then (
    if OpamPackage.Name.Set.cardinal not_installed = 1 then
      OpamGlobals.msg "%s is not installed.\n" (OpamPackage.Name.to_string (OpamPackage.Name.Set.choose not_installed))
    else
      OpamGlobals.msg "%s are not installed.\n" (OpamPackage.Name.Set.to_string not_installed)
  );

  if wish_remove <> [] then (
    let universe =
      OpamSolver.U (OpamPackage.Set.fold (fun nv l -> (debpkg_of_nv t `remove nv) :: l) (Lazy.force t.available_packages) []) in
    let depends =
      OpamSolver.get_forward_dependencies ~depopts:true universe
        (OpamSolver.P (List.rev_map
                     (fun vc -> debpkg_of_nv t `remove (nv_of_version_constraint vc))
                     wish_remove)) in
    let depends = OpamPackage.Set.of_list (List.rev_map OpamPackage.of_dpkg depends) in
    let depends = OpamPackage.Set.filter (fun nv -> OpamPackage.Set.mem nv t.installed) depends in
    let wish_remove = Heuristic.apply Heuristic.v_eq wish_remove in
    let solution = Heuristic.resolve `remove t
      (List.map
         (fun f_h ->
           let installed = Heuristic.get_installed t f_h in
           let installed =
             OpamPackage.Name.Map.filter
               (fun n _ -> not (OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = n) depends))
               installed in
           { wish_install = OpamPackage.Name.Map.values installed
           ; wish_remove
           ; wish_upgrade = [] })
         [ Heuristic.v_eq
         ; Heuristic.v_any ]) in
    error_if_no_solution solution
  )

let reinstall names =
  log "reinstall %s" (OpamPackage.Name.Set.to_string names);
  let t = load_state () in
  let packages = Heuristic.nv_of_names t names in
  let reinstall_new =
    OpamMisc.filter_map (function
      | V_any (n, _, Some v)
      | V_eq (n, v) -> Some (OpamPackage.create n v)
      | V_any (n, _, _) ->
          OpamGlobals.msg "%s is not installed" (OpamPackage.Name.to_string n);
          None
    ) packages in
  let reinstall_new = OpamPackage.Set.of_list reinstall_new in
  let reinstall_f = OpamPath.Alias.reinstall t.root t.alias in
  let reinstall_old = OpamFile.Reinstall.safe_read reinstall_f in
  OpamFile.Reinstall.write reinstall_f (OpamPackage.Set.union reinstall_new reinstall_old);
  upgrade names

let upload upload repo =
  log "upload %s" (string_of_upload upload);
  let t = load_state () in
  let opam = OpamFile.OPAM.read upload.upl_opam in
  let name = OpamFile.OPAM.name opam in
  let version = OpamFile.OPAM.version opam in
  let nv = OpamPackage.create name version in
  let repo = match repo with
  | None ->
      if OpamPackage.Name.Map.mem name t.repo_index then
        (* We upload the package to the first available repository. *)
        find_repo_by_name t (List.hd (OpamPackage.Name.Map.find name t.repo_index))
      else
        OpamGlobals.error_and_exit "No repository found to upload %s" (OpamPackage.to_string nv)
  | Some repo ->
      if mem_repository t repo then
        find_repo_by_name t repo
      else
        OpamGlobals.error_and_exit "Unbound repository %S (available = %s)"
          (OpamRepositoryName.to_string repo)
          (string_of_repositories t.repositories) in
  let repo_p = OpamPath.Repository.create t.root repo.repo_name in
  let upload_repo = OpamPath.Repository.raw (OpamPath.Repository.upload_dir repo_p) in
  let upload_opam = OpamPath.Repository.opam upload_repo nv in
  let upload_descr = OpamPath.Repository.descr upload_repo nv in
  let upload_archives = OpamPath.Repository.archive upload_repo nv in
  OpamFilename.copy upload.upl_opam upload_opam;
  OpamFilename.copy upload.upl_descr upload_descr;
  OpamFilename.copy upload.upl_archive upload_archives;
  OpamRepository.upload repo;
  OpamFilename.rmdir (OpamPath.Repository.package upload_repo nv);
  OpamFilename.remove (OpamPath.Repository.archive upload_repo nv)


let full_sections l =
  String.concat " " (List.map OpamVariable.Section.Full.to_string l)

let string_of_config_option t =
  Printf.sprintf "rec=%b bytecode=%b link=%b options=%s"
    t.conf_is_rec t.conf_is_byte t.conf_is_link (full_sections t.conf_options)

let string_of_config = function
  | CEnv        -> "env"
  | CList       -> "list-vars"
  | CVariable v -> Printf.sprintf "var(%s)" (OpamVariable.Full.to_string v)
  | CCompil c   -> string_of_config_option c
  | CSubst l    -> String.concat "," (List.map OpamFilename.Base.to_string l)
  | CIncludes (b,l) ->
      Printf.sprintf "include(%b,%s)"
        b (String.concat "," (List.map OpamPackage.Name.to_string l))

(* List all the available variables *)
let config_list t =
  let configs =
    OpamPackage.Set.fold (fun nv l ->
      let file = dot_config t (OpamPackage.name nv) in
      (nv, file) :: l
    ) t.installed [] in
  let variables =
    List.fold_left (fun accu (nv, c) ->
      let name = OpamPackage.name nv in
      (* add all the global variables *)
      let globals =
        List.fold_left (fun accu v ->
          (OpamVariable.Full.create_global name v, OpamFile.Dot_config.variable c v) :: accu
        ) accu (OpamFile.Dot_config.variables c) in
      (* then add the local variables *)
      List.fold_left
        (fun accu n ->
          let variables = OpamFile.Dot_config.Section.variables c n in
          List.fold_left (fun accu v ->
            (OpamVariable.Full.create_local name n v,
             OpamFile.Dot_config.Section.variable c n v) :: accu
          ) accu variables
        ) globals (OpamFile.Dot_config.Section.available c)
    ) [] configs in
  List.iter (fun (fv, contents) ->
    OpamGlobals.msg "%-20s : %s\n"
      (OpamVariable.Full.to_string fv)
      (OpamVariable.string_of_variable_contents contents)
  ) (List.rev variables)

(* Return the transitive closure of dependencies sorted in topological order *)
let get_transitive_dependencies t ?(depopts = false) names =
  let universe = OpamSolver.U (List.map (debpkg_of_nv t `config) (OpamPackage.Set.elements t.installed)) in
  (* Compute the transitive closure of dependencies *)
  let pkg_of_name n =
    let nv = find_installed_package_by_name t n in
    debpkg_of_nv t `config nv in
  let request = OpamSolver.P (List.map pkg_of_name names) in
  let depends = OpamSolver.get_backward_dependencies ~depopts universe request in
  let topo = List.map OpamPackage.of_dpkg depends in
  topo

let config_includes t is_rec names =
  let deps =
    if is_rec then
      List.map OpamPackage.name (get_transitive_dependencies t ~depopts:true names)
    else
      names in
  let includes =
    List.fold_left (fun accu n ->
      "-I" :: OpamFilename.Dir.to_string (OpamPath.Alias.lib t.root t.alias n) :: accu
    ) [] (List.rev deps) in
  OpamGlobals.msg "%s\n" (String.concat " " includes)

let config_compil t c =
  let comp = compiler_description t t.compiler in
  let names =
    OpamMisc.filter_map
      (fun (n,_) ->
        if OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = n) t.installed
        then Some n
        else None)
      (OpamFormula.atoms (OpamFile.Comp.packages comp))
    @ List.map OpamVariable.Section.Full.package c.conf_options in
  (* Compute the transitive closure of package dependencies *)
  let package_deps =
    if c.conf_is_rec then
      List.map OpamPackage.name (get_transitive_dependencies t ~depopts:true names)
    else
      names in
  (* Map from libraries to package *)
  (* NOTES: we check that the set of packages/libraries given on
     the command line is consistent, ie. there isn't two libraries
     with the same name in the transitive closure of
     depedencies *)
  let library_map =
    List.fold_left (fun accu n ->
      let nv = find_installed_package_by_name t n in
      let opam = find_opam t nv in
      let sections = (OpamFile.OPAM.libraries opam) @ (OpamFile.OPAM.syntax opam) in
      List.iter (fun s ->
        if OpamVariable.Section.Map.mem s accu then
          OpamGlobals.error_and_exit "Conflict: the library %s appears in %s and %s"
            (OpamVariable.Section.to_string s)
            (OpamPackage.Name.to_string n)
            (OpamPackage.Name.to_string (OpamVariable.Section.Map.find s accu))
      ) sections;
      List.fold_left (fun accu s -> OpamVariable.Section.Map.add s n accu) accu sections
    ) OpamVariable.Section.Map.empty package_deps in
  (* Compute the transitive closure of libraries dependencies *)
  let library_deps =
    let graph = OpamVariable.Section.G.create () in
    let todo = ref OpamVariable.Section.Set.empty in
    let add_todo s =
      if OpamVariable.Section.Map.mem s library_map then
        todo := OpamVariable.Section.Set.add s !todo
      else
        OpamGlobals.error_and_exit "Unbound section %S" (OpamVariable.Section.to_string s) in
    let seen = ref OpamVariable.Section.Set.empty in
    (* Init the graph with vertices from the command-line *)
    (* NOTES: we check that [todo] is initialized before the [loop] *)
    List.iter (fun s ->
      let name = OpamVariable.Section.Full.package s in
      let sections = match OpamVariable.Section.Full.section s with
        | None   ->
          let config = dot_config t name in
          OpamFile.Dot_config.Section.available config
        | Some s -> [s] in
      List.iter (fun s ->
        OpamVariable.Section.G.add_vertex graph s;
        add_todo s;
      ) sections
    ) c.conf_options;
    (* Also add the [requires] field of the compiler description *)
    List.iter (fun s ->
      OpamVariable.Section.G.add_vertex graph s;
      add_todo s
    ) (OpamFile.Comp.requires comp);
    (* Least fix-point to add edges and missing vertices *)
    let rec loop () =
      if not (OpamVariable.Section.Set.is_empty !todo) then
        let s = OpamVariable.Section.Set.choose !todo in
        todo := OpamVariable.Section.Set.remove s !todo;
        seen := OpamVariable.Section.Set.add s !seen;
        let name = OpamVariable.Section.Map.find s library_map in
        let config = dot_config t name in
        let childs = OpamFile.Dot_config.Section.requires config s in
        (* keep only the build reqs which are in the package dependency list
           and the ones we haven't already seen *)
        List.iter (fun child ->
          OpamVariable.Section.G.add_vertex graph child;
          OpamVariable.Section.G.add_edge graph child s;
        ) childs;
        let new_childs =
          List.filter (fun s ->
            OpamVariable.Section.Map.mem s library_map && not (OpamVariable.Section.Set.mem s !seen)
          ) childs in
        todo := OpamVariable.Section.Set.union (OpamVariable.Section.Set.of_list new_childs) !todo;
        loop ()
    in
    loop ();
    let nodes = ref [] in
    OpamVariable.Section.graph_iter (fun n -> nodes := n :: !nodes) graph;
    !nodes in
  let fn_comp = match c.conf_is_byte, c.conf_is_link with
    | true , true  -> OpamFile.Comp.bytelink
    | true , false -> OpamFile.Comp.bytecomp
    | false, true  -> OpamFile.Comp.asmlink
    | false, false -> OpamFile.Comp.asmcomp in
  let fn = match c.conf_is_byte, c.conf_is_link with
    | true , true  -> OpamFile.Dot_config.Section.bytelink
    | true , false -> OpamFile.Dot_config.Section.bytecomp
    | false, true  -> OpamFile.Dot_config.Section.asmlink
    | false, false -> OpamFile.Dot_config.Section.asmcomp in
  let strs =
    fn_comp comp ::
      List.fold_left (fun accu s ->
        let name = OpamVariable.Section.Map.find s library_map in
        let config = dot_config t name in
        fn config s :: accu
      ) [] library_deps in
  let output = String.concat " " (List.flatten strs) in
  log "OUTPUT: %S" output;
  OpamGlobals.msg "%s\n" output

let config request =
  log "config %s" (string_of_config request);
  let t = load_state () in
  match request with
  | CEnv                      -> print_env (get_env t)
  | CList                     -> config_list t
  | CSubst fs                 -> List.iter (substitute_file t) fs
  | CIncludes (is_rec, names) -> config_includes t is_rec names
  | CCompil c                 -> config_compil t c
  | CVariable v               ->
    let contents = contents_of_variable t v in
    OpamGlobals.msg "%s\n" (OpamVariable.string_of_variable_contents contents)

let rec remote action =
  log "remote %s" (string_of_remote action);
  let t = load_state () in
  let update_config repos =
    let new_config = OpamFile.Config.with_repositories t.config repos in
    OpamFile.Config.write (OpamPath.config t.root) new_config in
  let cleanup_repo repo =
    let repos = OpamRepositoryName.Map.keys t.repositories in
    update_config (List.filter ((<>) repo) repos);
    let t = load_state () in
    update_repo_index t;
    OpamFilename.rmdir (OpamPath.Repository.root (OpamPath.Repository.create t.root repo)) in
  match action with
  | RList  ->
      let pretty_print r =
        OpamGlobals.msg "%4d %-7s %10s     %s\n"
          r.repo_priority
          (Printf.sprintf "[%s]" r.repo_kind)
          (OpamRepositoryName.to_string r.repo_name)
          (OpamFilename.Dir.to_string r.repo_address) in
      let repos = sorted_repositories t in
      List.iter pretty_print repos;
  | RAdd (name, kind, address, priority) ->
      let repo = {
        repo_name     = name;
        repo_kind     = kind;
        repo_address  = address;
        repo_priority = min_int; (* we initially put it as low-priority *)
      } in
      if mem_repository t name then
        OpamGlobals.error_and_exit "%s is already a remote repository" (OpamRepositoryName.to_string name)
      else (
        (try OpamRepository.init repo with
        | OpamRepository.Unknown_backend ->
            OpamGlobals.error_and_exit "\"%s\" is not a supported backend" repo.repo_kind
        | e ->
            cleanup_repo repo.repo_name;
            raise e);
        log "Adding %s" (OpamRepository.to_string repo);
        update_config (repo.repo_name :: OpamRepositoryName.Map.keys t.repositories)
      );
      (try
         update [name];
         let priority = match priority with
           | None   -> 10 * (OpamRepositoryName.Map.cardinal t.repositories);
           | Some p -> p in
         remote (RPriority (name, priority))
       with e ->
         cleanup_repo name;
         raise e)
  | RRm name  ->
      if mem_repository t name then
        cleanup_repo name
      else
        OpamGlobals.error_and_exit "%s is not a a valid remote name"
          (OpamRepositoryName.to_string name)
  | RPriority (name, p) ->
    if mem_repository t name then (
      let config_f = OpamPath.Repository.config (OpamPath.Repository.create t.root name) in
      let config = OpamFile.Repo_config.read config_f in
      let config = { config with repo_priority = p } in
      OpamFile.Repo_config.write config_f config;
      let repo_index_f = OpamPath.repo_index t.root in
      let repo_index = OpamPackage.Name.Map.map (List.filter ((<>)name)) t.repo_index in
      OpamFile.Repo_index.write repo_index_f repo_index;
      let t = load_state () in
      update_repo_index t;
    ) else
        OpamGlobals.error_and_exit "%s is not a a valid remote name"
          (OpamRepositoryName.to_string name)

let pin action =
  log "pin %s" (string_of_pin action);
  let t = load_state () in
  let pin_f = OpamPath.Alias.pinned t.root t.alias in
  let pins = OpamFile.Pinned.safe_read pin_f in
  let name = action.pin_package in
  let update_config pins =
    OpamPackage.Version.Set.iter (fun version ->
      let nv = OpamPackage.create name version in
      OpamFilename.rmdir (OpamPath.Alias.build t.root t.alias nv)
    ) (available_versions t name);
    OpamFile.Pinned.write pin_f pins in
  if mem_installed_package_by_name t name then (
    let reinstall_f = OpamPath.Alias.reinstall t.root t.alias in
    let reinstall = OpamFile.Reinstall.safe_read reinstall_f in
    let nv = find_installed_package_by_name t name in
    OpamFile.Reinstall.write reinstall_f (OpamPackage.Set.add nv reinstall)
  );
  match action.pin_arg with
  | Unpin -> update_config (OpamPackage.Name.Map.remove name pins)
  | _     ->
      if OpamPackage.Name.Map.mem name pins then (
        let current = OpamPackage.Name.Map.find name pins in
        OpamGlobals.error_and_exit "Cannot pin %s to %s, it is already associated to %s."
          (OpamPackage.Name.to_string name)
          (path_of_pin_option action.pin_arg)
          (path_of_pin_option current);
      );
      log "Adding %s(%s) => %s"
        (path_of_pin_option action.pin_arg)
        (kind_of_pin_option action.pin_arg)
        (OpamPackage.Name.to_string name);
      update_config (OpamPackage.Name.Map.add name action.pin_arg pins)

let pin_list () =
  log "pin_list";
  let t = load_state () in
  let pins = OpamFile.Pinned.safe_read (OpamPath.Alias.pinned t.root t.alias) in
  let print n a =
    OpamGlobals.msg "%-20s %-8s %s\n" (OpamPackage.Name.to_string n) (kind_of_pin_option a) (path_of_pin_option a) in
  OpamPackage.Name.Map.iter print pins

let compiler_list () =
  log "compiler_list";
  let t = load_state () in
  let descrs = available_compilers t in
  let aliases = OpamFile.Aliases.read (OpamPath.aliases t.root) in
  OpamGlobals.msg "--- Installed compilers ---\n";
  OpamAlias.Map.iter (fun n c ->
    let current = if n = t.alias then "*" else " " in
    let compiler = OpamCompiler.to_string c in
    let alias_name = OpamAlias.to_string n in
    if alias_name = compiler then
      OpamGlobals.msg " %s %s\n" current alias_name
    else
      OpamGlobals.msg " %s %s [%s]\n" current compiler alias_name
  ) aliases;
  OpamGlobals.msg "\n--- Available compilers ---\n";
  OpamCompiler.Set.iter (fun c ->
    let comp = OpamFile.Comp.read (OpamPath.compiler t.root c) in
    let preinstalled = if OpamFile.Comp.preinstalled comp then "~" else " " in
    let version = OpamFile.Comp.version comp in
    let version, compiler =
      if OpamCompiler.Version.to_string version = OpamCompiler.to_string c then
        OpamCompiler.Version.to_string version, ""
      else
        OpamCompiler.Version.to_string version,
        Printf.sprintf "(%s)" (OpamCompiler.to_string c) in
    OpamGlobals.msg " %s %-8s %s\n" preinstalled version compiler
  ) descrs

let compiler_install quiet alias compiler =
  log "compiler_install %b %s %s" quiet
    (OpamAlias.to_string alias)
    (OpamCompiler.to_string compiler);

  (* install the new OCaml version *)
  init_ocaml (load_state ()) quiet alias compiler;

  (* install the compiler packages *)
  let t = load_state () in
  let packages =
    OpamPackage.Name.Map.of_list
      (List.rev_map
         (function (name, _), _ as nv -> OpamPackage.Name.of_string name, nv)
         (Heuristic.get_comp_packages t compiler Heuristic.v_eq)) in

  let is_ok =
    OpamPackage.Name.Map.for_all (fun n c ->
      if mem_installed_package_by_name t n then (
        let nv = find_installed_package_by_name t n in
        c = Heuristic.vpkg_of_nv_eq n (OpamPackage.version nv)
      ) else (
        false
      )
    ) packages in
  if not is_ok then (
    let _solution = Heuristic.resolve ~force:true `switch t
      [ { wish_install = OpamPackage.Name.Map.values packages
        ; wish_remove = []
        ; wish_upgrade = [] } ] in
    ()
  );

  print_env_warning t

let compiler_switch quiet alias =
  log "compiler_switch alias=%s" (OpamAlias.to_string alias);
  let t = load_state () in
  let comp_dir = OpamPath.Alias.root t.root alias in
  let compiler = OpamCompiler.of_string (OpamAlias.to_string alias) in
  let comp_f = OpamPath.compiler t.root compiler in
  if not (OpamFilename.exists_dir comp_dir) && not (OpamFilename.exists comp_f) then (
    OpamGlobals.error "The compiler's description for %s does not exist.\n" (OpamAlias.to_string alias);
    OpamGlobals.exit 1;
  );
  if not (OpamFilename.exists_dir comp_dir) then
    compiler_install quiet alias compiler
  else
    let config = OpamFile.Config.with_alias t.config alias in
    OpamFile.Config.write (OpamPath.config t.root) config;
    print_env_warning (load_state ())

let compiler_import filename =
  log "compiler_clone alias=%s" (OpamFilename.to_string filename);
  let t = load_state () in

  let imported = OpamFile.Installed.read filename in
  let new_packages = OpamPackage.Set.diff imported t.installed in
  let installed =
    OpamPackage.Set.filter (fun nv ->
      let name = OpamPackage.name nv in
      not (OpamPackage.Set.exists (fun nv -> name = OpamPackage.name nv) new_packages)
    ) t.installed in

  let constraints f_h = Heuristic.get t (OpamPackage.Set.union new_packages installed) f_h in

  let _solution = Heuristic.resolve `switch t
    (List.map (fun f_h ->
      { wish_install = OpamPackage.Name.Map.values (constraints f_h);
        wish_remove  = [];
        wish_upgrade = [] })
     [ Heuristic.v_eq; Heuristic.v_ge; Heuristic.v_any ]
    ) in
  ()

let compiler_export filename =
  let t = load_state () in
  OpamFile.Installed.write filename t.installed

let compiler_current () =
  let t = load_state () in
  OpamGlobals.msg "%s\n" (OpamAlias.to_string t.alias)

let compiler_remove alias =
  log "compiler_remove alias=%s" (OpamAlias.to_string alias);
  let t = load_state () in
  let comp_dir = OpamPath.Alias.root t.root alias in
  if not (OpamFilename.exists_dir comp_dir) then (
    OpamGlobals.msg "The compiler alias %s does not exist.\n" (OpamAlias.to_string alias);
    OpamGlobals.exit 1;
  );
  if t.alias = alias then (
    OpamGlobals.msg "Cannot remove %s as it is the current compiler.\n" (OpamAlias.to_string alias);
    OpamGlobals.exit 1;
  );
  let aliases = OpamAlias.Map.filter (fun a _ -> a <> alias) t.aliases in
  OpamFile.Aliases.write (OpamPath.aliases t.root) aliases;
  OpamFilename.rmdir comp_dir

let compiler_reinstall alias =
  log "compiler_reinstall alias=%s" (OpamAlias.to_string alias);
  let t = load_state () in
  if not (OpamAlias.Map.mem alias t.aliases) then (
    OpamGlobals.msg "The compiler alias %s does not exist.\n" (OpamAlias.to_string alias);
    OpamGlobals.exit 1;
  );
  let ocaml_version = OpamAlias.Map.find alias t.aliases in
  compiler_remove alias;
  compiler_install false alias ocaml_version

(** We protect each main functions with a lock depending on its access
on some read/write data. *)

let list ~print_short ~installed_only ?name_only ?case_sensitive pkg_str =
  check (Read_lock (fun () -> list ~print_short ~installed_only ?name_only ?case_sensitive pkg_str))

let info package =
  check (Read_lock (fun () -> info package))

let config request =
  check (Read_lock (fun () -> config request))

let install names =
  check (Alias_lock (fun () -> install names))

let reinstall names =
  check (Alias_lock (fun () -> reinstall names))

let upgrade names =
  check (Alias_lock (fun () -> upgrade names))

let remove names =
  check (Alias_lock (fun () -> remove names))

let update repos =
  check (Global_lock (fun () -> update repos))

let upload u r =
  check (Global_lock (fun () -> upload u r))

let remote action =
  check (Global_lock (fun () -> remote action))

let compiler_install quiet alias ocaml_version =
  check (Global_lock (fun () -> compiler_install quiet alias ocaml_version))

let compiler_import filename =
  check (Alias_lock (fun () -> compiler_import filename))

let compiler_export filename =
  check (Read_lock (fun () -> compiler_export filename))

let compiler_remove alias =
  check (Global_lock (fun () -> compiler_remove alias))

let compiler_switch quiet alias =
  check (Global_lock (fun () -> compiler_switch quiet alias))

let compiler_reinstall alias =
  check (Global_lock (fun () -> compiler_reinstall alias))

let compiler_list () =
  check (Read_lock compiler_list)

let compiler_current () =
  check (Read_lock compiler_current)

let pin action =
  check (Global_lock (fun () -> pin action))

let pin_list () =
  check (Read_lock pin_list)

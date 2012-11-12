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
open OpamState.Types
open OpamFilename.OP
open OpamMisc.OP

let log fmt = OpamGlobals.log "CLIENT" fmt

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
      match List.filter (OpamState.mem_repository_name t) repo_s with
      | []     -> repo_index
      | repo_s -> OpamPackage.Name.Map.add n repo_s repo_index
    ) repo_index OpamPackage.Name.Map.empty in

  (* Write ~/.opam/repo/index *)
  OpamFile.Repo_index.write (OpamPath.repo_index t.root) repo_index;

  (* suppress previous links, but keep metadata of installed packages
     (because you need them to uninstall the package) *)
  let all_installed =
    OpamSwitch.Map.fold (fun switch _ accu ->
      let installed_f = OpamPath.Switch.installed t.root switch in
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

  (* Create symbolic links from $repo dirs to main dir *)
  OpamPackage.Name.Map.iter (fun n repo_s ->
    let all_versions = ref OpamPackage.Version.Set.empty in
    List.iter (fun r ->
      let repo = OpamState.find_repository_name t r in
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

let create_default_compiler_description root = function
  | None         -> ()
  | Some version ->
    let f =
      OpamFile.Comp.create_preinstalled
        OpamCompiler.default version
        (if !OpamGlobals.base_packages then OpamState.base_packages else [])
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
  log "update_repositories %s" (OpamState.string_of_repositories repositories);

  let old_compilers = OpamState.compilers t in

  (* first update all the given repositories *)
  OpamRepositoryName.Map.iter (fun _ repo ->
    OpamRepository.update repo
  ) repositories;

  (* Display the new compilers available *)
  OpamRepositoryName.Map.iter (fun _ repo ->
    if show_compilers then
      print_compilers t old_compilers repo
  ) repositories;

  (* Delete compiler descritions which are not installed *)
  OpamCompiler.Set.iter (fun comp ->
    if comp <> OpamCompiler.default
    && OpamSwitch.Map.for_all (fun _ c -> comp <> c) t.aliases then (
      let comp_f = OpamPath.compiler t.root comp in
      OpamFilename.remove comp_f;
    )
  ) (OpamState.compilers t);

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

(* Update the package contents, display the new packages and update reinstall *)
let update_packages t ~show_packages repositories =
  log "update_packages %s" (OpamState.string_of_repositories repositories);
  (* Update the pinned packages *)
  let pinned_updated =
    OpamPackage.Set.of_list (
      OpamMisc.filter_map
        (function
          | n, (Path p | Git p as pin) ->
            if OpamState.mem_installed_package_by_name t n then
              let nv = OpamState.find_installed_package_by_name t n in
              OpamGlobals.msg "Synchronizing with %s\n" (OpamFilename.Dir.to_string p);
              match OpamState.update_pinned_package t nv pin with
              | Up_to_date _  -> None
              | Result _      -> Some nv
              | Not_available ->
                OpamGlobals.error "%s is not available" (OpamFilename.Dir.to_string p);
                None
            else
              None
          | _ -> None)
        (OpamPackage.Name.Map.bindings t.pinned)) in

  (* then update $opam/repo/index *)
  update_repo_index t;
  let t = OpamState.load_state () in
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
  OpamSwitch.Map.iter (fun switch _ ->
    let installed = OpamFile.Installed.safe_read (OpamPath.Switch.installed t.root switch) in
    let reinstall = OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall t.root switch) in
    let reinstall =
      OpamPackage.Set.fold (fun nv reinstall ->
        if OpamPackage.Set.mem nv installed then
          OpamPackage.Set.add nv reinstall
        else
          reinstall
      ) updated reinstall in
    if not (OpamPackage.Set.is_empty reinstall) then
      OpamFile.Reinstall.write (OpamPath.Switch.reinstall t.root switch) reinstall
  ) t.aliases;

  (* Check all the dependencies exist *)
  let t = OpamState.load_state () in
  let has_error = ref false in
  OpamPackage.Set.iter (fun nv ->
    let opam = OpamState.opam t nv in
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
      match OpamState.find_packages_by_name t d with
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

let list ~print_short ~installed_only ?(name_only = true) ?(case_sensitive = false) res =
  log "list";
  let t = OpamState.load_state () in
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
    OpamGlobals.msg "%s packages for %s:\n" kind (OpamSwitch.to_string t.switch);
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
  let t = OpamState.load_state () in

  (* Compute the installed versions, for each switch *)
  let installed =
    OpamSwitch.Map.fold (fun switch _ map ->
      let installed = OpamFile.Installed.safe_read (OpamPath.Switch.installed t.root switch) in
      if OpamState.mem_installed_package_by_name_aux installed package then
        let nv = OpamState.find_installed_package_by_name_aux installed package in
        if OpamPackage.Map.mem nv map then
          let aliases = OpamPackage.Map.find nv map in
          let map = OpamPackage.Map.remove nv map in
          OpamPackage.Map.add nv (switch :: aliases) map
        else
          OpamPackage.Map.add nv [switch] map
      else
        map
    ) t.aliases OpamPackage.Map.empty in

  let installed_str =
    let one (nv, aliases) =
      Printf.sprintf "%s [%s]"
        (OpamPackage.to_string nv)
        (String.concat " " (List.map OpamSwitch.to_string aliases)) in
    String.concat ", " (List.map one (OpamPackage.Map.bindings installed)) in

  (* All the version of the package *)
  let versions = OpamPackage.versions t.packages package in
  if OpamPackage.Version.Set.is_empty versions then
    OpamSolution.unknown_package package None;
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
            let opam = OpamState.opam t nv in
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

let dry_upgrade () =
  log "dry-upgrade";
  let t = OpamState.load_state () in
  let reinstall = OpamPackage.Set.inter t.reinstall t.installed in
  let solution = OpamSolution.resolve t (Upgrade reinstall)
    { wish_install = [];
      wish_remove  = [];
      wish_upgrade = OpamSolution.atoms_of_packages t.installed } in
  match solution with
  | Conflicts _ -> None
  | Success sol -> Some (OpamSolver.stats sol)

let upgrade names =
  log "UPGRADE %s" (OpamPackage.Name.Set.to_string names);
  let t = OpamState.load_state () in
  let reinstall = OpamPackage.Set.inter t.reinstall t.installed in
  let to_not_reinstall_yet = ref OpamPackage.Set.empty in
  let solution_found = ref No_solution in
  if OpamPackage.Name.Set.is_empty names then (
    let solution = OpamSolution.resolve_and_apply t (Upgrade reinstall)
      { wish_install = [];
        wish_remove  = [];
        wish_upgrade = OpamSolution.atoms_of_packages t.installed } in
    solution_found := solution;
  ) else (
    let names = OpamSolution.atoms_of_names t names in
    let partial_reinstall =
      OpamMisc.filter_map (fun (n,_) ->
        if OpamState.mem_installed_package_by_name t n then
          Some (OpamState.find_installed_package_by_name t n)
        else (
          OpamGlobals.msg "%s is not installed" (OpamPackage.Name.to_string n);
          None
        )
      ) names in
    let partial_reinstall = OpamPackage.Set.of_list partial_reinstall in
    to_not_reinstall_yet := OpamPackage.Set.diff reinstall partial_reinstall;
    let universe = OpamState.universe t Depends in
    let partial_reinstall =
      OpamPackage.Set.of_list
        (OpamSolver.forward_dependencies ~depopts:true ~installed:true universe partial_reinstall) in
    let installed = OpamPackage.Set.diff t.installed partial_reinstall in
    let solution = OpamSolution.resolve_and_apply t (Upgrade partial_reinstall)
      { wish_install = OpamSolution.atoms_of_packages installed;
        wish_remove  = [];
        wish_upgrade = OpamSolution.atoms_of_packages partial_reinstall } in
    solution_found := solution;
  );
  let t = OpamState.load_state () in
  begin match !solution_found with
    | OK            -> ()
    | Nothing_to_do -> OpamGlobals.msg "Already up-to-date.\n"
    | Aborted
    | No_solution   -> to_not_reinstall_yet := reinstall
  end;
  let reinstall = OpamPackage.Set.inter t.installed !to_not_reinstall_yet in
  let reinstall_f = OpamPath.Switch.reinstall t.root t.switch in
  if OpamPackage.Set.is_empty reinstall then
    OpamFilename.remove reinstall_f
  else
    OpamFile.Reinstall.write reinstall_f reinstall;
  OpamSolution.error_if_no_solution !solution_found

let check_opam_version () =
  let t = OpamState.load_state () in
  let n = OpamPackage.Name.of_string "opam" in
  match OpamState.find_packages_by_name t n with
  | None   -> ()
  | Some _ ->
    let max_version = OpamPackage.Version.Set.max_elt (OpamPackage.versions (Lazy.force t.available_packages) n) in
    let max_version = OpamVersion.of_string (OpamPackage.Version.to_string max_version) in
    if OpamVersion.compare max_version OpamVersion.current > 0 then (
      if OpamMisc.confirm "Your version of opam (%s) is not up-to-date. Do you want to upgrade to version %s ?"
        (OpamVersion.to_string OpamVersion.current)
        (OpamVersion.to_string max_version)
      then
        upgrade (OpamPackage.Name.Set.singleton n)
    )

let update repos =
  log "UPDATE %s" (OpamMisc.string_of_list OpamRepositoryName.to_string repos);
  let t = OpamState.load_state () in
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
      if OpamSolution.sum stats > 0 then (
        OpamGlobals.msg "%s\n" (OpamSolver.string_of_stats stats);
        OpamGlobals.msg "You can now run 'opam upgrade' to upgrade your system.\n"
      ) else
        OpamGlobals.msg "Already up-to-date.\n"

let init repo compiler cores =
  log "INIT %s" (OpamRepository.to_string repo);
  let root = OpamPath.default () in
  let config_f = OpamPath.config root in
  if OpamFilename.exists config_f then
    OpamGlobals.error_and_exit "%s already exist" (OpamFilename.to_string config_f)
  else try
    let repo_p = OpamPath.Repository.create root repo.repo_name in
    (* Create (possibly empty) configuration files *)
    let switch = match compiler with
      | None   -> OpamSwitch.default
      | Some c -> OpamSwitch.of_string (OpamCompiler.to_string c) in
    let compiler = match compiler with
      | None   -> OpamCompiler.default
      | Some c -> c in

    (* Create ~/.opam/compilers/system.comp *)
    let system_version = OpamCompiler.Version.current () in
    create_default_compiler_description root system_version;

    (* Create ~/.opam/config *)
    let config = OpamFile.Config.create OpamVersion.current switch [repo.repo_name] cores in
    OpamFile.Config.write config_f config;

    (* Create ~/.opam/aliases *)
    OpamFile.Aliases.write (OpamPath.aliases root) (OpamSwitch.Map.add switch compiler OpamSwitch.Map.empty);

    (* Init repository *)
    OpamFile.Repo_index.write (OpamPath.repo_index root) OpamPackage.Name.Map.empty;
    OpamFile.Repo_config.write (OpamPath.Repository.config repo_p) repo;
    OpamRepository.init repo;

    (* Init global dirs *)
    OpamFilename.mkdir (OpamPath.opam_dir root);
    OpamFilename.mkdir (OpamPath.descr_dir root);
    OpamFilename.mkdir (OpamPath.archives_dir root);
    OpamFilename.mkdir (OpamPath.compilers_dir root);

    (* Load the partial state, and update the repository state *)
    log "updating repository state";
    let t = OpamState.load_state () in
    update_repositories t ~show_compilers:false t.repositories;

    (* Load the partial state, and update the packages state *)
    log "updating package state";
    let t = OpamState.load_state () in
    let switch = OpamSwitch.of_string (OpamCompiler.to_string compiler) in
    let quiet = (compiler = OpamCompiler.default) in
    OpamState.install_compiler t quiet switch compiler;
    update_packages t ~show_packages:false t.repositories;

    (* Finally, load the complete state and install the compiler packages *)
    log "installing compiler packages";
    let t = OpamState.load_state () in
    let _solution = OpamSolution.resolve_and_apply ~force:true t Init
      { wish_install = [];
        wish_remove  = [];
        wish_upgrade = OpamState.get_compiler_packages t compiler } in

    OpamState.print_env_warning ~add_profile:true t

  with e ->
    if not !OpamGlobals.debug then
      OpamFilename.rmdir (OpamPath.root root);
    raise e

let install names =
  log "INSTALL %s" (OpamPackage.Name.Set.to_string names);
  let t = OpamState.load_state () in
  let atoms = OpamSolution.atoms_of_names t names in

  let pkg_skip, pkg_new =
    List.partition (fun (n,v) ->
      match v with
      | None         -> OpamState.mem_installed_package_by_name t n
      | Some (_,v) ->
        if OpamState.mem_installed_package_by_name t n then
          let nv = OpamState.find_installed_package_by_name t n in
          OpamPackage.version nv = v
        else
          false
    ) atoms in

  (* Display a message if at least one package is already installed *)
  List.iter
    (fun (n,_) ->
      let nv = OpamState.find_installed_package_by_name t n in
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
      (fun (n,v) ->
        let versions = match v with
          | None       -> OpamPackage.versions t.packages n
          | Some (_,v) -> OpamPackage.Version.Set.singleton v in
        OpamPackage.Version.Set.iter (fun v ->
          let nv = OpamPackage.create n v in
          let opam = OpamState.opam t nv in
          let f_warn (n, _) =
            if not (OpamPackage.Name.Map.mem n available) then
              OpamGlobals.warning "unknown package %S" (OpamPackage.Name.to_string n)
          in
          List.iter (OpamFormula.iter f_warn) [
            OpamFile.OPAM.depends opam;
            OpamFile.OPAM.depopts opam;
            OpamFile.OPAM.conflicts opam;
          ]
        ) versions
      ) pkg_new;

    let solution = OpamSolution.resolve_and_apply t Install
      { wish_install = OpamSolution.atoms_of_packages t.installed;
        wish_remove  = [] ;
        wish_upgrade = atoms } in
    OpamSolution.error_if_no_solution solution
  )

let remove names =
  log "REMOVE %s" (OpamPackage.Name.Set.to_string names);
  let t = OpamState.load_state () in
  let atoms = OpamSolution.atoms_of_names t names in
  let atoms =
    List.filter (fun (n,_) ->
      if n = OpamPackage.Name.default then (
        OpamGlobals.msg "Package %s can not be removed.\n"
          (OpamPackage.Name.to_string OpamPackage.Name.default);
        false
      ) else
        true
    ) atoms in
  let dummy_version = OpamPackage.Version.of_string "<dummy>" in
  let atoms, not_installed, does_not_exist =
    let aux (atoms, not_installed, does_not_exist) atom nv =
      if not (OpamPackage.Set.mem nv t.installed) then
        (atoms, nv :: not_installed, does_not_exist)
      else if not (OpamPackage.Set.mem nv (Lazy.force t.available_packages)) then
        (atoms, not_installed, nv :: does_not_exist)
      else
        (atom :: atoms, not_installed, does_not_exist) in
    List.fold_left
      (fun accu (n,v as atom) ->
        let nv = match v with
          | None ->
            if OpamState.mem_installed_package_by_name t n then
              OpamState.find_installed_package_by_name t n
            else
              OpamPackage.create n dummy_version
          | Some (_,v) -> OpamPackage.create n v in
        aux accu atom nv)
      ([], [], [])
      atoms in

  if does_not_exist <> [] then (
    List.iter (OpamSolution.proceed_to_delete ~rm_build:true t) does_not_exist;
    let installed_f = OpamPath.Switch.installed t.root t.switch in
    let installed = OpamFile.Installed.read installed_f in
    let installed = OpamPackage.Set.filter (fun nv -> not (List.mem nv does_not_exist)) installed in
    OpamFile.Installed.write installed_f installed;
  );

  if not_installed <> [] then (
    let to_string nv =
      if OpamPackage.version nv = dummy_version then
        OpamPackage.Name.to_string (OpamPackage.name nv)
      else
        OpamPackage.to_string nv in
    if List.length not_installed = 1 then
      OpamGlobals.msg "%s is not installed.\n" (to_string (List.hd not_installed))
    else
      OpamGlobals.msg "%s are not installed.\n" (OpamMisc.string_of_list to_string not_installed)
  );

  if atoms <> [] then (
    let packages = OpamPackage.Set.of_list (List.map (fun (n,_) -> OpamState.find_installed_package_by_name t n) atoms) in
    let universe = OpamState.universe t Depends in
    let to_remove =
      OpamPackage.Set.of_list
        (OpamSolver.forward_dependencies ~depopts:false ~installed:true universe packages) in
    let installed = OpamPackage.Set.diff t.installed to_remove in
    let solution = OpamSolution.resolve_and_apply t Remove
      { wish_install = OpamSolution.atoms_of_packages installed;
        wish_remove  = OpamSolution.atoms_of_packages to_remove;
        wish_upgrade = [] } in
    OpamSolution.error_if_no_solution solution
  )

let reinstall names =
  log "reinstall %s" (OpamPackage.Name.Set.to_string names);
  let t = OpamState.load_state () in
  let atoms = OpamSolution.atoms_of_names t names in
  let reinstall =
    OpamMisc.filter_map (function (n,v) ->
      match v with
      | None ->
        if not (OpamState.mem_installed_package_by_name t n) then (
          OpamGlobals.msg "%s is not installed\n" (OpamPackage.Name.to_string n);
          None
        ) else
          Some (OpamState.find_installed_package_by_name t n)
      | Some (_,v) ->
        let nv = OpamPackage.create n v in
        if OpamPackage.Set.mem nv t.installed then
          Some nv
        else (
          OpamGlobals.msg "%s is not installed\n" (OpamPackage.to_string nv);
          None
        )
    ) atoms in
  let reinstall = OpamPackage.Set.of_list reinstall in
  let depends =
    let universe = OpamState.universe t Depends in
    OpamSolver.forward_dependencies ~depopts:true ~installed:true universe reinstall in
  let to_process =
    List.map (fun pkg -> To_recompile pkg) depends in
  let solution = OpamSolution.apply_solution t (OpamSolver.sequential_solution to_process) in
  OpamSolution.error_if_no_solution solution

let upload upload repo =
  log "upload %s" (string_of_upload upload);
  let t = OpamState.load_state () in
  let opam = OpamFile.OPAM.read upload.upl_opam in
  let name = OpamFile.OPAM.name opam in
  let version = OpamFile.OPAM.version opam in
  let nv = OpamPackage.create name version in
  let repo = match repo with
  | None ->
      if OpamPackage.Name.Map.mem name t.repo_index then
        (* We upload the package to the first available repository. *)
        OpamState.find_repository_name t (List.hd (OpamPackage.Name.Map.find name t.repo_index))
      else
        OpamGlobals.error_and_exit "No repository found to upload %s" (OpamPackage.to_string nv)
  | Some repo ->
      if OpamState.mem_repository_name t repo then
        OpamState.find_repository_name t repo
      else
        OpamGlobals.error_and_exit "Unbound repository %S (available = %s)"
          (OpamRepositoryName.to_string repo)
          (OpamState.string_of_repositories t.repositories) in
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

let rec remote action =
  log "remote %s" (string_of_remote action);
  let t = OpamState.load_state () in
  let update_config repos =
    let new_config = OpamFile.Config.with_repositories t.config repos in
    OpamFile.Config.write (OpamPath.config t.root) new_config in
  let cleanup_repo repo =
    let repos = OpamRepositoryName.Map.keys t.repositories in
    update_config (List.filter ((<>) repo) repos);
    let t = OpamState.load_state () in
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
      if OpamState.mem_repository_name t name then
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
      if OpamState.mem_repository_name t name then
        cleanup_repo name
      else
        OpamGlobals.error_and_exit "%s is not a a valid remote name"
          (OpamRepositoryName.to_string name)
  | RPriority (name, p) ->
    if OpamState.mem_repository_name t name then (
      let config_f = OpamPath.Repository.config (OpamPath.Repository.create t.root name) in
      let config = OpamFile.Repo_config.read config_f in
      let config = { config with repo_priority = p } in
      OpamFile.Repo_config.write config_f config;
      let repo_index_f = OpamPath.repo_index t.root in
      let repo_index = OpamPackage.Name.Map.map (List.filter ((<>)name)) t.repo_index in
      OpamFile.Repo_index.write repo_index_f repo_index;
      let t = OpamState.load_state () in
      update_repo_index t;
    ) else
        OpamGlobals.error_and_exit "%s is not a a valid remote name"
          (OpamRepositoryName.to_string name)

let pin action =
  log "pin %s" (string_of_pin action);
  let t = OpamState.load_state () in
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  let pins = OpamFile.Pinned.safe_read pin_f in
  let name = action.pin_package in
  let update_config pins =
    OpamPackage.Version.Set.iter (fun version ->
      let nv = OpamPackage.create name version in
      OpamFilename.rmdir (OpamPath.Switch.build t.root t.switch nv)
    ) (OpamPackage.versions t.packages name);
    OpamFile.Pinned.write pin_f pins in
  if OpamState.mem_installed_package_by_name t name then (
    let reinstall_f = OpamPath.Switch.reinstall t.root t.switch in
    let reinstall = OpamFile.Reinstall.safe_read reinstall_f in
    let nv = OpamState.find_installed_package_by_name t name in
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
  let t = OpamState.load_state () in
  let pins = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned t.root t.switch) in
  let print n a =
    OpamGlobals.msg "%-20s %-8s %s\n" (OpamPackage.Name.to_string n) (kind_of_pin_option a) (path_of_pin_option a) in
  OpamPackage.Name.Map.iter print pins

(** We protect each main functions with a lock depending on its access
on some read/write data. *)

let list ~print_short ~installed_only ?name_only ?case_sensitive pkg_str =
 OpamState.check (Read_lock (fun () -> list ~print_short ~installed_only ?name_only ?case_sensitive pkg_str))

let info package =
  OpamState.check (Read_lock (fun () -> info package))

let config request =
  OpamState.check (Read_lock (fun () -> OpamConfigCommand.config request))

let install names =
  OpamState.check (Switch_lock (fun () -> install names))

let reinstall names =
  OpamState.check (Switch_lock (fun () -> reinstall names))

let upgrade names =
  OpamState.check (Switch_lock (fun () -> upgrade names))

let remove names =
  OpamState.check (Switch_lock (fun () -> remove names))

let update repos =
  OpamState.check (Global_lock (fun () -> update repos))

let upload u r =
  OpamState.check (Global_lock (fun () -> upload u r))

let remote action =
  OpamState.check (Global_lock (fun () -> remote action))

let switch_install quiet switch ocaml_version =
  OpamState.check (Global_lock (fun () -> OpamSwitchCommand.install quiet switch ocaml_version))

let switch_import filename =
  OpamState.check (Switch_lock (fun () -> OpamSwitchCommand.import filename))

let switch_export filename =
  OpamState.check (Read_lock (fun () -> OpamSwitchCommand.export filename))

let switch_remove switch =
  OpamState.check (Global_lock (fun () -> OpamSwitchCommand.remove switch))

let switch quiet name =
  OpamState.check (Global_lock (fun () -> OpamSwitchCommand.switch quiet name))

let switch_reinstall switch =
  OpamState.check (Global_lock (fun () -> OpamSwitchCommand.reinstall switch))

let switch_list () =
  OpamState.check (Read_lock OpamSwitchCommand.list)

let switch_current () =
  OpamState.check (Read_lock OpamSwitchCommand.current)

let pin action =
  OpamState.check (Global_lock (fun () -> pin action))

let pin_list () =
  OpamState.check (Read_lock pin_list)

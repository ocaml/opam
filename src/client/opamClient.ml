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
open OpamMisc.OP

let log fmt = OpamGlobals.log "CLIENT" fmt

let print_updated_packages t updated pinned_updated =
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

let print_updated_compilers ~old_compilers ~new_compilers =
  let print comp = OpamGlobals.msg " - %s\n" (OpamCompiler.to_string comp) in
  let (--) = OpamCompiler.Set.diff in
  let system = OpamCompiler.Set.singleton OpamCompiler.system in
  let added_compilers = new_compilers -- old_compilers -- system in
  let deleted_compilers = old_compilers -- new_compilers -- system in
  if not (OpamCompiler.Set.is_empty added_compilers) then (
    OpamGlobals.msg "New compiler descriptions available:\n";
    OpamCompiler.Set.iter print added_compilers;
  );
  if not (OpamCompiler.Set.is_empty deleted_compilers) then (
    OpamGlobals.msg "Some compilers are not available anymore:\n";
    OpamCompiler.Set.iter print deleted_compilers
  )

(* sync the repositories, display the new compilers, and create
   compiler description file links *)
(* XXX: the compiler things should splitted out, but the handling of
   compiler description files is a bit had-hoc *)
let update_repositories t ~show_compilers repositories =
  log "update_repositories %s" (OpamState.string_of_repositories repositories);

  let old_compilers = OpamState.compilers ~root:t.root in

  OpamRepositoryName.Map.iter (fun _ repo ->
    OpamRepository.update repo
  ) repositories;

  let new_compilers =
    (* We get all the compiler description new available *)
    let map = OpamState.compiler_repository_map t in
    OpamCompiler.Set.of_list (OpamCompiler.Map.keys map) in

  (* Delete compiler descritions, but keep the ones who disapeared and
     are still installed *)
  (* keep if: =system || (deleted && used) *)
  let deleted = OpamCompiler.Set.diff old_compilers new_compilers in
  OpamCompiler.Set.iter (fun comp ->
    if comp <> OpamCompiler.system
    && not (OpamCompiler.Set.mem comp deleted
            && OpamSwitch.Map.exists (fun _ c -> comp = c) t.aliases) then (
      let comp_f = OpamPath.compiler t.root comp in
      let descr_f = OpamPath.compiler_descr t.root comp in
      OpamFilename.remove comp_f;
      OpamFilename.remove descr_f;
    )
  ) (OpamState.compilers ~root:t.root);

  (* Link existing compiler description files, following the
     repository priorities *)
  let map = OpamState.compiler_repository_map t in
  OpamCompiler.Map.iter (fun comp (repo_comp, repo_descr) ->
    let global_comp  = OpamPath.compiler t.root comp in
    let global_descr = OpamPath.compiler_descr t.root comp in
    OpamFilename.remove global_comp;
    OpamFilename.remove global_descr;
    if OpamFilename.exists repo_comp then
      OpamFilename.link ~src:repo_comp ~dst:global_comp;
    match repo_descr with
    | None   -> ()
    | Some d ->
      if OpamFilename.exists d then
        OpamFilename.link ~src:d ~dst:global_descr
  ) map;

  (* Display the new compilers available *)
  if show_compilers then
    print_updated_compilers ~old_compilers ~new_compilers

(* Check for updates in pinned packages *)
let update_pinned_packages t packages =
  log "update-pinned-packages packages=%s" (OpamPackage.Name.Set.to_string packages);
  let pinned =
    OpamPackage.Name.Map.filter
      (fun n _ -> OpamPackage.Name.Set.mem n packages)
      t.pinned in
  let pinned = OpamPackage.Name.Map.bindings pinned in
  (* Check if a pinned packages has been updated. *)
  let aux = function
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
    | _ -> None in
  let updates = OpamMisc.filter_map aux pinned in
  OpamPackage.Set.of_list updates

(* Update the package contents, display the new packages and update
   reinstall *)
let update_packages t ~show_packages repositories pinned_packages =
  log "update_packages %s" (OpamState.string_of_repositories repositories);

  let pinned_updated = update_pinned_packages t pinned_packages in

  (* then update $opam/repo/index *)
  let map = OpamRepositoryCommand.update_index t in
  let t = OpamState.load_state ~save_cache:false "update-packages-1" in
  let updated =
    let all_updated_files = ref [] in
    let read_updated_files repo =
      let r = OpamPath.Repository.create t.root repo.repo_name in
      if List.mem_assoc r !all_updated_files then
        List.assoc r !all_updated_files
      else (
        let u = OpamFile.Updated.safe_read (OpamPath.Repository.updated r) in
        all_updated_files := (r, u) :: !all_updated_files;
        u
      ) in
    OpamPackage.Map.fold (fun nv r updated ->
      let name = OpamPackage.name nv in
      if OpamPackage.Name.Map.mem name t.pinned then
        (* we will check for updates of pinned packages below. *)
        updated
      else (
        let updated_files = read_updated_files r in
        if OpamPackage.Set.mem nv updated_files then
          OpamPackage.Set.add nv updated
        else
          updated
      )
    ) map OpamPackage.Set.empty in

  if show_packages then
    print_updated_packages t updated pinned_updated;

  let updated = OpamPackage.Set.union pinned_updated updated in
  (* update $opam/$oversion/reinstall for all installed switches *)
  OpamState.add_to_reinstall ~all:true t updated;

  (* Check all the dependencies exist *)
  let t = OpamState.load_state ~save_cache:false "update-packages-2" in
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

let s_not_installed = "--"

type item = {
  name: name;
  current_version: version;
  installed_version: version option;
  synopsis: string;
  descr: string;
}

let names_of_regexp t ~installed_only ~name_only ~case_sensitive ~all regexps =
  log "names_of_regexp regexps=%s" (OpamMisc.string_of_list (fun x -> x) regexps);
  let universe = OpamState.universe t Depends in
  (* the regexp can also simply be a package. *)
  let fix_versions =
    let packages = OpamMisc.filter_map OpamPackage.of_string_opt regexps in
    List.fold_left
      (fun map nv -> OpamPackage.Name.Map.add (OpamPackage.name nv) nv map)
      OpamPackage.Name.Map.empty
      packages in
  let regexps =
    OpamMisc.filter_map (fun str ->
      let re =
        match OpamPackage.of_string_opt str with
        | Some nv ->
          let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
          Re_glob.globx name
        | None   ->
          let re = Re_glob.globx str in
          if case_sensitive then re else Re.no_case re in
      try Some (Re.compile re)
      with Re_glob.Parse_error ->
        OpamGlobals.error "%S is not a valid package descriptor." str;
        None
    ) regexps in
  let exact_match str =
    List.exists (fun re -> OpamMisc.exact_match re str) regexps in
  let partial_match str =
    List.exists (fun re -> Re.execp re str) regexps in
  let packages =
    if all then
      t.packages
    else
      OpamSolver.installable universe in
  let names =
    OpamPackage.Set.fold
      (fun nv set -> OpamPackage.Name.Set.add (OpamPackage.name nv) set)
      packages
      OpamPackage.Name.Set.empty in
  let names =
    OpamPackage.Name.Set.fold (fun name map ->
      let has_name nv = OpamPackage.name nv = name in
      let installed_version =
        if OpamPackage.Set.exists has_name t.installed then
          let nv = OpamPackage.Set.find has_name t.installed in
          Some (OpamPackage.version nv)
        else
          None in
      let current_version =
        if OpamPackage.Name.Map.mem name fix_versions then
          let nv = OpamPackage.Name.Map.find name fix_versions in
          OpamPackage.version nv
        else match installed_version with
          | Some v -> v
          | None   ->
            let nv = OpamPackage.Set.max_elt (OpamPackage.Set.filter has_name packages) in
            OpamPackage.version nv in
      let nv = OpamPackage.create name current_version in
      let descr_f = OpamPackage.Map.find nv t.descrs in
      let synopsis = OpamFile.Descr.synopsis descr_f in
      let descr = OpamFile.Descr.full descr_f in
      OpamPackage.Name.Map.add name { name; current_version; installed_version; synopsis; descr }map
    ) names OpamPackage.Name.Map.empty in

  (* Filter the list of packages, depending on user predicates *)
  OpamPackage.Name.Map.filter (fun name { installed_version; synopsis; descr } ->
    (not installed_only || installed_version <> None)                 (* installp *)
    && (regexps = []                                                  (* allp     *)
        || name_only && exact_match (OpamPackage.Name.to_string name) (* namep    *)
        || not name_only                                              (* descrp   *)
           && (partial_match (OpamPackage.Name.to_string name)
               || partial_match synopsis
               || partial_match descr))
  ) names

module API = struct

  let list ~print_short ~installed_only ~installed_roots ?(name_only = true) ?(case_sensitive = false) regexp =
    let t = OpamState.load_state "list" in
    let names = names_of_regexp t ~installed_only ~name_only ~case_sensitive ~all:false regexp in
    let names =
      if installed_roots then
        OpamPackage.Name.Map.filter (fun name { current_version } ->
          let nv = OpamPackage.create name current_version in
          OpamPackage.Set.mem nv t.installed_roots
        ) names
      else
        names in
    if not print_short && OpamPackage.Name.Map.cardinal names > 0 then (
      let kind = if installed_only then "Installed" else "Available" in
      OpamGlobals.msg "%s packages for %s:\n" kind (OpamSwitch.to_string t.switch);
    );
    let max_n, max_v =
      OpamPackage.Name.Map.fold (fun name { installed_version } (max_n, max_v) ->
        let max_n = max max_n (String.length (OpamPackage.Name.to_string name)) in
        let v_str = match installed_version with
          | None   -> s_not_installed
          | Some v -> OpamPackage.Version.to_string v in
        let max_v = max max_v (String.length v_str) in
        max_n, max_v
      ) names (0,0) in
    OpamPackage.Name.Map.iter (
      if print_short then
        fun name _ -> Printf.printf "%s " (OpamPackage.Name.to_string name)
      else
        let synop_len =
          let col = OpamMisc.terminal_columns () in
          max 0 (col - max_n - max_v - 4) in
        fun name { installed_version; synopsis } ->
          let name = OpamPackage.Name.to_string name in
          let version = match installed_version with
            | None   -> s_not_installed
            | Some v -> OpamPackage.Version.to_string v in
          Printf.printf "%s  %s  %s\n"
            (OpamMisc.indent_left name max_n)
            (OpamMisc.indent_right version max_v)
            (OpamMisc.sub_at synop_len synopsis)
    ) names

  let info ~fields regexps =
    let t = OpamState.load_state "info" in
    let names =
      names_of_regexp t
        ~installed_only:false
        ~name_only:true
        ~case_sensitive:false
        ~all:true
        regexps in

    let show_fields = List.length fields <> 1 in

    let print_one name  { current_version } =

      (* Compute the installed versions, for each switch *)
      let installed =
        OpamSwitch.Map.fold (fun switch _ map ->
          let installed = OpamFile.Installed.safe_read (OpamPath.Switch.installed t.root switch) in
          if OpamState.mem_installed_package_by_name_aux installed name then
            let nv = OpamState.find_installed_package_by_name_aux installed name in
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

      let nv = OpamPackage.create name current_version in
      let opam = OpamState.opam t nv in

      (* All the version of the package *)
      let versions = OpamPackage.versions_of_name t.packages name in
      let versions =
        OpamPackage.Version.Set.filter (fun v ->
          OpamPackage.Map.for_all (fun nv _ -> OpamPackage.version nv <> v) installed
        ) versions in

      let installed_version = match OpamPackage.Map.cardinal installed with
        | 0 -> []
        | 1 -> [ "installed-version" , installed_str ]
        | _ -> [ "installed-versions", installed_str ] in

      let available_versions =
        match List.map OpamPackage.Version.to_string (OpamPackage.Version.Set.elements versions) with
        | []  -> []
        | [v] -> [ "available-version" , v ]
        | l   -> [ "available-versions", String.concat ", " l ] in

      (*let libraries, syntax = match OpamPackage.Map.cardinal installed with
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
          libraries, syntax in*)

      let mk (empty, get, to_string) name field =
        let v = field opam in
        if empty = v then
          []
        else
          [name, to_string (get v)] in

      let string = mk (
          None,
          (function Some x -> x | None -> assert false),
          (fun x -> x)
        ) in
      let strings = mk (
          [],
          (fun l -> l),
          (String.concat ", ")
        ) in
      let formula = mk (
          Empty,
          (fun f -> f),
          OpamFormula.to_string
        ) in

      let authors  = strings "authors"  OpamFile.OPAM.authors in
      let homepage = string  "homepage" OpamFile.OPAM.homepage in
      let license  = string  "license"  OpamFile.OPAM.license in
      let doc      = string  "doc"      OpamFile.OPAM.doc in
      let tags     = strings "tags"     OpamFile.OPAM.tags in
      let depends  = formula "depends"  OpamFile.OPAM.depends in
      let depopts  = formula "depopts"  OpamFile.OPAM.depopts in

      let descr =
        let d = OpamPackage.Map.find nv t.descrs in
        let d = OpamFile.Descr.full d in
        let short, long = match OpamMisc.cut_at d '\n' with
          | None       -> OpamMisc.strip d, ""
          | Some (s,l) -> s, OpamMisc.strip l in
        let long = match long with
          | "" -> ""
          | _  -> Printf.sprintf "\n\n%s" long in
        ["description", short ^ long] in

      let all_fields =
        [ "package", OpamPackage.Name.to_string name ]
        @ [ "version", OpamPackage.Version.to_string current_version ]
        @ homepage
        @ authors
        @ license
        @ doc
        @ tags
        @ depends
        @ depopts
        @ installed_version
        @ available_versions
        @ descr in

      let all_fields = match fields with
        | [] -> all_fields
        | f  -> List.filter (fun (d,_) -> List.mem d f) all_fields in

      List.iter (fun (f, desc) ->
        if show_fields then OpamGlobals.msg "%20s: " f;
        OpamGlobals.msg "%s\n" desc
      ) all_fields in

    OpamPackage.Name.Map.iter print_one names

  let dry_upgrade () =
    log "dry-upgrade";
    let t = OpamState.load_state ~save_cache:false "dry-upgrade" in
    let reinstall = OpamPackage.Set.inter t.reinstall t.installed in
    let solution = OpamSolution.resolve ~verbose:false t (Upgrade reinstall)
        { wish_install = [];
          wish_remove  = [];
          wish_upgrade = OpamSolution.atoms_of_packages t.installed } in
    match solution with
    | Conflicts _ -> None
    | Success sol -> Some (OpamSolver.stats sol)

  let update_aux ~save_cache repos =
    log "UPDATE %s" (OpamMisc.string_of_list OpamRepositoryName.to_string repos);
    let t = OpamState.load_state ~save_cache "update" in
    let repositories =
      if repos = [] then
        t.repositories
      else
        let aux r _ = List.mem r repos in
        OpamRepositoryName.Map.filter aux t.repositories in
    let repositories_need_update =
      not (OpamRepositoryName.Map.is_empty repositories) in

    let pinned_packages =
      if repos = [] then
        OpamPackage.Name.Set.of_list (OpamPackage.Name.Map.keys t.pinned)
      else
        let names =
          List.rev_map (OpamRepositoryName.to_string |> OpamPackage.Name.of_string) repos in
        OpamPackage.Name.Set.of_list (List.filter (OpamState.is_pinned t) names) in
    let pinned_packages_need_update =
      not (OpamPackage.Name.Set.is_empty pinned_packages) in

    let valid_repositories =
      OpamMisc.StringSet.of_list
        (List.rev_map OpamRepositoryName.to_string
           (OpamRepositoryName.Map.keys repositories)) in
    let valid_pinned_packages =
      OpamMisc.StringSet.of_list
        (List.rev_map OpamPackage.Name.to_string
           (OpamPackage.Name.Map.keys t.pinned)) in
    let unknown_names, not_pinned =
      if repos = [] then
        [], []
      else
        let all =
          OpamMisc.StringSet.of_list (List.rev_map OpamRepositoryName.to_string repos) in
        let valid_names =
          OpamMisc.StringSet.of_list
            (List.rev_map
               (OpamPackage.name |> OpamPackage.Name.to_string)
               (OpamPackage.Set.elements t.packages)) in
        let (--) = OpamMisc.StringSet.diff in
        let unknown_names = all -- valid_repositories -- valid_names in
        let not_pinned = (OpamMisc.StringSet.inter all valid_names) -- valid_pinned_packages in
        OpamMisc.StringSet.elements unknown_names,
        OpamMisc.StringSet.elements not_pinned in

    begin
      let valid_repositories =
        match OpamMisc.StringSet.elements valid_repositories with
        | []  -> ""
        | [s] -> Printf.sprintf " Valid repository is %s." s
        | l   ->
          Printf.sprintf
            " Valid repositories are %s."
            (OpamMisc.pretty_list l) in
      match unknown_names with
      | []  -> ()
      | [s] ->
        OpamGlobals.msg
          "Cannot update the repository %s.%s\n"
          s valid_repositories
      | _   ->
        OpamGlobals.msg
          "Cannot update the repositories %s.%s\n"
          (OpamMisc.pretty_list unknown_names) valid_repositories
    end;
    begin
      let valid_pinned_packages =
        match OpamMisc.StringSet.elements valid_pinned_packages with
        | []  -> ""
        | [s] -> Printf.sprintf " Only %s is currently pinned." s
        | l   ->
          Printf.sprintf
            " The currently pinned packages are %s."
            (OpamMisc.pretty_list l) in
      match not_pinned with
      | []  -> ()
      | [s] ->
        OpamGlobals.msg
          "Cannot update the package %s because it is not pinned.%s\n"
          s valid_pinned_packages
      | _   ->
        OpamGlobals.msg
          "Cannot update %s because none of them is pinned.%s\n"
          (OpamMisc.pretty_list not_pinned) valid_pinned_packages
    end;

    if repositories_need_update then
      update_repositories t ~show_compilers:true repositories;

    if repositories_need_update
    || pinned_packages_need_update then (

      update_packages t ~show_packages:true repositories pinned_packages;

      if repositories_need_update then
        OpamState.rebuild_state_cache ();

      match dry_upgrade () with
      | None   -> OpamGlobals.msg "Everything is up-to-date.\n"
      | Some stats ->
        if OpamSolution.sum stats > 0 then (
          OpamGlobals.msg "%s\n" (OpamSolver.string_of_stats stats);
          OpamGlobals.msg "You can now run 'opam upgrade' to upgrade your system.\n"
        ) else
          OpamGlobals.msg "Everything is up-to-date.\n"
    )

  let () =
    OpamState.update_hook := update_aux

  let update = update_aux ~save_cache:true

  let upgrade names =
    log "UPGRADE %s"
      (match names with
       | None -> "<all>"
       | Some n -> OpamPackage.Name.Set.to_string n);
    let t = OpamState.load_state "upgrade" in
    let to_reinstall = OpamPackage.Set.inter t.reinstall t.installed in
    let solution_found = match names with
      | None ->
        OpamSolution.resolve_and_apply t (Upgrade to_reinstall)
          { wish_install = [];
            wish_remove  = [];
            wish_upgrade = OpamSolution.atoms_of_packages t.installed }
      | Some names ->
        let names = OpamSolution.atoms_of_names t names in
        let to_reinstall =
          let packages =
            OpamMisc.filter_map (fun (n,_) ->
              if OpamState.mem_installed_package_by_name t n then
                Some (OpamState.find_installed_package_by_name t n)
              else (
                OpamGlobals.msg "%s is not installed.\n" (OpamPackage.Name.to_string n);
                None
              )
            ) names in
          OpamPackage.Set.inter t.reinstall (OpamPackage.Set.of_list packages) in
        let installed_roots = OpamPackage.Set.diff t.installed_roots to_reinstall in
        OpamSolution.resolve_and_apply t (Upgrade to_reinstall)
          { wish_install = OpamSolution.eq_atoms_of_packages installed_roots;
            wish_remove  = [];
            wish_upgrade = OpamSolution.atoms_of_packages to_reinstall }
    in
    begin match solution_found with
      | Aborted
      | No_solution
      | Error _
      | OK            -> ()
      | Nothing_to_do -> OpamGlobals.msg "Already up-to-date.\n"
    end;
    OpamSolution.check_solution solution_found

  let init repo compiler ~jobs shell dot_profile update_config =
    log "INIT %s" (OpamRepository.to_string repo);
    let root = OpamPath.default () in
    let config_f = OpamPath.config root in
    if OpamFilename.exists config_f then
      OpamGlobals.error_and_exit "OPAM has already been initialized."
    else try
      let repo_p = OpamPath.Repository.create root repo.repo_name in
      (* Create (possibly empty) configuration files *)
      let switch =
        if compiler = OpamCompiler.system then
          OpamSwitch.default
        else
          OpamSwitch.of_string (OpamCompiler.to_string compiler) in

      (* Create ~/.opam/compilers/system.comp *)
      let system_version = OpamCompiler.Version.current () in
      OpamState.create_system_compiler_description root system_version;

      (* Create ~/.opam/config *)
      let config = OpamFile.Config.create OpamVersion.current switch [repo.repo_name] jobs in
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
      let t = OpamState.load_repository_state "init" in
      update_repositories t ~show_compilers:false t.repositories;

      (* Load the partial state, and update the packages state *)
      log "updating package state";
      let t = OpamState.load_state ~save_cache:false "init-1" in
      let switch = OpamSwitch.of_string (OpamCompiler.to_string compiler) in
      let quiet = (compiler = OpamCompiler.system) in
      OpamState.install_compiler t ~quiet switch compiler;
      update_packages t ~show_packages:false t.repositories OpamPackage.Name.Set.empty;

      (* Finally, load the complete state and install the compiler packages *)
      log "installing compiler packages";
      let t = OpamState.load_state "init-2" in
      let compiler_packages = OpamState.get_compiler_packages t compiler in
      let compiler_names = OpamPackage.Name.Set.of_list (List.rev_map fst compiler_packages) in
      let _solution = OpamSolution.resolve_and_apply ~force:true t (Init compiler_names)
          { wish_install = [];
            wish_remove  = [];
            wish_upgrade = compiler_packages } in

      let dot_profile_o = Some dot_profile in
      let user = Some { shell; ocamlinit = true; dot_profile = dot_profile_o } in
      begin match update_config with
        | `ask -> OpamState.update_setup_interactive t shell dot_profile
        | `no  -> ()
        | `yes ->
          let global = Some { complete = true; switch_eval = true } in
          OpamState.update_setup t user global
      end;
      OpamState.print_env_warning t user

    with e ->
      if not !OpamGlobals.debug then
        OpamFilename.rmdir (OpamPath.root root);
      raise e

  let install names =
    log "INSTALL %s" (OpamPackage.Name.Set.to_string names);
    let t = OpamState.load_state "install" in
    let atoms = OpamSolution.atoms_of_names t names in
    let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in

    let pkg_skip, pkg_new =
      List.partition (fun (n,v) ->
        match v with
        | None       -> OpamState.mem_installed_package_by_name t n
        | Some (_,v) ->
          if OpamState.mem_installed_package_by_name t n then
            let nv = OpamState.find_installed_package_by_name t n in
            OpamPackage.version nv = v
          else
            false
      ) atoms in


    (* Add the packages to the list of package roots and display a
       warning for already installed package roots. *)
    let current_roots = t.installed_roots in
    let t =
      List.fold_left (fun t (n,_) ->
        let nv = OpamState.find_installed_package_by_name t n in
        if OpamPackage.Set.mem nv t.installed_roots then (
          OpamGlobals.msg
            "Package %s is already installed (current version is %s)\n"
            (OpamPackage.Name.to_string (OpamPackage.name nv))
            (OpamPackage.Version.to_string (OpamPackage.version nv));
          t;
        ) else (
          let installed_roots = OpamPackage.Set.add nv t.installed_roots in
          { t with installed_roots }
        )
      )  t pkg_skip in
    if t.installed_roots <> current_roots then (
      let diff = OpamPackage.Set.diff t.installed_roots current_roots in
      let diff = OpamPackage.Set.elements diff in
      let diff = List.rev (List.rev_map OpamPackage.to_string diff) in
      OpamGlobals.msg
        "Adding %s to the list of installed roots.\n"
        (OpamMisc.pretty_list diff);
      let file = OpamPath.Switch.installed_roots t.root t.switch in
      OpamFile.Installed_roots.write file t.installed_roots;
    );

    if pkg_new <> [] then (

      (* Display a warning if at least one package contains
         dependencies to some unknown packages *)
      let available = OpamPackage.to_map (Lazy.force t.available_packages) in
      List.iter
        (fun (n,v) ->
          let versions = match v with
            | None       -> OpamPackage.versions_of_name t.packages n
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

      let request =
        if OpamCudf.external_solver_available ()
        then
          { wish_install = atoms;
            wish_remove  = [] ;
            wish_upgrade = [] }
        else
          { wish_install = OpamSolution.atoms_of_packages t.installed_roots;
            wish_remove  = [] ;
            wish_upgrade = atoms }
      in
      let solution = OpamSolution.resolve_and_apply t (Install names) request in
      OpamSolution.check_solution solution
    )

  let remove ~autoremove names =
    log "REMOVE autoremove:%b %s" autoremove (OpamPackage.Name.Set.to_string names);
    let t = OpamState.load_state "remove" in
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
      List.iter (OpamAction.remove_package ~rm_build:true ~metadata:false t) does_not_exist;
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

    if autoremove || atoms <> [] then (
      let packages =
        OpamPackage.Set.of_list
          (List.rev_map (fun (n,_) -> OpamState.find_installed_package_by_name t n) atoms) in
      let universe = OpamState.universe t Depends in
      let to_remove =
        OpamPackage.Set.of_list
          (OpamSolver.reverse_dependencies ~depopts:false ~installed:true universe packages) in
      let installed_roots =
        if autoremove then
          OpamPackage.Set.diff t.installed_roots to_remove
        else
          OpamPackage.Set.diff t.installed to_remove in
      let installed =
        OpamPackage.Set.of_list
          (OpamSolver.dependencies ~depopts:true ~installed:true universe installed_roots) in
      let to_remove =
        if atoms = [] then
          OpamPackage.Set.diff t.installed installed
        else
          to_remove in
      let solution = OpamSolution.resolve_and_apply t Remove
          { wish_install = OpamSolution.eq_atoms_of_packages installed;
            wish_remove  = OpamSolution.atoms_of_packages to_remove;
            wish_upgrade = [] } in
      OpamSolution.check_solution solution
    ) else
      OpamGlobals.msg "Nothing to do.\n"

  let reinstall names =
    log "reinstall %s" (OpamPackage.Name.Set.to_string names);
    let t = OpamState.load_state "reinstall" in
    let atoms = OpamSolution.atoms_of_names t names in
    let reinstall =
      OpamMisc.filter_map (function (n,v) ->
        match v with
        | None ->
          if not (OpamState.mem_installed_package_by_name t n) then (
            OpamGlobals.msg "%s is not installed.\n" (OpamPackage.Name.to_string n);
            None
          ) else
            let nv = OpamState.find_installed_package_by_name t n in
            if OpamState.is_pinned t n then (
              (* [opam reinstall <pkg>] when pkg is pinned means we want
                 to sync the pkg with the pinned source. *)
              match OpamState.update_pinned_package t n with
              | Up_to_date _  -> None
              | Result _      -> Some nv
              | Not_available -> None
            ) else
              Some nv
        | Some (_,v) ->
          let nv = OpamPackage.create n v in
          if OpamPackage.Set.mem nv t.installed then
            Some nv
          else (
            OpamGlobals.msg "%s is not installed.\n" (OpamPackage.to_string nv);
            None
          )
      ) atoms in
    let reinstall = OpamPackage.Set.of_list reinstall in
    let depends =
      let universe = OpamState.universe t Depends in
      OpamSolver.reverse_dependencies ~depopts:true ~installed:true universe reinstall in
    let to_process = List.rev_map (fun pkg -> To_recompile pkg) depends in
    let solution = OpamSolution.apply t Reinstall (OpamSolver.sequential_solution to_process) in
    OpamSolution.check_solution solution

  let upload upload repo =
    log "upload %s %s" (string_of_upload upload) (OpamRepositoryName.to_string repo);
    let t = OpamState.load_state "upload" in
    let opam = OpamFile.OPAM.read upload.upl_opam in
    let name = OpamFile.OPAM.name opam in
    let version = OpamFile.OPAM.version opam in
    let nv = OpamPackage.create name version in
    let repo =
      if OpamState.mem_repository_name t repo then
        OpamState.find_repository_name t repo
      else
        OpamGlobals.error_and_exit "Unbound repository %S (available = %s)"
          (OpamRepositoryName.to_string repo)
          (OpamState.string_of_repositories t.repositories) in
    let repo_p = OpamPath.Repository.create t.root repo.repo_name in
    let upload_repo = OpamPath.Repository.upload_dir repo_p in
    let upload_opam = OpamPath.Repository.opam upload_repo None nv in
    let upload_descr = OpamPath.Repository.descr upload_repo None nv in
    let upload_archives = OpamPath.Repository.archive upload_repo nv in
    OpamFilename.copy ~src:upload.upl_opam ~dst:upload_opam;
    OpamFilename.copy ~src:upload.upl_descr ~dst:upload_descr;
    OpamFilename.copy ~src:upload.upl_archive ~dst:upload_archives;
    OpamRepository.upload repo;
    OpamFilename.rmdir (OpamPath.Repository.package upload_repo None nv);
    OpamFilename.remove (OpamPath.Repository.archive upload_repo nv)


  module PIN        = OpamPinCommand
  module REPOSITORY = OpamRepositoryCommand
  module CONFIG     = OpamConfigCommand
  module SWITCH     = OpamSwitchCommand

end

let read_lock f =
  OpamState.check (Read_lock f)

let switch_lock f =
  OpamState.check (Switch_lock f)

let global_lock f =
  OpamState.check (Global_lock f)

(** We protect each main functions with a lock depending on its access
    on some read/write data. *)

module SafeAPI = struct

  let init = API.init

  let list
      ~print_short ~installed_only ~installed_roots
      ?name_only ?case_sensitive pkg_str =
    read_lock (fun () ->
      API.list
        ~print_short ~installed_only ~installed_roots
        ?name_only ?case_sensitive pkg_str
    )

  let info ~fields regexps =
    read_lock (fun () -> API.info ~fields regexps)

  let install names =
    switch_lock (fun () -> API.install names)

  let reinstall names =
    switch_lock (fun () -> API.reinstall names)

  let upgrade names =
    switch_lock (fun () -> API.upgrade names)

  let remove ~autoremove names =
    switch_lock (fun () -> API.remove ~autoremove names)

  let update repos =
    global_lock (fun () -> API.update repos)

  let upload u r =
    global_lock (fun () -> API.upload u r)

  module CONFIG = struct

    let config option =
      read_lock (fun () -> API.CONFIG.config option)

    let env ~csh =
      read_lock (fun () -> API.CONFIG.env ~csh)

    let setup local global =
      global_lock (fun () -> API.CONFIG.setup local global)

    let setup_list shell dot_profile =
      read_lock (fun () -> API.CONFIG.setup_list shell dot_profile)

    let exec command =
      read_lock (fun () -> API.CONFIG.exec command)

    let list names =
      read_lock (fun () -> API.CONFIG.list names)

    let variable var =
      read_lock (fun () -> API.CONFIG.variable var)

    let subst files =
      read_lock (fun () -> API.CONFIG.subst files)

    let includes ~is_rec names =
      read_lock (fun () -> API.CONFIG.includes ~is_rec names)

  end

  module REPOSITORY = struct

    let list ~short =
      global_lock (fun () -> API.REPOSITORY.list ~short)

    let add name kind address ~priority =
      global_lock (fun () -> API.REPOSITORY.add name kind address ~priority)

    let remove name =
      global_lock (fun () -> API.REPOSITORY.remove name)

    let priority name ~priority =
      global_lock (fun () -> API.REPOSITORY.priority name ~priority)

  end

  module SWITCH = struct

    let switch ~quiet ~warning name =
      global_lock (fun () -> API.SWITCH.switch ~quiet ~warning name)

    let install ~quiet ~warning switch ocaml_version =
      global_lock (fun () -> API.SWITCH.install ~quiet ~warning switch ocaml_version)

    let import filename =
      switch_lock (fun () -> API.SWITCH.import filename)

    let export filename =
      read_lock (fun () -> API.SWITCH.export filename)

    let remove switch =
      global_lock (fun () -> API.SWITCH.remove switch)

    let reinstall switch =
      global_lock (fun () -> API.SWITCH.reinstall switch)

    let list ~print_short ~installed_only =
      read_lock (fun () -> API.SWITCH.list ~print_short ~installed_only)

    let show () =
      read_lock API.SWITCH.show

  end

  module PIN = struct

    let pin ~force action =
      global_lock (fun () -> API.PIN.pin ~force action)

    let list () =
      read_lock API.PIN.list

  end

end

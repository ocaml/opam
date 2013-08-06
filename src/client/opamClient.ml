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
open OpamMisc.OP

let log fmt = OpamGlobals.log "CLIENT" fmt

let s_not_installed = "--"

type item = {
  name: name;
  current_version: version;
  installed_version: version option;
  synopsis: string Lazy.t;
  descr: string Lazy.t;
  tags: string list;
}

let names_of_regexp t ~filter ~exact_name ~case_sensitive regexps =
  log "names_of_regexp regexps=%s" (OpamMisc.string_of_list (fun x -> x) regexps);
  let universe = OpamState.universe t Depends in
  (* the regexp can also simply be a package. *)
  let fix_versions =
    let packages = OpamMisc.filter_map OpamPackage.of_string_opt regexps in
    List.fold_left
      (fun map nv ->
        if OpamPackage.Set.mem nv t.packages then
          OpamPackage.Name.Map.add (OpamPackage.name nv) nv map
        else
          map)
      OpamPackage.Name.Map.empty
      packages in
  let regexps =
    OpamMisc.filter_map (fun str ->
      let re =
        match OpamPackage.of_string_opt str with
        | Some nv ->
          if OpamPackage.Set.mem nv t.packages then
            let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
            Re_glob.globx name
          else
            Re_glob.globx str
        | None   -> Re_glob.globx str in
      let re =
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
  let partial_matchs strs =
    List.exists partial_match strs in
  let packages = match filter with
    | `all -> t.packages
    | _    -> OpamSolver.installable universe in
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

            let nv =
              OpamPackage.Set.max_elt (OpamPackage.Set.filter has_name packages) in
            OpamPackage.version nv in
      let nv = OpamPackage.create name current_version in
      let descr_f = OpamPackage.Map.find nv t.descrs in
      let synopsis = lazy (
        OpamFile.Descr.synopsis (Lazy.force descr_f)
      ) in
      let descr = lazy (
        OpamFile.Descr.full (Lazy.force descr_f)
      ) in
      let tags = OpamFile.OPAM.tags (OpamState.opam t nv) in
      OpamPackage.Name.Map.add
        name { name; current_version; installed_version; synopsis; descr; tags }
        map
    ) names OpamPackage.Name.Map.empty in

  (* Filter the list of packages, depending on user predicates *)
  let names =
    OpamPackage.Name.Map.filter (fun name
      { installed_version; synopsis; descr; tags } ->
      (match filter with
        | `installed -> installed_version <> None
        | `roots     ->
          begin match installed_version with
            | None   -> false
            | Some v -> OpamPackage.Set.mem (OpamPackage.create name v)
                          t.installed_roots
          end
        | _  -> true)
      &&
      (regexps = []
       || exact_match (OpamPackage.Name.to_string name)
       || not exact_name &&
          (partial_match (OpamPackage.Name.to_string name)
           || partial_match (Lazy.force synopsis)
           || partial_match (Lazy.force descr)
           || partial_matchs tags))
    ) names in

  if not (OpamPackage.Set.is_empty t.packages)
  && OpamPackage.Name.Map.is_empty names then
    OpamGlobals.error_and_exit "No packages found."
  else
    names

module API = struct

  let list ~print_short ~filter ~exact_name ~case_sensitive regexp =
    let t = OpamState.load_state "list" in
    let names = names_of_regexp t ~filter ~exact_name ~case_sensitive regexp in
    if not print_short && OpamPackage.Name.Map.cardinal names > 0 then (
      let kind = match filter with
        | `roots
        | `installed -> "Installed"
        | _          -> "Available" in
      OpamGlobals.msg "%s packages for %s:\n" kind (OpamSwitch.to_string t.switch);
    );
    let names = OpamPackage.Name.Map.mapi (fun name stats ->
        if OpamState.is_name_installed t name
        && OpamState.is_locally_pinned t name then
          { stats with installed_version = Some (OpamPackage.Version.pinned) }
        else
          stats
      ) names in
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
            (OpamMisc.sub_at synop_len (Lazy.force synopsis))
    ) names

  let info ~fields regexps =
    let t = OpamState.load_state "info" in
    let names =
      names_of_regexp t ~filter:`all ~exact_name:true ~case_sensitive:false regexps in

    let show_fields = List.length fields <> 1 in

    let print_one name  { current_version } =

      (* Compute the installed versions, for each switch *)
      let installed = OpamState.installed_versions t name in
      let installed = OpamPackage.Map.fold (fun nv alias map ->
          let nv =
            if OpamState.is_locally_pinned t name then OpamPackage.pinned name
            else nv in
          OpamPackage.Map.add nv alias map
        ) installed OpamPackage.Map.empty in
      let installed_str =
        let one (nv, aliases) =
          Printf.sprintf "%s [%s]"
            (OpamPackage.to_string nv)
            (String.concat " " (List.map OpamSwitch.to_string aliases)) in
        String.concat ", " (List.map one (OpamPackage.Map.bindings installed)) in

      let nv = OpamPackage.create name current_version in
      let opam = OpamState.opam t nv in

      (* where does it come from (eg. which repository) *)
      (* Note: this is a bit costly to do so, so only do it when
         --verbose is active. *)
      let repo_name_and_prefix =
        if not !OpamGlobals.verbose then None
        else
          try
            let res = OpamPackage.Map.find nv (Lazy.force t.package_index) in
            Some res
          with Not_found ->
            None in

      let repository =
        if OpamState.is_locally_pinned t name then
          ["pinned", "true"]
        else if OpamRepositoryName.Map.cardinal t.repositories <= 1 then
          []
        else match repo_name_and_prefix with
          | None       -> []
          | Some (r,_) -> [ "repository", OpamRepositoryName.to_string r ] in

      let revision =
        if OpamState.is_locally_pinned t name
        && OpamState.is_name_installed t name then
          let repo = OpamState.repository_of_locally_pinned_package t name in
          match OpamRepository.revision repo with
          | None   -> []
          | Some v -> [ "revision", OpamPackage.Version.to_string v ]
        else
          [] in

      let url = match OpamState.url t nv with
        | None   -> []
        | Some u ->
          let kind =
            match OpamFile.URL.kind u with
            | None   -> "http"
            | Some k -> string_of_repository_kind k in
          let url = string_of_address (OpamFile.URL.url u) in
          let checksum = OpamFile.URL.checksum u in
          [ "upstream-url" , url;
            "upstream-kind", kind ]
          @ match checksum with
            | None   -> []
            | Some c -> [ "upstream-checksum", c ] in

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
        let strings = List.map OpamPackage.Version.to_string
            (OpamPackage.Version.Set.elements versions) in
        match strings with
        | []  -> []
        | [v] -> [ "available-version" , v ]
        | l   -> [ "available-versions", String.concat ", " l ] in

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
        let d = Lazy.force (OpamPackage.Map.find nv t.descrs) in
        let d = OpamFile.Descr.full d in
        let short, long = match OpamMisc.cut_at d '\n' with
          | None       -> OpamMisc.strip d, ""
          | Some (s,l) -> s, OpamMisc.strip l in
        let long = match long with
          | "" -> ""
          | _  -> Printf.sprintf "\n\n%s" long in
        ["description", short ^ long] in

      let version =
        if OpamState.is_locally_pinned t name then OpamPackage.Version.pinned
        else current_version in

      let all_fields =
        [ "package", OpamPackage.Name.to_string name ]
        @ [ "version", OpamPackage.Version.to_string version ]
        @ revision
        @ repository
        @ url
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

  let update repos =
    let t = OpamState.load_state ~save_cache:true "update" in
    log "UPDATE %s" (OpamMisc.string_of_list OpamRepositoryName.to_string repos);
    let repositories =
      if repos = [] then
        t.repositories
      else
        let aux r _ = List.mem r repos in
        OpamRepositoryName.Map.filter aux t.repositories in
    let repositories_need_update =
      not (OpamRepositoryName.Map.is_empty repositories) in

    let dev_packages =
      let all = OpamState.dev_packages t in
      if repos = [] then
        all
      else
        OpamPackage.Set.filter (fun nv ->
            let name repo_name =
              (OpamRepositoryName.to_string |> OpamPackage.Name.of_string) repo_name
              =  OpamPackage.name nv in
            let package repo_name =
              (OpamRepositoryName.to_string |> OpamPackage.of_string) repo_name
              = nv in
            List.exists (fun repo_name ->
                name repo_name || package repo_name
              ) repos
          ) all in
    let dev_packages_need_update =
      not (OpamPackage.Set.is_empty dev_packages) in

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
          OpamMisc.StringSet.of_list
            (List.rev_map OpamRepositoryName.to_string repos) in
        let valid_names =
          OpamMisc.StringSet.of_list
            (List.rev_map
               (OpamPackage.name |> OpamPackage.Name.to_string)
               (OpamPackage.Set.elements t.packages)) in
        let (--) = OpamMisc.StringSet.diff in
        let unknown_names = all -- valid_repositories -- valid_names in
        let not_pinned =
          (OpamMisc.StringSet.inter all valid_names) -- valid_pinned_packages in
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

    if repositories_need_update then (
      let repos = OpamRepositoryName.Map.values repositories in
      let child repo =
        try OpamRepository.update repo
        with _ ->
          OpamGlobals.error "Skipping %s as the repository is not available.\n"
            (string_of_address repo.repo_address) in

      (* Update each remote backend *)
      OpamRepository.Parallel.iter_l (2 * OpamState.jobs t) repos
        ~child ~post:ignore ~pre:ignore;
      OpamRepositoryCommand.update_index t;
      let compiler_updates =
        OpamRepositoryCommand.fix_compiler_descriptions t ~verbose:true in
      let package_updates =
        OpamRepositoryCommand.fix_package_descriptions t ~verbose:true in

      (* Eventually output some JSON file *)
      if OpamJson.verbose () then
        let json to_json update =
          `O [ ("created", to_json update.created);
               ("updated", to_json update.updated);
               ("deleted", to_json update.deleted);
               ("changed", to_json update.changed); ] in
        let updates = `O [
            "package-updates" , (json OpamPackage.Set.to_json package_updates);
            "compiler-updates", (json OpamCompiler.Set.to_json compiler_updates);
          ] in
        OpamJson.add updates;
    );

    if dev_packages_need_update then (
      let updates =
        OpamRepositoryCommand.update_dev_packages ~verbose:true t dev_packages in
      let json = `O [ "dev-packages-update", OpamPackage.Set.to_json updates ] in
      OpamJson.add json
    );

    OpamState.rebuild_state_cache ();

    match dry_upgrade () with
    | None   -> OpamGlobals.msg "Everything is up-to-date.\n"
    | Some stats ->
      if OpamSolution.sum stats > 0 then (
        OpamGlobals.msg "%s\n" (OpamSolver.string_of_stats stats);
        OpamGlobals.msg "You can now run 'opam upgrade' to upgrade your system.\n"
      ) else
        OpamGlobals.msg "Everything is up-to-date.\n"

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
        let to_upgrade =
          let packages =
            OpamMisc.filter_map (fun (n,_) ->
              if OpamState.is_name_installed t n then
                Some (OpamState.find_installed_package_by_name t n)
              else (
                OpamGlobals.msg
                  "%s is not installed.\n" (OpamPackage.Name.to_string n);
                None
              )
            ) names in
          (OpamPackage.Set.of_list packages) in
        let installed_roots = OpamPackage.Set.diff t.installed_roots to_reinstall in
        OpamSolution.resolve_and_apply t (Upgrade to_reinstall)
          { wish_install = OpamSolution.eq_atoms_of_packages installed_roots;
            wish_remove  = [];
            wish_upgrade = OpamSolution.atoms_of_packages to_upgrade }
    in
    begin match solution_found with
      | Aborted
      | No_solution
      | Error _
      | OK _          -> ()
      | Nothing_to_do -> OpamGlobals.msg "Already up-to-date.\n"
    end;
    OpamSolution.check_solution t solution_found

  let init repo compiler ~jobs shell dot_profile update_config =
    log "INIT %s" (OpamRepository.to_string repo);
    let root = OpamPath.root () in
    let config_f = OpamPath.config root in
    let dot_profile_o = Some dot_profile in
    let user = { shell; ocamlinit = true; dot_profile = dot_profile_o } in
    let update_setup t =
      let updated = match update_config with
        | `ask -> OpamState.update_setup_interactive t shell dot_profile
        | `no  -> false
        | `yes ->
          let global = { complete = true; switch_eval = true } in
          OpamState.update_setup t (Some user) (Some global);
          true in
      if updated then OpamState.print_env_warning_at_switch t
      else OpamState.print_env_warning_at_init t user in

    if OpamFilename.exists config_f then (
      OpamGlobals.msg "OPAM has already been initialized.";
      let t = OpamState.load_state "init" in
      update_setup t
    ) else try
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
        let config =
          OpamFile.Config.create OpamVersion.current switch [repo.repo_name] jobs in
        OpamFile.Config.write config_f config;

        (* Create ~/.opam/aliases *)
        OpamFile.Aliases.write
          (OpamPath.aliases root)
          (OpamSwitch.Map.add switch compiler OpamSwitch.Map.empty);

        (* Init repository *)
        OpamFile.Repo_index.write
          (OpamPath.repo_index root) OpamPackage.Name.Map.empty;
        OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
        OpamRepository.init repo;

        (* Init global dirs *)
        OpamFilename.mkdir (OpamPath.packages_dir root);
        OpamFilename.mkdir (OpamPath.compilers_dir root);

        (* Load the partial state, and update the global state *)
        log "updating repository state";
        let t = OpamState.load_repository_state "init-1" in
        OpamRepositoryCommand.update_index t;
        let _ = OpamRepositoryCommand.fix_compiler_descriptions t ~verbose:false in
        let _ = OpamRepositoryCommand.fix_package_descriptions t ~verbose:false in

        (* Load the partial state, and install the new compiler if needed *)
	log "updating package state";
        let t = OpamState.load_state ~save_cache:false "init-2" in
        let switch = OpamSwitch.of_string (OpamCompiler.to_string compiler) in
        let quiet = (compiler = OpamCompiler.system) in
        OpamState.install_compiler t ~quiet switch compiler;
        OpamState.update_switch_config t switch;

        (* Finally, load the complete state and install the compiler packages *)
        log "installing compiler packages";
        let t = OpamState.load_state "init-3" in
        let compiler_packages = OpamState.get_compiler_packages t compiler in
        let compiler_names =
          OpamPackage.Name.Set.of_list (List.rev_map fst compiler_packages) in
        (* Ugly hack to quiet OPAM on base packages *)
        let display_messages = !OpamGlobals.display_messages in
        OpamGlobals.display_messages := false;
        let _solution =
          OpamSolution.resolve_and_apply ~force:true t (Init compiler_names)
            { wish_install = [];
              wish_remove  = [];
              wish_upgrade = compiler_packages } in
        OpamGlobals.display_messages := display_messages;
        update_setup t

      with e ->
        if not !OpamGlobals.debug then OpamFilename.rmdir root;
        raise e

  let install names =
    log "INSTALL %s" (OpamPackage.Name.Set.to_string names);
    let t = OpamState.load_state "install" in
    let atoms = OpamSolution.atoms_of_names t names in
    let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in

    let pkg_skip, pkg_new =
      List.partition (fun (n,v) ->
        match v with
        | None       -> OpamState.is_name_installed t n
        | Some (_,v) ->
          if OpamState.is_name_installed t n then
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
      OpamSolution.check_solution t solution
    )

  let remove ~autoremove names =
    log "REMOVE autoremove:%b %s" autoremove (OpamPackage.Name.Set.to_string names);
    let t = OpamState.load_state "remove" in
    let atoms = OpamSolution.atoms_of_names t names in
    let atoms =
      List.filter (fun (n,_) ->
        if n = OpamPackage.Name.global_config then (
          OpamGlobals.msg "Package %s can not be removed.\n"
            (OpamPackage.Name.to_string OpamPackage.Name.global_config);
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
              if OpamState.is_name_installed t n then
                OpamState.find_installed_package_by_name t n
              else
                OpamPackage.create n dummy_version
            | Some (_,v) -> OpamPackage.create n v in
          aux accu atom nv)
        ([], [], [])
        atoms in

    if does_not_exist <> [] then (
      List.iter
        (OpamAction.remove_package ~rm_build:true ~metadata:false t)
        does_not_exist;
      let installed_f = OpamPath.Switch.installed t.root t.switch in
      let installed = OpamFile.Installed.read installed_f in
      let installed =
        OpamPackage.Set.filter (fun nv -> not (List.mem nv does_not_exist)) installed in
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
        OpamGlobals.msg "%s are not installed.\n"
          (OpamMisc.string_of_list to_string not_installed)
    );

    if autoremove || atoms <> [] then (
      let packages =
        OpamPackage.Set.of_list (List.rev_map (fun (n,_) ->
            OpamState.find_installed_package_by_name t n
          ) atoms) in
      let universe = OpamState.universe t Remove in
      let to_remove =
        OpamPackage.Set.of_list
          (OpamSolver.reverse_dependencies
             ~depopts:false ~installed:true universe packages) in
      let installed_roots =
        if autoremove then
          OpamPackage.Set.diff t.installed_roots to_remove
        else
          OpamPackage.Set.diff t.installed to_remove in
      let installed =
        OpamPackage.Set.of_list
          (OpamSolver.dependencies
             ~depopts:true ~installed:true universe installed_roots) in
      (* installed includes the depopts, because we don't want to autoremove
         them. But that may re-include packages that we wanted removed, so we
         need to remove them again *)
      let installed = OpamPackage.Set.diff installed to_remove in
      let to_remove =
        if atoms = [] then
          OpamPackage.Set.diff t.installed installed
        else
          to_remove in
      let solution = OpamSolution.resolve_and_apply t Remove
          { wish_install = OpamSolution.eq_atoms_of_packages installed;
            wish_remove  = OpamSolution.atoms_of_packages to_remove;
            wish_upgrade = [] } in
      OpamSolution.check_solution t solution
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
          if not (OpamState.is_name_installed t n) then (
            OpamGlobals.msg "%s is not installed.\n" (OpamPackage.Name.to_string n);
            None
          ) else
            let nv = OpamState.find_installed_package_by_name t n in
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
      OpamSolver.reverse_dependencies
        ~depopts:true ~installed:true universe reinstall in
    let to_process = List.map (fun pkg -> To_recompile pkg) depends in
    let solution =
      OpamSolution.apply t Reinstall (OpamSolver.sequential_solution to_process) in
    OpamSolution.check_solution t solution

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

  let list ~print_short ~filter ~exact_name ~case_sensitive pkg_str =
    read_lock (fun () ->
      API.list ~print_short ~filter ~exact_name ~case_sensitive pkg_str
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

  module CONFIG = struct

    let config option =
      read_lock (fun () -> API.CONFIG.config option)

    let env ~csh ~sexp ~fish =
      read_lock (fun () -> API.CONFIG.env ~csh ~sexp ~fish)

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

    let install ~quiet ~warning ~update_config switch ocaml_version =
      global_lock (fun () ->
        API.SWITCH.install ~quiet ~warning ~update_config switch ocaml_version)

    let import filename =
      switch_lock (fun () -> API.SWITCH.import filename)

    let export filename =
      read_lock (fun () -> API.SWITCH.export filename)

    let remove switch =
      global_lock (fun () -> API.SWITCH.remove switch)

    let reinstall switch =
      global_lock (fun () -> API.SWITCH.reinstall switch)

    let list ~print_short ~installed =
      read_lock (fun () -> API.SWITCH.list ~print_short ~installed)

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

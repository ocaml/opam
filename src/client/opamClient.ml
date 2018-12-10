(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStateTypes
open OpamStd.Op
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "CLIENT" fmt
let slog = OpamConsole.slog

(* When packages are removed from upstream, they normally disappear from the
   'available' packages set and can't be seen by the solver anymore. This is a
   problem for several reasons, so we compute the set of orphan packages here:
   - they are checked for conflicts with the user request
   - they are re-added to the universe if (transitively) unrelated to the
     request (the [changes] parameter)
   - they are otherwise put in [wish_remove] in case we use the internal
     solver
   This function separates full orphans (no version of the package available
   anymore) from orphan versions, because they have a different impact on
   the request (needs version change VS needs uninstall).
   See also preprocess_request and check_conflicts *)
let orphans ?changes ?(transitive=false) t =
  let all = t.packages ++ t.installed in
  let allnames = OpamPackage.names_of_packages all in
  let universe =
    OpamSwitchState.universe t ~requested:OpamPackage.Name.Set.empty Reinstall
  in
  (* Basic definition of orphan packages *)
  let orphans = t.installed -- Lazy.force t.available_packages in
  (* Restriction to the request-related packages *)
  let changes = match changes with
    | None -> None
    | Some ch ->
      Some
        (OpamPackage.Name.Set.fold (fun name ch ->
             try
               OpamPackage.Set.add
                 (OpamPackage.package_of_name t.installed name) ch
             with Not_found -> ch)
            (OpamPackage.names_of_packages ch)
            ch)
  in
  let orphans = match changes with
    | None -> orphans
    | Some ch ->
      if OpamPackage.Set.is_empty orphans then orphans else
      let recompile_cone =
        OpamPackage.Set.of_list @@
        OpamSolver.reverse_dependencies
          ~depopts:true ~installed:true ~unavailable:true
          ~build:true ~post:false
          universe ch
      in
      orphans %% recompile_cone
  in
  (* Pinned versions of packages remain always available *)
  let orphans = orphans -- OpamPinned.packages t in
  (* Splits between full orphans (no version left) and partial ones *)
  let full_partition orphans =
    let orphan_names = (* names for which there is no version left *)
      OpamPackage.Name.Set.diff
        allnames
        (OpamPackage.names_of_packages (all -- orphans)) in
    OpamPackage.Set.partition
      (fun nv -> OpamPackage.Name.Set.mem nv.name orphan_names)
      orphans
  in
  let full_orphans, orphan_versions = full_partition orphans in
  (* Closure *)
  let full_orphans, orphan_versions =
    if not transitive then full_orphans, orphan_versions else
    let rec add_trans full_orphans orphan_versions =
      (* fixpoint to check all packages with no available version *)
      let new_orphans =
        OpamPackage.Set.of_list @@
        OpamSolver.reverse_dependencies
          ~depopts:false ~installed:false ~unavailable:true
          ~build:true ~post:false
          universe full_orphans
      in
      let full, versions = full_partition (new_orphans++orphan_versions) in
      if OpamPackage.Set.equal full_orphans full
      then full, versions
      else add_trans full versions
    in
    add_trans full_orphans orphan_versions
  in
  (* Installed packages outside the set of changes are otherwise safe:
     re-add them to the universe *)
  let t =
    if changes = None then t else
    let available_packages =
      lazy (Lazy.force t.available_packages ++
            (t.installed -- orphans)) in
    { t with available_packages } in
  log "Orphans: (changes: %a, transitive: %b) -> full %a, versions %a"
    (slog @@ OpamStd.Option.to_string OpamPackage.Set.to_string) changes
    transitive
    (slog @@ OpamPackage.Name.Set.to_string @* OpamPackage.names_of_packages)
    full_orphans
    (slog OpamPackage.Set.to_string) orphan_versions;
  t, full_orphans, orphan_versions

(* Splits a list of atoms into the installed and uninstalled ones*)
let get_installed_atoms t atoms =
  List.fold_left (fun (packages, not_installed) atom ->
      try
        let nv =
          OpamPackage.Set.find (OpamFormula.check atom) t.installed in
        nv :: packages, not_installed
      with Not_found ->
        packages, atom :: not_installed)
    ([],[]) atoms

(* Check atoms for pinned packages, and update them. Returns the state that
   may have been reloaded if there were changes *)
let update_dev_packages_t atoms t =
  (* Check last update of the repo *)
  let last_update =
    (Unix.stat (OpamFilename.to_string
                  (OpamPath.state_cache
                     (OpamStateConfig.(!r.root_dir))))).Unix.st_mtime
  in
  let too_old = float_of_int (3600*24*21) in
  if (Unix.time () -. last_update) > too_old then
    OpamConsole.note "It seems you have not updated your repositories \
                      for a while. Consider updating them with:\n%s\n"
      (OpamConsole.colorise `bold "opam update");

  if OpamClientConfig.(!r.skip_dev_update) then t else
  let working_dir = OpamClientConfig.(!r.working_dir) in
  let to_update =
    List.fold_left (fun to_update (name,_) ->
        try
          let nv = OpamPackage.package_of_name t.pinned name in
          if OpamSwitchState.is_dev_package t nv then
            OpamPackage.Set.add nv to_update
          else to_update
        with Not_found -> to_update)
      OpamPackage.Set.empty atoms
  in
  if OpamPackage.Set.is_empty to_update then t else (
    OpamConsole.header_msg "Synchronising pinned packages";
    try
      let working_dir =
        if working_dir then Some (OpamSwitchState.packages_of_atoms t atoms)
        else None
      in
      let _success, t, _pkgs =
        OpamUpdate.dev_packages t ?working_dir to_update in
      OpamConsole.msg "\n";
      t
    with e ->
      OpamStd.Exn.fatal e;
      OpamConsole.msg "\n";
      t
  )

let compute_upgrade_t
    ?(strict_upgrade=true) ?(auto_install=false) ~all atoms t =
  let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
  let atoms =
    List.map (function
        | (n,None) when strict_upgrade ->
          (* force strict upgrade for unchanged, non dev or pinned packages
             (strict update makes no sense for pinned packages which have
             a fixed version) *)
          (try
             let nv = OpamSwitchState.find_installed_package_by_name t n in
             if OpamSwitchState.is_dev_package t nv ||
                OpamPackage.has_name t.pinned n ||
                OpamPackage.Set.mem nv t.reinstall
             then (n, None)
             else
             let atom = (n, Some (`Gt, nv.version)) in
             if OpamPackage.Set.exists (OpamFormula.check atom)
                 (Lazy.force t.available_packages)
             then atom
             else (n, None)
           with Not_found -> (n,None))
        | atom -> atom
      ) atoms in
  let requested_installed, not_installed =
    List.fold_left (fun (packages, not_installed) (n,_ as atom) ->
        try
          let nv =
            OpamPackage.Set.find (fun nv -> nv.name = n)
              t.installed in
          OpamPackage.Set.add nv packages, not_installed
        with Not_found ->
          packages, atom :: not_installed)
      (OpamPackage.Set.empty,[]) atoms in
  let to_install =
    if not_installed = [] then [] else
    if auto_install ||
       OpamConsole.confirm "%s %s not installed. Install %s?"
         (OpamStd.Format.pretty_list
            (List.rev_map OpamFormula.short_string_of_atom not_installed))
         (match not_installed with [_] -> "is" | _ -> "are")
         (match not_installed with [_] -> "it" | _ -> "them")
    then not_installed
    else []
  in
  if all then
    let t, full_orphans, orphan_versions = orphans ~transitive:true t in
    let to_upgrade = t.installed -- full_orphans in
    names,
    OpamSolution.resolve t Upgrade
      ~orphans:(full_orphans ++ orphan_versions)
      ~requested:names
      ~reinstall:t.reinstall
      (OpamSolver.request
         ~install:to_install
         ~upgrade:(OpamSolution.atoms_of_packages to_upgrade)
         ~criteria:`Upgrade ())
  else
  let changes =
    requested_installed ++ OpamSwitchState.packages_of_atoms t to_install
  in
  let t, full_orphans, orphan_versions = orphans ~changes t in
  let to_remove = requested_installed %% full_orphans in
  let to_upgrade = requested_installed -- full_orphans in
  let upgrade_atoms =
    (* packages corresponds to the currently installed versions.
       Not what we are interested in, recover the original atom constraints *)
    List.map (fun nv ->
        let name = nv.name in
        try name, List.assoc name atoms
        with Not_found -> name, None)
      (OpamPackage.Set.elements to_upgrade) in
  names,
  OpamSolution.resolve t Upgrade
    ~orphans:(full_orphans ++ orphan_versions)
    ~requested:names
    (OpamSolver.request
       ~install:to_install
       ~remove:(OpamSolution.atoms_of_packages to_remove)
       ~upgrade:upgrade_atoms
       ())

let upgrade_t ?strict_upgrade ?auto_install ?ask ?(check=false) ~all atoms t =
  log "UPGRADE %a"
    (slog @@ function [] -> "<all>" | a -> OpamFormula.string_of_atoms a)
    atoms;
  match compute_upgrade_t ?strict_upgrade ?auto_install ~all atoms t with
  | requested, Conflicts cs ->
    log "conflict!";
    if not (OpamPackage.Name.Set.is_empty requested) then
      (OpamConsole.msg "%s"
         (OpamCudf.string_of_conflict t.packages
            (OpamSwitchState.unavailable_reason t) cs);
       OpamStd.Sys.exit_because `No_solution);
    let reasons, chains, cycles =
      OpamCudf.strings_of_conflict t.packages
        (OpamSwitchState.unavailable_reason t) cs in
    if cycles <> [] then begin
      OpamConsole.error
        "Dependency errors in the upgrade actions. Please update, and \
         report the following to the package maintainers if the error \
         persists:";
      OpamConsole.errmsg "%s\n%s\n"
        (OpamStd.Format.itemize (fun x -> x) cycles)
        "You may try upgrading packages individually to work around this."
    end else begin
      OpamConsole.warning
        "Upgrade is not possible because of conflicts or packages that \
         are no longer available:";
      OpamConsole.errmsg "%s" (OpamStd.Format.itemize (fun x -> x) reasons);
      if chains <> [] then
        OpamConsole.errmsg
          "The following dependencies are the cause:\n%s"
          (OpamStd.Format.itemize (fun x -> x) chains);
      OpamConsole.errmsg
        "\nYou may run \"opam upgrade --fixup\" to let opam fix the \
         current state.\n"
    end;
    OpamStd.Sys.exit_because `No_solution
  | requested, Success solution ->
    if check then
      OpamStd.Sys.exit_because
        (if OpamSolver.solution_is_empty solution
         then `False
         else `Success)
    else
    let t, result = OpamSolution.apply ?ask t Upgrade ~requested solution in
    if result = Nothing_to_do then (
      let to_check =
        if OpamPackage.Name.Set.is_empty requested then t.installed
        else OpamPackage.packages_of_names t.installed requested
      in
      let latest =
        OpamPackage.Name.Set.fold (fun name acc ->
            OpamPackage.Set.add (OpamPackage.max_version t.packages name) acc)
          (OpamPackage.names_of_packages to_check)
          OpamPackage.Set.empty in
      let notuptodate = latest -- to_check in
      if OpamPackage.Set.is_empty notuptodate then
        OpamConsole.msg "Already up-to-date.\n"
      else
        (let hdmsg = "Everything as up-to-date as possible" in
         let unav = notuptodate -- Lazy.force t.available_packages in
         let unopt = notuptodate %% Lazy.force t.available_packages in
         let base =
           OpamPackage.packages_of_names unopt
             (OpamPackage.names_of_packages t.compiler_packages)
         in
         let unopt = unopt -- base in
         let conflicts =
           let get_formula pkg =
             OpamStd.Option.map
               (fun opam ->
                  OpamFilter.filter_formula ~default:false
                    (OpamPackageVar.resolve_switch ~package:pkg t)
                    (OpamFile.OPAM.conflicts opam))
               (OpamSwitchState.opam_opt t pkg)
           in
           OpamPackage.Set.fold (fun unopt_pkg map ->
               let set =
                 OpamSwitchState.conflicts_with t
                   (OpamPackage.Set.singleton unopt_pkg) latest in
               OpamPackage.Set.fold (fun installed_pkg map ->
                   match get_formula installed_pkg with
                   | None -> map
                   | Some conflicts_formula ->
                     OpamFormula.fold_left
                       (fun map (n,formula) ->
                          if OpamPackage.name unopt_pkg = n &&
                             OpamFormula.check_version_formula formula
                               (OpamPackage.version unopt_pkg)
                          then
                            OpamPackage.Map.update unopt_pkg
                              (OpamStd.List.cons (installed_pkg, formula)) [] map
                          else map
                       ) map conflicts_formula
                 ) set map
             ) unopt OpamPackage.Map.empty
         in
         (* First, folding on [latest] packages: for each one, check if
            a [unopt] package does not verify [latest] package
            dependency formula *)
         let incompatibilities =
           let get_formula pkg =
             OpamStd.Option.map (OpamPackageVar.all_depends t)
               (OpamSwitchState.opam_opt t pkg)
           in
           OpamPackage.Set.fold (fun latest_pkg map ->
               match get_formula latest_pkg with
               | None -> map
               | Some depends_formula ->
                 OpamPackage.Set.fold
                   (fun unopt_pkg map ->
                      OpamFormula.fold_left
                        (fun map (n, formula) ->
                           if OpamPackage.name unopt_pkg = n &&
                              formula <> OpamFormula.Empty &&
                              not (OpamFormula.check_version_formula formula
                                     (OpamPackage.version unopt_pkg))
                           then
                             OpamPackage.Map.update unopt_pkg
                               (OpamStd.List.cons (latest_pkg, formula)) [] map
                           else map
                        ) map depends_formula
                   ) unopt map
             ) latest OpamPackage.Map.empty
         in

         if (OpamConsole.verbose ()) && not (OpamPackage.Set.is_empty unav) then
           (OpamConsole.formatted_msg
              "%s.\n\
               The following newer versions couldn't be installed:\n"
              hdmsg;
            OpamConsole.msg "%s"
              (OpamStd.Format.itemize (fun p ->
                   Printf.sprintf "%s.%s: %s"
                     (OpamConsole.colorise `bold
                        (OpamPackage.name_to_string p))
                     (OpamPackage.version_to_string p)
                     (OpamSwitchState.unavailable_reason t
                        ~default:"unavailable for unknown reasons (this may \
                                  be a bug in opam)"
                        (OpamPackage.name p,
                         Atom (`Eq, OpamPackage.version p))))
                  (OpamPackage.Set.elements unav)))
         else
           OpamConsole.formatted_msg
             "%s (run with --verbose to show unavailable upgrades).\n" hdmsg;
         if not (OpamPackage.Set.is_empty unopt) then
           (let bullet =
              OpamConsole.(colorise `red
                             (utf8_symbol Symbols.asterisk_operator "--"))
              ^ " "
            in
            let string_dep pkg map reason =
              List.fold_right (fun (p, f) acc ->
                   Printf.sprintf "%s\n%s%s is installed and %s %s"
                     acc bullet
                     (OpamPackage.to_string p)
                     reason
                     (OpamFormula.to_string (Atom (pkg.name, f)))
                )
                (OpamStd.Option.default [] (OpamPackage.Map.find_opt pkg map))
                ""
            in
            OpamConsole.formatted_msg
              "\nThe following packages are not being upgraded because the new \
               versions conflict with other installed packages:\n";
            OpamConsole.msg "%s"
              (OpamStd.Format.itemize
                 (fun pkg -> Printf.sprintf "%s.%s%s%s"
                     (OpamConsole.colorise `bold
                        (OpamPackage.name_to_string pkg))
                     (OpamPackage.version_to_string pkg)
                     (string_dep pkg incompatibilities "requires")
                     (string_dep pkg conflicts "conflicts with")
                 ) (OpamPackage.Set.elements unopt))
           );
         OpamConsole.formatted_msg
           "However, you may \"opam upgrade\" these packages explicitly, \
            which will ask permission to downgrade or uninstall the \
            conflicting packages.\n";

        )
    );
    OpamSolution.check_solution t result;
    t

let upgrade t ?check ~all names =
  let atoms = OpamSolution.sanitize_atom_list t names in
  let t = update_dev_packages_t atoms t in
  upgrade_t ?check ~strict_upgrade:(not all) ~all atoms t

let fixup t =
  log "FIXUP";
  let t, full_orphans, orphan_versions = orphans ~transitive:true t in
  let all_orphans = full_orphans ++ orphan_versions in
  let resolve pkgs =
    pkgs,
    OpamSolution.resolve t Upgrade
      ~orphans:all_orphans
      ~requested:(OpamPackage.names_of_packages pkgs)
      (OpamSolver.request
         ~install:(OpamSolution.atoms_of_packages pkgs)
         ~criteria:`Fixup
         ())
  in
  let is_success = function
    | _, Success _ -> true
    | _, Conflicts cs ->
      log "conflict: %a"
        (slog (OpamCudf.string_of_conflict t.packages @@
               OpamSwitchState.unavailable_reason t))
        cs;
      false
  in
  let requested, solution =
    let s =
      log "fixup-1/ keep installed packages with orphaned versions and roots";
      resolve (t.installed_roots %% t.installed
               -- full_orphans ++ orphan_versions)
    in
    if is_success s then s else
    let s =
      log "fixup-2/ keep just roots";
      resolve (t.installed_roots %% t.installed -- full_orphans)
    in
    if is_success s then s else
    let s =
      log "fixup-3/ keep packages with orphaned versions";
      resolve orphan_versions
    in
    if is_success s then s else
    let s =
      log "fixup-4/ last resort: no constraints. This should never fail";
      resolve OpamPackage.Set.empty
    in
    s
    (* Could still fail with uninstallable base packages actually, but we
       can only fix so far *)
  in
  let t, result = match solution with
    | Conflicts cs -> (* ouch... *)
      OpamConsole.error
        "It appears that the base packages for this switch are no longer \
         available. Either fix their prerequisites or change them through \
         'opam list --base' and 'opam switch set-base'.";
      OpamConsole.errmsg "%s"
        (OpamCudf.string_of_conflict t.packages
           (OpamSwitchState.unavailable_reason t) cs);
      t, No_solution
    | Success solution ->
      let _, req_rm, _ = orphans ~transitive:false t in
      OpamSolution.apply ~ask:true t Upgrade
        ~requested:(OpamPackage.names_of_packages (requested ++ req_rm))
        solution
  in
  OpamSolution.check_solution t result;
  t

let update
    gt ~repos_only ~dev_only ?(all=false) names =
  log "UPDATE %a" (slog @@ String.concat ", ") names;
  let rt = OpamRepositoryState.load `Lock_none gt in
  let st, repos_only =
    match OpamStateConfig.get_switch_opt () with
    | None -> OpamSwitchState.load_virtual gt rt, true
    | Some sw -> OpamSwitchState.load `Lock_none gt rt sw, repos_only
  in
  let repo_names =
    let all_repos = OpamRepositoryName.Map.keys rt.repositories in
    if dev_only then []
    else if names <> [] then
      List.filter
        (fun r -> List.mem (OpamRepositoryName.to_string r) names)
        all_repos
    else if all then all_repos
    else OpamSwitchState.repos_list st
  in
  let packages, ignore_packages =
    if repos_only then OpamPackage.Set.empty, OpamPackage.Set.empty else
    let packages = st.installed ++ st.pinned in
    let packages =
      if names = [] then packages else
        OpamPackage.Set.filter (fun nv ->
            let name = OpamPackage.Name.to_string nv.name in
            let pkg = OpamPackage.to_string nv in
            List.exists (fun s -> s = name || s = pkg) names &&
            let pinned = OpamPinned.package_opt st nv.name in
            pinned = None || pinned = Some nv
          ) packages
    in
    let dev_packages, nondev_packages =
      OpamPackage.Set.partition (OpamSwitchState.is_dev_package st) packages
    in
    let dev_packages = dev_packages -- (st.compiler_packages -- st.pinned) in
    let nondev_packages =
      if names = [] || OpamPackage.Set.is_empty nondev_packages then
        OpamPackage.Set.empty
      else
        (OpamConsole.warning
           "The following are not development packages (no dynamic or version \
            controlled upstream) and can't be updated individually: %s\n\
            You may want to update your repositories with just %s or to \
            upgrade your package%s with %s %s"
           (OpamStd.List.concat_map ", " ~last_sep:"and" OpamPackage.to_string
              (OpamPackage.Set.elements nondev_packages))
           (OpamConsole.colorise `bold "opam update")
           (if OpamPackage.Set.is_singleton nondev_packages then "" else "s")
           (OpamConsole.colorise `bold "opam upgrade")
           (OpamConsole.colorise `bold
              (OpamStd.List.concat_map " " OpamPackage.name_to_string
                 (OpamPackage.Set.elements nondev_packages)));
         nondev_packages)
    in
    let dirty_dev_packages, dev_packages =
      if names <> [] then OpamPackage.Set.empty, dev_packages else
        OpamPackage.Set.partition
          (fun nv ->
             let src_cache = OpamSwitchState.source_dir st nv in
             let cache_url =
               OpamUrl.of_string (OpamFilename.Dir.to_string src_cache)
             in
             match OpamSwitchState.primary_url st nv with
             | Some { OpamUrl.backend = #OpamUrl.version_control as vc; _ } ->
               OpamProcess.Job.run @@
               OpamRepository.is_dirty { cache_url with OpamUrl.backend = vc }
             | _ -> false)
          dev_packages
    in
    OpamPackage.Set.iter (fun nv ->
        OpamConsole.note "%s has previously been updated with --working-dir, \
                          not resetting unless explicitly selected"
          (OpamPackage.to_string nv))
      dirty_dev_packages;
    dev_packages, nondev_packages
  in

  let remaining =
    let ps = packages ++ ignore_packages in
    List.filter (fun n -> not (
        List.mem (OpamRepositoryName.of_string n) repo_names ||
        (try OpamPackage.has_name ps (OpamPackage.Name.of_string n)
         with Failure _ -> false) ||
        (try OpamPackage.Set.mem (OpamPackage.of_string n) ps
         with Failure _ -> false)
      )) names
  in
  if remaining <> [] then
    OpamConsole.error
      "Unknown repositories or installed packages: %s"
      (String.concat ", " remaining);

  (* Do the updates *)
  let rt_before = rt in
  let repo_update_failure, rt =
    if repo_names = [] then [], rt else
      OpamRepositoryState.with_write_lock rt @@ fun rt ->
      OpamConsole.header_msg "Updating package repositories";
      OpamRepositoryCommand.update_with_auto_upgrade rt repo_names
  in
  let repo_changed =
    not
      (OpamRepositoryName.Map.equal
         (OpamPackage.Map.equal (OpamFile.OPAM.effectively_equal))
         rt_before.repo_opams rt.repo_opams)
  in

  (* st is still based on the old rt, it's not a problem at this point, but
     don't return it *)
  let (dev_update_success, dev_changed), _st =
    if OpamPackage.Set.is_empty packages then
      (true, false), st
    else
      OpamSwitchState.with_write_lock st @@ fun st ->
      OpamConsole.header_msg "Synchronising development packages";
      let success, st, updates = OpamUpdate.dev_packages st packages in
      if OpamClientConfig.(!r.json_out <> None) then
        OpamJson.append "dev-packages-updates"
          (OpamPackage.Set.to_json updates);
      (success, not (OpamPackage.Set.is_empty updates)), st
  in
  repo_update_failure = [] && dev_update_success && remaining = [] &&
  OpamPackage.Set.is_empty ignore_packages,
  repo_changed || dev_changed,
  rt

let init_checks ?(hard_fail_exn=true) init_config =
  (* Check for the external dependencies *)
  let check_external_dep name =
    OpamSystem.resolve_command name <> None
  in
  OpamConsole.msg "Checking for available remotes: ";
  let repo_types =
    ["rsync", "rsync and local";
     "git", "git"; "hg", "mercurial"; "darcs", "darcs"]
  in
  let available_repos, unavailable_repos =
    List.partition (check_external_dep @* fst) repo_types in
  OpamConsole.msg "%s.%s\n"
    (match available_repos with
     | [] -> "none"
     | r -> String.concat ", " (List.map snd r))
    (if unavailable_repos = [] then " Perfect!" else
       "\n" ^ OpamStd.Format.itemize (fun (cmd,msg) ->
           Printf.sprintf
             "you won't be able to use %s repositories unless you \
              install the %s command on your system."
             msg (OpamConsole.colorise `bold cmd))
         unavailable_repos);
  let soft_fail =
    if OpamCudfSolver.has_builtin_solver () then false else
    let external_solvers = ["aspcud"; "packup"; "mccs"] in
    if not (List.exists check_external_dep external_solvers) then
      (OpamConsole.error
         "No external solver found. You should get one of %s, or use a \
          version of opam compiled with a built-in solver (see \
          http://opam.ocaml.org/doc/External_solvers.html for \
          details)"
         (OpamStd.Format.pretty_list ~last:"or"
            (List.map (OpamConsole.colorise `bold) external_solvers));
       true)
    else false
  in
  let env v =
    let vs = OpamVariable.Full.variable v in
    OpamStd.Option.Op.(OpamStd.Option.of_Not_found (List.assoc vs)
                         OpamSysPoll.variables >>= Lazy.force)
  in
  let filter_tools =
    OpamStd.List.filter_map (fun (cmd,str,oflt) ->
        match oflt with
        | None -> Some (cmd,str)
        | Some flt -> if (OpamFilter.eval_to_bool env flt) then
            Some (cmd,str) else None)
  in
  let check_tool logf tools =
    match List.filter (not @* (List.exists check_external_dep) @* fst) tools with
    | [] -> false
    | missing -> (logf
                    (OpamStd.Format.itemize
                       (fun (miss,msg) -> Printf.sprintf "%s%s"
                           (OpamStd.List.concat_map " or "
                              (OpamConsole.colorise `bold) miss)
                           (match msg with | None -> "" | Some m -> ": "^m))
                       missing);
                  true)
  in
  let advised_deps =
    filter_tools (OpamFile.InitConfig.recommended_tools init_config)
  in
  let _ = check_tool
      (fun s -> OpamConsole.warning
          "Recommended dependencies -- most packages rely on these:";
        OpamConsole.errmsg "%s" s)
      advised_deps in

  let required_deps =
    filter_tools (OpamFile.InitConfig.required_tools init_config)
  in
  let hard_fail =
    let msg = if hard_fail_exn then OpamConsole.error else OpamConsole.warning in
    check_tool (fun s -> msg
                   "Missing dependencies -- \
                    the following commands are required for opam to operate:";
                 OpamConsole.errmsg "%s" s)
      required_deps
  in

  if hard_fail && hard_fail_exn then OpamStd.Sys.exit_because `Configuration_error
  else not (soft_fail || hard_fail)

let update_with_init_config ?(overwrite=false) config init_config =
  let module I = OpamFile.InitConfig in
  let module C = OpamFile.Config in
  let setifnew getter setter v conf =
    if overwrite then setter v conf
    else if getter conf = getter C.empty then setter v conf
    else conf
  in
  config |>
  setifnew C.jobs C.with_jobs (match I.jobs init_config with
      | Some j -> j
      | None -> Lazy.force OpamStateConfig.(default.jobs)) |>
  setifnew C.dl_tool C.with_dl_tool_opt (I.dl_tool init_config) |>
  setifnew C.dl_jobs C.with_dl_jobs
    (OpamStd.Option.default OpamStateConfig.(default.dl_jobs)
       (I.dl_jobs init_config)) |>
  setifnew C.criteria C.with_criteria (I.solver_criteria init_config) |>
  setifnew C.solver C.with_solver_opt (I.solver init_config) |>
  setifnew C.wrappers C.with_wrappers (I.wrappers init_config) |>
  setifnew C.global_variables C.with_global_variables
    (I.global_variables init_config) |>
  setifnew C.eval_variables C.with_eval_variables
    (I.eval_variables init_config) |>
  setifnew C.default_compiler C.with_default_compiler
    (I.default_compiler init_config)

let reinit ?(init_config=OpamInitDefaults.init_config()) ~interactive
    ?dot_profile ?update_config ?env_hook ?completion config shell =
  let root = OpamStateConfig.(!r.root_dir) in
  let config = update_with_init_config config init_config in
  let _all_ok = init_checks ~hard_fail_exn:false init_config in
  OpamFile.Config.write (OpamPath.config root) config;
  let custom_init_scripts =
    let env v =
      let vs = OpamVariable.Full.variable v in
      OpamStd.Option.Op.(OpamStd.Option.of_Not_found
                           (List.assoc vs) OpamSysPoll.variables >>= Lazy.force)
    in
    OpamStd.List.filter_map (fun ((nam,scr),oflt) -> match oflt with
        | None -> Some (nam,scr)
        | Some flt ->
          if OpamFilter.eval_to_bool env flt then Some (nam,scr) else None)
      (OpamFile.InitConfig.init_scripts init_config)
  in
  OpamEnv.write_custom_init_scripts root custom_init_scripts;
  OpamEnv.setup root ~interactive
    ?dot_profile ?update_config ?env_hook ?completion shell;
  let gt = OpamGlobalState.load `Lock_write in
  let rt = OpamRepositoryState.load `Lock_write gt in
  OpamConsole.header_msg "Updating repositories";
  let _failed, rt =
    OpamRepositoryCommand.update_with_auto_upgrade rt
      (OpamRepositoryName.Map.keys rt.repos_definitions)
  in
  let _rt = OpamRepositoryState.unlock rt in
  ()

let init
    ~init_config ~interactive
    ?repo ?(bypass_checks=false)
    ?dot_profile ?update_config ?env_hook ?(completion=true)
    shell =
  log "INIT %a"
    (slog @@ OpamStd.Option.to_string OpamRepositoryBackend.to_string) repo;
  let root = OpamStateConfig.(!r.root_dir) in
  let config_f = OpamPath.config root in
  let root_empty =
    not (OpamFilename.exists_dir root) || OpamFilename.dir_is_empty root in

  let gt, rt, default_compiler =
    if OpamFile.exists config_f then (
      OpamConsole.msg "Opam has already been initialized.\n";
      let gt = OpamGlobalState.load `Lock_write in
      gt, OpamRepositoryState.load `Lock_none gt, OpamFormula.Empty
    ) else (
      if not root_empty then (
        OpamConsole.warning "%s exists and is not empty"
          (OpamFilename.Dir.to_string root);
        if not (OpamConsole.confirm "Proceed?") then
          OpamStd.Sys.exit_because `Aborted);
      try
        (* Create the content of ~/.opam/config *)
        let repos = match repo with
          | Some r -> [r.repo_name, (r.repo_url, r.repo_trust)]
          | None -> OpamFile.InitConfig.repositories init_config
        in
        let config =
          update_with_init_config OpamFile.Config.empty init_config |>
          OpamFile.Config.with_repositories (List.map fst repos)
        in
        OpamFile.Config.write config_f config;

        let dontswitch =
          if bypass_checks then false else
          let all_ok = init_checks init_config in
          if not all_ok &&
             not (OpamConsole.confirm "Continue initialisation anyway ?")
          then OpamStd.Sys.exit_because `Configuration_error
          else not all_ok
        in
        let custom_scripts =
          let env v =
            let vs = OpamVariable.Full.variable v in
            OpamStd.Option.Op.(OpamStd.Option.of_Not_found (List.assoc vs)
                                 OpamSysPoll.variables >>= Lazy.force)
          in
          let scripts = OpamFile.InitConfig.init_scripts init_config in
          OpamStd.List.filter_map (fun ((nam,scr),oflt) -> match oflt with
              | None -> Some (nam,scr)
              | Some flt -> if OpamFilter.eval_to_bool env flt then
                  Some (nam,scr) else None) scripts
        in
        OpamEnv.write_custom_init_scripts root custom_scripts;
        let repos_config =
          OpamRepositoryName.Map.of_list repos |>
          OpamRepositoryName.Map.map OpamStd.Option.some
        in
        OpamFile.Repos_config.write (OpamPath.repos_config root)
          repos_config;

        log "updating repository state";
        let gt = OpamGlobalState.load `Lock_write in
        let rt = OpamRepositoryState.load `Lock_write gt in
        OpamConsole.header_msg "Fetching repository information";
        let failed, rt =
          OpamRepositoryCommand.update_with_auto_upgrade rt
            (List.map fst repos)
        in
        if failed <> [] then
          OpamConsole.error_and_exit `Sync_error
            "Initial download of repository failed";
        gt, OpamRepositoryState.unlock rt,
        (if dontswitch then OpamFormula.Empty
         else OpamFile.InitConfig.default_compiler init_config)
      with e ->
        OpamStd.Exn.finalise e @@ fun () ->
        if not (OpamConsole.debug ()) && root_empty then begin
          OpamSystem.release_all_locks ();
          OpamFilename.rmdir root
        end)
  in
  OpamEnv.setup root ~interactive
    ?dot_profile ?update_config ?env_hook ~completion shell;
  gt, rt, default_compiler


(* Checks a request for [atoms] for conflicts with the orphan packages *)
let check_conflicts t atoms =
  let changes = OpamSwitchState.packages_of_atoms t atoms in
  let t, full_orphans, orphan_versions = orphans ~changes t in
  let available_changes = changes %% Lazy.force t.available_packages in
  (* packages which still have local data are OK for install/reinstall *)
  let has_no_local_data nv =
    not (OpamFile.exists
           (OpamPath.Switch.installed_opam t.switch_global.root t.switch nv))
  in
  let full_orphans, full_orphans_with_local_data =
    OpamPackage.Set.partition has_no_local_data
      full_orphans in
  let orphan_versions, orphan_versions_with_local_data =
    OpamPackage.Set.partition
      (fun nv -> has_no_local_data nv ||
                 OpamPackage.has_name available_changes nv.name)
      orphan_versions in
  let available = lazy (t.packages -- full_orphans -- orphan_versions) in
  let orphans = full_orphans ++ orphan_versions in
  let conflict_atoms =
    List.filter
      (fun (name,_ as a) ->
         not (OpamPackage.has_name t.pinned name) &&
         OpamPackage.Set.exists (OpamFormula.check a) orphans && (*optim*)
         not (OpamPackage.Set.exists (OpamFormula.check a) (* real check *)
                (Lazy.force available)))
      atoms in
  if conflict_atoms <> [] then
    OpamConsole.error_and_exit `Not_found
      "Sorry, these packages are no longer available \
       from the repositories: %s"
      (OpamStd.Format.pretty_list
         (List.map OpamFormula.string_of_atom conflict_atoms))
  else
    {t with available_packages = lazy
              (Lazy.force t.available_packages ++
               full_orphans_with_local_data ++
               orphan_versions_with_local_data )},
    full_orphans,
    orphan_versions

let assume_built_restrictions t atoms =
  let installed_fixed, not_installed_fixed =
    let rec all_deps set pkgs =
      let universe =
        OpamSwitchState.universe t
          ~requested:(OpamPackage.names_of_packages pkgs)
          Install
      in
      let deps =
        OpamPackage.Set.of_list
          (OpamSolver.dependencies ~build:false ~post:true
             ~depopts:false ~installed:false ~unavailable:true universe pkgs)
      in
      let deps = deps -- pkgs in
      if OpamPackage.Set.is_empty deps then set
      else all_deps (set ++ deps) deps
    in
    let pkg_of_atoms =
      OpamPackage.Set.filter
        (fun p -> List.exists (fun a -> OpamFormula.check a p) atoms)
        t.packages
    in
    let all_fixed = all_deps OpamPackage.Set.empty pkg_of_atoms in
    OpamSolution.eq_atoms_of_packages all_fixed |> get_installed_atoms t
  in
  let atoms =
      atoms @ OpamSolution.eq_atoms_of_packages
        (OpamPackage.Set.of_list installed_fixed)
  in
  let t =
      let avp =
        OpamPackage.Set.filter
          (fun p -> not (List.exists (fun a -> OpamFormula.check a p)
                           not_installed_fixed))
          (Lazy.force t.available_packages)
      in
      { t with available_packages = lazy avp}
  in
  t, atoms

let filter_unpinned_locally t atoms f =
  OpamStd.List.filter_map (fun at ->
      let n,_ = at in
      if OpamSwitchState.is_pinned t n &&
         OpamStd.Option.Op.(OpamPinned.package_opt t n >>=
                            OpamSwitchState.primary_url t >>=
                            OpamUrl.local_dir) <> None
      then
        Some (f at)
      else
        (log "Package %a is not pinned locally and assume built \
              option is set, skipping"
           (slog OpamPackage.Name.to_string) n;
         None))
    atoms

let install_t t ?ask atoms add_to_roots ~deps_only ~assume_built =
  log "INSTALL %a" (slog OpamFormula.string_of_atoms) atoms;
  let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in

  let t, full_orphans, orphan_versions = check_conflicts t atoms in

  let atoms =
    let compl = function
      | (_, Some _) as at -> at
      | (name, None) as at ->
        match OpamPinned.package_opt t name with
        | Some nv -> OpamSolution.eq_atom_of_package nv
        | None -> at
    in
    if assume_built then filter_unpinned_locally t atoms compl
    else List.map compl atoms
  in
  let pkg_skip, pkg_new =
    get_installed_atoms t atoms in
  let pkg_reinstall =
    if assume_built then OpamPackage.Set.of_list pkg_skip
    else t.reinstall %% OpamPackage.Set.of_list pkg_skip
  in
  (* Add the packages to the list of package roots and display a
     warning for already installed package roots. *)
  let current_roots = t.installed_roots in
  let t =
    if deps_only then t else
      List.fold_left (fun t nv ->
          if OpamPackage.Set.mem nv t.installed then
            match add_to_roots with
            | None ->
              if not (OpamPackage.Set.mem nv pkg_reinstall) then
                OpamConsole.note
                  "Package %s is already installed (current version is %s)."
                  (OpamPackage.Name.to_string nv.name)
                  (OpamPackage.Version.to_string nv.version);
              t
            | Some true ->
              if OpamPackage.Set.mem nv t.installed_roots then
                OpamConsole.note
                  "Package %s is already installed as a root."
                  (OpamPackage.Name.to_string nv.name);
              { t with installed_roots =
                         OpamPackage.Set.add nv t.installed_roots }
            | Some false ->
              if OpamPackage.Set.mem nv t.compiler_packages then
                (OpamConsole.note
                   "Package %s is part of the compiler base and can't be set \
                    as 'installed automatically'"
                   (OpamPackage.name_to_string nv);
                 t)
              else if OpamPackage.Set.mem nv t.installed_roots then
                { t with installed_roots =
                           OpamPackage.Set.remove nv t.installed_roots }
              else
                (OpamConsole.note
                   "Package %s is already marked as 'installed automatically'."
                   (OpamPackage.Name.to_string nv.name);
                 t)
          else t
        ) t pkg_skip in
  if t.installed_roots <> current_roots then (
    let diff = t.installed_roots -- current_roots in
    if not (OpamPackage.Set.is_empty diff) then
      let diff = OpamPackage.Set.elements diff in
      let diff = List.rev (List.rev_map OpamPackage.to_string diff) in
      OpamConsole.msg
        "Adding %s to the list of installed roots.\n"
        (OpamStd.Format.pretty_list diff)
    else (
      let diff = current_roots -- t.installed_roots in
      let diff = OpamPackage.Set.elements diff in
      let diff = List.rev (List.rev_map OpamPackage.to_string diff) in
      OpamConsole.msg
        "Removing %s from the list of installed roots.\n"
        (OpamStd.Format.pretty_list diff)
    );
    OpamSwitchAction.write_selections t
  );

  let available_packages = Lazy.force t.available_packages in
  let available_packages =
    if deps_only then
      (* Assume the named packages are available *)
      List.fold_left (fun avail (name, _ as atom) ->
          if OpamPackage.Set.exists (OpamFormula.check atom) avail then avail
          else match OpamPinned.package_opt t name with
            | Some nv when OpamFormula.check atom nv ->
              OpamPackage.Set.add nv avail
            | _ ->
              avail ++
              OpamPackage.Set.filter (OpamFormula.check atom) t.packages)
        available_packages atoms
    else
      (OpamSolution.check_availability t available_packages atoms;
       available_packages) in
  let t = {t with available_packages = lazy available_packages} in

  if pkg_new = [] && OpamPackage.Set.is_empty pkg_reinstall then t else
  let t, atoms =
    if assume_built then
      assume_built_restrictions t atoms
    else t, atoms
  in
  let request = OpamSolver.request ~install:atoms () in
  let solution =
    let reinstall = if assume_built then Some pkg_reinstall else None in
    OpamSolution.resolve t Install
      ~orphans:(full_orphans ++ orphan_versions)
      ~requested:names
      ?reinstall
      request in
  let t, solution = match solution with
    | Conflicts cs ->
      log "conflict!";
      OpamConsole.msg "%s"
        (OpamCudf.string_of_conflict t.packages
           (OpamSwitchState.unavailable_reason t) cs);
      t, No_solution
    | Success solution ->
      let solution =
        if deps_only then
          OpamSolver.filter_solution (fun nv ->
              not (OpamPackage.Name.Set.mem nv.name names))
            solution
        else solution in
      let add_roots =
        OpamStd.Option.map (function
            | true -> names
            | false -> OpamPackage.Name.Set.empty)
          add_to_roots
      in
      OpamSolution.apply ?ask t Install ~requested:names ?add_roots
        ~assume_built solution
  in
  OpamSolution.check_solution t solution;
  t

let install t ?autoupdate ?add_to_roots
    ?(deps_only=false) ?(assume_built=false) names =
  let atoms = OpamSolution.sanitize_atom_list ~permissive:true t names in
  let autoupdate_atoms = match autoupdate with
    | None -> atoms
    | Some a -> OpamSolution.sanitize_atom_list ~permissive:true t a
  in
  let t = update_dev_packages_t autoupdate_atoms t in
  install_t t atoms add_to_roots ~deps_only ~assume_built

let remove_t ?ask ~autoremove ~force atoms t =
  log "REMOVE autoremove:%b %a" autoremove
    (slog OpamFormula.string_of_atoms) atoms;

  let t, full_orphans, orphan_versions =
    let changes =
      if autoremove then None
      else Some (OpamSwitchState.packages_of_atoms t atoms) in
    orphans ?changes t
  in

  let nothing_to_do = ref true in
  let packages, not_installed =
    get_installed_atoms t atoms in
  if not_installed <> [] then (
    if force then
      let force_remove atom =
        let candidates = OpamPackage.Set.filter (OpamFormula.check atom) t.packages in
        try
          let nv = OpamPackage.max_version candidates (fst atom) in
          OpamConsole.note "Forcing removal of (uninstalled) %s" (OpamPackage.to_string nv);
          OpamProcess.Job.run (OpamAction.remove_package t nv);
          OpamAction.cleanup_package_artefacts t nv;
          nothing_to_do := false
        with Not_found ->
          OpamConsole.error "No package %s found for (forced) removal.\n"
            (OpamFormula.short_string_of_atom atom)
      in
      List.iter force_remove not_installed
    else
      OpamConsole.note "%s %s not installed.\n"
        (OpamStd.Format.pretty_list
           (List.map OpamFormula.short_string_of_atom not_installed))
        (match not_installed with [_] -> "is" | _ -> "are")
  );

  if autoremove || packages <> [] then (
    let packages = OpamPackage.Set.of_list packages in
    let universe =
      OpamSwitchState.universe t
        ~requested:(OpamPackage.names_of_packages packages)
        Remove
    in
    let to_remove =
      OpamPackage.Set.of_list
        (OpamSolver.reverse_dependencies ~build:true ~post:true
           ~depopts:false ~installed:true universe packages) in
    let to_keep =
      (if autoremove then t.installed_roots %% t.installed else t.installed)
      ++ universe.u_base
      -- to_remove -- full_orphans -- orphan_versions
    in
    let to_keep =
      OpamPackage.Set.of_list
        (OpamSolver.dependencies ~build:true ~post:true
           ~depopts:true ~installed:true universe to_keep) in
    (* to_keep includes the depopts, because we don't want to autoremove
       them. But that may re-include packages that we wanted removed, so we
       need to remove them again *)
    let to_keep = to_keep -- to_remove in
    let requested = OpamPackage.names_of_packages packages in
    let to_remove =
      if autoremove then
        let to_remove = t.installed -- to_keep in
        if atoms = [] then to_remove
        else (* restrict to the dependency cone of removed pkgs *)
          to_remove %%
          (OpamPackage.Set.of_list
             (OpamSolver.dependencies ~build:true ~post:true
                ~depopts:true ~installed:true universe to_remove))
      else to_remove in
    let t, solution =
      OpamSolution.resolve_and_apply ?ask t Remove ~requested
        ~orphans:(full_orphans ++ orphan_versions)
        (OpamSolver.request
           ~install:(OpamSolution.eq_atoms_of_packages to_keep)
           ~remove:(OpamSolution.atoms_of_packages to_remove)
           ())
    in
    OpamSolution.check_solution t solution;
    t
  ) else if !nothing_to_do then (
    OpamConsole.msg "Nothing to do.\n";
    t
  ) else t

let remove t ~autoremove ~force names =
  let atoms = OpamSolution.sanitize_atom_list t names in
  remove_t ~autoremove ~force atoms t

let reinstall_t t ?ask ?(force=false) ~assume_built atoms =
  log "reinstall %a" (slog OpamFormula.string_of_atoms) atoms;

  let atoms =
    if assume_built then filter_unpinned_locally t atoms (fun x -> x)
    else atoms
  in

  let reinstall, not_installed =
    get_installed_atoms t atoms in
  let to_install =
    if not_installed <> [] then
      if
        force || assume_built ||
        OpamConsole.confirm "%s %s not installed. Install %s?"
          (OpamStd.Format.pretty_list
             (List.rev_map OpamFormula.short_string_of_atom not_installed))
          (match not_installed with [_] -> "is" | _ -> "are")
          (match not_installed with [_] -> "it" | _ -> "them")
      then not_installed
      else OpamStd.Sys.exit_because `Aborted
    else []
  in

  let reinstall = OpamPackage.Set.of_list reinstall in

  let atoms =
    to_install @ OpamSolution.eq_atoms_of_packages reinstall in

  let t, full_orphans, orphan_versions = check_conflicts t atoms in

  let requested =
    OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in

  let t, atoms =
    if assume_built then
      assume_built_restrictions t atoms
    else t, atoms
  in

  let request = OpamSolver.request ~install:atoms ~criteria:`Fixup () in

  let t, solution =
    OpamSolution.resolve_and_apply ?ask t Reinstall
      ~orphans:(full_orphans ++ orphan_versions)
      ~reinstall:(OpamPackage.packages_of_names t.installed requested)
      ~requested
      ~assume_built
      request in

  OpamSolution.check_solution t solution;
  t

let reinstall t ?(assume_built=false) names =
  let atoms = OpamSolution.sanitize_atom_list t names in
  let t = update_dev_packages_t atoms t in
  reinstall_t t ~assume_built atoms

module PIN = struct
  open OpamPinCommand

  let post_pin_action st names =
    try
      upgrade_t ~strict_upgrade:false ~auto_install:true ~ask:true ~all:false
       (List.map (fun name -> name, None) names) st
    with e ->
      OpamConsole.note
        "Pinning command successful, but your installed packages \
         may be out of sync.";
      raise e

  let get_upstream t name =
    try match
        OpamStd.Option.Op.(
          OpamSwitchState.get_package t name |>
          OpamSwitchState.opam_opt t >>=
          OpamFile.OPAM.dev_repo
        )
      with
      | None ->
        OpamConsole.error_and_exit `Not_found
          "\"dev-repo\" field missing in %s metadata, you'll need to specify \
           the pinning location"
          (OpamPackage.Name.to_string name)
      | Some url -> url
    with Not_found ->
      OpamConsole.error_and_exit `Not_found
        "No package named %S found"
        (OpamPackage.Name.to_string name)

  let pin st name ?(edit=false) ?version ?(action=true) target =
    try
      let pinned = st.pinned in
      let st =
        match target with
        | `Source url -> source_pin st name ?version ~edit (Some url)
        | `Version v ->
          let st = version_pin st name v in
          if edit then OpamPinCommand.edit st name else st
        | `Dev_upstream ->
          source_pin st name ?version ~edit (Some (get_upstream st name))
        | `None -> source_pin st name ?version ~edit None
      in
      if action then
        let names =
          OpamPackage.Set.Op.(st.pinned -- pinned)
          |> OpamPackage.Set.elements
          |> List.map OpamPackage.name
        in
        (OpamConsole.msg "\n"; post_pin_action st names)
      else st
    with
    | OpamPinCommand.Aborted -> OpamStd.Sys.exit_because `Aborted
    | OpamPinCommand.Nothing_to_do -> st

  let edit st ?(action=true) ?version name =
    let pinned = st.pinned in
    let st =
      if OpamPackage.has_name st.pinned name then
        edit st ?version name
      else
      let pin_nv =
        match version with
        | Some v ->
          let nv = OpamPackage.create name v in
          if OpamPackage.Set.mem nv st.packages then Some nv else None
        | None ->
          OpamStd.Option.of_Not_found (OpamSwitchState.get_package st) name
      in
      match pin_nv with
      | Some nv ->
        if OpamConsole.confirm
            "Package %s is not pinned. Edit as a new pinning to version %s?"
            (OpamPackage.Name.to_string name)
            (OpamPackage.version_to_string nv)
        then
          let target =
            OpamStd.Option.Op.(OpamSwitchState.url st nv >>| OpamFile.URL.url)
          in
          let opam = OpamPackage.Map.find_opt nv st.repos_package_index in
          try source_pin st name ~edit:true ?version ?opam target
          with OpamPinCommand.Aborted -> OpamStd.Sys.exit_because `Aborted
             | OpamPinCommand.Nothing_to_do -> st
        else
          OpamStd.Sys.exit_because `Aborted
      | None ->
        OpamConsole.error_and_exit `Not_found
          "Package is not pinned, and no existing version was supplied."
    in
    if action then
      let names =
        OpamPackage.Set.Op.(st.pinned -- pinned)
        |> OpamPackage.Set.elements
        |> List.map OpamPackage.name
      in
      post_pin_action st names
    else st

  let unpin st ?(action=true) names =
    let pinned_before = st.pinned in
    let st = unpin st names in
    let available = Lazy.force st.available_packages in
    let installed_unpinned = (pinned_before -- st.pinned) %% st.installed in
    if action && not (OpamPackage.Set.is_empty installed_unpinned) then
      let atoms =
        OpamPackage.Set.fold (fun nv acc ->
            if OpamPackage.Set.mem nv available then
              (nv.name, Some (`Eq, nv.version)) :: acc
            else (nv.name, None) :: acc)
          installed_unpinned []
      in
      upgrade_t ~strict_upgrade:false ~auto_install:true ~ask:true ~all:false
        atoms st
    else st

  let list = list
end

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
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
let update_dev_packages_t ?autolock ?(only_installed=false) atoms t =
  if OpamClientConfig.(!r.skip_dev_update) then t else
  let working_dir = OpamClientConfig.(!r.working_dir || !r.inplace_build) in
  let to_update =
    List.fold_left (fun to_update (name,_) ->
        try
          let nv = OpamPackage.package_of_name t.pinned name in
          if OpamSwitchState.is_dev_package t nv &&
             ( not only_installed ||
               OpamPackage.Set.exists (fun nv -> nv.name = name) t.installed )
          then
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
        OpamUpdate.dev_packages t ?autolock ?working_dir to_update in
      OpamConsole.msg "\n";
      t
    with e ->
      OpamStd.Exn.fatal e;
      OpamConsole.msg "\n";
      t
  )

let compute_upgrade_t
    ?(strict_upgrade=true) ?(auto_install=false) ?(only_installed=false)
    ~all ~formula atoms t =
  let packages = OpamFormula.packages_of_atoms t.packages atoms in
  let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
  let atoms =
    if strict_upgrade then
      List.map (fun (n, cstr as atom) ->
          if cstr <> None then atom else
          try
            let nv = OpamSwitchState.find_installed_package_by_name t n in
            let strict_upgrade_atom = (n, Some (`Gt, nv.version)) in
            if not (OpamSwitchState.is_dev_package t nv) &&
               not (OpamPackage.has_name t.pinned n) &&
               not (OpamPackage.Set.mem nv (Lazy.force t.reinstall)) &&
               OpamPackage.Set.exists
                 (not @* OpamSwitchState.avoid_version t)
                 (OpamFormula.packages_of_atoms
                    (Lazy.force t.available_packages)
                    [strict_upgrade_atom])
            then strict_upgrade_atom
            else atom
          with Not_found -> atom)
        atoms
    else
      atoms
  in
  let installed, not_installed =
    List.partition (fun (n,_) -> OpamPackage.has_name t.installed n) atoms
  in
  let atoms =
    if not_installed = [] ||
       auto_install ||
       not only_installed &&
       OpamConsole.confirm
         (match not_installed with
          | [_] -> "%s is not installed. Install it?"
          | _ -> "%s are not installed. Install them?")
         (OpamStd.Format.pretty_list
            (List.rev_map OpamFormula.short_string_of_atom not_installed))
    then atoms
    else installed
  in
  let to_install, to_upgrade =
    List.partition (fun (n,_) ->
        match OpamPackage.package_of_name_opt t.installed n with
        | None -> true
        | Some nv -> not (OpamPackage.Set.mem nv (Lazy.force t.available_packages)))
      atoms
  in
  let criteria = if to_install = [] then `Upgrade else `Default in
  if all then
    names,
    OpamSolution.resolve t Upgrade
      ~requested:packages
      ~reinstall:(Lazy.force t.reinstall)
      (OpamSolver.request
         ~install:to_install
         ~upgrade:to_upgrade
         ~deprequest:(OpamFormula.to_atom_formula formula)
         ~all:[]
         ~criteria ())
  else
  names,
  OpamSolution.resolve t Upgrade
    ~requested:packages
    (OpamSolver.request
       ~install:to_install
       ~upgrade:to_upgrade
       ~deprequest:(OpamFormula.to_atom_formula formula)
       ~criteria
       ())

let print_requested requested formula =
  OpamFormula.fold_left
    (fun req (name,_) -> OpamPackage.Name.Set.add name req)
    requested formula

let upgrade_t
    ?strict_upgrade ?auto_install ?ask ?(check=false) ?(terse=false)
    ?only_installed ~all atoms ?(formula=OpamFormula.Empty) t
  =
  log "UPGRADE %a"
    (slog @@ function [] -> "<all>" | a -> OpamFormula.string_of_atoms a)
    atoms;
  match
    compute_upgrade_t ?strict_upgrade ?auto_install ?only_installed ~all
      ~formula atoms t
  with
  | requested, Conflicts cs ->
    log "conflict!";
    if not (OpamPackage.Name.Set.is_empty requested) then
      (OpamConsole.error "Package conflict!";
       OpamConsole.errmsg "%s"
         (OpamCudf.string_of_conflicts t.packages
            (OpamSwitchState.unavailable_reason t) cs);
       OpamStd.Sys.exit_because `No_solution);
    let reasons, cycles =
      OpamCudf.conflict_explanations t.packages
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
      OpamConsole.errmsg "  %s"
        (OpamStd.Format.itemize (OpamCudf.string_of_conflict ~start_column:2)
           reasons);
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
    let packages = OpamPackage.packages_of_names t.packages requested in
    let t, result =
      OpamSolution.apply ?ask t ~requested:packages
        ~print_requested:(print_requested requested formula)
        solution
    in
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
      else if terse then
        OpamConsole.msg "No package build needed.\n"
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
    OpamSolution.check_solution t (Success result);
    t

let upgrade t ?formula ?check ?only_installed ~all names =
  let atoms = OpamSolution.sanitize_atom_list t names in
  let t = update_dev_packages_t ~autolock:true ?only_installed atoms t in
  upgrade_t ?check ~strict_upgrade:(not all) ?only_installed ~all
    atoms ?formula t

let fixup ?(formula=OpamFormula.Empty) t =
  (* @LG reimplement as an alias for 'opam upgrade --criteria=fixup --best-effort --update-invariant *)
  log "FIXUP";
  let resolve pkgs =
    pkgs,
    OpamSolution.resolve t Upgrade
      ~requested:pkgs
      (OpamSolver.request
         ~install:(OpamSolution.atoms_of_packages pkgs)
         ~all:[]
         ~criteria:`Fixup
         ~deprequest:(OpamFormula.to_atom_formula formula)
         ())
  in
  let is_success = function
    | _, Success _ -> true
    | _, Conflicts cs ->
      log "conflict: %a"
        (slog (OpamCudf.string_of_conflicts t.packages @@
               OpamSwitchState.unavailable_reason t))
        cs;
      false
  in
  let requested, solution =
    let s =
      log "fixup-1/ keep installed packages with orphaned versions and roots";
      resolve (t.installed_roots %% t.installed
               %% Lazy.force t.available_packages)
    in
    if is_success s then s else
    let s =
      log "fixup-2/ last resort: no constraints. This should never fail";
      resolve OpamPackage.Set.empty
    in
    s
    (* Could still fail with uninstallable base packages actually, but we
       can only fix so far *)
  in
  let t, result = match solution with
    | Conflicts cs -> (* ouch... *)
      OpamConsole.error
        "It appears that the switch invariant is no longer satisfiable. \
         Either fix the package prerequisites or change the invariant \
         with 'opam switch set-invariant'.";
      OpamConsole.errmsg "%s"
        (OpamCudf.string_of_conflicts t.packages
           (OpamSwitchState.unavailable_reason t) cs);
      t, Conflicts cs
    | Success solution ->
      let print_requested =
        print_requested (OpamPackage.names_of_packages requested) formula
      in
      let t, res =
        OpamSolution.apply ~ask:true t
          ~requested
          ~print_requested
          solution in
      t, Success res
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
      if names <> [] || OpamClientConfig.(!r.drop_working_dir) then
        OpamPackage.Set.empty, dev_packages
      else
        OpamPackage.Set.partition
          (fun nv ->
             let src_cache = OpamSwitchState.source_dir st nv in
             let cache_url =
               OpamUrl.of_string (OpamFilename.Dir.to_string src_cache)
             in
             match OpamSwitchState.primary_url st nv with
             | Some { OpamUrl.backend = #OpamUrl.version_control as vc; _ } ->
               (try
                 OpamProcess.Job.run @@
                 OpamRepository.is_dirty { cache_url with OpamUrl.backend = vc }
                with OpamSystem.Process_error _ ->
                  log "Skipping %s, not a git repo" (OpamPackage.to_string nv);
                  false)
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
  let (dev_update_success, dev_changed), st =
    if OpamPackage.Set.is_empty packages then
      (true, false), st
    else
      OpamSwitchState.with_write_lock st @@ fun st ->
      let working_dir =
        if OpamClientConfig.(!r.working_dir) && names <> [] then
          Some (OpamPackage.packages_of_names packages
                  (OpamPackage.Name.(Set.of_list (List.map of_string names))))
        else None
      in
      OpamConsole.header_msg "Synchronising development packages";
      let success, st, updates =
        OpamUpdate.dev_packages st ~autolock:true ?working_dir packages
      in
      if OpamClientConfig.(!r.json_out <> None) then
        OpamJson.append "dev-packages-updates"
          (OpamPackage.Set.to_json updates);
      (success, not (OpamPackage.Set.is_empty updates)), st
  in
  OpamSwitchState.drop st;
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
  (match I.jobs init_config with
      | Some j -> setifnew C.jobs C.with_jobs j
      | None -> fun c -> c) |>
  setifnew C.dl_tool C.with_dl_tool_opt (I.dl_tool init_config) |>
  setifnew C.dl_cache C.with_dl_cache (I.dl_cache init_config) |>
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
    (I.default_compiler init_config) |>
  setifnew C.default_invariant C.with_default_invariant
    (I.default_invariant init_config)

let reinit ?(init_config=OpamInitDefaults.init_config()) ~interactive
    ?dot_profile ?update_config ?env_hook ?completion ?inplace
    ?(check_sandbox=true) ?(bypass_checks=false)
    config shell =
  let root = OpamStateConfig.(!r.root_dir) in
  let config = update_with_init_config config init_config in
  let _all_ok =
    if bypass_checks then false else
      init_checks ~hard_fail_exn:false init_config
  in
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
  let config =
    if check_sandbox then
      OpamAuxCommands.check_and_revert_sandboxing root config
    else config
  in
  OpamFile.Config.write (OpamPath.config root) config;
  OpamEnv.setup root ~interactive
    ?dot_profile ?update_config ?env_hook ?completion ?inplace shell;
  let gt = OpamGlobalState.load `Lock_write in
  let rt = OpamRepositoryState.load `Lock_write gt in
  OpamConsole.header_msg "Updating repositories";
  let _failed, rt =
    OpamRepositoryCommand.update_with_auto_upgrade rt
      (OpamRepositoryName.Map.keys rt.repos_definitions)
  in
  OpamRepositoryState.drop rt

let init
    ~init_config ~interactive
    ?repo ?(bypass_checks=false)
    ?dot_profile ?update_config ?env_hook ?(completion=true)
    ?(check_sandbox=true)
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
      if OpamFile.Config.installed_switches gt.config = [] then
        OpamConsole.msg
          "... but you have no switches installed, use `opam switch \
           create <compiler-or-version>' to get started.";
      gt, OpamRepositoryState.load `Lock_none gt, []
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
          update_with_init_config
            OpamFile.Config.(with_opam_root_version root_version empty)
            init_config |>
          OpamFile.Config.with_repositories (List.map fst repos)
        in

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
        let config =
          if check_sandbox then
            OpamAuxCommands.check_and_revert_sandboxing root config
          else config
        in
        OpamFile.Config.write config_f config;
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
          (if root_empty then
             (try OpamFilename.rmdir root with _ -> ());
           OpamConsole.error_and_exit `Sync_error
             "Initial download of repository failed.");
        let default_compiler =
          if dontswitch then [] else
          let chrono = OpamConsole.timer () in
          let alternatives =
            OpamFormula.to_dnf
              (OpamFile.InitConfig.default_compiler init_config)
          in
          let invariant = OpamFile.InitConfig.default_invariant init_config in
          let virt_st =
            OpamSwitchState.load_virtual ~avail_default:false gt rt
          in
          let univ =
            OpamSwitchState.universe virt_st
              ~requested:OpamPackage.Set.empty Query
          in
          let univ = { univ with u_invariant = invariant } in
          let default_compiler =
            OpamStd.List.find_opt
              (OpamSolver.atom_coinstallability_check univ)
            alternatives
            |> OpamStd.Option.default []
          in
          log "Selected default compiler %s in %0.3fs"
            (OpamFormula.string_of_atoms default_compiler)
            (chrono ());
          default_compiler
        in
        gt, OpamRepositoryState.unlock ~cleanup:false rt, default_compiler
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

let check_installed ~build ~post t atoms =
  let available = (Lazy.force t.available_packages) in
  let uninstalled = OpamPackage.Set.Op.(available -- t.installed) in
  let pkgs =
    OpamPackage.to_map
      (OpamFormula.packages_of_atoms available atoms)
  in
  let test = OpamStateConfig.(!r.build_test) in
  let doc = OpamStateConfig.(!r.build_doc) in
  let dev_setup = OpamStateConfig.(!r.dev_setup) in
  let env p =
    OpamFilter.deps_var_env ~build ~post ~test ~doc ~dev_setup
      ~dev:(OpamSwitchState.is_dev_package t p)
  in
  OpamPackage.Name.Map.fold (fun name versions map ->
      let compliant, missing_opt =
        OpamPackage.Version.Set.fold (fun version (found, missing) ->
            if found then (found, missing)
            else
            let pkg = OpamPackage.create name version in
            let cnf_formula =
              OpamSwitchState.opam t pkg
              |> OpamFile.OPAM.depends
              |> OpamFilter.filter_formula (env pkg)
              |> OpamFormula.to_cnf
            in
            let missing_conj =
              List.filter
                (List.for_all (fun ((n,_vc) as atom) ->
                     OpamPackage.Set.for_all
                       (fun p -> not (OpamFormula.check atom p))
                       (OpamPackage.packages_of_name t.installed n)))
                cnf_formula
            in
            if missing_conj = [] then true, None
            else false, Some (pkg,missing_conj))
          versions (false,None)
      in
      if compliant then map else
      match missing_opt with
      | None -> assert false (* version set can't be empty *)
      | Some (pkg, missing_conj) ->
        OpamPackage.Map.add pkg
          (OpamPackage.names_of_packages
             (List.fold_left (fun names disj ->
                  OpamPackage.Set.union names
                    (OpamFormula.packages_of_atoms uninstalled disj))
                 OpamPackage.Set.empty missing_conj))
          map
    ) pkgs OpamPackage.Map.empty

let assume_built_restrictions ?available_packages t atoms =
  let missing = check_installed ~build:false ~post:false t atoms in
  let atoms =
    if OpamPackage.Map.is_empty missing then atoms else
      (OpamConsole.warning
         "You specified '--assume-built' but the following dependencies \
          aren't installed, skipping\n%s\
          Launch 'opam install %s --deps-only' (and recompile) to \
          install them.\n"
         (OpamStd.Format.itemize (fun (nv, names) ->
              Printf.sprintf "%s: %s" (OpamPackage.name_to_string nv)
                (OpamStd.List.concat_map " " OpamPackage.Name.to_string
                   (OpamPackage.Name.Set.elements names)))
             (OpamPackage.Map.bindings missing))
         (OpamStd.List.concat_map " " OpamPackage.name_to_string
            (OpamPackage.Map.keys missing));
       List.filter (fun (n,_) ->
           not (OpamPackage.Map.exists (fun nv _ ->
               OpamPackage.name nv = n) missing))
         atoms)
  in
  let pinned =
    (* Not pinned atoms already removed. *)
    OpamPackage.Set.filter
      (fun p -> List.exists (fun a -> OpamFormula.check a p) atoms)
      t.pinned
  in
  let installed_dependencies =
    OpamSolver.dependencies ~build:false ~post:false
      ~depopts:false ~installed:true ~unavailable:false
      (OpamSwitchState.universe t
         ~requested:pinned Query)
      pinned
  in
  let available_packages =
    match available_packages with
    | Some a -> a
    | None -> Lazy.force t.available_packages
  in
  let uninstalled_dependencies =
    (OpamPackage.Map.values missing
     |> List.fold_left OpamPackage.Name.Set.union OpamPackage.Name.Set.empty
     |> OpamPackage.packages_of_names available_packages)
    -- installed_dependencies
  in
  let available_packages = lazy (
    (available_packages -- uninstalled_dependencies) ++ t.installed ++ pinned
  ) in
  let fixed_atoms =
    List.map (fun nv ->
        (OpamPackage.name nv , Some (`Eq, OpamPackage.version nv)))
      (OpamPackage.Set.elements pinned @
       OpamPackage.Set.elements installed_dependencies)
  in
  { t with available_packages }, fixed_atoms

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
        (OpamConsole.warning
           "Package %s is not pinned locally and assume built \
            option is set, skipping"
           (OpamPackage.Name.to_string n);
         None))
    atoms

let install_t t ?ask ?(ignore_conflicts=false) ?(depext_only=false)
    ?(download_only=false) atoms ?(formula=OpamFormula.Empty)
    add_to_roots ~deps_only ~assume_built =
  log "INSTALL %a" (slog OpamFormula.string_of_atoms) atoms;
  let available_packages = Lazy.force t.available_packages in

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
  let names =
    OpamPackage.Name.Set.of_list (List.rev_map fst atoms)
  in
  let dname_map =
    if deps_only then
      (* pkgname -> name of fake package for handling pkg deps *)
      OpamPackage.Name.Set.fold (fun name ->
          let open OpamPackage.Name in
          let rec nodup i name =
            if OpamPackage.has_name t.packages name then
              nodup (i+1) @@
              of_string (Printf.sprintf "deps-of-%d-%s" i (to_string name))
            else name
          in
          let dname = nodup 2 @@ of_string ("deps-of-" ^ to_string name) in
          OpamPackage.Name.Map.add name dname)
        names OpamPackage.Name.Map.empty
    else OpamPackage.Name.Map.empty
  in
  let t, deps_of_packages =
    (* add deps-of-xxx packages to replace each atom *)
    OpamPackage.Name.Map.fold (fun name dname (t, deps_of_packages) ->
        let ats = List.filter (fun (n,_) -> n = name) atoms in
        let nvs = OpamSwitchState.packages_of_atoms t ats in
        OpamPackage.Set.fold (fun nv (t, deps_of_packages) ->
            let module O = OpamFile.OPAM in
            let dnv = OpamPackage.create dname nv.version in
            let opam = OpamSwitchState.opam t nv in
            let depends =
              OpamFormula.map (fun (n,c as at) ->
                  try Atom (OpamPackage.Name.Map.find n dname_map, c)
                  with Not_found -> Atom at)
                (O.depends opam)
            in
            let conflicts =
              let vstring = OpamPackage.Version.to_string nv.version in
              OpamFormula.ors
                (Atom (nv.name, Atom (Constraint (`Neq, FString vstring))) ::
                 if ignore_conflicts then [] else [ O.conflicts opam ])
            in
            let url =
              if OpamSwitchState.is_dev_package t nv then
                Some (OpamFile.URL.create OpamUrl.{empty with backend = `git})
              else None
            in
            let dopam =
              O.create dnv |>
              O.with_depends depends |>
              O.with_conflicts conflicts |>
              O.with_depexts (O.depexts opam) |>
              O.with_url_opt url |>
              (* Note: the following avoids selecting unavailable versions as
                 much possible, but it won't really work for packages that
                 already have the flag *)
              O.with_flags (if OpamPackage.Set.mem nv (available_packages)
                            then O.flags opam else [Pkgflag_AvoidVersion])
            in
            let t =
              if OpamPackage.Set.mem nv t.installed
              then {t with installed = OpamPackage.Set.add dnv t.installed}
              else t
            in
            OpamSwitchState.update_package_metadata dnv dopam t,
            OpamPackage.Set.add dnv deps_of_packages)
          nvs (t, deps_of_packages))
      dname_map (t, OpamPackage.Set.empty)
  in
  let atoms, deps_atoms =
    if deps_only then
      [],
      List.map (fun (n, c) -> OpamPackage.Name.Map.find n dname_map, c) atoms
    else
      atoms, []
  in
  let pkg_skip, pkg_new =
    get_installed_atoms t atoms in
  let pkg_reinstall =
    if assume_built then OpamPackage.Set.of_list pkg_skip
    else Lazy.force t.reinstall %% OpamPackage.Set.of_list pkg_skip
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
              if OpamPackage.Set.mem nv t.installed_roots then begin
                if OpamPackage.Set.mem nv t.compiler_packages then
                  OpamConsole.note
                    "Package %s is part of the switch invariant and won't be \
                     uninstalled unless the invariant is updated."
                    (OpamPackage.name_to_string nv);
                { t with installed_roots =
                           OpamPackage.Set.remove nv t.installed_roots }
              end else
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

  OpamSolution.check_availability t available_packages atoms;

  if pkg_new = [] && OpamPackage.Set.is_empty pkg_reinstall &&
     formula = OpamFormula.Empty &&
     deps_atoms = []
  then t else
  let t, atoms =
    if assume_built then
      assume_built_restrictions ~available_packages t atoms
    else t, atoms
  in
  let request =
    OpamSolver.request ()
      ~install:(atoms @ deps_atoms)
      ~deprequest:(OpamFormula.to_atom_formula formula)
  in
  let requested =
    OpamPackage.Name.Set.of_list (List.rev_map fst (atoms @ deps_atoms))
  in
  let packages = OpamFormula.packages_of_atoms t.packages (atoms @ deps_atoms) in
  let solution =
    let reinstall = if assume_built then Some pkg_reinstall else None in
    OpamSolution.resolve t Install
      ~requested:packages
      ?reinstall
      request in
  let t = {t with installed = t.installed -- deps_of_packages} in
  let t, solution = match solution with
    | Conflicts cs ->
      log "conflict!";
      OpamConsole.error "Package conflict!";
      let (conflicts, _cycles) as explanations =
        OpamCudf.conflict_explanations_raw t.packages cs
      in
      let has_missing_depexts =
        let check = function
          | `Missing (_, _, fdeps) ->
            OpamFormula.fold_right (fun a x ->
                match OpamSwitchState.unavailable_reason_raw t x with
                | `MissingDepexts _ -> true
                | _ -> a)
              false fdeps
          | _ -> false
        in
        List.exists check conflicts
      in
      let extra_message =
        if has_missing_depexts then
          OpamStd.Option.map_default (fun s -> s ^ ".\n\n") ""
            (OpamSysInteract.repo_enablers ())
        else
          ""
      in
      OpamConsole.errmsg "%s%s"
        (OpamCudf.string_of_explanations
           (OpamSwitchState.unavailable_reason t) explanations)
        extra_message;
      t, if depext_only then None else Some (Conflicts cs)
    | Success solution ->
      let skip =
        let inst = OpamSolver.new_packages solution in
        OpamPackage.Name.Map.fold (fun n dn map ->
            match OpamPackage.package_of_name_opt inst dn with
            | Some dpkg ->
              (* todo: display the versions that have been chosen if there was
                 an ambiguity ? *)
              OpamPackage.Map.add dpkg (OpamPackage.create n dpkg.version) map
            | None -> map)
          dname_map OpamPackage.Map.empty
      in
      if depext_only then
        (OpamSolution.install_depexts ~force_depext:true ~confirm:false t
           (OpamSolver.all_packages solution)), None
      else
      let add_roots =
        OpamStd.Option.map (function
            | true ->
              OpamPackage.Name.Set.union requested @@
              OpamPackage.Name.Set.of_list (List.rev_map fst deps_atoms)
            | false -> OpamPackage.Name.Set.empty)
          add_to_roots
      in
      let t, res =
        OpamSolution.apply ?ask t
          ~requested:packages
          ~print_requested:(print_requested requested formula)
          ?add_roots ~skip
          ~download_only ~assume_built solution in
      t, Some (Success res)
  in
  OpamStd.Option.iter (OpamSolution.check_solution t) solution;
  t

let install t ?formula ?autoupdate ?add_to_roots
    ?(deps_only=false) ?(ignore_conflicts=false) ?(assume_built=false)
    ?(download_only=false) ?(depext_only=false) names =
  let atoms = OpamSolution.sanitize_atom_list ~permissive:true t names in
  let autoupdate_atoms = match autoupdate with
    | None -> atoms
    | Some a -> OpamSolution.sanitize_atom_list ~permissive:true t a
  in
  let t = update_dev_packages_t autoupdate_atoms t in
  install_t t atoms ?formula add_to_roots
    ~ignore_conflicts ~depext_only ~deps_only ~download_only ~assume_built

let remove_t ?ask ~autoremove ~force ?(formula=OpamFormula.Empty) atoms t =
  log "REMOVE autoremove:%b %a" autoremove
    (slog OpamFormula.string_of_atoms) atoms;

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
    let to_remove =
      if autoremove then
        let universe =
          OpamSwitchState.universe t ~requested:packages Remove
        in
        let keep =
          universe.u_base ++ t.installed_roots %% t.installed -- packages
        in
        let keep_cone =
          keep |> OpamSolver.dependencies universe
            ~build:true ~post:true ~depopts:true ~installed:true
        in
        let autoremove =
          packages ++ (t.installed -- keep_cone)
        in
        if atoms = [] then autoremove else
        (* restrict to the dependency cone of removed pkgs *)
        let remove_cone =
          packages |> OpamSolver.reverse_dependencies universe
            ~build:true ~post:true ~depopts:false ~installed:true
        in
        autoremove %%
        (remove_cone |> OpamSolver.dependencies universe
           ~build:true ~post:true ~depopts:false ~installed:true)
      else
        packages
    in
    let request =
      OpamSolver.request
        ~remove:(OpamSolution.atoms_of_packages to_remove)
        ~deprequest:(OpamFormula.to_atom_formula formula)
        ()
    in
    let print_requested =
      print_requested (OpamPackage.names_of_packages packages) formula
    in
    let t, solution =
      OpamSolution.resolve_and_apply ?ask t Remove
        ~force_remove:force
        ~requested:packages
        ~print_requested
        ~add_roots:OpamPackage.Name.Set.empty
        request
    in
    OpamSolution.check_solution t solution;
    t
  ) else if !nothing_to_do then (
    OpamConsole.msg "Nothing to do.\n";
    t
  ) else t

let remove t ~autoremove ~force ?formula names =
  let atoms =
    OpamSolution.sanitize_atom_list ~installed:true ~permissive:true t names
  in
  remove_t ~autoremove ~force ?formula atoms t

let reinstall_t t ?ask ?(force=false) ~assume_built atoms =
  log "reinstall %a" (slog OpamFormula.string_of_atoms) atoms;

  let packages = OpamFormula.packages_of_atoms t.packages atoms in

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

  let requested =
    OpamFormula.packages_of_atoms t.installed atoms in

  let t, atoms =
    if assume_built then
      assume_built_restrictions t atoms
    else t, atoms
  in

  let request =
    let criteria = if to_install = [] then `Fixup else `Default in
    OpamSolver.request ~install:atoms ~criteria ()
  in

  let t, solution =
    OpamSolution.resolve_and_apply ?ask t Reinstall
      ~reinstall:requested
      ~requested:packages
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

  let post_pin_action st was_pinned names =
    let names =
      OpamPackage.Set.Op.(st.pinned -- was_pinned)
      |> OpamPackage.names_of_packages
      |> (fun s ->
          List.fold_left
            (fun s p -> OpamPackage.Name.Set.add p s)
            s names)
      |> OpamPackage.Name.Set.elements
    in
    try
      upgrade_t
        ~strict_upgrade:false ~auto_install:true ~ask:true ~terse:true
        ~all:false
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

  let pin st name ?(edit=false) ?version ?(action=true) ?subpath ?locked target =
    try
      let pinned = st.pinned in
      let st =
        match target with
        | `Source url -> source_pin st name ?version ~edit ?subpath ?locked (Some url)
        | `Version v ->
          let st = version_pin st name v in
          if edit then OpamPinCommand.edit st name else st
        | `Source_version (srcv, version) ->
          let url =
            let nv = (OpamPackage.create name srcv) in
            match OpamPackage.Map.find_opt nv st.repos_package_index with
            | Some opam ->
              (match
                 OpamStd.Option.Op.(OpamFile.OPAM.url opam >>| OpamFile.URL.url)
               with
               | Some u -> u
               | None ->
                 OpamConsole.error_and_exit `Not_found
                   "Package %s has no url defined in its opam file description"
                   (OpamPackage.to_string nv))
            | None ->
              OpamConsole.error_and_exit `Not_found
                "Package %s has no known version %s in the repositories"
                (OpamPackage.Name.to_string name)
                (OpamPackage.Version.to_string version)
          in
          source_pin st name ~version ~edit ?locked (Some url)
        | `Dev_upstream ->
          source_pin st name ?version ~edit ?locked (Some (get_upstream st name))
        | `None -> source_pin st name ?version ~edit ?locked None
      in
      if action then (OpamConsole.msg "\n"; post_pin_action st pinned [name])
      else st
    with
    | OpamPinCommand.Aborted -> OpamStd.Sys.exit_because `Aborted
    | OpamPinCommand.Nothing_to_do -> st

  let url_pins st ?edit ?(action=true) ?locked ?(pre=fun _ -> ()) pins =
    let names = List.map (fun p -> p.pinned_name) pins in
    (match names with
     | _::_::_ ->
       if not (OpamConsole.confirm
                 "This will pin the following packages: %s. Continue?"
                 (OpamStd.List.concat_map ", "
                    OpamPackage.Name.to_string names))
       then
         OpamStd.Sys.exit_because `Aborted
     | _ -> ());
    let pins = OpamPinCommand.fetch_all_pins st pins in
    let pinned = st.pinned in
    let st =
      List.fold_left (fun st pin ->
          pre pin;
          try
            OpamPinCommand.source_pin st pin.pinned_name
              ?version:pin.pinned_version
              ?opam:pin.pinned_opam
              ?subpath:pin.pinned_subpath
              ?edit ?locked (Some pin.pinned_url)
          with
          | OpamPinCommand.Aborted -> OpamStd.Sys.exit_because `Aborted
          | OpamPinCommand.Nothing_to_do -> st)
        st pins
    in
    if action then
      (OpamConsole.msg "\n";
       post_pin_action st pinned names)
    else st

  let edit st ?(action=true) ?version ?locked name =
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
          try source_pin st name ~edit:true ?version ?opam ?locked target
          with OpamPinCommand.Aborted -> OpamStd.Sys.exit_because `Aborted
             | OpamPinCommand.Nothing_to_do -> st
        else
          OpamStd.Sys.exit_because `Aborted
      | None ->
        OpamConsole.error_and_exit `Not_found
          "Package is not pinned, and no existing version was supplied."
    in
    if action then post_pin_action st pinned [name]
    else st

  let unpin st ?(action=true) names =
    let pinned_before = st.pinned in
    let st = unpin st names in
    let installed_unpinned = (pinned_before -- st.pinned) %% st.installed in
    if action && not (OpamPackage.Set.is_empty installed_unpinned) then
      let all =
        OpamPackage.Set.fold (fun nv acc -> (nv.name, None) :: acc)
          installed_unpinned []
      in
      let requested =
        OpamPackage.packages_of_names pinned_before
          (OpamPackage.Name.Set.of_list names)
      in
      let st, solution =
        OpamSolution.resolve_and_apply st Upgrade
          ~requested
          (OpamSolver.request ~all ())
      in
      OpamSolution.check_solution st solution;
      st
    else st

  let list = list
end

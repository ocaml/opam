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
open OpamStateTypes
open OpamStd.Op
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "CLIENT" fmt
let slog = OpamConsole.slog

let with_switch_backup _command f =
  let t = OpamSwitchState.load_full_compat "client"
      (OpamStateConfig.get_switch ()) in
  let file = OpamPath.Switch.backup t.switch_global.root t.switch in
  OpamFilename.mkdir (OpamPath.Switch.backup_dir t.switch_global.root t.switch);
  OpamFile.SwitchSelections.write file (OpamSwitchState.selections t);
  try
    f t;
    (* Note: We might want to keep it even if successful ? *)
    OpamFilename.remove (OpamFile.filename file)
  with
  | OpamStd.Sys.Exit 0 as e -> raise e
  | err ->
    OpamStd.Exn.register_backtrace err;
    let t1 = OpamSwitchState.load_full_compat "switch-backup-err"
        (OpamStateConfig.get_switch ()) in
    if OpamPackage.Set.equal t.installed t1.installed &&
       OpamPackage.Set.equal t.installed_roots t1.installed_roots then
      OpamFilename.remove (OpamFile.filename file)
    else
      (prerr_string
         (OpamStd.Format.reformat
            (Printf.sprintf
               "\nThe former state can be restored with:\n    \
                %s switch import %S\n%!"
               Sys.argv.(0) (OpamFile.to_string file))));
    raise err

module API = struct

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
    let universe = OpamSwitchState.universe t (Reinstall OpamPackage.Set.empty) in
    (* Basic definition of orphan packages *)
    let orphans = t.installed -- Lazy.force t.available_packages in
    (* Restriction to the request-related packages *)
    let orphans = match changes with
      | None -> orphans
      | Some ch ->
        if OpamPackage.Set.is_empty orphans then orphans else
        let recompile_cone =
          OpamPackage.Set.of_list @@
          OpamSolver.reverse_dependencies
            ~depopts:true ~installed:true ~unavailable:true ~build:true
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
                ~depopts:false ~installed:false ~unavailable:true ~build:true
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
    log "Orphans: full %a, versions %a"
      (slog @@ OpamPackage.Name.Set.to_string @* OpamPackage.names_of_packages)
      full_orphans
      (slog OpamPackage.Set.to_string) orphan_versions;
    t, full_orphans, orphan_versions

  (* The internal "solver" needs some rewrites of the requests, to make them
     more explicit. This has no effect when using the external solver. *)
  let preprocessed_request t full_orphans orphan_versions
    ?wish_install ?wish_remove ?wish_upgrade ?criteria () =
    let request =
      OpamSolver.request ?install:wish_install ?remove:wish_remove
        ?upgrade:wish_upgrade ?criteria ()
    in
    if OpamCudf.external_solver_available () then request else
    let { wish_install; wish_remove; wish_upgrade; criteria; _ } = request in
    (* Convert install to upgrade when necessary, request roots installed *)
    let eqnames, neqnames =
      List.partition (function (_,Some(`Eq,_)) -> true | _ -> false)
        wish_install in
    let add_wish_install =
      List.rev_append eqnames
        (OpamSolution.atoms_of_packages
           (t.installed_roots %% Lazy.force t.available_packages)) in
    let base_packages =
      OpamSolution.eq_atoms_of_packages t.compiler_packages in
    let base_packages =
      List.map (fun atom ->
          try OpamPackage.Set.find (OpamFormula.check atom) t.installed
              |> OpamSolution.eq_atom_of_package
          with Not_found -> atom)
        base_packages in
    let wish_install = List.rev_append add_wish_install wish_install in
    let wish_install = List.rev_append base_packages wish_install in
    let uninstalled_eqnames =
      List.filter (fun (name,_) -> not (OpamSwitchState.is_name_installed t name))
        eqnames in
    let wish_upgrade = List.rev_append neqnames wish_upgrade in
    let wish_upgrade = List.rev_append uninstalled_eqnames wish_upgrade in
    (* Remove orphans *)
    let wish_remove =
      OpamSolution.atoms_of_packages full_orphans @
      OpamSolution.eq_atoms_of_packages orphan_versions @
      wish_remove in
    let available =
      Lazy.force t.available_packages -- orphan_versions -- full_orphans in
    let still_available ?(up=false) (name,_ as atom) =
      let installed =
        if up then
          try Some (OpamPackage.version @@ OpamPackage.Set.choose_one @@
                    OpamPackage.packages_of_name t.installed name)
          with Not_found -> None
        else None in
       OpamPackage.Set.exists
        (fun p -> OpamFormula.check atom p &&
                  match installed with Some i -> OpamPackage.version p >= i
                                     | None -> true)
         available in
    let upgradeable, non_upgradeable =
      List.partition (still_available ~up:true) wish_upgrade in
    let wish_install =
      List.filter (still_available ~up:false)
        (non_upgradeable @ wish_install) in
    let wish_upgrade =
      List.filter (still_available ~up:true) upgradeable in
    let nrequest = { wish_install; wish_remove; wish_upgrade;
                     criteria; extra_attributes = [] } in
    log "Preprocess request: %a => %a"
      (slog OpamSolver.string_of_request) request
      (slog OpamSolver.string_of_request) nrequest;
    nrequest

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
    let to_update =
      List.fold_left (fun to_update (name,_) ->
          match OpamPackage.Name.Map.find_opt name t.pinned with
          | Some (v, Source _) ->
            OpamPackage.Set.add (OpamPackage.create name v) to_update
          | None | Some (_, Version _) -> to_update)
        OpamPackage.Set.empty atoms
    in
    if OpamPackage.Set.is_empty to_update then t else (
      OpamConsole.header_msg "Synchronising pinned packages";
      try
        let updated = OpamUpdate.dev_packages t to_update in
        if OpamPackage.Set.is_empty updated then t
        else OpamSwitchState.load_full_compat "reload-dev-package-updated" t.switch
      with e ->
        OpamStd.Exn.fatal e;
        t
    )

  let compute_upgrade_t atoms t =
    let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
    if atoms = [] then
      let to_reinstall = t.reinstall %% t.installed in
      let t, full_orphans, orphan_versions = orphans ~transitive:true t in
      let to_upgrade = t.installed -- full_orphans -- orphan_versions in
      let to_install = t.installed -- full_orphans in
      let requested = OpamPackage.Name.Set.empty in
      let action = Upgrade to_reinstall in
      requested,
      action,
      OpamSolution.resolve t action
        ~orphans:(full_orphans ++ orphan_versions)
        (preprocessed_request t full_orphans orphan_versions
           ~wish_install:(OpamSolution.atoms_of_packages to_install)
           ~wish_upgrade:(OpamSolution.atoms_of_packages to_upgrade)
           ~criteria:`Upgrade ())
    else
    let atoms =
      List.map (function
          | (n,None) ->
            (* force strict update for unchanged, non dev or pinned packages
               (strict update makes no sense for pinned packages which have
               a fixed version) *)
            (try
               let nv = OpamSwitchState.find_installed_package_by_name t n in
               if OpamSwitchState.is_dev_package t nv ||
                  (OpamPackage.Name.Map.mem n t.pinned) ||
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
    let to_upgrade, not_installed =
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
      if OpamConsole.confirm "%s %s not installed. Install %s ?"
          (OpamStd.Format.pretty_list
             (List.rev_map OpamFormula.short_string_of_atom not_installed))
          (match not_installed with [_] -> "is" | _ -> "are")
          (match not_installed with [_] -> "it" | _ -> "them")
      then not_installed
      else []
    in
    let changes = to_upgrade ++ OpamSwitchState.packages_of_atoms t to_install in
    let to_reinstall =
      (* Only treat related reinstalls (i.e. the ones belonging to the
         dependency cone of packages specified to update) *)
      let universe = OpamSwitchState.universe t (Upgrade OpamPackage.Set.empty) in
      let all_deps =
        OpamPackage.names_of_packages @@ OpamPackage.Set.of_list @@
        OpamSolver.dependencies ~depopts:true ~build:false ~installed:true
          universe changes
      in
      OpamPackage.Set.filter
        (fun nv -> OpamPackage.Name.Set.mem nv.name all_deps)
        t.reinstall
    in
    let t, full_orphans, orphan_versions = orphans ~changes t in
    let to_remove = to_upgrade %% full_orphans in
    let to_upgrade = to_upgrade -- full_orphans in
    let requested = names in
    let action = Upgrade to_reinstall in
    let upgrade_atoms =
      (* packages corresponds to the currently installed versions.
         Not what we are interested in, recover the original atom constraints *)
      List.map (fun nv ->
          let name = nv.name in
          try name, List.assoc name atoms
          with Not_found -> name, None)
        (OpamPackage.Set.elements to_upgrade) in
    requested,
    action,
    OpamSolution.resolve t action
      ~orphans:(full_orphans ++ orphan_versions)
      (preprocessed_request t full_orphans orphan_versions
         ~wish_install:to_install
         ~wish_remove:(OpamSolution.atoms_of_packages to_remove)
         ~wish_upgrade:upgrade_atoms
         ())

  let upgrade_t ?ask atoms t =
    log "UPGRADE %a"
      (slog @@ function [] -> "<all>" | a -> OpamFormula.string_of_atoms a)
      atoms;
    match compute_upgrade_t atoms t with
    | requested, _action, Conflicts cs ->
      log "conflict!";
      if not (OpamPackage.Name.Set.is_empty requested) then
        (OpamConsole.msg "%s"
           (OpamCudf.string_of_conflict (OpamSwitchState.unavailable_reason t) cs);
         OpamStd.Sys.exit 3);
      let reasons, chains, cycles =
        OpamCudf.strings_of_conflict (OpamSwitchState.unavailable_reason t) cs in
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
            "The following dependencies are in cause:\n%s"
            (OpamStd.Format.itemize (fun x -> x) chains);
        if OpamCudf.external_solver_available () then
          OpamConsole.errmsg
            "\nYou may run \"opam upgrade --fixup\" to let OPAM fix the \
             current state.\n"
      end;
      OpamStd.Sys.exit 3
    | requested, action, Success solution ->
      let result = OpamSolution.apply ?ask t action ~requested solution in
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
           if (OpamConsole.verbose ()) && not (OpamPackage.Set.is_empty unav) then
             OpamConsole.formatted_msg
               "%s.\n\
                The following newer versions couldn't be installed:\n%s"
               hdmsg
               (OpamStd.Format.itemize (fun p ->
                    OpamSwitchState.unavailable_reason t
                      (OpamSolution.eq_atom
                         (OpamPackage.name p) (OpamPackage.version p)))
                   (OpamPackage.Set.elements unav))
           else
             OpamConsole.formatted_msg
               "%s (run with --verbose to show unavailable upgrades).\n" hdmsg;
           if not (OpamPackage.Set.is_empty unopt) then
             (OpamConsole.formatted_msg
                "The following would require downgrades or uninstalls, but \
                 you may upgrade them explicitly:\n%s"
                (OpamStd.Format.itemize OpamPackage.to_string
                   (OpamPackage.Set.elements unopt)));
          )
      );
      OpamSolution.check_solution t result

  let upgrade names =
    with_switch_backup "upgrade" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list t names in
    let t = update_dev_packages_t atoms t in
    upgrade_t atoms t

  let fixup_t t =
    log "FIXUP";
    if not (OpamCudf.external_solver_available ()) then
      (OpamConsole.formatted_msg
         "Sorry, \"--fixup\" is not available without an external solver. \
          You'll have to select the packages to change or remove by hand, \
          or install aspcud or another solver on your system.\n";
       OpamStd.Sys.exit 1)
    else
    let t, full_orphans, orphan_versions = orphans ~transitive:true t in
    let action = Upgrade OpamPackage.Set.empty in
    let all_orphans = full_orphans ++ orphan_versions in
    let resolve pkgs =
      pkgs,
      OpamSolution.resolve t action ~orphans:all_orphans
        (OpamSolver.request
           ~install:(OpamSolution.atoms_of_packages pkgs)
           ~criteria:`Fixup
           ())
    in
    let is_success = function
      | _, Success _ -> true
      | _, Conflicts cs ->
        log "conflict: %a"
          (slog (OpamCudf.string_of_conflict @@ OpamSwitchState.unavailable_reason t))
          cs;
        false
    in
    let requested, solution =
      let s =
        log "fixup-1/ keep installed packages with orphaned versions and roots";
        resolve (t.installed_roots -- full_orphans ++ orphan_versions)
      in
      if is_success s then s else
      let s =
        log "fixup-2/ keep just roots";
        resolve (t.installed_roots -- full_orphans)
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
    let result = match solution with
      | Conflicts cs -> (* ouch... *)
        OpamConsole.msg "%s"
          (OpamCudf.string_of_conflict (OpamSwitchState.unavailable_reason t) cs);
        No_solution
      | Success solution ->
        let _, req_rm, _ = orphans ~transitive:false t in
        OpamSolution.apply ~ask:true t action
          ~requested:(OpamPackage.names_of_packages (requested ++ req_rm))
          solution
    in
    OpamSolution.check_solution t result

  let fixup () = with_switch_backup "fixup" fixup_t

  let update ~repos_only ~dev_only ?(no_stats=false) names =
    let gt = OpamGlobalState.load () in
    let rt = OpamRepositoryState.load gt in
    log "UPDATE %a" (slog @@ String.concat ", ") names;
    let repositories =
      if dev_only then OpamRepositoryName.Map.empty
      else if names = [] then rt.repositories
      else
        OpamRepositoryName.(
          Map.filter (fun r _ -> List.mem (to_string r) names) rt.repositories
        )
    in
    let packages =
      if repos_only then OpamPackage.Set.empty
      else if names = [] then OpamGlobalState.all_installed gt
      else
        OpamPackage.Set.filter (fun nv ->
            let name = OpamPackage.Name.to_string nv.name in
            let pkg = OpamPackage.to_string nv in
            List.exists (fun s -> s = name || s = pkg) names
          ) (OpamGlobalState.all_installed gt)
    in
    let remaining =
      List.filter (fun n -> not (
          OpamRepositoryName.Map.mem
            (OpamRepositoryName.of_string n)
            repositories ||
          (try OpamPackage.has_name packages (OpamPackage.Name.of_string n)
           with Failure _ -> false) ||
          (try OpamPackage.Set.mem (OpamPackage.of_string n) packages
           with Failure _ -> false)
        )) names
    in

    if remaining <> [] then
      OpamConsole.error_and_exit
        "Unknown repositories or installed packages: %s"
        (String.concat ", " remaining);

    let rt =
      if OpamRepositoryName.Map.is_empty repositories then rt
      else
      let repos = OpamRepositoryName.Map.values repositories in
      OpamConsole.header_msg "Updating package repositories";
      let command repo =
        OpamProcess.Job.ignore_errors ~default:(fun t -> t)
          ~message:("Could not update repository " ^
                    OpamRepositoryName.to_string repo.repo_name) @@
        OpamRepositoryCommand.update gt repo
      in
      OpamRepositoryState.Cache.remove ();
      let rt =
        OpamParallel.reduce
          ~jobs:OpamStateConfig.(!r.dl_jobs)
          ~command
          ~merge:(fun f1 f2 x -> f1 (f2 x))
          ~nil:(fun x -> x)
          repos
          rt
      in
      OpamRepositoryCommand.update_package_index rt
(*
      let changed =
        OpamRepositoryCommand.check_package_upstream_changes rt st
      in
        OpamRepositoryCommand.fix_package_descriptions rt
          ~verbose:(OpamCoreConfig.(!r.verbose_level) >= 2)
      in
*)

      (* !X todo
         - cleanup obsolete archives of uninstalled packages ?
         - handle sync_archives
      *)
    in

    if OpamStateConfig.(!r.current_switch) = None then () else

    let st = OpamSwitchState.load gt rt (OpamStateConfig.get_switch ()) in

    let dev_packages, nondev_packages =
      if repos_only then OpamPackage.Set.empty, OpamPackage.Set.empty
      else
        OpamPackage.Set.partition (OpamSwitchState.is_dev_package st) packages
    in

    if names <> [] && not (OpamPackage.Set.is_empty nondev_packages) then
      OpamConsole.warning
        "The following are not development packages (no dynamic or version \
         controlled upstream) and can't be updated individually. What you want \
         is probably to update your repositories: %s"
        (OpamPackage.Set.to_string nondev_packages);

    let st =
      if OpamPackage.Set.is_empty dev_packages then st
      else
      let () = OpamConsole.header_msg "Synchronizing development packages" in
      let updates = OpamUpdate.dev_packages st dev_packages in
      if OpamStateConfig.(!r.json_out <> None) then
        OpamJson.append "dev-packages-updates"
          (OpamPackage.Set.to_json updates);
      (* !X update_dev_packages should provide an updated switch state
         already *)
      OpamSwitchState.load gt rt  (OpamStateConfig.get_switch ())
    in

    log "dry-upgrade";
    let broken_state_message ~need_fixup conflicts =
      let reasons, chains, _cycles =
        OpamCudf.strings_of_conflict (OpamSwitchState.unavailable_reason st)
          conflicts
      in
      OpamConsole.warning
        "A conflict was detected in your installation. \
         This can be caused by updated constraints or conflicts in your \
         installed packages:\n%s"
        (OpamStd.Format.itemize (fun x -> x) reasons);
      if chains <> [] then (
        OpamConsole.formatted_msg "The following dependencies are in cause:\n";
        List.iter (OpamConsole.msg "  - %s\n") chains);
      OpamConsole.formatted_msg
        "\nYou should run \"opam upgrade%s\" to resolve the situation.\n"
        (if need_fixup && OpamCudf.external_solver_available () then " --fixup"
         else "")
    in
    if no_stats then () else
    let universe =
      OpamSwitchState.universe st (Upgrade OpamPackage.Set.empty)
    in
    match OpamSolver.check_for_conflicts universe with
    | Some cs ->
      let need_fixup = match compute_upgrade_t [] st with
        | _, _, Success _ -> false
        | _, _, Conflicts _ -> true
      in
      broken_state_message ~need_fixup cs
    | None ->
      match compute_upgrade_t [] st with
      | _, _, Success upgrade ->
        let stats = OpamSolver.stats upgrade in
        if OpamSolution.sum stats > 0 then
          OpamConsole.msg
            "\nUpdates available for %s, apply them with 'opam upgrade':\n\
             ===== %s =====\n"
            (OpamSwitch.to_string st.switch)
            (OpamSolver.string_of_stats stats)
      | _, _, Conflicts cs ->
        log "State isn't broken but upgrade fails: something might be wrong.";
        broken_state_message ~need_fixup:true cs

  let init repo shell dot_profile update_config =
    log "INIT %a" (slog OpamRepositoryBackend.to_string) repo;
    let root = OpamStateConfig.(!r.root_dir) in
    let config_f = OpamPath.config root in
    let root_empty =
      not (OpamFilename.exists_dir root) || OpamFilename.dir_is_empty root in

    if OpamFile.exists config_f then (
      OpamConsole.msg "OPAM has already been initialized.";
    ) else (
      if not root_empty then (
        OpamConsole.warning "%s exists and is not empty"
          (OpamFilename.Dir.to_string root);
        if not (OpamConsole.confirm "Proceed ?") then OpamStd.Sys.exit 1);
      try

        (* Check for the external dependencies *)
        let check_external_dep name =
          OpamSystem.command_exists name
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
        let external_solvers = ["aspcud"; "packup"; "mccs"] in
        if not (List.exists check_external_dep external_solvers) then
          OpamConsole.warning
            "No external solver found, one of %s is recommended (see \
             http://opam.ocaml.org/doc/Install.html#ExternalSolvers for \
             details)"
            (OpamStd.Format.pretty_list ~last:"or"
               (List.map (OpamConsole.colorise `bold) external_solvers));
        let advised_deps =
          [OpamStateConfig.(Lazy.force !r.makecmd); "m4"; "cc"]
        in
        (match List.filter (not @* check_external_dep) advised_deps with
         | [] -> ()
         | missing ->
           OpamConsole.warning
             "Recommended dependencies -- \
              most packages rely on these:\n%s"
             (OpamStd.Format.itemize (OpamConsole.colorise `bold) missing));
        let fetch_cmd_user =
          let open OpamStd.Option.Op in
          match
            OpamStd.Env.getopt "OPAMCURL",
            OpamStd.Env.getopt "OPAMFETCH" >>| fun s ->
            OpamStd.String.split s ' '
          with
          | Some cmd, _ | _, Some (cmd::_) -> check_external_dep cmd
          | _ -> false
        in
        let required_deps =
          ["curl or wget",
           fetch_cmd_user ||
           check_external_dep "curl" ||
           check_external_dep "wget";
           "patch", check_external_dep "patch";
           "tar", check_external_dep "tar";
           "unzip", check_external_dep "unzip" ]
        in
        (match List.filter (not @* snd) required_deps with
         | [] -> ()
         | missing ->
           OpamConsole.error_and_exit
             "Missing dependencies -- \
              the following commands are required for OPAM to operate:\n%s"
             (OpamStd.Format.itemize (OpamConsole.colorise `bold @* fst) missing));

        (*
        (* Create (possibly empty) configuration files *)
        let switch =
          if compiler = OpamCompiler.system then
            OpamSwitch.system
          else
            OpamSwitch.of_string (OpamCompiler.to_string compiler) in

        (* Create ~/.opam/compilers/system.comp *)
        (* !X port to the new system !!
           OpamState.create_system_compiler_description root; *)
        *)
        (* Create ~/.opam/config *)
        let config =
          OpamFile.Config.create [] None [repo.repo_name]
            OpamStateConfig.(Lazy.force default.jobs)
            OpamStateConfig.(default.dl_jobs)
        in
        OpamStateConfig.write root config;

        (* Create ~/.opam/aliases *)
        (* OpamFile.Aliases.write *)
        (*   (OpamPath.aliases root) *)
        (*   (OpamSwitch.Map.singleton switch compiler); *)

        (* Init repository *)
        (*
        OpamFile.Package_index.write (OpamPath.package_index root)
          OpamPackage.Map.empty;
        OpamFile.Compiler_index.write (OpamPath.compiler_index root)
          OpamCompiler.Map.empty;
        *)
        OpamFile.Repo_config.write (OpamRepositoryPath.config repo) repo;
        OpamProcess.Job.run
          OpamProcess.Job.Op.(OpamRepository.init repo @@+ fun () ->
                              OpamRepository.update repo);
        (* OpamSwitchAction.install_global_config root switch *)
        (*   (OpamSwitchAction.gen_global_config root switch); *)

        (* Load the partial state, and update the global state *)
        log "updating repository state";
        let gt = OpamGlobalState.load () in
        let rt = OpamRepositoryState.load ~save_cache:false gt in
        OpamConsole.header_msg "Fetching repository information";
        ignore (OpamProcess.Job.run (OpamRepositoryCommand.update gt repo) rt)
        (*
        (* Load the partial state, and install the new compiler if needed *)
        log "updating package state";
        let quiet = (compiler = OpamCompiler.system) in
        OpamSwitchAction.install_compiler gt ~quiet switch compiler;

        (* Finally, load the complete state and install the compiler packages *)
        log "installing compiler packages";
        let st = OpamSwitchState.load gt rt switch in
        OpamSwitchCommand.install_packages st
           *)
      with e ->
        OpamStd.Exn.register_backtrace e;
        OpamConsole.error "Initialisation failed";
        OpamConsole.errmsg "%s\n" (Printexc.to_string e);
        if not (OpamConsole.debug ()) && root_empty then
          OpamFilename.rmdir root;
        raise e);
    let gt = OpamGlobalState.load () in
    let updated = match update_config with
      | `no  -> false
      | `ask -> OpamEnv.setup_interactive root ~dot_profile shell
      | `yes ->
        OpamEnv.update_user_setup root ~ocamlinit:true ~dot_profile shell;
        OpamEnv.write_static_init_scripts root ~switch_eval:true ~completion:true;
        true
    in
    if not updated then
      OpamEnv.print_env_warning_at_init gt ~ocamlinit:true ~dot_profile shell


  (* Checks a request for [atoms] for conflicts with the orphan packages *)
  let check_conflicts t atoms =
    let changes = OpamSwitchState.packages_of_atoms t atoms in
    let t, full_orphans, orphan_versions = orphans ~changes t in
    (* packages which still have local data are OK for install/reinstall *)
    let has_no_local_data nv =
      not (OpamFile.exists
             (OpamPath.Switch.installed_opam t.switch_global.root t.switch nv)) in
    let full_orphans, full_orphans_with_local_data =
      OpamPackage.Set.partition has_no_local_data
        full_orphans in
    let orphan_versions, orphan_versions_with_local_data =
      OpamPackage.Set.partition has_no_local_data
        orphan_versions in
    let available = lazy (t.packages -- full_orphans -- orphan_versions) in
    let orphans = full_orphans ++ orphan_versions in
    let conflict_atoms =
      List.filter
        (fun (name,_ as a) ->
           not (OpamPackage.Name.Map.mem name t.pinned) &&
           OpamPackage.Set.exists (OpamFormula.check a) orphans && (*optim*)
           not (OpamPackage.Set.exists (OpamFormula.check a) (* real check *)
                  (Lazy.force available)))
        atoms in
    if conflict_atoms <> [] then
      OpamConsole.error_and_exit
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

  let install_t ?ask atoms add_to_roots ~deps_only ~upgrade t =
    log "INSTALL %a" (slog OpamFormula.string_of_atoms) atoms;
    let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in

    let t, full_orphans, orphan_versions = check_conflicts t atoms in

    let pkg_skip, pkg_new =
      get_installed_atoms t atoms in

    (* Add the packages to the list of package roots and display a
       warning for already installed package roots. *)
    let current_roots = t.installed_roots in
    let t =
      List.fold_left (fun t nv ->
          if OpamPackage.Set.mem nv t.installed then
            match add_to_roots with
            | None ->
              if not upgrade then
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
        )  t pkg_skip in
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
        OpamPackage.Name.Set.fold (fun name avail ->
            if OpamPackage.has_name available_packages name then avail
            else avail ++ OpamPackage.packages_of_name t.packages name)
          names available_packages
      else
        (OpamSolution.check_availability t available_packages atoms;
         available_packages) in
    let t = {t with available_packages = lazy available_packages} in

    let wish_upgrade =
      if upgrade then List.filter (fun at -> not (List.mem at pkg_new)) atoms
      else [] in

    if pkg_new <> [] || wish_upgrade <> [] then (

      let request =
        preprocessed_request t full_orphans orphan_versions
          ~wish_install:atoms ~wish_upgrade ();
      in
      let action =
        if wish_upgrade <> [] then Upgrade (OpamPackage.Set.of_list pkg_skip)
        (* Fixme: the above won't properly handle setting as a root *)
        else match add_to_roots, deps_only with
          | Some false, _ | None, true ->
            Install OpamPackage.Name.Set.empty
          | _ -> Install names in
      let solution =
        OpamSolution.resolve t action
          ~orphans:(full_orphans ++ orphan_versions)
          request in
      let solution = match solution with
        | Conflicts cs ->
          log "conflict!";
          OpamConsole.msg "%s"
            (OpamCudf.string_of_conflict (OpamSwitchState.unavailable_reason t) cs);
          No_solution
        | Success solution ->
          let solution =
            if deps_only then
              OpamSolver.filter_solution (fun nv ->
                  not (OpamPackage.Name.Set.mem nv.name names))
                solution
            else solution in
          OpamSolution.apply ?ask t action ~requested:names solution in
      OpamSolution.check_solution t solution
    )

  let install names add_to_roots ~deps_only ~upgrade =
    with_switch_backup "install" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list ~permissive:true t names in
    let t = update_dev_packages_t atoms t in
    install_t atoms add_to_roots ~deps_only ~upgrade t

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
      let universe = OpamSwitchState.universe t Remove in
      let to_remove =
        OpamPackage.Set.of_list
          (OpamSolver.reverse_dependencies ~build:true
             ~depopts:false ~installed:true universe packages) in
      let to_keep =
        (if autoremove then t.installed_roots else t.installed)
        -- to_remove -- full_orphans -- orphan_versions
      in
      let to_keep =
        OpamPackage.Set.of_list
          (OpamSolver.dependencies ~build:true
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
               (OpamSolver.dependencies ~build:true
                  ~depopts:true ~installed:true universe to_remove))
        else to_remove in
      let solution =
        OpamSolution.resolve_and_apply ?ask t Remove ~requested
          ~orphans:(full_orphans ++ orphan_versions)
          (OpamSolver.request
             ~install:(OpamSolution.eq_atoms_of_packages to_keep)
             ~remove:(OpamSolution.atoms_of_packages to_remove)
             ())
      in
      OpamSolution.check_solution t solution
    ) else if !nothing_to_do then
      OpamConsole.msg "Nothing to do.\n"

  let remove ~autoremove ~force names =
    with_switch_backup "remove" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list t names in
    remove_t ~autoremove ~force atoms t

  let reinstall_t ?ask ?(force=false) atoms t =
    log "reinstall %a" (slog OpamFormula.string_of_atoms) atoms;

    let reinstall, not_installed =
      get_installed_atoms t atoms in
    let to_install =
      if not_installed <> [] then
        if
          force ||
          OpamConsole.confirm "%s %s not installed. Install %s ?"
            (OpamStd.Format.pretty_list
               (List.rev_map OpamFormula.short_string_of_atom not_installed))
            (match not_installed with [_] -> "is" | _ -> "are")
            (match not_installed with [_] -> "it" | _ -> "them")
        then not_installed
        else OpamStd.Sys.exit 1
      else []
    in

    let reinstall = OpamPackage.Set.of_list reinstall in

    let atoms =
      to_install @ OpamSolution.eq_atoms_of_packages reinstall in

    let t, full_orphans, orphan_versions = check_conflicts t atoms in

    let requested =
      OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in

    let request =
      preprocessed_request t full_orphans orphan_versions
        ~wish_install:atoms
        ~criteria:`Fixup
        ()
    in

    let solution =
      OpamSolution.resolve_and_apply ?ask t (Reinstall reinstall) ~requested
        ~orphans:(full_orphans ++ orphan_versions)
        request in

    OpamSolution.check_solution t solution

  let reinstall names =
    with_switch_backup "reinstall" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list t names in
    let t = update_dev_packages_t atoms t in
    reinstall_t atoms t

  module PIN = struct
    open OpamPinCommand

    let post_pin_action t name =
      let nv = try Some (OpamPinned.package t name) with Not_found -> None in
      try match nv with
      | Some nv ->
        let v = nv.version in
        OpamConsole.msg "%s needs to be %sinstalled.\n"
          (OpamPackage.Name.to_string name)
          (if OpamPackage.has_name t.installed name then "re" else "");
        if OpamPackage.Set.mem nv t.installed then
          reinstall_t ~ask:true [name, Some (`Eq,v)] t (* same version *)
        else
          install_t ~ask:true [name, Some (`Eq,v)] None
            ~deps_only:false ~upgrade:false t
          (* != version or new *)
      | None ->
        try
          let nv = OpamPackage.max_version t.installed name in
          if OpamPackage.has_name (Lazy.force t.available_packages) name then
            (OpamConsole.msg "%s needs to be reinstalled.\n"
               (OpamPackage.Name.to_string name);
             if OpamPackage.Set.mem nv (Lazy.force t.available_packages)
             then reinstall_t ~ask:true [name, Some (`Eq, nv.version)] t
             else upgrade_t ~ask:true [name, Some (`Neq, nv.version)] t)
          else
            (OpamConsole.msg "%s needs to be removed.\n" (OpamPackage.to_string nv);
             (* Package no longer available *)
             remove_t ~ask:true ~autoremove:false ~force:false [name, None] t)
        with Not_found -> ()
      with e ->
        OpamConsole.note
          "Pinning command successful, but your installed packages \
           may be out of sync.";
        raise e

    let get_upstream t name =
      try
        let nv = OpamSwitchState.get_package t name in
        match OpamSwitchState.opam_opt t nv with
        | None -> raise Not_found
        | Some o -> match OpamFile.OPAM.dev_repo o with
          | None -> raise Not_found
          | Some pin -> Source pin
      with Not_found ->
        OpamConsole.error_and_exit
          "\"dev-repo\" field missing in %s metadata, you'll need to specify \
           the pinning location"
          (OpamPackage.Name.to_string name)

    let pin name ?(edit=false) ?version ?(action=true) pin_option_opt =
      let pin_option = match pin_option_opt with
        | Some o -> o
        | None ->
          let t = OpamSwitchState.load_full_compat "pin-get-upstream"
              (OpamStateConfig.get_switch ()) in
          get_upstream t name
      in
      let needs_reinstall = pin name ?version pin_option in
      with_switch_backup "pin-reinstall" @@ fun t ->
      OpamConsole.msg "\n";
      let updated =
        OpamProcess.Job.run
          (OpamUpdate.pinned_package t ?fixed_version:version name)
      in
      (*
      if not updated then
        (ignore (unpin t.switch_global ~state:t [name]);
         OpamStd.Sys.exit 1);
      *)
      OpamConsole.msg "\n";
      let opam_f = OpamPath.Switch.Overlay.opam t.switch_global.root t.switch name in
      let empty_opam = OpamFile.OPAM.(
          empty = with_name_opt (with_version_opt (read opam_f) None) None
        ) in
      let needs_reinstall2 =
        if edit || empty_opam then
          try OpamPinCommand.edit t name
          with Not_found ->
            (OpamConsole.error "No valid metadata available.";
             ignore (unpin t.switch_global ~state:t [name]);
             OpamStd.Sys.exit 1)
        else None
      in
      if action then
        let t = OpamSwitchState.load_full_compat "pin-reinstall-2" t.switch in
        if not (OpamPackage.has_name t.installed name) ||
           needs_reinstall <> None ||
           needs_reinstall2 <> None
        then post_pin_action t name

    let edit ?(action=true) name =
      with_switch_backup "pin-edit" @@ fun t ->
      match edit t name with
      | None -> ()
      | Some true ->
        if action then post_pin_action t name
      | Some false ->
        (* Version changed: reload the state to ensure consistency *)
        let t = OpamSwitchState.load_full_compat "pin-edit-2" t.switch in
        if action then post_pin_action t name

    let unpin ?(action=true) names =
      let gt = OpamGlobalState.load () in
      let reinstall = unpin gt names in
      if action && reinstall <> [] then
        with_switch_backup "pin-reinstall" @@ fun t ->
        let t,atoms =
          List.fold_left (fun (t,atoms) name ->
              try
                let nv = OpamPackage.max_version t.installed name in
                let avail = Lazy.force t.available_packages in
                if OpamPackage.Set.mem nv avail then
                  {t with reinstall = OpamPackage.Set.add nv t.reinstall},
                  (name, Some (`Eq, nv.version))::atoms
                else
                  t, (name, None)::atoms
              with Not_found -> t, atoms
            )
            (t,[]) names
        in
        upgrade_t ~ask:true atoms t

    let list = list
  end

  module REPOSITORY = OpamRepositoryCommand
  module CONFIG     = OpamConfigCommand
  module SWITCH     = OpamSwitchCommand
  module LIST       = OpamListCommand

end

(* !X Reimplement with finer grain and pass states. Functions kept because they
   still do provide some information *)
let read_lock f = f ()
  (* OpamState.check (Read_lock f) *)

let switch_lock f = f ()
  (* OpamState.check *)
  (*   (Switch_lock ((fun () -> (OpamStateConfig.get_switch ())), f)) *)

let global_lock f = f ()
  (* OpamState.check (Global_lock f) *)

let global_then_switch_lock f =
  let _switch, g = f () in
  g ()
  (* OpamState.check (Global_with_switch_cont_lock f) *)

(** We protect each main functions with a lock depending on its access
    on some read/write data. *)

module SafeAPI = struct

  let init = API.init

  let install names add_to_roots ~deps_only ~upgrade =
    switch_lock (fun () -> API.install names add_to_roots ~deps_only ~upgrade)

  let reinstall names =
    switch_lock (fun () -> API.reinstall names)

  let upgrade names =
    switch_lock (fun () -> API.upgrade names)

  let fixup () =
    switch_lock API.fixup

  let remove ~autoremove ~force names =
    switch_lock (fun () -> API.remove ~autoremove ~force names)

  let update ~repos_only ~dev_only ?no_stats repos =
    global_lock (fun () -> API.update ~repos_only ~dev_only ?no_stats repos)

  module CONFIG = struct

    let env ~csh ~sexp ~fish ~inplace_path =
      API.CONFIG.env ~csh ~sexp ~fish ~inplace_path

    let setup
        ?dot_profile ~ocamlinit ~switch_eval ~completion ~shell
        ~user ~global =
      global_lock (fun () -> API.CONFIG.setup
                      ?dot_profile ~ocamlinit ~switch_eval ~completion ~shell
                      ~user ~global)

    let setup_list shell dot_profile =
      read_lock (fun () -> API.CONFIG.setup_list shell dot_profile)

    let exec ~inplace_path command =
      API.CONFIG.exec ~inplace_path command

    let list names =
      read_lock (fun () -> API.CONFIG.list names)

    let set var value =
      switch_lock (fun () -> API.CONFIG.set var value)

    let expand str =
      read_lock (fun () -> API.CONFIG.expand str)

    let variable var =
      read_lock (fun () -> API.CONFIG.variable var)

    let subst files =
      read_lock (fun () -> API.CONFIG.subst files)

  end

  module REPOSITORY = struct

    let list ~short =
      read_lock (fun () -> API.REPOSITORY.list ~short)

    let add name address ~priority =
      global_lock (fun () -> API.REPOSITORY.add name address ~priority)

    let remove name =
      global_lock (fun () -> API.REPOSITORY.remove name)

    let priority name ~priority =
      global_lock (fun () -> API.REPOSITORY.priority name ~priority)

    let set_url name address =
      global_lock (fun () -> API.REPOSITORY.set_url name address)

  end

  module SWITCH = struct

    let switch ~quiet ~packages name =
      global_then_switch_lock (fun () ->
        API.SWITCH.switch_cont ~quiet ~packages name)

    let install ~quiet ~update_config ~packages switch =
      global_then_switch_lock (fun () ->
        API.SWITCH.install_cont ~quiet ~update_config
          ~packages switch)

    let import filename =
      switch_lock (fun () -> API.SWITCH.import filename)

    let export filename =
      read_lock (fun () -> API.SWITCH.export filename)

    let remove gt ?confirm switch =
      global_lock (fun () -> API.SWITCH.remove gt ?confirm switch)

    let reinstall switch =
      global_then_switch_lock (fun () ->
          switch, (fun () -> API.SWITCH.reinstall switch))

    let list ~print_short ~installed ~all =
      read_lock (fun () -> API.SWITCH.list ~print_short ~installed ~all)

    let show () =
      read_lock API.SWITCH.show

  end

  module PIN = struct

    let pin name ?edit ?version ?action pin_option =
      switch_lock (fun () -> API.PIN.pin name ?edit ?version ?action pin_option)

    let edit ?action name =
      switch_lock (fun () -> API.PIN.edit ?action name)

    let unpin ?action names =
      switch_lock (fun () -> API.PIN.unpin ?action names)

    let list ~short () =
      read_lock (fun () -> API.PIN.list ~short ())

  end

  module LIST = struct

    let list ~print_short ~filter ~order ~exact_name ~case_sensitive
        ?depends ?reverse_depends ?recursive_depends ?resolve_depends
        ?depopts ?depexts ?dev
        pkg_str =
      read_lock (fun () ->
          OpamListCommand.list
            ~print_short ~filter ~order ~exact_name ~case_sensitive
            ?depends ?reverse_depends ?recursive_depends ?resolve_depends
            ?depopts ?depexts ?dev
            pkg_str
        )

    let info ~fields ~raw_opam ~where regexps =
      read_lock (fun () -> OpamListCommand.info ~fields ~raw_opam ~where regexps)

  end
end

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
open OpamPackage.Set.Op
open OpamStd.Op

module S = OpamFile.SwitchSelections

let log fmt = OpamConsole.log "SWITCH" fmt
let slog = OpamConsole.slog

let list gt ~print_short =
  log "list";
  let gt = OpamGlobalState.fix_switch_list gt in
  if print_short then
    List.iter (OpamConsole.msg "%s\n" @* OpamSwitch.to_string)
      (List.sort compare (OpamFile.Config.installed_switches gt.config))
  else
  let installed_switches =
    OpamGlobalState.fold_switches (fun sw sel acc ->
        let opams =
          OpamPackage.Set.fold (fun nv acc ->
              match
                OpamFile.OPAM.read_opt
                  (OpamPath.Switch.installed_opam gt.root sw nv)
              with
              | Some opam -> OpamPackage.Map.add nv opam acc
              | None -> acc)
            sel.sel_compiler OpamPackage.Map.empty
        in
        let ifempty default m =
          if OpamPackage.Map.is_empty m then default else m
        in
        let comp =
          OpamPackage.Map.filter
            (fun nv _ -> OpamPackage.Set.mem nv sel.sel_roots)
            opams
          |> ifempty opams
        in
        let comp =
          OpamPackage.Map.filter
            (fun _ opam -> OpamFile.OPAM.has_flag Pkgflag_Compiler opam)
            comp
          |> ifempty comp
        in
        let conf =
          OpamStateConfig.Switch.read_opt ~lock_kind:`Lock_read gt sw
        in
        let descr = match conf with
          | Some c -> c.OpamFile.Switch_config.synopsis
          | None -> OpamConsole.colorise `red "Missing config file"
        in
        OpamSwitch.Map.add sw (OpamPackage.keys comp, descr) acc)
      gt
      OpamSwitch.Map.empty
  in
  let list = OpamSwitch.Map.bindings installed_switches in

  let table =
    List.map (OpamConsole.colorise `blue)
      ["#"; "switch"; "compiler"; "description" ] ::
    List.map (fun (switch, (packages, descr)) ->
        let current = Some switch = OpamStateConfig.get_switch_opt () in
        List.map
          (if current then OpamConsole.colorise `bold else fun s -> s)
          [ if current then
              OpamConsole.(utf8_symbol Symbols.rightwards_arrow "->")
            else "";
            OpamSwitch.to_string switch;
            OpamStd.List.concat_map ","
              (OpamConsole.colorise `yellow @* OpamPackage.to_string)
              (OpamPackage.Set.elements packages);
            descr ])
      list
  in
  OpamConsole.print_table stdout ~sep:"  "
    (OpamStd.Format.align_table table);

  match OpamStateConfig.get_switch_opt (), OpamStateConfig.(!r.switch_from)
  with
  | None, _ when OpamFile.Config.installed_switches gt.config <> [] ->
    OpamConsole.note
      "No switch is currently set, you should use 'opam switch <switch>' \
       to set an active switch"
  | Some switch, `Env ->
    let sys = OpamFile.Config.switch gt.config in
    if not (OpamGlobalState.switch_exists gt switch) then
      (OpamConsole.msg "\n";
       OpamConsole.warning
         "The OPAMSWITCH variable does not point to a valid switch: %S"
         (OpamSwitch.to_string switch))
    else if sys <> Some switch then
      (OpamConsole.msg "\n";
       OpamConsole.note
         "Current switch is set locally through the OPAMSWITCH variable.\n\
          The current global system switch is %s."
         (OpamStd.Option.to_string ~none:"unset"
            (fun s -> OpamConsole.colorise `bold (OpamSwitch.to_string s)) sys))
    else
      (match OpamStateConfig.get_current_switch_from_cwd gt.root with
       | None -> ()
       | Some sw ->
         OpamConsole.msg "\n";
         OpamConsole.note
           "Current switch is set globally and through the OPAMSWITCH variable.\n\
            Thus, the local switch found at %s was ignored."
           (OpamConsole.colorise `bold (OpamSwitch.to_string sw)))
  | Some switch, `Default when not (OpamGlobalState.switch_exists gt switch) ->
    OpamConsole.msg "\n";
    OpamConsole.warning
      "The currently selected switch (%S) is invalid.\n%s"
      (OpamSwitch.to_string switch)
      (if OpamSwitch.is_external switch
       then "Stale '_opam' directory or link ?"
       else "Fix the selection with 'opam switch set SWITCH'.")
  | Some switch, `Default when OpamSwitch.is_external switch ->
    OpamConsole.msg "\n";
    OpamConsole.note
      "Current switch has been selected based on the current directory.\n\
       The current global system switch is %s."
      (OpamStd.Option.to_string ~none:"unset"
         (fun s -> OpamConsole.colorise `bold (OpamSwitch.to_string s))
         (OpamFile.Config.switch gt.config));
    if not (OpamEnv.is_up_to_date_switch gt.root switch) then
      OpamConsole.warning
        "The environment is not in sync with the current switch.\n\
         You should run: %s"
        (OpamEnv.eval_string gt (Some switch))
  | Some switch, `Default ->
    if not (OpamEnv.is_up_to_date_switch gt.root switch) then
      (OpamConsole.msg "\n";
       OpamConsole.warning
         "The environment is not in sync with the current switch.\n\
          You should run: %s"
         (OpamEnv.eval_string gt (Some switch)))
  | _ -> ()

let clear_switch ?(keep_debug=false) (gt: rw global_state) switch =
  let module C = OpamFile.Config in
  let config = gt.config in
  let config =
    C.with_installed_switches
      (List.filter ((<>) switch) (C.installed_switches config))
      config
  in
  let config =
    if C.switch config = Some switch then C.with_switch_opt None config
    else config
  in
  let gt = { gt with config } in
  OpamGlobalState.write gt;
  let comp_dir = OpamPath.Switch.root gt.root switch in
  if keep_debug && (OpamClientConfig.(!r.keep_build_dir) || (OpamConsole.debug ())) then
    (OpamConsole.note "Keeping %s despite errors (debug mode), \
                       you may want to remove it by hand"
       (OpamFilename.Dir.to_string comp_dir);
     gt)
  else
  try OpamFilename.rmdir comp_dir; gt
  with OpamSystem.Internal_error _ -> gt

let remove gt ?(confirm = true) switch =
  log "remove switch=%a" (slog OpamSwitch.to_string) switch;
  if not (OpamGlobalState.switch_exists gt switch) then (
    OpamConsole.msg "The compiler switch %s does not exist.\n"
      (OpamSwitch.to_string switch);
    OpamStd.Sys.exit_because `Not_found;
  );
  if not confirm ||
     OpamConsole.confirm
       "Switch %s and all its packages will be wiped. Are you sure?"
       (OpamSwitch.to_string switch)
  then
    clear_switch gt switch
  else gt

let set_invariant_raw st invariant =
  let switch_config = {st.switch_config with invariant = Some invariant} in
  let st = {st with switch_invariant = invariant; switch_config } in
  if not (OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show)) then
    OpamSwitchAction.install_switch_config st.switch_global.root st.switch
      switch_config;
  st

let install_compiler
    ?(additional_installs=[]) ?(deps_only=false) ?(ask=false) t =
  let invariant = t.switch_invariant in
  if invariant = OpamFormula.Empty && additional_installs = [] then begin
    (if not OpamClientConfig.(!r.show) &&
        not OpamStateConfig.(!r.dryrun) then
       OpamFile.Environment.write
         (OpamPath.Switch.environment t.switch_global.root t.switch)
         (OpamEnv.compute_updates t);
     OpamEnv.check_and_print_env_warning t);
    t
  end else
  let atoms = OpamFormula.atoms invariant in
  let names_of_atoms at = OpamPackage.Name.Set.of_list (List.map fst at) in
  let comp_roots = names_of_atoms atoms in
  let add_names = names_of_atoms additional_installs in
  let roots =
    OpamPackage.Name.Set.union comp_roots add_names
  in
  OpamConsole.header_msg "Installing new switch packages";
  OpamConsole.msg "Switch invariant: %s\n"
    (OpamFileTools.dep_formula_to_string invariant);
  let solution =
    OpamSolution.resolve t Switch
      ~requested:(OpamPackage.packages_of_names t.packages roots)
      (OpamSolver.request ~install:additional_installs ())
  in
  let solution = match solution with
    | Success s -> s
    | Conflicts cs ->
      OpamConsole.error
        "Could not determine which packages to install for this switch:";
      OpamConsole.errmsg "%s\n"
        (OpamCudf.string_of_conflicts t.packages
           (OpamSwitchState.unavailable_reason t) cs);
      OpamStd.Sys.exit_because `No_solution
  in
  let () = match OpamSolver.stats solution with
    | { s_install = _; s_reinstall = 0; s_upgrade = 0;
        s_downgrade=0; s_remove = 0 } -> ()
    | stats ->
      OpamConsole.error_and_exit `No_solution
        "Inconsistent resolution of packages:\n%s"
        (OpamSolver.string_of_stats stats)
  in
  let to_install_pkgs = OpamSolver.new_packages solution in
  let base_comp = OpamPackage.packages_of_names to_install_pkgs comp_roots in
  let has_comp_flag =
    let is_comp nv =
      try OpamFile.OPAM.has_flag Pkgflag_Compiler (OpamSwitchState.opam t nv)
      with Not_found -> false
    in
    (* Packages with the Compiler flag, or with a direct dependency with that
       flag (just for the warning) *)
    OpamPackage.Set.filter
      (fun nv ->
         is_comp nv ||
         OpamPackage.Set.exists is_comp
           (OpamFormula.packages t.packages
              (OpamPackageVar.all_depends ~filter_default:true t
                 (OpamSwitchState.opam t nv))))
      base_comp
  in
  if invariant = OpamFormula.Empty then
    OpamConsole.note
      "No invariant was set, you may want to use `opam switch set-invariant' \
       to keep a stable compiler version on upgrades."
  else if OpamPackage.Set.is_empty has_comp_flag then
    OpamConsole.note
      "Packages %s don't have the 'compiler' flag set (nor any of their \
       direct dependencies).\n\
       You may want to use `opam switch set-invariant' to keep a stable \
       compiler version on upgrades."
      (OpamStd.List.concat_map ", " OpamPackage.to_string
         (OpamPackage.Set.elements base_comp));
  let t =
    if t.switch_config.OpamFile.Switch_config.synopsis = "" then
      let synopsis =
        match OpamPackage.Set.elements base_comp with
        | [] -> OpamSwitch.to_string t.switch
        | [pkg] ->
          let open OpamStd.Option.Op in
          (OpamSwitchState.opam_opt t pkg >>= OpamFile.OPAM.synopsis) +!
          OpamPackage.to_string pkg
        | pkgs -> OpamStd.List.concat_map " " OpamPackage.to_string pkgs
      in
      let switch_config =
        { t.switch_config with OpamFile.Switch_config.synopsis }
      in
      if not (OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show)) then
        OpamSwitchAction.install_switch_config t.switch_global.root t.switch
          switch_config;
      { t with switch_config }
    else t
  in
  let t = { t with compiler_packages = base_comp } in
  let solution =
    if deps_only then
      OpamSolver.filter_solution (fun nv ->
          not (OpamPackage.Name.Set.mem nv.name add_names))
        solution
    else solution
  in
  let t, result =
    OpamSolution.apply t
      ~ask:(OpamClientConfig.(!r.show) || ask)
      ~requested:(OpamPackage.packages_of_names t.packages roots)
      ~add_roots:roots
      solution in
  OpamSolution.check_solution ~quiet:OpamClientConfig.(not !r.show) t
    (Success result);
  t

let create
    gt ~rt ?synopsis ?repos ~update_config ~invariant switch post =
  let update_config = update_config && not (OpamSwitch.is_external switch) in
  let comp_dir = OpamPath.Switch.root gt.root switch in
  let simulate = OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show) in
  if OpamGlobalState.switch_exists gt switch then
    OpamConsole.error_and_exit `Bad_arguments
      "There already is an installed switch named %s"
      (OpamSwitch.to_string switch);
  if Sys.file_exists (OpamFilename.Dir.to_string comp_dir) then
    OpamConsole.error_and_exit `Bad_arguments
      "Directory %S already exists, please choose a different name"
      (OpamFilename.Dir.to_string comp_dir);
  let gt, st =
    if not simulate then
      let gt =
        OpamSwitchAction.create_empty_switch gt ?synopsis ?repos ~invariant
          switch
      in
      let rt =
        ({ rt with repos_global = (gt :> unlocked global_state)  }
         :> unlocked repos_state)
      in
      gt, OpamSwitchState.load `Lock_write gt rt switch
    else
      let rt = (rt :> unlocked repos_state) in
      let st = OpamSwitchState.load_virtual ?repos_list:repos gt rt in
      let switch_config =
        OpamSwitchAction.gen_switch_config gt.root ?repos switch ~invariant
      in
      let st = { st with switch_invariant = invariant } in
      let available_packages =
        lazy (OpamSwitchState.compute_available_packages gt switch switch_config
                ~pinned:OpamPackage.Set.empty
                ~opams:st.opams)
      in
      gt, { st with switch; switch_config; available_packages }
  in
  match post st with
  | ret, st ->
    let st =
      if update_config && not simulate
      then OpamSwitchAction.set_current_switch gt st
      else st
    in
    OpamGlobalState.drop gt;
    ret, st
  | exception e when not simulate ->
    let () =
      try OpamStd.Exn.fatal e with e ->
        OpamStd.Exn.finalise e @@ fun () ->
        OpamConsole.warning "Switch %s left partially installed"
          (OpamSwitch.to_string st.switch)
    in
    OpamStd.Exn.finalise e @@ fun () ->
    let gt, st =
      if OpamConsole.confirm "Switch initialisation failed: clean up? \
                              ('n' will leave the switch partially installed)"
      then clear_switch gt st.switch, st
      else if update_config && not simulate
      then gt, OpamSwitchAction.set_current_switch gt st
      else gt, st
    in
    OpamSwitchState.drop st;
    OpamGlobalState.drop gt

let switch lock gt switch =
  log "switch switch=%a" (slog OpamSwitch.to_string) switch;
  if OpamGlobalState.switch_exists gt switch then
    OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
    let st = OpamSwitchState.load lock gt rt switch in
    let st =
      if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show) then st
      else OpamSwitchAction.set_current_switch gt st
    in
    OpamEnv.check_and_print_env_warning st
  else
  let installed_switches = OpamFile.Config.installed_switches gt.config in
  OpamConsole.error_and_exit `Not_found
    "No switch %s is currently installed. Did you mean \
     'opam switch create %s'?\n\
     Installed switches are:\n%s"
    (OpamSwitch.to_string switch) (OpamSwitch.to_string switch)
    (OpamStd.Format.itemize OpamSwitch.to_string installed_switches)

let switch_previous lock gt =
  match OpamFile.Config.previous_switch gt.config with
  | Some switch_name ->
    OpamConsole.msg
      (if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show)
       then "Would select opam switch %s.\n"
       else "Selecting opam switch %s.\n")
      (OpamConsole.colorise `yellow (OpamSwitch.to_string switch_name));
    switch lock gt switch_name
  | None ->
    OpamConsole.error_and_exit `Not_found
      "No previously used switch could be found"

let import_t ?ask importfile t =
  log "import switch";

  let extra_files = importfile.OpamFile.SwitchExport.extra_files in
  let xfiles_dir =
    OpamPath.Switch.extra_files_dir t.switch_global.root t.switch
  in
  OpamHash.Map.iter (fun hash content ->
      let value = Base64.decode_exn content in
      let my = OpamHash.compute_from_string ~kind:(OpamHash.kind hash) value in
      if OpamHash.contents my = OpamHash.contents hash then
        let dst =
          let base = OpamFilename.Base.of_string (OpamHash.contents hash) in
          OpamFilename.create xfiles_dir base
        in
          OpamFilename.write dst value
      else
        failwith "Bad hash for inline extra-files") extra_files;

  let import_sel = importfile.OpamFile.SwitchExport.selections in
  let import_opams = importfile.OpamFile.SwitchExport.overlays in

  (* Check that pinned packages need reinstall *)
  let to_reinstall =
    OpamPackage.Name.Map.fold (fun name imported reinst ->
        try
          let pkg =
            OpamSwitchState.find_installed_package_by_name t name
          in
          let installed = OpamSwitchState.opam t pkg in
          if OpamFile.OPAM.effectively_equal
              ~modulo_state:true installed imported then
            reinst
          else OpamPackage.Set.add pkg reinst
        with Not_found -> reinst)
      import_opams OpamPackage.Set.empty
  in
  let t =
    { t with reinstall =
               lazy OpamPackage.Set.Op.(Lazy.force t.reinstall ++ to_reinstall) }
  in

  let opams =
    OpamPackage.Name.Map.fold (fun name opam opams ->
        let nv = OpamPackage.create name (OpamFile.OPAM.version opam) in
        OpamPackage.Map.add nv opam opams)
      import_opams t.opams
  in

  let packages = t.packages ++ OpamPackage.keys opams in

  let pinned =
    let names = OpamPackage.names_of_packages import_sel.sel_pinned in
    OpamPackage.Set.filter
      (fun nv -> not (OpamPackage.Name.Set.mem nv.name names)) t.pinned ++
    import_sel.sel_pinned
  in

  let available =
    OpamSwitchState.compute_available_packages
      t.switch_global t.switch t.switch_config
      ~pinned ~opams
  in

  let compiler_packages, to_install =
    if OpamPackage.Set.is_empty t.compiler_packages then
      import_sel.sel_compiler %% available,
      import_sel.sel_installed
    else
      t.compiler_packages,
      import_sel.sel_installed -- import_sel.sel_compiler
  in

  let t =
    { t with
      available_packages = lazy available;
      packages;
      compiler_packages;
      pinned;
      opams; }
  in

  let unavailable_version, unavailable =
    let available_names = OpamPackage.names_of_packages available in
    OpamPackage.Set.partition
      (fun nv -> OpamPackage.Name.Set.mem nv.name available_names)
      (to_install -- available)
  in

  if not (OpamPackage.Set.is_empty unavailable_version) then
    OpamConsole.warning
      "These packages aren't available at the specified versions, \
       version constraints have been discarded:\n%s"
      (OpamStd.Format.itemize OpamPackage.to_string
         (OpamPackage.Set.elements unavailable_version));
  if not (OpamPackage.Set.is_empty unavailable) then
    OpamConsole.warning
      "These packages are unavailable, they have been ignored from \
       the import file:\n%s"
      (OpamStd.Format.itemize OpamPackage.to_string
         (OpamPackage.Set.elements unavailable));

  let t, solution =
    let to_import =
      OpamSolution.eq_atoms_of_packages (to_install %% available) @
      OpamSolution.atoms_of_packages unavailable_version
    in

    let add_roots = OpamPackage.names_of_packages import_sel.sel_roots in

    OpamSolution.resolve_and_apply ?ask t Import
      ~requested:((to_install %% available) ++ unavailable_version)
      ~add_roots
      (OpamSolver.request ~install:to_import ())
  in
  OpamSolution.check_solution t solution;
  OpamFilename.rmdir xfiles_dir;
  if not (OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show))
  then begin
    (* Put imported overlays in place *)
    OpamPackage.Set.iter (fun nv ->
        match OpamPackage.Name.Map.find_opt nv.name import_opams with
        | None -> ()
        | Some opam ->
          OpamFilename.rmdir
            (OpamPath.Switch.Overlay.package t.switch_global.root
               t.switch nv.name);
          OpamFile.OPAM.write
            (OpamPath.Switch.Overlay.opam t.switch_global.root
               t.switch nv.name)
            opam)
      pinned;
    (* Save new pinnings *)
    let sel = OpamSwitchState.load_selections t.switch_global t.switch in
    S.write
      (OpamPath.Switch.selections t.switch_global.root t.switch)
      { sel with sel_pinned = pinned }
  end;
  t

let freeze_opam src_dir nv opam =
  match OpamFile.OPAM.url opam with
  | None -> opam
  | Some url ->
    let url_t = OpamFile.URL.url url in
    match url_t.backend with
    | #OpamUrl.version_control ->
      (match OpamProcess.Job.run
               (OpamRepository.revision (src_dir nv) url_t) with
      | None ->
        OpamConsole.error_and_exit `Not_found
          "Unable to retrieve %s url revision: %s, \
           it can't be exported with --freeze."
          (OpamPackage.to_string nv)
          (OpamUrl.to_string url_t)
      | Some hash ->
        OpamFile.OPAM.with_url
          (OpamFile.URL.with_url
             { url_t with hash = Some (OpamPackage.Version.to_string hash) }
             url)
          opam)
    | `http ->
      (match OpamFile.URL.checksum url with
       | [] ->
         OpamConsole.error_and_exit `Not_found
           "%s url doesn't have an associated checksum, \
            it can't be exported with --freeze."
           (OpamPackage.Name.to_string nv.name)
       | _ -> opam)
    | `rsync ->
      OpamConsole.error_and_exit `Not_found
        "%s is path pinned, it can't be exported with --freeze."
        (OpamPackage.Name.to_string nv.name)

let export rt ?(freeze=false) ?(full=false)
    ?(switch=OpamStateConfig.get_switch ()) filename =
  let root = OpamStateConfig.(!r.root_dir) in
  let export =
    OpamFilename.with_flock `Lock_none (OpamPath.Switch.lock root switch)
    @@ fun _ ->
    let selections =
      OpamStateConfig.Switch.safe_read_selections
        ~lock_kind:`Lock_none rt.repos_global switch
    in
    let opams =
      let read_opams read pkgs =
        let src_dir nv =
          if OpamPackage.Set.mem nv selections.sel_pinned then
            OpamPath.Switch.pinned_package root switch nv.name
          else
            OpamPath.Switch.sources root switch nv
        in
        OpamPackage.Set.fold (fun nv map ->
            match read nv with
            | Some opam ->
              let opam =
                if not freeze then opam else
                  freeze_opam src_dir nv opam
              in
              OpamPackage.Map.add nv opam map
            | None -> map) pkgs OpamPackage.Map.empty
      in
      let overlays =
        read_opams (fun nv ->
            OpamFileTools.read_opam
              (OpamPath.Switch.Overlay.package root switch nv.name))
          selections.sel_pinned
      in
      if not full then overlays else
        OpamPackage.Map.union (fun a _ -> a) overlays
        @@ read_opams (fun nv -> OpamFile.OPAM.read_opt
                          (OpamPath.Switch.installed_opam root switch nv))
          (selections.sel_installed -- selections.sel_pinned)
    in
    let overlays =
      OpamPackage.Map.fold (fun nv opam nmap ->
          OpamPackage.Name.Map.add nv.name opam nmap)
        opams OpamPackage.Name.Map.empty
    in
    let extra_files =
      let repos_roots = OpamRepositoryState.get_root rt in
      OpamPackage.Map.fold (fun nv opam hmap ->
          match OpamFile.OPAM.get_extra_files ~repos_roots opam with
          | [] -> hmap
          | files ->
            let hmap, err =
              List.fold_left (fun (hmap,err) (file, base, hash) ->
                  if OpamFilename.exists file &&
                     OpamHash.check_file (OpamFilename.to_string file) hash then
                    let value = Base64.encode_string (OpamFilename.read file) in
                    OpamHash.Map.add hash value hmap, err
                  else hmap, base::err)
                (hmap,[]) files
            in
            if err <> [] then
              OpamConsole.warning "Invalid hash%s, ignoring package %s extra-file%s: %s"
                (match err with | [_] -> "" | _ -> "es")
                (OpamPackage.to_string nv)
                (match err with | [_] -> "" | _ -> "s")
                (OpamStd.Format.pretty_list (List.map OpamFilename.Base.to_string err));
            hmap)
        opams OpamHash.Map.empty
    in
    { OpamFile.SwitchExport.selections; extra_files; overlays }
  in
  match filename with
  | None   -> OpamFile.SwitchExport.write_to_channel stdout export
  | Some f -> OpamFile.SwitchExport.write f export

let show () =
  OpamConsole.msg "%s\n"
    (OpamSwitch.to_string (OpamStateConfig.get_switch ()))

let reinstall init_st =
  let switch = init_st.switch in
  log "reinstall switch=%a" (slog OpamSwitch.to_string) switch;
  let gt = init_st.switch_global in

  let switch_root = OpamPath.Switch.root gt.root switch in
  let opam_subdir = OpamPath.Switch.meta gt.root switch in
  let pkg_dirs =
    List.filter ((<>) opam_subdir) (OpamFilename.dirs switch_root)
  in
  List.iter OpamFilename.cleandir pkg_dirs;
  List.iter OpamFilename.remove (OpamFilename.files switch_root);
  OpamFilename.cleandir (OpamPath.Switch.config_dir gt.root switch);
  OpamFilename.cleandir (OpamPath.Switch.installed_opams gt.root switch);
  let st =
    { init_st with
      installed = OpamPackage.Set.empty;
      installed_roots = OpamPackage.Set.empty;
      reinstall = lazy OpamPackage.Set.empty; }
  in
  import_t { OpamFile.SwitchExport.
             selections = OpamSwitchState.selections init_st;
             extra_files = OpamHash.Map.empty;
             overlays = OpamPackage.Name.Map.empty; }
    st

let import st filename =
  let import_str = match filename with
    | None   -> OpamSystem.string_of_channel stdin
    | Some f -> OpamFilename.read (OpamFile.filename f)
  in
  let importfile =
    try OpamFile.SwitchExport.read_from_string ?filename import_str
    with OpamPp.Bad_format _ as e ->
      log "Error loading export file, trying the old file format";
      try
        let selections = OpamFile.LegacyState.read_from_string import_str in
        { OpamFile.SwitchExport.selections;
          extra_files = OpamHash.Map.empty;
          overlays = OpamPackage.Name.Map.empty }
      with e1 -> OpamStd.Exn.fatal e1; raise e
  in
  import_t importfile st

let set_invariant ?(force=false) st invariant =
  let satisfied = OpamFormula.satisfies_depends st.installed invariant in
  let names =
    OpamPackage.Name.Set.of_list (List.map fst (OpamFormula.atoms invariant))
  in
  let name_unknown =
    OpamPackage.Name.Set.filter
      (fun n -> not (OpamPackage.has_name st.packages n))
      names
  in
  if not (OpamPackage.Name.Set.is_empty name_unknown) then
    (if satisfied || force then OpamConsole.warning
     else OpamConsole.error_and_exit `Not_found)
      "No packages by these names found: %s"
      (OpamStd.List.concat_map ", "
         OpamPackage.Name.to_string
         (OpamPackage.Name.Set.elements name_unknown));
  let packages = OpamFormula.packages st.installed invariant in
  let not_comp =
    OpamPackage.Set.filter (fun nv ->
        match OpamSwitchState.opam_opt st nv with
        | Some opam -> not (OpamFile.OPAM.has_flag Pkgflag_Compiler opam)
        | None -> false)
      packages
  in
  if not (OpamPackage.Set.is_empty not_comp) then
    OpamConsole.warning
      "Packages %s don't have the 'compiler' flag set."
      (OpamStd.Format.pretty_list
         (List.map OpamPackage.Name.to_string
            (OpamPackage.Name.Set.elements
               (OpamPackage.names_of_packages not_comp))));
  set_invariant_raw st invariant

let get_compiler_packages ?repos rt =
  let repos = match repos with
    | None -> OpamGlobalState.repos_list rt.repos_global
    | Some r -> r
  in
  let package_index = OpamRepositoryState.build_index rt repos in
  OpamPackage.Map.filter
    (fun _ opam ->
       OpamFile.OPAM.has_flag Pkgflag_Compiler opam &&
       OpamFilter.eval_to_bool ~default:false
         (OpamPackageVar.resolve_global rt.repos_global)
         (OpamFile.OPAM.available opam))
    package_index
  |> OpamPackage.keys

let guess_compiler_invariant ?repos rt strings =
  let repos = match repos with
    | None -> OpamGlobalState.repos_list rt.repos_global
    | Some r -> r
  in
  let opams = OpamRepositoryState.build_index rt repos  in
  let packages = OpamPackage.keys opams in
  let compiler_packages =
    OpamPackage.Map.filter
      (fun _ -> OpamFile.OPAM.has_flag Pkgflag_Compiler)
      opams
    |> OpamPackage.keys
  in
  let invariant =
    List.fold_left (fun acc str ->
        try
          let (name, _) as atom = OpamFormula.atom_of_string str in
          if OpamPackage.Set.exists (OpamFormula.check atom)
              (OpamPackage.packages_of_name packages name)
          then OpamFormula.ands [acc; Atom atom]
          else raise Not_found
        with Failure _ | Not_found ->
        try
          let v = OpamPackage.Version.of_string str in
          let candidates =
            OpamPackage.Set.filter (fun nv -> nv.version = v)
              compiler_packages
          in
          if OpamPackage.Set.is_empty candidates then
            raise Not_found
          else
          let disj =
            OpamPackage.Set.fold
              (fun nv acc ->
                 OpamFormula.ors
                   [acc; Atom (OpamSolution.eq_atom_of_package nv)])
              candidates OpamFormula.Empty
          in
          OpamFormula.ands [acc; disj]
        with
        | Failure _ ->
          OpamConsole.error_and_exit `Bad_arguments
            "Invalid package specification or version %S"
            str
        | Not_found ->
          OpamConsole.error_and_exit `Not_found
            "No compiler matching `%s' found, use `opam switch list-available' \
             to see what is available, or use `--packages' to select packages \
             explicitly."
            str)
      OpamFormula.Empty
      strings
  in
  OpamFormula.of_atom_formula invariant

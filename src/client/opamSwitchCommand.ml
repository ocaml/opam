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
open OpamPackage.Set.Op
open OpamStd.Op

module S = OpamFile.SwitchSelections

let log fmt = OpamConsole.log "SWITCH" fmt
let slog = OpamConsole.slog

(* name + state + compiler + description *)
(* TODO: add repo *)
let list ~print_short ~installed ~all =
  log "list";
  let gt = OpamGlobalState.load () in

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
        OpamSwitch.Map.add sw comp acc)
      gt
      OpamSwitch.Map.empty
  in
  let available, notshown =
    if installed then OpamPackage.Map.empty, 0 else
    let rt = OpamRepositoryState.load gt in
    let st = OpamSwitchState.load_virtual gt rt in
    let is_main_comp_re =
      Re.(compile (seq [bos; rep1 (alt [digit; char '.']); eos]))
    in
    OpamPackage.Map.fold
      (fun nv opam (acc,notshown) ->
         if OpamFile.OPAM.has_flag Pkgflag_Compiler opam &&
            OpamPackage.Set.mem nv (Lazy.force st.available_packages)
         then
           if all ||
              Re.(execp is_main_comp_re (OpamPackage.version_to_string nv))
           then OpamPackage.Map.add nv opam acc, notshown
           else acc, notshown + 1
         else acc, notshown)
      st.opams (OpamPackage.Map.empty, 0)
  in
  let list =
    OpamSwitch.Map.fold (fun sw opams acc ->
        let descr =
          match OpamPackage.Map.values opams with
          | [opam] -> OpamFile.OPAM.descr opam
          | _ -> None
        in
        (Some sw, OpamPackage.keys opams, descr) :: acc)
      installed_switches []
  in
  let list =
    OpamPackage.Map.fold (fun nv opam acc ->
        let packages = OpamPackage.Set.singleton nv in
        if
          List.exists
            (fun (_, nvs, _) -> OpamPackage.Set.equal nvs packages)
            list
        then acc
        else (None, packages, OpamFile.OPAM.descr opam) :: acc)
      available list
  in
  let list = List.sort compare list in

  let table =
    List.map (OpamConsole.colorise `blue)
      ["# switch"; "compiler"; "description" ] ::
    List.map (fun (swopt, packages, descr) ->
        List.map
          (if swopt <> None && swopt = OpamStateConfig.(!r.current_switch)
           then OpamConsole.colorise `bold else fun s -> s)
          [ OpamStd.Option.to_string ~none:"-" OpamSwitch.to_string swopt;
            OpamStd.List.concat_map ","
              (OpamConsole.colorise `yellow @* OpamPackage.to_string)
              (OpamPackage.Set.elements packages);
            OpamStd.Option.to_string ~none:"" OpamFile.Descr.synopsis descr ])
      list
  in
  OpamStd.Format.print_table stdout ~sep:"  "
    (OpamStd.Format.align_table table);

  if not print_short && notshown > 0 then
    OpamConsole.msg "# %d more patched or experimental compilers, \
                     use '--all' to show\n"
      notshown;
  if not print_short then
  match OpamStateConfig.(!r.current_switch), OpamStateConfig.(!r.switch_from)
  with
  | None, _ when OpamFile.Config.installed_switches gt.config <> [] ->
    OpamConsole.note
      "No switch is currently set, you should use 'opam switch <switch>' \
       to set an active switch"
  | Some switch, `Env ->
    let sys = OpamFile.Config.switch gt.config in
    if sys <> Some switch then
      (OpamConsole.msg "\n";
       OpamConsole.note
         "Current switch is set locally through the OPAMSWITCH variable.\n\
          The current global system switch is %s."
         (OpamStd.Option.to_string ~none:"unset"
            (fun s -> OpamConsole.colorise `bold (OpamSwitch.to_string s)) sys))
  | Some switch, `Default ->
    let up_to_date =
      match OpamStd.Env.getopt "PATH" with
      | None -> false
      | Some path -> path = OpamEnv.path ~force_path:false gt.root switch
    in
     if not up_to_date then
       (OpamConsole.msg "\n";
        OpamConsole.warning
          "The environment is not in sync with the current switch.\n\
           You should run: %s"
          (OpamEnv.eval_string gt (Some switch)))
  | _ -> ()

let clear_switch ?(keep_debug=false) gt switch =
  let module C = OpamFile.Config in
  let config = gt.config in
  let config =
    C.with_installed_switches config
      (List.filter ((<>) switch) (C.installed_switches config))
  in
  let config =
    if C.switch config = Some switch then C.with_switch_opt config None
    else config
  in
  OpamStateConfig.write gt.root config;
  let comp_dir = OpamPath.Switch.root gt.root switch in
  if keep_debug && (OpamStateConfig.(!r.keep_build_dir) || (OpamConsole.debug ())) then
    OpamConsole.note "Keeping %s despite errors (debug mode), \
                      you may want to remove it by hand"
      (OpamFilename.Dir.to_string comp_dir)
  else
  try OpamFilename.rmdir comp_dir
  with OpamSystem.Internal_error _ -> ()

let remove gt ?(confirm = true) switch =
  log "remove switch=%a" (slog OpamSwitch.to_string) switch;
  let comp_dir = OpamPath.Switch.root gt.root switch in
  if not (OpamFilename.exists_dir comp_dir) then (
    OpamConsole.msg "The compiler switch %s does not exist.\n"
      (OpamSwitch.to_string switch);
    OpamStd.Sys.exit 1;
  );
  if not confirm ||
     OpamConsole.confirm
       "Switch %s and all its packages will be wiped. Are you sure ?"
       (OpamSwitch.to_string switch)
  then
    clear_switch gt switch

let install_compiler_packages t atoms =
  (* install the compiler packages *)
  let roots = OpamPackage.Name.Set.of_list (List.map fst atoms) in
  let not_found =
    OpamPackage.Name.Set.diff roots @@
    OpamPackage.names_of_packages @@
    OpamPackage.packages_of_names t.packages roots
  in
  if not (OpamPackage.Name.Set.is_empty not_found) then
    OpamConsole.error_and_exit
      "No packages %s found."
      (OpamPackage.Name.Set.to_string not_found);
  let solution =
    OpamSolution.resolve t (Switch roots)
      ~orphans:OpamPackage.Set.empty
      { wish_install = [];
        wish_remove  = [];
        wish_upgrade = atoms;
        criteria = `Default;
        extra_attributes = []; } in
  let solution = match solution with
    | Success s -> s
    | Conflicts cs ->
      OpamConsole.error_and_exit "Could not resolve set of base packages:\n%s"
        (OpamCudf.string_of_conflict
           (OpamSwitchState.unavailable_reason t) cs);
  in
  let () = match OpamSolver.stats solution with
    | { s_install = _; s_reinstall = 0; s_upgrade = 0;
        s_downgrade=0; s_remove = 0 } -> ()
    | stats ->
      OpamConsole.error_and_exit
        "Inconsistent resolution of base package installs:\n%s"
        (OpamSolver.string_of_stats stats)
  in
  let to_install_pkgs = OpamSolver.new_packages solution in
  let base_comp = OpamPackage.packages_of_names to_install_pkgs roots in
  let non_comp =
    OpamPackage.Set.filter
      (fun nv ->
         not (OpamFile.OPAM.has_flag Pkgflag_Compiler
                (OpamSwitchState.opam t nv)))
      base_comp
  in
  if not (OpamPackage.Set.is_empty non_comp) &&
     not (OpamConsole.confirm ~default:false
            "Packages %s don't have the 'compiler' flag set. Are you sure \
             you want to set them as the compiler base for this switch ?"
            (OpamPackage.Set.to_string non_comp))
  then
    OpamConsole.error_and_exit "Aborted installation of non-compiler packages \
                                as switch base.";
  let t = { t with compiler_packages = to_install_pkgs } in
    let result =
      OpamSolution.apply ~ask:false t (Switch roots)
        ~requested:roots
        solution in
    OpamSolution.check_solution t result

let install_cont ~quiet ~update_config ~packages switch =
  let gt = OpamGlobalState.load () in
  let comp_dir = OpamPath.Switch.root gt.root switch in
  if List.mem switch (OpamFile.Config.installed_switches gt.config) then
    OpamConsole.error_and_exit
      "There already is an installed compiler switch named %s"
      (OpamSwitch.to_string switch);
  if Sys.file_exists (OpamFilename.Dir.to_string comp_dir) then
    OpamConsole.error_and_exit
      "Directory %S already exists, please choose a different name"
      (OpamFilename.Dir.to_string comp_dir);
  let gt = OpamSwitchAction.create_empty_switch gt switch in
  let st =
    if update_config then OpamSwitchAction.set_current_switch gt switch
    else OpamSwitchState.load gt (OpamRepositoryState.load gt) switch
  in
  switch,
  fun () ->
    try install_compiler_packages st packages
    with e ->
      clear_switch ~keep_debug:true gt switch;
      raise e

let install ~quiet ~update_config ~packages switch =
  (snd (install_cont ~quiet ~update_config ~packages switch)) ()

let switch_cont ~quiet ~packages switch =
  log "switch switch=%a" (slog OpamSwitch.to_string) switch;
  let gt = OpamGlobalState.load () in
  let switch, cont =
    if List.mem switch (OpamFile.Config.installed_switches gt.config) then
      let t = OpamSwitchAction.set_current_switch gt switch in
      OpamEnv.check_and_print_env_warning t;
      switch, fun () -> ()
    else
      install_cont ~quiet ~update_config:true ~packages switch
  in
  switch,
  cont

let switch ~quiet ~packages switch =
  (snd (switch_cont ~quiet ~packages switch)) ()

(** !X todo: include overlays in state/export file *)
let import_t importfile t =
  log "import switch";

  let prev_state = OpamSwitchState.selections t in

  let pinned =
    OpamPackage.Name.Map.merge (fun _ current import ->
        match current, import with
        | _, (Some _ as p) -> p
        | p, None -> p)
      prev_state.sel_pinned importfile.sel_pinned
  in
  (* Add the imported pins in case they don't exist already *)
  let available =
    OpamPackage.Set.fold (fun nv available ->
        if OpamPackage.Set.mem nv available then available else
        if OpamPackage.Name.Map.mem nv.name importfile.sel_pinned
        then OpamPackage.Set.add nv available
        else (
          OpamConsole.warning "%s Skipping."
            (OpamSwitchState.unavailable_reason t
               (OpamSolution.eq_atom
                  nv.name nv.version));
          available
        )
      )
      importfile.sel_installed (Lazy.force t.available_packages)
  in
  let imported = importfile.sel_installed %% available in
  let import_roots = importfile.sel_roots %% available in
  let import_compiler = importfile.sel_compiler %% available in
  let revert_pins () = (* Called in case of error or abortion *)
    if not (OpamStateConfig.(!r.dryrun) || OpamStateConfig.(!r.show)) then
      let switch_state = OpamSwitchState.load_selections t.switch_global t.switch in
      let state_f = OpamPath.Switch.selections t.switch_global.root t.switch in
      S.write state_f { switch_state with sel_pinned = prev_state.sel_pinned }
  in
  let t =
    {t with available_packages = lazy available;
            packages = t.packages ++ available;
            compiler_packages = import_compiler;
            pinned }
  in
  let solution =
    try
      let _ =
        OpamPackage.Name.Map.iter (fun name (version,pin) ->
            let overlay_dir =
              OpamPath.Switch.Overlay.package t.switch_global.root t.switch name
            in
            if not (OpamFilename.exists_dir overlay_dir) then
              OpamPinned.add_overlay t.switch_repos ~version
                t.switch name pin)
          pinned;
        if not (OpamPackage.Name.Map.is_empty pinned) then (
          OpamConsole.header_msg "Synchronising pinned packages";
          OpamUpdate.pinned_packages t
            (OpamPackage.Name.Set.of_list (OpamPackage.Name.Map.keys pinned))
        ) else OpamPackage.Set.empty
      in

      let t = (* needs to be reloaded after the update *)
        if OpamStateConfig.(!r.dryrun) || OpamStateConfig.(!r.show) then t
        else
          (OpamSwitchAction.write_selections t;
           OpamSwitchState.load_full_compat "pin-import" t.switch)
      in

      let available =
        imported %% (Lazy.force t.available_packages ++ t.installed) in

      let to_import =
        OpamSolution.eq_atoms_of_packages available @
        OpamSolution.atoms_of_packages (imported -- available) in

      let roots = OpamPackage.names_of_packages import_roots in

      OpamSolution.resolve_and_apply t (Import roots)
        ~requested:(OpamPackage.names_of_packages imported)
        ~orphans:OpamPackage.Set.empty
        { wish_install = to_import;
          wish_remove  = [];
          wish_upgrade = [];
          criteria = `Default;
          extra_attributes = []; }
    with e ->
      revert_pins ();
      raise e
  in
  (match solution with
   | No_solution | Aborted -> revert_pins ()
   | Error _ | OK _ | Nothing_to_do -> ());
  OpamSolution.check_solution t solution

let export filename =
  let switch = (OpamStateConfig.get_switch ()) in
  let root = OpamStateConfig.(!r.root_dir) in
  let st = S.safe_read (OpamPath.Switch.selections root switch) in
  match filename with
  | None   -> S.write_to_channel stdout st
  | Some f -> S.write f st

let show () =
  OpamConsole.msg "%s\n"
    (OpamSwitch.to_string (OpamStateConfig.get_switch ()))

let reinstall_gt gt switch =
  log "reinstall switch=%a" (slog OpamSwitch.to_string) switch;
  let export = OpamSwitchState.load_selections gt switch in

  (* Remove the directory (except the overlays, backups and lock) *)
  let switch_root = OpamPath.Switch.root gt.root switch in
  let keep_dirs = [
    OpamPath.Switch.Overlay.dir gt.root switch;
    OpamPath.Switch.backup_dir gt.root switch;
  ] in
  let keep_files = [
    OpamPath.Switch.lock gt.root switch;
  ] in
  List.iter
    (fun d -> if not (List.mem d keep_dirs) then OpamFilename.rmdir d)
    (OpamFilename.dirs switch_root);
  List.iter
    (fun f -> if not (List.mem f keep_files) then OpamFilename.remove f)
    (OpamFilename.files switch_root);

  let gt = OpamSwitchAction.create_empty_switch gt switch in
  let st = OpamSwitchState.load gt (OpamRepositoryState.load gt) switch in
  import_t export st

(* !X unify with OpamClient.with_switch_backup; set a number of backups and just
   rename older versions .1, .2 etc. ? *)
let with_backup gt switch f =
  let state_file = OpamPath.Switch.selections gt.root switch in
  let backup_state_file = OpamPath.backup gt.root in
  OpamFilename.copy
    ~src:(OpamFile.filename state_file)
    ~dst:(OpamFile.filename backup_state_file);
  try
    f gt switch;
    OpamFilename.remove (OpamFile.filename backup_state_file)
  with
  | OpamStd.Sys.Exit 0 as e -> raise e
  | err ->
    let new_state = OpamFile.SwitchSelections.safe_read state_file in
    let old_state = OpamFile.SwitchSelections.safe_read backup_state_file in
    if OpamPackage.Set.equal new_state.sel_installed old_state.sel_installed &&
       OpamPackage.Set.equal
         new_state.sel_roots old_state.sel_roots &&
       OpamPackage.Set.equal new_state.sel_compiler old_state.sel_compiler
    then OpamFilename.remove (OpamFile.filename backup_state_file)
    else
      Printf.eprintf "The former package state can be restored with \
                      %s switch import %S%s\n"
        Sys.argv.(0) (OpamFile.to_string backup_state_file)
        (if OpamStateConfig.(!r.current_switch <> Some switch ||
                             !r.switch_from = `Command_line)
         then Printf.sprintf " --switch %s" (OpamSwitch.to_string switch)
         else "");
    raise err

let reinstall gt switch =
  with_backup gt switch reinstall_gt

let import gt switch filename =
  with_backup gt switch
    (fun gt switch ->
       let importfile = match filename with
         | None   -> S.read_from_channel stdin
         | Some f -> S.read f in
       let st = OpamSwitchState.load gt (OpamRepositoryState.load gt) switch in
       import_t importfile st)

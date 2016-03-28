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
let list gt ~print_short ~installed ~all =
  log "list";

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
    let rt = OpamRepositoryState.load `Lock_none gt in
    let st = OpamSwitchState.load_virtual gt rt in
    let is_main_comp_re =
      Re.(compile @@ seq [ bos; rep (diff any (char '+')); eos])
    in
    OpamPackage.Map.fold
      (fun nv opam (acc,notshown) ->
         if OpamFile.OPAM.has_flag Pkgflag_Compiler opam &&
            OpamPackage.Set.mem nv (Lazy.force st.available_packages)
         then
           if all ||
              Re.(execp is_main_comp_re (OpamPackage.name_to_string nv))
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
    C.with_installed_switches
      (List.filter ((<>) switch) (C.installed_switches config))
      config
  in
  let config =
    if C.switch config = Some switch then C.with_switch_opt None config
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
  let t, result =
    OpamSolution.apply ~ask:false t (Switch roots)
      ~requested:roots
      solution in
  OpamSolution.check_solution t result;
  t

let install_cont gt ~update_config ~packages switch =
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
  (if update_config
   then fun f ->
     f (OpamSwitchAction.set_current_switch `Lock_write gt switch)
   else
     OpamSwitchState.with_ `Lock_write gt ~switch)
  @@ fun st ->
  let _gt = OpamGlobalState.unlock gt in
  switch,
  fun () ->
    try
      let st = install_compiler_packages st packages in
      ignore (OpamSwitchState.unlock st)
    with e ->
      let _st = OpamSwitchState.unlock st in
      OpamConsole.warning "Switch %s left partially installed"
        (OpamSwitch.to_string switch);
      raise e

let install gt ~update_config ~packages switch =
  (snd (install_cont gt ~update_config ~packages switch)) ()

let switch_cont gt ~packages switch =
  log "switch switch=%a" (slog OpamSwitch.to_string) switch;
  let switch, cont =
    if List.mem switch (OpamFile.Config.installed_switches gt.config) then
      let st = OpamSwitchAction.set_current_switch `Lock_none gt switch in
      OpamEnv.check_and_print_env_warning st;
      switch, fun () -> ()
    else
      install_cont gt ~update_config:true ~packages switch
  in
  switch,
  cont

let switch gt ~packages switch =
  (snd (switch_cont gt ~packages switch)) ()

let import_t importfile t =
  log "import switch";

  let import_sel = importfile.OpamFile.SwitchExport.selections in
  let import_opams = importfile.OpamFile.SwitchExport.overlays in

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
      (* fixme: the install should be two step, since the installation of the
         compiler defines variables that are later needed to decide
         availabillity of packages *)
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

    let roots = OpamPackage.names_of_packages import_sel.sel_roots in

    OpamSolution.resolve_and_apply t (Import roots)
      ~requested:(OpamPackage.Name.Set.of_list @@ List.map fst to_import)
      ~orphans:OpamPackage.Set.empty
      { wish_install = to_import;
        wish_remove  = [];
        wish_upgrade = [];
        criteria = `Default;
        extra_attributes = []; }
  in
  OpamSolution.check_solution t solution;
  if not (OpamStateConfig.(!r.dryrun) || OpamStateConfig.(!r.show))
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

let read_overlays (read: package -> OpamFile.OPAM.t option) packages =
  OpamPackage.Set.fold (fun nv acc ->
      match read nv with
      | Some opam ->
        if OpamFile.OPAM.extra_files opam <> None then
          (OpamConsole.warning
             "Metadata of package %s uses a files/ subdirectory, it may not be \
              re-imported correctly (skipping definition)"
             (OpamPackage.to_string nv);
           acc)
        else OpamPackage.Name.Map.add nv.name opam acc
      | None -> acc)
    packages
    OpamPackage.Name.Map.empty

let export ?(full=false) filename =
  let switch = OpamStateConfig.get_switch () in
  let root = OpamStateConfig.(!r.root_dir) in
  let export =
    OpamFilename.with_flock `Lock_none (OpamPath.Switch.lock root switch)
    @@ fun () ->
    let selections = S.safe_read (OpamPath.Switch.selections root switch) in
    let overlays =
      read_overlays (fun nv ->
          OpamFileHandling.read_opam
            (OpamPath.Switch.Overlay.package root switch nv.name))
        selections.sel_pinned
    in
    let overlays =
      if full then
        OpamPackage.Name.Map.union (fun a _ -> a) overlays @@
        read_overlays (fun nv ->
            OpamFile.OPAM.read_opt
              (OpamPath.Switch.installed_opam root switch nv))
          (selections.sel_installed -- selections.sel_pinned)
      else overlays
    in
    { OpamFile.SwitchExport.selections; overlays }
  in
  match filename with
  | None   -> OpamFile.SwitchExport.write_to_channel stdout export
  | Some f -> OpamFile.SwitchExport.write f export

let show () =
  OpamConsole.msg "%s\n"
    (OpamSwitch.to_string (OpamStateConfig.get_switch ()))

let reinstall gt switch =
  log "reinstall switch=%a" (slog OpamSwitch.to_string) switch;

  OpamSwitchState.with_ `Lock_write gt ~switch @@ fun init_st ->

  let switch_root = OpamPath.Switch.root gt.root switch in
  let opam_subdir = OpamPath.Switch.meta gt.root switch in
  let pkg_dirs =
    List.filter ((<>) opam_subdir) (OpamFilename.dirs switch_root)
  in
  List.iter OpamFilename.rmdir pkg_dirs;
  List.iter OpamFilename.remove (OpamFilename.files switch_root);
  OpamFilename.cleandir (OpamPath.Switch.config_dir gt.root switch);
  OpamFilename.cleandir (OpamPath.Switch.installed_opams gt.root switch);
  let st =
    { init_st with
      installed = OpamPackage.Set.empty;
      installed_roots = OpamPackage.Set.empty;
      reinstall = OpamPackage.Set.empty; }
  in
  ignore @@
  import_t { OpamFile.SwitchExport.
             selections = OpamSwitchState.selections st;
             overlays = OpamPackage.Name.Map.empty; }
    st

let import gt switch filename =
  let importfile = match filename with
    | None   -> OpamFile.SwitchExport.read_from_channel stdin
    | Some f -> OpamFile.SwitchExport.read f in
  OpamSwitchState.with_ `Lock_write gt ~switch @@ fun st ->
  ignore (import_t importfile st)

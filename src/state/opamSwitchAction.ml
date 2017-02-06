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
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "SWACT" fmt
let slog = OpamConsole.slog

let gen_switch_config root ?(synopsis="") ?repos switch =
  let vars =
    List.map (fun (s,p) -> OpamVariable.of_string s, S p) [
      ("user" ,
       try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name
       with Not_found -> "user");
      ("group",
       try (Unix.getgrgid (Unix.getgid ())).Unix.gr_name
       with Not_found -> "group");
      ("make" , OpamStateConfig.(Lazy.force !r.makecmd));
      ("os"   , OpamStd.Sys.os_string ());
    ]
  in
  let paths = [
    (Prefix, OpamFilename.Dir.to_string (OpamPath.Switch.root root switch));
  ]
  in
  { OpamFile.Switch_config.
    opam_version = OpamVersion.current_nopatch;
    synopsis;
    variables = vars;
    paths;
    opam_root = Some root;
    repos;
    wrappers = OpamFile.Wrappers.empty; }

let install_switch_config root switch config =
  log "install_switch_config switch=%a" (slog OpamSwitch.to_string) switch;
  OpamFile.Switch_config.write
    (OpamPath.Switch.switch_config root switch)
    config

let create_empty_switch gt ?synopsis ?repos switch =
  log "create_empty_switch at %a" (slog OpamSwitch.to_string) switch;
  let root = gt.root in
  let switch_dir = OpamPath.Switch.root root switch in

  (* Do some clean-up if necessary *)
  if OpamFilename.exists_dir switch_dir then
    failwith (Printf.sprintf "Directory %s already exists"
                (OpamFilename.Dir.to_string switch_dir));

  try
    (* Create base directories *)
    OpamFilename.mkdir switch_dir;

    let config = gen_switch_config root ?synopsis ?repos switch in

    OpamFilename.mkdir (OpamPath.Switch.lib_dir root switch config);
    OpamFilename.mkdir (OpamPath.Switch.stublibs root switch config);
    OpamFilename.mkdir (OpamPath.Switch.toplevel root switch config);
    OpamFilename.mkdir (OpamPath.Switch.build_dir root switch);
    OpamFilename.mkdir (OpamPath.Switch.bin root switch config);
    OpamFilename.mkdir (OpamPath.Switch.sbin root switch config);
    OpamFilename.mkdir (OpamPath.Switch.doc_dir root switch config);
    OpamFilename.mkdir (OpamPath.Switch.man_dir root switch config);
    OpamFilename.mkdir (OpamPath.Switch.install_dir root switch);
    OpamFilename.mkdir (OpamPath.Switch.config_dir root switch);
    List.iter (fun num ->
        OpamFilename.mkdir (OpamPath.Switch.man_dir ~num root switch config)
      ) ["1";"1M";"2";"3";"4";"5";"6";"7";"9"];

    install_switch_config root switch config;

    let root_config =
      OpamFile.Config.with_installed_switches
        (switch::OpamFile.Config.installed_switches gt.config)
        gt.config
    in
    let gt = { gt with config = root_config } in
    OpamGlobalState.write gt;
    gt
  with e ->
    if not (OpamConsole.debug ()) then
      OpamFilename.rmdir switch_dir;
    raise e

let write_selections st =
  if not OpamStateConfig.(!r.dryrun) then
    let f = OpamPath.Switch.selections st.switch_global.root st.switch in
    let env = OpamPath.Switch.environment st.switch_global.root st.switch in
    OpamFile.SwitchSelections.write f (OpamSwitchState.selections st);
    OpamFile.Environment.write env (OpamEnv.compute_updates st)

let add_to_reinstall st ~unpinned_only packages =
  log "add-to-reinstall unpinned_only:%b packages:%a" unpinned_only
    (slog OpamPackage.Set.to_string) packages;
  let root = st.switch_global.root in
  let packages =
    if unpinned_only then
      OpamPackage.Set.filter
        (fun nv -> not (OpamPackage.has_name st.pinned nv.name))
        packages
    else packages
  in
  let reinstall_file = OpamPath.Switch.reinstall root st.switch in
  let current_reinstall = OpamFile.PkgList.safe_read reinstall_file in
  let reinstall = current_reinstall ++ packages %% st.installed in
  if OpamPackage.Set.equal current_reinstall reinstall then ()
  else if OpamPackage.Set.is_empty reinstall then
    OpamFilename.remove (OpamFile.filename reinstall_file)
  else
    OpamFile.PkgList.write reinstall_file reinstall;
  { st with reinstall = st.reinstall ++ packages %% st.installed }

let set_current_switch lock gt ?rt switch =
  if OpamSwitch.is_external switch then
    OpamConsole.error_and_exit
      "Can not set external switch '%s' globally. To set it in the current \
       shell use:\n %s"
      (OpamSwitch.to_string switch)
      (OpamEnv.eval_string gt (Some switch));
  let config = OpamFile.Config.with_switch switch gt.config in
  let gt = { gt with config } in
  OpamGlobalState.write gt;
  let rt = match rt with
    | Some rt -> { rt with repos_global = gt }
    | None -> OpamRepositoryState.load `Lock_none gt
  in
  let st = OpamSwitchState.load lock gt rt switch in
  OpamEnv.write_dynamic_init_scripts st;
  st

let install_metadata st nv =
  let opam = OpamSwitchState.opam st nv in
  OpamFile.OPAM.write
    (OpamPath.Switch.installed_opam st.switch_global.root st.switch nv)
    opam;
  List.iter (fun (f, rel_path, _hash) ->
      let dst =
        OpamFilename.create
          (OpamPath.Switch.installed_opam_files_dir
             st.switch_global.root st.switch nv)
          rel_path
      in
      OpamFilename.mkdir (OpamFilename.dirname dst);
      OpamFilename.copy ~src:f ~dst)
    (OpamFile.OPAM.get_extra_files opam)

let remove_metadata st packages =
  OpamPackage.Set.iter (fun nv ->
      OpamFilename.rmdir
        (OpamPath.Switch.installed_package_dir
           st.switch_global.root st.switch nv))
    packages

let update_switch_state ?installed ?installed_roots ?reinstall ?pinned st =
  let open OpamStd.Option.Op in
  let open OpamPackage.Set.Op in
  let installed = installed +! st.installed in
  let reinstall0 = st.reinstall in
  let reinstall = (reinstall +! reinstall0) %% installed in
  let compiler_packages =
    if OpamPackage.Set.is_empty (st.compiler_packages -- installed) then
      st.compiler_packages
    else (* adjust version of installed compiler packages *)
      let names = OpamPackage.names_of_packages st.compiler_packages in
      let installed_base = OpamPackage.packages_of_names installed names in
      installed_base ++
      (* keep version of uninstalled compiler packages *)
      OpamPackage.packages_of_names st.compiler_packages
        (OpamPackage.Name.Set.diff names
           (OpamPackage.names_of_packages installed_base))
  in
  let old_selections = OpamSwitchState.selections st in
  let st =
    { st with
      installed;
      installed_roots = installed_roots +! st.installed_roots;
      reinstall;
      pinned = pinned +! st.pinned;
      compiler_packages; }
  in
  if not OpamStateConfig.(!r.dryrun) then (
    if OpamSwitchState.selections st <> old_selections then write_selections st;
    if not (OpamPackage.Set.equal reinstall0 reinstall) then
      OpamFile.PkgList.write
        (OpamPath.Switch.reinstall st.switch_global.root st.switch)
        (OpamPackage.Set.filter (OpamSwitchState.is_dev_package st) reinstall)
  );
  st

let add_to_installed st ?(root=false) nv =
  let st =
    update_switch_state st
      ~installed:(OpamPackage.Set.add nv st.installed)
      ~reinstall:(OpamPackage.Set.remove nv st.reinstall)
      ~installed_roots:
        (let roots =
           OpamPackage.Set.filter (fun nv1 -> nv1.name <> nv.name)
             st.installed_roots
         in
         if root then OpamPackage.Set.add nv roots else st.installed_roots)
  in
  let opam = OpamSwitchState.opam st nv in
  let conf =
    OpamFile.Dot_config.safe_read
      (OpamPath.Switch.config st.switch_global.root st.switch nv.name)
  in
  let st = { st with conf_files = OpamPackage.Map.add nv conf st.conf_files } in
  if not OpamStateConfig.(!r.dryrun) then (
    install_metadata st nv;
    if OpamFile.OPAM.env opam <> [] &&
       OpamSwitchState.is_switch_globally_set st
    then
      OpamEnv.write_dynamic_init_scripts st;
  );
  st

let remove_from_installed ?(keep_as_root=false) st nv =
  let rm = OpamPackage.Set.remove nv in
  let st =
    update_switch_state st
      ~installed:(rm st.installed)
      ?installed_roots:(if keep_as_root then None
                        else Some (rm st.installed_roots))
      ~reinstall:(rm st.reinstall)
  in
  let has_setenv =
    match OpamStd.Option.map OpamFile.OPAM.env (OpamSwitchState.opam_opt st nv)
    with Some (_::_) -> true | _ -> false
  in
  if not OpamStateConfig.(!r.dryrun) &&
     has_setenv && OpamSwitchState.is_switch_globally_set st
  then
    (* note: don't remove_metadata just yet *)
    OpamEnv.write_dynamic_init_scripts st;
  { st with conf_files = OpamPackage.Map.remove nv st.conf_files }

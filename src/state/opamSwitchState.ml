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
open OpamStd.Op
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "STATE" fmt
let slog = OpamConsole.slog

open OpamStateTypes

let load_selections gt switch =
  OpamFile.SwitchSelections.safe_read (OpamPath.Switch.selections gt.root switch)

let load_switch_config gt switch =
  let f = OpamPath.Switch.switch_config gt.root switch in
  match OpamFile.Switch_config.read_opt f with
  | Some c -> c
  | None ->
    OpamConsole.error
      "No config file found for switch %s. Switch broken ?"
      (OpamSwitch.to_string switch);
    OpamFile.Switch_config.empty

let compute_available_packages gt switch switch_config ~pinned ~opams =
  (* remove all versions of pinned packages, but the pinned-to version *)
  let pinned_names = OpamPackage.names_of_packages pinned in
  let opams =
    OpamPackage.Map.filter
      (fun nv _ ->
         not (OpamPackage.Name.Set.mem nv.name pinned_names) ||
         OpamPackage.Set.mem nv pinned)
      opams
  in
  let avail_map =
    OpamPackage.Map.filter (fun package opam ->
        OpamFilter.eval_to_bool ~default:false
          (OpamPackageVar.resolve_switch_raw ~package gt switch switch_config)
          (OpamFile.OPAM.available opam))
      opams
  in
  OpamPackage.keys avail_map

let repos_list_raw rt switch_config =
  let global, repos =
    match switch_config.OpamFile.Switch_config.repos with
    | None -> true, OpamGlobalState.repos_list rt.repos_global
    | Some repos -> false, repos
  in
  let found, notfound =
    List.partition (fun r ->
        OpamRepositoryName.Map.mem r rt.repositories)
      repos
  in
  List.iter (fun r ->
      log "Ignoring %s-selected repository %S, no configured repository by \
           this name found"
        (if global then "globally" else "switch")
        (OpamRepositoryName.to_string r))
    notfound;
  found

let repos_list st =
  repos_list_raw st.switch_repos st.switch_config

let load lock_kind gt rt switch =
  let chrono = OpamConsole.timer () in
  log "LOAD-SWITCH-STATE @ %a" (slog OpamSwitch.to_string) switch;
  if not @@ match OpamSwitch.is_external switch with
    | true -> OpamFilename.exists_dir (OpamSwitch.get_root gt.root switch)
    | false -> List.mem switch (OpamFile.Config.installed_switches gt.config)
  then
    (log "The switch %a does not appear to be installed according to %a"
       (slog OpamSwitch.to_string) switch
       (slog @@ OpamFile.to_string @* OpamPath.config) gt.root;

     OpamConsole.error_and_exit
       (if OpamStateConfig.(!r.switch_from = `Command_line) then `Bad_arguments
        else `Configuration_error)
       "The selected switch %s is not installed.%s"
       (OpamSwitch.to_string switch)
     @@ match OpamStateConfig.(!r.switch_from) with
     | `Command_line -> ""
     | `Default ->
       "Please choose a different one using 'opam switch <name>', or use the \
        '--switch <name>' flag."
     | `Env ->
       "Please fix the value of the OPAMSWITCH environment variable, or use \
        the '--switch <name>' flag")
  else
  let lock =
    OpamFilename.flock lock_kind (OpamPath.Switch.lock gt.root switch)
  in
  let switch_config = load_switch_config gt switch in
  if OpamVersion.compare
      (OpamVersion.nopatch (switch_config.OpamFile.Switch_config.opam_version))
      (OpamVersion.nopatch OpamFormatUpgrade.latest_version)
     <> 0 then
    OpamConsole.error_and_exit `Configuration_error
      "Could not load opam switch %s: it reports version %s while %s was \
       expected"
      (OpamSwitch.to_string switch)
      (OpamVersion.to_string
         (switch_config.OpamFile.Switch_config.opam_version))
      (OpamVersion.to_string OpamFormatUpgrade.latest_version);
  let { sel_installed = installed; sel_roots = installed_roots;
        sel_pinned = pinned; sel_compiler = compiler_packages; } =
    load_selections gt switch
  in
  let pinned, pinned_opams =
    OpamPackage.Set.fold (fun nv (pinned,opams) ->
        let overlay_dir =
          OpamPath.Switch.Overlay.package gt.root switch nv.name
        in
        match OpamFileTools.read_opam overlay_dir with
        | None -> (* No overlay => just pinned to a version *)
          OpamPackage.Set.add nv pinned, opams
        | Some o ->
          let version =
            match OpamFile.OPAM.version_opt o with
            | Some v when v <> nv.version ->
              log "warn: %s has conflicting pinning versions between \
                   switch-state (%s) and overlay (%s). Using %s."
                (OpamPackage.Name.to_string nv.name)
                (OpamPackage.Version.to_string nv.version)
                (OpamPackage.Version.to_string v)
                (OpamPackage.Version.to_string v);
              v
            | _ -> nv.version
          in
          let nv = OpamPackage.create nv.name version in
          let o = OpamFile.OPAM.with_version version o in
          OpamPackage.Set.add nv pinned,
          OpamPackage.Map.add nv o opams
      )
      pinned (OpamPackage.Set.empty, OpamPackage.Map.empty)
  in
  let installed_opams =
    OpamPackage.Set.fold (fun nv opams ->
        OpamStd.Option.Op.(
          (OpamFile.OPAM.read_opt
             (OpamPath.Switch.installed_opam gt.root switch nv)
           >>| fun opam -> OpamPackage.Map.add nv opam opams)
          +! opams))
      installed OpamPackage.Map.empty
  in
  let repos_package_index =
    OpamRepositoryState.build_index rt (repos_list_raw rt switch_config)
  in
  let opams =
    OpamPackage.Map.union (fun _ x -> x) repos_package_index pinned_opams
  in
  let packages = OpamPackage.keys opams in
  let available_packages =
    lazy (compute_available_packages gt switch switch_config
            ~pinned ~opams)
  in
  let opams =
    (* Keep definitions of installed packages, but lowest priority, and after
       computing availability *)
    OpamPackage.Map.union (fun _ x -> x) installed_opams opams
  in
  let installed_without_def =
    OpamPackage.Set.fold (fun nv nodef ->
        if OpamPackage.Map.mem nv installed_opams then nodef else
        try
          let o = OpamPackage.Map.find nv opams in
          if lock_kind = `Lock_write then (* auto-repair *)
            (log "Definition missing for installed package %s, \
                  copying from repo"
               (OpamPackage.to_string nv);
             OpamFile.OPAM.write
               (OpamPath.Switch.installed_opam gt.root switch nv) o);
          nodef
        with Not_found -> OpamPackage.Set.add nv nodef)
      installed OpamPackage.Set.empty
  in
  if not (OpamPackage.Set.is_empty installed_without_def) then
    OpamConsole.error
      "No definition found for the following installed packages: %s\n\
       This switch may need to be reinstalled"
      (OpamPackage.Set.to_string installed_without_def);
  let changed =
    (* Note: This doesn't detect changed _dev_ packages, since it's based on the
       metadata or the archive hash changing and they don't have an archive
       hash. Therefore, dev package update needs to add to the reinstall file *)
    OpamPackage.Map.merge (fun _ opam_new opam_installed ->
        match opam_new, opam_installed with
        | Some r, Some i when not (OpamFile.OPAM.effectively_equal i r) ->
          Some ()
        | _ -> None)
      opams installed_opams
    |> OpamPackage.keys
  in
  let changed =
    changed --
    OpamPackage.Set.filter
      (fun nv -> not (OpamPackage.has_name pinned nv.name))
      compiler_packages
  in
  log "Detected changed packages (marked for reinstall): %a"
    (slog OpamPackage.Set.to_string) changed;
  (* Detect and initialise missing switch description *)
  let switch_config =
    if switch_config <> OpamFile.Switch_config.empty &&
       switch_config.OpamFile.Switch_config.synopsis = "" then
      let synopsis =
        match OpamPackage.Set.elements (compiler_packages %% installed_roots)
        with
        | [] -> OpamSwitch.to_string switch
        | [nv] ->
          let open OpamStd.Option.Op in
          (OpamPackage.Map.find_opt nv opams >>= OpamFile.OPAM.synopsis) +!
          OpamPackage.to_string nv
        | pkgs -> OpamStd.List.concat_map " " OpamPackage.to_string pkgs
      in
      let conf = { switch_config with OpamFile.Switch_config.synopsis } in
      if lock_kind = `Lock_write then (* auto-repair *)
        OpamFile.Switch_config.write
          (OpamPath.Switch.switch_config gt.root switch)
          conf;
      conf
    else switch_config
  in
  let conf_files =
    OpamPackage.Set.fold (fun nv acc ->
        OpamPackage.Map.add nv
          (OpamFile.Dot_config.safe_read
             (OpamPath.Switch.config gt.root switch nv.name))
          acc)
      installed OpamPackage.Map.empty
  in
  let ext_files_changed =
    OpamPackage.Map.fold (fun nv conf acc ->
        if
          List.exists (fun (file, hash) ->
              let deleted = not (OpamFilename.exists file) in
              let changed =
                deleted ||
                not (OpamHash.check_file (OpamFilename.to_string file) hash)
              in
              (* /!\ fixme: the package removal instructions won't actually ever
                 be called in this case *)
              if deleted then
                OpamConsole.error
                  "System file %s, which package %s depends upon, \
                   no longer exists.\n\
                   The package has been marked as removed, and opam will \
                   try to reinstall it if necessary, but you should reinstall \
                   its system dependencies first."
                  (OpamFilename.to_string file) (OpamPackage.to_string nv)
              else if changed then
                OpamConsole.warning
                  "File %s, which package %s depends upon, \
                   was changed on your system. \
                   %s has been marked as removed, and will be reinstalled if \
                   necessary."
                  (OpamFilename.to_string file) (OpamPackage.to_string nv)
                  (OpamPackage.name_to_string nv);
              changed)
            (OpamFile.Dot_config.file_depends conf)
        then OpamPackage.Set.add nv acc
        else acc)
      conf_files
      OpamPackage.Set.empty
  in
  let installed =
    installed -- ext_files_changed
  in
  let reinstall =
    OpamFile.PkgList.safe_read (OpamPath.Switch.reinstall gt.root switch) ++
    changed
  in
  let st = {
    switch_global = (gt :> unlocked global_state);
    switch_repos = (rt :> unlocked repos_state);
    switch_lock = lock;
    switch; compiler_packages; switch_config;
    repos_package_index; installed_opams;
    installed; pinned; installed_roots;
    opams; conf_files;
    packages; available_packages; reinstall;
  } in
  log "Switch state loaded in %.3fs" (chrono ());
  st

let load_virtual ?repos_list gt rt =
  let repos_list = match repos_list with
    | Some rl -> rl
    | None -> OpamGlobalState.repos_list gt
  in
  let opams =
    OpamRepositoryState.build_index rt repos_list
  in
  let packages = OpamPackage.keys opams in
  {
    switch_global = (gt :> unlocked global_state);
    switch_repos = (rt :> unlocked repos_state);
    switch_lock = OpamSystem.lock_none;
    switch = OpamSwitch.unset;
    compiler_packages = OpamPackage.Set.empty;
    switch_config = OpamFile.Switch_config.empty;
    installed = OpamPackage.Set.empty;
    installed_opams = OpamPackage.Map.empty;
    pinned = OpamPackage.Set.empty;
    installed_roots = OpamPackage.Set.empty;
    repos_package_index = opams;
    opams;
    conf_files = OpamPackage.Map.empty;
    packages;
    available_packages = lazy packages;
    reinstall = OpamPackage.Set.empty;
  }

let selections st =
  { sel_installed = st.installed;
    sel_roots = st.installed_roots;
    sel_compiler = st.compiler_packages;
    sel_pinned = st.pinned; }

let unlock st =
  OpamSystem.funlock st.switch_lock;
  (st :> unlocked switch_state)

let with_write_lock ?dontblock st f =
  let ret, st =
    OpamFilename.with_flock_upgrade `Lock_write ?dontblock st.switch_lock
    @@ fun _ -> f ({ st with switch_lock = st.switch_lock } : rw switch_state)
    (* We don't actually change the field value, but this makes restricting the
       phantom lock type possible*)
  in
  ret, { st with switch_lock = st.switch_lock }

let opam st nv = OpamPackage.Map.find nv st.opams

let opam_opt st nv = try Some (opam st nv) with Not_found -> None

let descr_opt st nv =
  OpamStd.Option.Op.(opam_opt st nv >>= OpamFile.OPAM.descr)

let descr st nv =
  OpamStd.Option.Op.(descr_opt st nv +! OpamFile.Descr.empty)

let url st nv =
  OpamStd.Option.Op.(opam_opt st nv >>= OpamFile.OPAM.url)

let primary_url st nv =
  OpamStd.Option.Op.(url st nv >>| OpamFile.URL.url)

let files st nv =
  match opam_opt st nv with
  | None -> []
  | Some opam ->
    List.map (fun (file,_base,_hash) -> file)
      (OpamFile.OPAM.get_extra_files opam)

let package_config st name =
  OpamPackage.Map.find
    (OpamPackage.package_of_name st.installed name)
    st.conf_files

let is_name_installed st name =
  OpamPackage.has_name st.installed name

let find_installed_package_by_name st name =
  OpamPackage.package_of_name st.installed name

let packages_of_atoms st atoms = OpamFormula.packages_of_atoms st.packages atoms

let get_package st name =
  try OpamPinned.package st name with Not_found ->
  try find_installed_package_by_name st name with Not_found ->
  try OpamPackage.max_version (Lazy.force st.available_packages) name
  with Not_found ->
    OpamPackage.max_version st.packages name

let is_dev_package st nv =
  match opam_opt st nv with
  | Some opam -> OpamPackageVar.is_dev_package st opam
  | None -> false

let is_pinned st name =
  OpamPackage.has_name st.pinned name

let is_version_pinned st name =
  match OpamPackage.package_of_name_opt st.pinned name with
  | None -> false
  | Some nv ->
    match opam_opt st nv with
    | Some opam ->
      OpamPackage.Map.find_opt nv st.repos_package_index = Some opam
    | None -> false

let source_dir st nv =
  if OpamPackage.Set.mem nv st.pinned
  then OpamPath.Switch.pinned_package st.switch_global.root st.switch nv.name
  else OpamPath.Switch.sources st.switch_global.root st.switch nv

let depexts st nv =
  let env v = OpamPackageVar.resolve_switch ~package:nv st v in
  match opam_opt st nv with
  | None -> OpamStd.String.Set.empty
  | Some opam ->
    List.fold_left (fun depexts (names, filter) ->
        if OpamFilter.eval_to_bool ~default:false env filter then
          List.fold_left (fun depexts n -> OpamStd.String.Set.add n depexts)
            depexts names
        else depexts)
      OpamStd.String.Set.empty
      (OpamFile.OPAM.depexts opam)

let dev_packages st =
  OpamPackage.Set.filter (is_dev_package st)
    (st.installed ++ OpamPinned.packages st)

let conflicts_with st subset =
  let forward_conflicts, conflict_classes =
    OpamPackage.Set.fold (fun nv (cf,cfc) ->
        try
          let opam = OpamPackage.Map.find nv st.opams in
          let conflicts =
            OpamFilter.filter_formula ~default:false
              (OpamPackageVar.resolve_switch ~package:nv st)
              (OpamFile.OPAM.conflicts opam)
          in
          OpamFormula.ors [cf; conflicts],
          List.fold_right OpamPackage.Name.Set.add
            (OpamFile.OPAM.conflict_class opam) cfc
        with Not_found -> cf, cfc)
      subset (OpamFormula.Empty, OpamPackage.Name.Set.empty)
  in
  OpamPackage.Set.filter
    (fun nv ->
       not (OpamPackage.has_name subset nv.name) &&
       (OpamFormula.verifies forward_conflicts nv ||
        let opam = OpamPackage.Map.find nv st.opams in
        List.exists (fun cl -> OpamPackage.Name.Set.mem cl conflict_classes)
          (OpamFile.OPAM.conflict_class opam)
        ||
        let backwards_conflicts =
          OpamFilter.filter_formula ~default:false
            (OpamPackageVar.resolve_switch ~package:nv st)
            (OpamFile.OPAM.conflicts opam)
        in
        OpamPackage.Set.exists
          (OpamFormula.verifies backwards_conflicts) subset))

let remove_conflicts st subset pkgs =
  pkgs -- conflicts_with st subset pkgs

let get_conflicts st packages opams_map =
  let conflict_classes =
    OpamPackage.Map.fold (fun nv opam acc ->
        List.fold_left (fun acc cc ->
            OpamPackage.Name.Map.update cc
              (OpamPackage.Set.add nv) OpamPackage.Set.empty acc)
          acc
          (OpamFile.OPAM.conflict_class opam))
      opams_map
      OpamPackage.Name.Map.empty
  in
  let conflict_class_formulas =
    OpamPackage.Name.Map.map (fun pkgs ->
        OpamPackage.to_map pkgs |>
        OpamPackage.Name.Map.mapi (fun name versions ->
            let all_versions = OpamPackage.versions_of_name packages name in
            if OpamPackage.Version.Set.equal versions all_versions then Empty
            else
              (* OpamFormula.simplify_version_set all_versions (*a possible optimisation?*) *)
                (OpamFormula.ors
                   (List.map (fun v -> Atom (`Eq, v))
                      (OpamPackage.Version.Set.elements versions)))))
      conflict_classes
  in
  OpamPackage.Map.fold (fun nv opam acc ->
      let conflicts =
        OpamFilter.filter_formula ~default:false
          (OpamPackageVar.resolve_switch ~package:nv st)
          (OpamFile.OPAM.conflicts opam)
      in
      let conflicts =
        List.fold_left (fun acc cl ->
            let cmap =
              OpamPackage.Name.Map.find cl conflict_class_formulas |>
              OpamPackage.Name.Map.remove nv.name
            in
            OpamPackage.Name.Map.fold
              (fun name vformula acc ->
                 OpamFormula.ors [acc; Atom (name, vformula)])
              cmap acc)
          conflicts
          (OpamFile.OPAM.conflict_class opam)
      in
      OpamPackage.Map.add nv conflicts acc)
    opams_map
    OpamPackage.Map.empty

let universe st
    ?(test=OpamStateConfig.(!r.build_test))
    ?(doc=OpamStateConfig.(!r.build_doc))
    ?(force_dev_deps=false)
    ?reinstall
    ~requested
    user_action =
  let requested_allpkgs =
    OpamPackage.packages_of_names st.packages requested
  in
  let env nv v =
    if List.mem v OpamPackageVar.predefined_depends_variables then
      match OpamVariable.Full.to_string v with
      | "dev" ->
        Some (B (force_dev_deps || is_dev_package st nv))
      | "with-test" ->
        Some (B (test && OpamPackage.Set.mem nv requested_allpkgs))
      | "with-doc" ->
        Some (B (doc && OpamPackage.Set.mem nv requested_allpkgs))
      | _ -> None (* Computation delayed to the solver *)
    else
    let r = OpamPackageVar.resolve_switch ~package:nv st v in
    if r = None then
      (if OpamFormatConfig.(!r.strict) then
         OpamConsole.error_and_exit `File_error
           "undefined filter variable in dependencies of %s: %s"
       else
         log
           "ERR: undefined filter variable in dependencies of %s: %s")
        (OpamPackage.to_string nv) (OpamVariable.Full.to_string v);
    r
  in
  let get_deps f opams =
    OpamPackage.Map.mapi (fun nv opam ->
        OpamFilter.partial_filter_formula (env nv) (f opam)
      ) opams
  in
  let u_depends =
    let depend =
      let ignored = OpamStateConfig.(!r.ignore_constraints_on) in
      if OpamPackage.Name.Set.is_empty ignored then OpamFile.OPAM.depends
      else fun opam ->
        OpamFormula.map (fun (name, cstr as atom) ->
            if OpamPackage.Name.Set.mem name ignored then
              let cstr =
                OpamFormula.map
                  (function Constraint _ -> Empty | Filter _ as f -> Atom f)
                  cstr
              in
              Atom (name, cstr)
            else Atom atom)
          (OpamFile.OPAM.depends opam)
    in
    get_deps depend st.opams
  in
  let u_conflicts = get_conflicts st st.packages st.opams in
  let base =
    if OpamStateConfig.(!r.unlock_base) then OpamPackage.Set.empty
    else st.compiler_packages
  in
  let u_available =
    remove_conflicts st base (Lazy.force st.available_packages)
  in
  let u_reinstall = match reinstall with
    | Some set -> set
    | None ->
      OpamPackage.Set.filter
        (fun nv -> OpamPackage.Name.Set.mem nv.name requested)
        st.reinstall
  in
  let u =
{
  u_packages  = st.packages;
  u_action = user_action;
  u_installed = st.installed;
  u_available;
  u_depends;
  u_depopts = get_deps OpamFile.OPAM.depopts st.opams;
  u_conflicts;
  u_installed_roots = st.installed_roots;
  u_pinned    = OpamPinned.packages st;
  u_base      = base;
  u_reinstall;
  u_attrs     = ["opam-query", requested_allpkgs];
}
  in
  u

let dump_pef_state st oc =
  let conflicts = get_conflicts st st.packages st.opams in
  let print_def nv opam =
    Printf.fprintf oc "package: %s\n" (OpamPackage.name_to_string nv);
    Printf.fprintf oc "version: %s\n" (OpamPackage.version_to_string nv);
    let installed = OpamPackage.Set.mem nv st.installed in
    (* let root = OpamPackage.Set.mem nv st.installed_roots in *)
    let base = OpamPackage.Set.mem nv st.compiler_packages in
    let pinned = OpamPackage.Set.mem nv st.pinned in
    let available = OpamPackage.Set.mem nv (Lazy.force st.available_packages) in
    let reinstall = OpamPackage.Set.mem nv st.reinstall in
    let dev = OpamPackageVar.is_dev_package st opam in
    (* current state *)
    Printf.fprintf oc "available: %b\n" available;
    if installed then output_string oc "installed: true\n";
    if pinned then output_string oc "pinned: true\n";
    if base then output_string oc "base: true\n";
    if reinstall then output_string oc "reinstall: true\n";

    (* metadata (resolved for the current switch) *)
    OpamStd.List.concat_map ~left:"maintainer: " ~right:"\n" ~nil:"" " , "
      String.escaped (OpamFile.OPAM.maintainer opam) |>
    output_string oc;

    OpamFile.OPAM.depends opam |>
    OpamPackageVar.filter_depends_formula ~default:false ~dev
      ~env:(OpamPackageVar.resolve_switch ~package:nv st) |>
    OpamFormula.to_cnf |>
    OpamStd.List.concat_map ~left:"depends: " ~right:"\n" ~nil:"" " , "
      (OpamStd.List.concat_map " | " OpamFormula.string_of_atom) |>
    output_string oc;

    OpamFile.OPAM.depopts opam |>
    OpamPackageVar.filter_depends_formula ~default:false ~dev
      ~env:(OpamPackageVar.resolve_switch ~package:nv st) |>
    OpamFormula.to_cnf |>
    OpamStd.List.concat_map ~left:"recommends: " ~right:"\n" ~nil:"" " , "
      (OpamStd.List.concat_map " | " OpamFormula.string_of_atom) |>
    output_string oc;

    OpamFormula.ors
      [Atom (nv.name, Empty); OpamPackage.Map.find nv conflicts] |>
    OpamFormula.set_to_disjunction st.packages |>
    OpamStd.List.concat_map ~left:"conflicts: " ~right:"\n" ~nil:"" " , "
      OpamFormula.string_of_atom |>
    output_string oc;

    output_string oc "\n";
  in
  OpamPackage.Map.iter print_def st.opams


(* User-directed helpers *)

let is_switch_globally_set st =
  OpamFile.Config.switch st.switch_global.config = Some st.switch

let not_found_message st (name, cstr) =
  match cstr with
  | Some (relop,v) when OpamPackage.has_name st.packages name ->
    Printf.sprintf "Package %s has no version %s%s."
      (OpamPackage.Name.to_string name)
      (match relop with `Eq -> "" | r -> OpamPrinter.relop r)
      (OpamPackage.Version.to_string v)
  | _ ->
    Printf.sprintf "No package named %s found."
      (OpamPackage.Name.to_string name)

(* Display a meaningful error for an unavailable package *)
let unavailable_reason st ?(default="") (name, vformula) =
  let candidates = OpamPackage.packages_of_name st.packages name in
  let candidates =
    OpamPackage.Set.filter
      (fun nv -> OpamFormula.check_version_formula vformula nv.version)
      candidates
  in
  if OpamPackage.Set.is_empty candidates then
    (if OpamPackage.has_name st.packages name then "no matching version"
     else "unknown package")
  else
  let nv =
    try OpamPinned.package st name
    with Not_found ->
      match vformula with
      | Atom (_, v) when
          OpamPackage.Set.mem (OpamPackage.create name v) candidates ->
        OpamPackage.create name v
      | _ -> OpamPackage.max_version candidates name
  in
  match opam_opt st nv with
  | None -> "no package definition found"
  | Some opam ->
    let avail = OpamFile.OPAM.available opam in
    if not (OpamPackage.Set.mem nv candidates) then
      Printf.sprintf
        "not available because the package is pinned to version %s"
        (OpamPackage.version_to_string nv)
    else if not (OpamFilter.eval_to_bool ~default:false
                   (OpamPackageVar.resolve_switch ~package:nv st)
                   avail)
    then
      Printf.sprintf "unmet availability conditions%s%s"
        (if OpamPackage.Set.cardinal candidates = 1 then ": "
         else ", e.g. ")
        (OpamFilter.to_string avail)
    else if OpamPackage.has_name
        (Lazy.force st.available_packages --
         remove_conflicts st st.compiler_packages
           (Lazy.force st.available_packages))
        name
    then
      "conflict with the base packages of this switch"
    else if OpamPackage.has_name st.compiler_packages name &&
            not OpamStateConfig.(!r.unlock_base) then
      "base of this switch, can't be changed (use `--unlock-base' to force)"
    else
      default

let update_package_metadata nv opam st =
  { st with
    opams = OpamPackage.Map.add nv opam st.opams;
    packages = OpamPackage.Set.add nv st.packages;
    available_packages = lazy (
      if OpamFilter.eval_to_bool ~default:false
          (OpamPackageVar.resolve_switch_raw ~package:nv
             st.switch_global st.switch st.switch_config)
          (OpamFile.OPAM.available opam)
      then OpamPackage.Set.add nv (Lazy.force st.available_packages)
      else OpamPackage.Set.remove nv (Lazy.force st.available_packages)
    );
    reinstall =
      (match OpamPackage.Map.find_opt nv st.installed_opams with
       | Some inst ->
         if OpamFile.OPAM.effectively_equal inst opam
         then OpamPackage.Set.remove nv (st.reinstall)
         else OpamPackage.Set.add nv (st.reinstall)
       | _ -> st.reinstall);
  }

let remove_package_metadata nv st =
  { st with
    opams = OpamPackage.Map.remove nv st.opams;
    packages = OpamPackage.Set.remove nv st.packages;
    available_packages =
      lazy (OpamPackage.Set.remove nv (Lazy.force st.available_packages));
  }

let update_pin nv opam st =
  let version =
    OpamStd.Option.default nv.version (OpamFile.OPAM.version_opt opam)
  in
  let nv = OpamPackage.create nv.name version in
  update_package_metadata nv opam @@
  { st with
    pinned =
      OpamPackage.Set.add nv
        (OpamPackage.filter_name_out st.pinned nv.name);
    available_packages = lazy (
      OpamPackage.filter_name_out (Lazy.force st.available_packages) nv.name
    );
  }

let do_backup lock st = match lock with
  | `Lock_write ->
    let file = OpamPath.Switch.backup st.switch_global.root st.switch in
    let previous_selections = selections st in
    OpamFile.SwitchSelections.write file previous_selections;
    (function
      | true -> OpamFilename.remove (OpamFile.filename file)
      | false ->
        (* Reload, in order to skip the message if there were no changes *)
        let new_selections = load_selections st.switch_global st.switch in
        if new_selections.sel_installed = previous_selections.sel_installed
        then OpamFilename.remove (OpamFile.filename file)
        else
          OpamConsole.errmsg "%s"
            (OpamStd.Format.reformat
               (Printf.sprintf
                  "\nThe former state can be restored with:\n\
                  \    %s switch import %S\n"
                  Sys.argv.(0) (OpamFile.to_string file) ^
                if OpamPackage.Set.is_empty
                    (new_selections.sel_roots -- new_selections.sel_installed)
                then "" else
                  Printf.sprintf
                    "Or you can retry to install your package selection with:\n\
                    \    %s install --restore\n"
                  Sys.argv.(0))))
  | _ -> fun _ -> ()

let with_ lock ?rt ?(switch=OpamStateConfig.get_switch ()) gt f =
  (match rt with
   | Some rt -> fun f -> f (rt :> unlocked repos_state)
   | None -> OpamRepositoryState.with_ `Lock_none gt)
  @@ fun rt ->
  let st = load lock gt rt switch in
  let cleanup_backup = do_backup lock st in
  try let r = f st in ignore (unlock st); cleanup_backup true; r
  with e ->
    OpamStd.Exn.finalise e @@ fun () ->
    ignore (unlock st);
    if not OpamCoreConfig.(!r.keep_log_dir) then cleanup_backup false

let update_repositories gt update_fun switch =
  OpamFilename.with_flock `Lock_write (OpamPath.Switch.lock gt.root switch)
  @@ fun _ ->
  let conf = load_switch_config gt switch in
  let repos =
    match conf.OpamFile.Switch_config.repos with
    | None -> OpamGlobalState.repos_list gt
    | Some repos -> repos
  in
  let conf =
    { conf with
      OpamFile.Switch_config.repos = Some (update_fun repos) }
  in
  OpamFile.Switch_config.write
    (OpamPath.Switch.switch_config gt.root switch)
    conf

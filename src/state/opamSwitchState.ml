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
open OpamTypesBase
open OpamStd.Op
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "STATE" fmt
let slog = OpamConsole.slog

open OpamStateTypes

let load_selections gt switch =
  OpamFile.SwitchSelections.safe_read (OpamPath.Switch.selections gt.root switch)

let load_switch_config gt switch =
  let f = OpamPath.Switch.global_config gt.root switch in
  match OpamFile.Dot_config.read_opt f with
  | Some c -> c
  | None ->
    OpamConsole.error
      "No global config file found for switch %s. Switch broken ?"
      (OpamSwitch.to_string switch);
    OpamFile.Dot_config.empty

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
    OpamPackage.Map.filter (fun _ opam ->
        OpamFilter.eval_to_bool ~default:false
          (OpamPackageVar.resolve_switch_raw gt switch switch_config)
          (OpamFile.OPAM.available opam))
      opams
  in
  OpamPackage.keys avail_map

let load lock_kind gt rt switch =
  let chrono = OpamConsole.timer () in
  log "LOAD-SWITCH-STATE";

  if not (List.mem switch (OpamFile.Config.installed_switches gt.config)) then
    (log "The switch %a does not appear to be installed according to %a"
       (slog OpamSwitch.to_string) switch
       (slog @@ OpamFile.to_string @* OpamPath.config) gt.root;
     OpamSwitch.not_installed switch)
  else
  let lock =
    OpamFilename.flock lock_kind (OpamPath.Switch.lock gt.root switch)
  in
  let switch_config = load_switch_config gt switch in
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
    OpamRepositoryState.build_index rt (OpamGlobalState.repos_list gt)
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
              let changed = deleted || OpamFilename.digest file <> hash in
              if deleted then
                OpamConsole.error
                  "System file %s, which package %s depends upon, \
                   no longer exists.\n\
                   The package has been marked for reinstallation, but you \
                   should reinstall its system dependencies first."
                  (OpamFilename.to_string file) (OpamPackage.to_string nv)
              else if changed then
                OpamConsole.warning
                  "File %s was changed on your system. \
                   %s has been marked for reinstallation."
                  (OpamFilename.to_string file) (OpamPackage.to_string nv);
              changed)
            (OpamFile.Dot_config.file_depends conf)
        then OpamPackage.Set.add nv acc
        else acc)
      conf_files
      OpamPackage.Set.empty
  in
  let reinstall =
    OpamFile.PkgList.safe_read (OpamPath.Switch.reinstall gt.root switch) ++
    changed ++
    ext_files_changed
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

let load_virtual gt rt =
  let opams =
    OpamRepositoryState.build_index rt (OpamGlobalState.repos_list gt)
  in
  let packages = OpamPackage.keys opams in
  {
    switch_global = (gt :> unlocked global_state);
    switch_repos = (rt :> unlocked repos_state);
    switch_lock = OpamSystem.lock_none;
    switch = OpamSwitch.of_string "none";
    compiler_packages = OpamPackage.Set.empty;
    switch_config = OpamFile.Dot_config.empty;
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
  OpamFilename.with_flock_upgrade `Lock_write ?dontblock st.switch_lock @@ fun () ->
  f ({ st with switch_lock = st.switch_lock } : rw switch_state)
(* We don't actually change the field value, but this makes restricting the
   phantom lock type possible*)


let opam st nv = OpamPackage.Map.find nv st.opams

let opam_opt st nv = try Some (opam st nv) with Not_found -> None

let descr_opt st nv =
  OpamStd.Option.Op.(opam_opt st nv >>= OpamFile.OPAM.descr)

let descr st nv =
  OpamStd.Option.Op.(descr_opt st nv +! OpamFile.Descr.empty)

let url st nv =
  OpamStd.Option.Op.(opam_opt st nv >>= OpamFile.OPAM.url)

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

let packages_of_atoms st atoms =
  (* Conjunction for constraints over the same name, but disjunction on the
     package names *)
  let by_name =
    List.fold_left (fun acc (n,_ as atom) ->
        OpamPackage.Name.Map.update n (fun a -> atom::a) [] acc)
      OpamPackage.Name.Map.empty atoms
  in
  OpamPackage.Name.Map.fold (fun name atoms acc ->
      acc ++
      OpamPackage.Set.filter
        (fun nv -> List.for_all (fun a -> OpamFormula.check a nv) atoms)
        (OpamPackage.packages_of_name st.packages name))
    by_name OpamPackage.Set.empty

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

let dev_packages st =
  OpamPackage.Set.filter (is_dev_package st)
    (st.installed ++ OpamPinned.packages st)

let universe st action =
  let env nv v =
    if List.mem v OpamPackageVar.predefined_depends_variables then None else
    let r = OpamPackageVar.resolve_switch st v in
    if r = None then
      (if OpamFormatConfig.(!r.strict) then
         OpamConsole.error_and_exit
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
{
  u_packages  = st.packages;
  u_action    = action;
  u_installed = st.installed;
  u_available = Lazy.force st.available_packages;
  u_depends   = get_deps OpamFile.OPAM.depends st.opams;
  u_depopts   = get_deps OpamFile.OPAM.depopts st.opams;
  u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts st.opams;
  u_installed_roots = st.installed_roots;
  u_pinned    = OpamPinned.packages st;
  u_dev       = dev_packages st;
  u_base      = st.compiler_packages;
  u_attrs     = [];
  u_test      = OpamStateConfig.(!r.build_test);
  u_doc       = OpamStateConfig.(!r.build_doc);
}



(* User-directed helpers *)

let is_switch_globally_set st =
  OpamFile.Config.switch st.switch_global.config = Some st.switch

let not_found_message st (name, cstr) =
  match cstr with
  | Some (relop,v) when OpamPackage.has_name st.packages name ->
    Printf.sprintf "Package %s has no version %s%s."
      (OpamPackage.Name.to_string name)
      (match relop with `Eq -> "" | r -> string_of_relop r)
      (OpamPackage.Version.to_string v)
  | _ ->
    Printf.sprintf "No package named %s found."
      (OpamPackage.Name.to_string name)

(* Display a meaningful error for an unavailable package *)
let unavailable_reason st (name, _ as atom) =
  let candidates =
    OpamPackage.Set.filter (OpamFormula.check atom) st.packages in
  if OpamPackage.Set.is_empty candidates then
    Printf.sprintf "No package matching \"%s\" found"
      (OpamFormula.short_string_of_atom atom)
  else
  let nv = OpamPackage.max_version candidates name in
  let avail = OpamFile.OPAM.available (opam st nv) in
  if not (OpamFilter.eval_to_bool ~default:false
            (OpamPackageVar.resolve_switch st)
            avail)
  then
    Printf.sprintf "%s has unmet availability conditions: %s"
      (OpamFormula.short_string_of_atom atom)
      (OpamFilter.to_string avail)
  else
  try
    let nv = OpamPackage.package_of_name st.pinned name in
    Printf.sprintf
      "%s is not available because the package is pinned to %s."
      (OpamFormula.short_string_of_atom atom)
      (OpamPackage.to_string nv)
  with Not_found ->
    not_found_message st atom

let update_package_metadata nv opam st =
  { st with
    opams = OpamPackage.Map.add nv opam st.opams;
    packages = OpamPackage.Set.add nv st.packages;
    available_packages = lazy (
      if OpamFilter.eval_to_bool ~default:false
          (OpamPackageVar.resolve_switch_raw
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
    OpamFile.SwitchSelections.write file (selections st);
    (function
      | true -> OpamFilename.remove (OpamFile.filename file)
      | false ->
        (* Reload, in order to skip the message if there were no changes *)
        let st1 = load `Lock_none st.switch_global st.switch_repos st.switch in
        if OpamPackage.Set.equal st.installed st1.installed &&
           OpamPackage.Set.equal st.installed_roots st1.installed_roots &&
           OpamPackage.Set.equal st.compiler_packages st1.compiler_packages &&
           OpamPackage.Set.equal st.pinned st1.pinned
        then OpamFilename.remove (OpamFile.filename file)
        else
          prerr_string
            (OpamStd.Format.reformat
               (Printf.sprintf
                  "\nThe former state can be restored with:\n    \
                   %s switch import %S\n%!"
                  Sys.argv.(0) (OpamFile.to_string file))))
  | _ -> fun _ -> ()

let with_ lock ?rt ?(switch=OpamStateConfig.get_switch ()) gt f =
  (match rt with
   | Some rt -> fun f -> f (rt :> unlocked repos_state)
   | None -> OpamRepositoryState.with_ `Lock_none gt)
  @@ fun rt ->
  let st = load lock gt rt switch in
  let cleanup_backup = do_backup lock st in
  try let r = f st in ignore (unlock st); cleanup_backup true; r
  with e -> ignore (unlock st); cleanup_backup false; raise e

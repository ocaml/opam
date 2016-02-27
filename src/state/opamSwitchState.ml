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
open OpamFilename.Op
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

let load ?(lock=Lock_readonly) gt rt switch =
  let chrono = OpamConsole.timer () in
  log "LOAD-SWITCH-STATE";

  if not (List.mem switch (OpamFile.Config.installed_switches gt.config)) then
    (log "The switch %a does not appear to be installed according to %a"
       (slog OpamSwitch.to_string) switch
       (slog @@ OpamFile.to_string @* OpamPath.config) gt.root;
     OpamSwitch.not_installed switch)
  else
  let switch_config = load_switch_config gt switch in
  let { sel_installed = installed; sel_roots = installed_roots;
        sel_pinned = pinned; sel_compiler = compiler_packages; } =
    load_selections gt switch
  in
  let pinned, pinned_opams =
    (* Pinned packages with overlays *)
    OpamPackage.Name.Map.fold (fun name (version,pin) (pinned,opams) ->
        let nv = OpamPackage.create name version in
        let overlay_dir = OpamPath.Switch.Overlay.package gt.root switch name in
        match OpamFileHandling.read_opam overlay_dir with
        | None ->
          OpamPackage.Name.Map.add name (version, pin) pinned,
          opams
        | Some o ->
          let version, o =
            match pin, OpamFile.OPAM.version_opt o with
            | Version version, Some v when v <> version ->
              log "warn: %s pinned to %s but has overlay at %s"
                (OpamPackage.Name.to_string name)
                (OpamPackage.Version.to_string version)
                (OpamPackage.Version.to_string v);
              version, OpamFile.OPAM.with_version o version
            | Version _, _ ->
              version, OpamFile.OPAM.with_version o version
            | Source _, Some v when v <> version ->
              log "warn: updating version of pinned %s as in overlay (was: %s)"
                (OpamPackage.to_string nv)
                (OpamPackage.Version.to_string v);
              v, o
            | Source _, _ ->
              version, o
          in
          OpamPackage.Name.Map.add name (version,pin) pinned,
          OpamPackage.Map.add (OpamPackage.create name version) o opams
      )
      pinned (OpamPackage.Name.Map.empty, OpamPackage.Map.empty)
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
  let opams =
    (* Add/override overlays of pinned packages and local mirror of metadata for
       all installed packages (from ~/.opam/switch/.opam-switch/packages) *)
    let ( ++ ) = OpamPackage.Map.union (fun _ x -> x) in
    rt.repo_opams ++ pinned_opams ++ installed_opams
  in
  let packages = OpamPackage.keys opams in
  let available_packages = lazy (
    let pinned_names = OpamPackage.Name.(Set.of_list (Map.keys pinned)) in
    let from_repos =
      OpamPackage.Map.filter
        (fun nv _ ->
           not (OpamPackage.Name.Set.mem nv.name pinned_names))
        rt.repo_opams
    in
    let all_with_metadata =
      OpamPackage.Map.union (fun _ _ -> assert false) from_repos pinned_opams
    in
    let avail_map =
      OpamPackage.Map.filter (fun _ opam ->
          OpamFilter.eval_to_bool ~default:false
            (OpamPackageVar.resolve_switch_raw gt switch switch_config)
            (OpamFile.OPAM.available opam))
        all_with_metadata
    in
    OpamPackage.keys avail_map
  ) in
  let changed =
    (* Note: This doesn't detect changed _dev_ packages, since it's based on the
       metadata or the archive hash changing and they don't have an archive
       hash. *)
    OpamPackage.Map.merge (fun _ opam_repo opam_installed ->
        match opam_repo, opam_installed with
        | Some r, Some i
          when OpamFile.OPAM.effective_part i <> OpamFile.OPAM.effective_part r ->
          Some ()
        | _ -> None)
      rt.repo_opams installed_opams
    |> OpamPackage.keys
  in
  let ext_files_changed =
    OpamPackage.Map.filter (fun nv _ ->
        let conf = (* !X todo: cache this and use for variables resolution *)
          OpamFile.Dot_config.safe_read
            (OpamPath.Switch.config gt.root switch nv.name)
        in
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
          (OpamFile.Dot_config.file_depends conf))
      installed_opams
    |> OpamPackage.keys
  in
  let reinstall =
    OpamFile.PkgList.safe_read (OpamPath.Switch.reinstall gt.root switch) ++
    changed ++
    ext_files_changed
  in
  let st = {
    switch_global = gt;
    switch_repos = rt;
    switch_lock = lock; switch; compiler_packages; switch_config;
    installed; pinned; installed_roots; opams; packages;
    available_packages; reinstall;
  } in
  log "Switch state loaded in %.3fs" (chrono ());
  (* !X check system dependencies of installed packages *)
  st
  (*
  (* Check whether the system compiler has been updated *)
  if system_needs_upgrade t then (
    reinstall_system_compiler t;
    if OpamConsole.confirm "\nSystem update successful. Go on with %S ?"
        (String.concat " " (Array.to_list Sys.argv))
    then t
    else OpamStd.Sys.exit 0
  ) else
    t
*)

let load_virtual gt rt =
  let opams = rt.repo_opams in
  let packages = OpamPackage.keys opams in
  {
    switch_global = gt;
    switch_repos = rt;
    switch_lock = Lock_none;
    switch = OpamSwitch.of_string "none";
    compiler_packages = OpamPackage.Set.empty;
    switch_config = OpamFile.Dot_config.empty;
    installed = OpamPackage.Set.empty;
    pinned = OpamPackage.Name.Map.empty;
    installed_roots = OpamPackage.Set.empty;
    opams;
    packages;
    available_packages = lazy packages;
    reinstall = OpamPackage.Set.empty;
  }

let selections st =
  { sel_installed = st.installed;
    sel_roots = st.installed_roots;
    sel_compiler = st.compiler_packages;
    sel_pinned = st.pinned; }

let opam st nv = OpamPackage.Map.find nv st.opams

let opam_opt st nv = try Some (opam st nv) with Not_found -> None

let descr_opt st nv =
  OpamStd.Option.Op.(opam_opt st nv >>= OpamFile.OPAM.descr)

let descr st nv =
  OpamStd.Option.Op.(descr_opt st nv +! OpamFile.Descr.empty)

let url st nv =
  OpamStd.Option.Op.(opam_opt st nv >>= OpamFile.OPAM.url)

let files st nv =
  OpamStd.Option.Op.(
    opam_opt st nv >>= OpamFile.OPAM.metadata_dir >>|
    (fun dir -> dir / "files" ) >>=
    OpamFilename.opt_dir
  )

let is_name_installed st name =
  OpamPackage.Set.exists (fun nv -> nv.name = name) st.installed

let find_installed_package_by_name st name =
  OpamPackage.Set.find (fun nv -> nv.name = name) st.installed

let packages_of_atoms st atoms =
  let check_atoms nv =
    let name = nv.name in
    let atoms = List.filter (fun (n,_) -> n = name) atoms in
    atoms <> [] && List.for_all (fun a -> OpamFormula.check a nv) atoms in
  OpamPackage.Set.filter check_atoms st.packages

let get_package st name =
  try OpamPinned.package st name with Not_found ->
  try find_installed_package_by_name st name with Not_found ->
  try OpamPackage.max_version (Lazy.force st.available_packages) name
  with Not_found ->
    OpamPackage.max_version st.packages name

let is_dev_package st nv =
  match url st nv with
  | None -> false
  | Some urlf ->
    match OpamFile.URL.(url urlf, checksum urlf) with
    | { OpamUrl.backend = `http; _ }, _ | _, Some _ -> false
    | _, None -> true

let dev_packages st =
  OpamPackage.Set.filter (is_dev_package st)
    (st.installed ++ OpamPinned.packages st)

let universe st action = {
  u_packages  = st.packages;
  u_action    = action;
  u_installed = st.installed;
  u_available = Lazy.force st.available_packages;
  u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends st.opams;
  u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts st.opams;
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
      (OpamPackage.Name.to_string name)
      (OpamFilter.to_string avail)
  else
  try
    let (version, pin) = OpamPackage.Name.Map.find name st.pinned in
    Printf.sprintf
      "%s is not available because the package is pinned to %s."
      (OpamFormula.short_string_of_atom atom)
      (match pin with
       | Version v ->
         Printf.sprintf "version %s" (OpamPackage.Version.to_string v)
       | _ ->
         Printf.sprintf "%s, version %s" (string_of_pin_option pin)
           (OpamPackage.Version.to_string version))
  with Not_found ->
    not_found_message st atom

let load_full_compat _ switch =
  let gt = OpamGlobalState.load () in
  let rt = OpamRepositoryState.load gt in
  load gt rt switch

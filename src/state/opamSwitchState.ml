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

let load_state_file gt switch =
    OpamFile.State.safe_read (OpamPath.Switch.state gt.root switch)

let load_switch_config gt switch =
  let f = OpamPath.Switch.global_config gt.root switch in
  if OpamFilename.exists f then OpamFile.Dot_config.read f
  else
    (OpamConsole.error "No global config file found for switch %s. \
                        Switch broken ?"
       (OpamSwitch.to_string switch);
     OpamFile.Dot_config.empty)

let load_switch gt rt switch =
  let switch_config = load_switch_config gt switch in
  let { OpamFile.State. installed; installed_roots; pinned;
        compiler = compiler_packages; } =
    load_state_file gt switch
  in
  let pinned, pinned_opams =
    (* Pinned packages with overlays *)
    OpamPackage.Name.Map.fold (fun name pin (pinned,opams) ->
        let overlay_dir = OpamPath.Switch.Overlay.package gt.root switch name in
        match OpamFileHandling.read_opam overlay_dir with
        | Some o ->
          let version, o = match pin with
            | Version version ->
              version, OpamFile.OPAM.with_version o version
            | Source _ ->
              OpamStd.Option.default (OpamPackage.Version.of_string "0")
                (OpamFile.OPAM.version_opt o), o
          in
          OpamPackage.Name.Map.add name (version,pin) pinned,
          OpamPackage.Map.add (OpamPackage.create name version) o opams
        | None ->
          OpamPackage.Name.Map.add name (OpamPackage.Version.of_string "0", pin)
            pinned,
          opams)
      pinned (OpamPackage.Name.Map.empty, OpamPackage.Map.empty)
  in
  let opams = pinned_opams in
  (*
    OpamPackage.Map.union (fun _ x -> x) rt.repo_opams pinned_opams
  in
  *)
  let opams =
    (* Add installed packages without repository (from ~/.opam/packages) *)
    OpamPackage.Set.fold (fun nv opams ->
        if OpamPackage.Map.mem nv opams then opams else
        try
          OpamStd.Option.Op.(
            (OpamFileHandling.read_opam (OpamPath.packages gt.root nv) >>| fun opam ->
             OpamPackage.Map.add nv opam opams)
            +! opams)
        with
        | OpamFormat.Bad_format _ | Lexer_error _
        | Parsing.Parse_error | OpamSystem.Internal_error _ -> opams
      )
      installed opams
  in
  let packages = OpamPackage.keys opams ++ installed in
  let available_packages = lazy (
    let pinned_names = OpamPackage.Name.(Set.of_list (Map.keys pinned)) in
    let from_repos =
      OpamPackage.Map.filter
        (fun nv _ ->
           not (OpamPackage.Name.Set.mem (OpamPackage.name nv) pinned_names))
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
  let reinstall =
    OpamFile.PkgList.safe_read (OpamPath.Switch.reinstall gt.root switch)
  in
  {
    switch; compiler_packages; switch_config;
    installed; pinned; installed_roots; opams; packages;
    available_packages; reinstall;
  }

let load ?(lock=Lock_readonly) gt rt switch =
  let chrono = OpamConsole.timer () in
  log "LOAD-SWITCH-STATE (%s)" (OpamSwitch.to_string switch);

  if not (OpamSwitch.Map.mem switch gt.aliases) then
    (log "%a does not contain the compiler name associated to the switch %a"
       (slog @@ OpamFilename.to_string @* OpamPath.aliases) gt.root
       (slog OpamSwitch.to_string) switch;
     OpamSwitch.not_installed switch)
  else
  let st = {
    switch_global = gt;
    switch_repos = rt;
    switch_lock = lock;
    current_switch = switch;
    switchmap = OpamSwitch.Map.singleton switch (load_switch gt rt switch)
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

let state_file sst =
  { OpamFile.State.
    installed = sst.installed;
    installed_roots = sst.installed_roots;
    compiler = sst.compiler_packages;
    pinned = OpamPackage.Name.Map.map snd sst.pinned; }

let add_switch st sst =
  { st with
    switchmap =
      OpamSwitch.Map.add sst.switch sst st.switchmap
  }

(* load a new switch. Does not change the current switch *)
let load_add_switch st rt switch =
  let sst = load_switch st.switch_global rt switch in
  add_switch st sst

let opam sst nv =
  OpamPackage.Map.find nv sst.opams

let opam_opt sst nv = try Some (opam sst nv) with Not_found -> None

let descr_opt sst nv =
  OpamStd.Option.Op.(opam_opt sst nv >>= OpamFile.OPAM.descr)

let descr sst nv =
  OpamStd.Option.Op.(descr_opt sst nv +! OpamFile.Descr.empty)

let url sst nv =
  OpamStd.Option.Op.(opam_opt sst nv >>= OpamFile.OPAM.url)

let files sst nv =
  OpamStd.Option.Op.(
    opam_opt sst nv >>= OpamFile.OPAM.metadata_dir >>|
    (fun dir -> dir / "files" ) >>=
    OpamFilename.opt_dir
  )

let is_name_installed sst name =
  OpamPackage.Set.exists (fun nv -> OpamPackage.name nv = name) sst.installed

let find_installed_package_by_name sst name =
  OpamPackage.Set.find (fun nv -> OpamPackage.name nv = name) sst.installed

let packages_of_atoms sst atoms =
  let check_atoms nv =
    let name = OpamPackage.name nv in
    let atoms = List.filter (fun (n,_) -> n = name) atoms in
    atoms <> [] && List.for_all (fun a -> OpamFormula.check a nv) atoms in
  OpamPackage.Set.filter check_atoms sst.packages

let get_package sst name =
  try OpamPinned.package sst name with Not_found ->
  try find_installed_package_by_name sst name with Not_found ->
  try OpamPackage.max_version (Lazy.force sst.available_packages) name
  with Not_found ->
    OpamPackage.max_version sst.packages name

let is_dev_package sst nv =
  match url sst nv with
  | None -> false
  | Some urlf ->
    match OpamFile.URL.(url urlf, checksum urlf) with
    | { OpamUrl.backend = `http; _ }, _ | _, Some _ -> false
    | _, None -> true

let dev_packages sst =
  OpamPackage.Set.filter (is_dev_package sst)
    (sst.installed ++ OpamPinned.packages sst)

let universe st u_action u_orphans =
  let switch = OpamSwitch.to_string st.current_switch in
  let all_switches = OpamSwitch.Set.of_list (OpamSwitch.Map.keys st.switch_global.aliases) in
  let profiles =
    let l = ref [] in
    if OpamStateConfig.(!r.build_test) then l := "test"::!l;
    if OpamStateConfig.(!r.build_doc); then l := "doc"::!l;
    !l
  in
  (*
  let action_switches =
    match u_action with
    |Reinstall (_,sw) |Upgrade (_,sw) |Install (_,sw) -> sw
    |_ -> OpamSwitch.Set.singleton t.switch_current
  in
  *)
  (* Here is where we create the PEF universe and pass the comparison function 
   * We consider all packages in all switches that are relevant for the current
   * action. 
  *)
  let switches =
    (* let sw = OpamSwitch.Set.inter all_switches action_switches in *)
    OpamSwitch.Set.elements all_switches
  in
  let (u_installed,u_packages,u_available) =
    let acc = (OpamPackage.Set.empty,OpamPackage.Set.empty,OpamPackage.Set.empty) in
    List.fold_left (fun (inst,pkgs,avai) sw ->
      let ct = OpamStateTypes.get_switch st sw in
      let u_installed = inst ++ ct.installed in
      let u_packages = pkgs ++ ct.installed ++ ct.packages in
      let u_available = avai ++ (Lazy.force ct.available_packages) in
      (u_installed,u_packages,u_available)
    ) acc switches
  in
  let u_options = (switch,List.map OpamSwitch.to_string switches,profiles) in
  let u_pefuniv = OpamPef.pef_packageuniv switches st in
  (* XXX This could be done in a smarter way *)
  let pefpkglist = Hashtbl.fold (fun _ v acc -> v::acc) u_pefuniv [] in
  let u_versionmap = Pef.Pefcudf.init_tables Versioning.Debian.compare pefpkglist in
  { u_pefuniv; u_options; u_versionmap; u_packages;
    u_action; u_installed; u_available; u_orphans }

(* User-directed helpers *)

let is_switch_globally_set st =
  OpamFile.Config.switch st.switch_global.config = st.current_switch

let not_found_message st switch (name, cstr) =
  let sst = OpamStateTypes.get_switch st switch in
  match cstr with
  | Some (relop,v) when OpamPackage.has_name sst.packages name ->
    Printf.sprintf "Package %s has no version %s%s."
      (OpamPackage.Name.to_string name)
      (match relop with `Eq -> "" | r -> string_of_relop r)
      (OpamPackage.Version.to_string v)
  | _ ->
    Printf.sprintf "No package named %s found."
      (OpamPackage.Name.to_string name)

(* Display a meaningful error for an unavailable package *)
let unavailable_reason st switch (name, _ as atom) =
  let sst = OpamStateTypes.get_switch st switch in
  let candidates =
    OpamPackage.Set.filter (OpamFormula.check atom) sst.packages in
  let nv = OpamPackage.max_version candidates name in
  let avail = OpamFile.OPAM.available (opam sst nv) in
  if not (OpamFilter.eval_to_bool ~default:false
            (OpamPackageVar.resolve_switch st sst.switch)
            avail)
  then
    Printf.sprintf "%s has unmet availability conditions: %s"
      (OpamPackage.Name.to_string name)
      (OpamFilter.to_string avail)
  else
  try
    let (version, pin) = OpamPackage.Name.Map.find name sst.pinned in
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
    not_found_message st sst.switch atom

let load_full_compat _ switch =
  let gt = OpamGlobalState.load () in
  let rt = OpamRepositoryState.load gt in
  load gt rt switch

(* This function is stupidly expensive XXX *)
let string_of_conflict st switch cs =
  let universe = universe st Depends OpamPackage.Set.empty in
  let sst = OpamStateTypes.get_switch st switch in
  let un_re = unavailable_reason st switch in
  OpamCudf.string_of_conflict universe.u_versionmap un_re cs

let strings_of_conflict st switch cs =
  let universe = universe st Depends OpamPackage.Set.empty in
  let sst = OpamStateTypes.get_switch st switch in
  let un_re = unavailable_reason st switch in
  OpamCudf.strings_of_conflict universe.u_versionmap un_re cs

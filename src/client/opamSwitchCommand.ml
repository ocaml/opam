(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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
open OpamState.Types

let log fmt = OpamGlobals.log "SWITCH" fmt

(* name + state + compiler + description *)
(* TODO: add repo *)
let list ~print_short ~installed =
  log "list";
  let t = OpamState.load_state "switch-list" in
  let descr c =
    if c = OpamCompiler.system then
      let system_version = match OpamCompiler.Version.system () with
        | None   -> "<none>"
        | Some v -> OpamCompiler.Version.to_string v in
      Printf.sprintf "System compiler (%s)" system_version
    else
      OpamFile.Comp_descr.safe_read (OpamPath.compiler_descr t.root c) in

  let installed_str     = "I" in
  let current_str       = "C" in
  let not_installed_str = "--" in

  let installed_s =
    OpamSwitch.Map.fold (fun name comp acc ->
      let s =
        if name = t.switch
        then current_str
        else installed_str in
      let n = OpamSwitch.to_string name in
      let c = OpamCompiler.to_string comp in
      let d = descr comp in
      (n, s, c, d) :: acc
    ) t.aliases [] in

  let descrs =
    OpamCompiler.Set.filter (fun comp ->
      OpamSwitch.Map.for_all (fun s c ->
        (* it is either not installed *)
        c <> comp
        (* or it is installed with an alias name. *)
        || OpamSwitch.to_string s <> OpamCompiler.to_string comp
      ) t.aliases
    ) t.compilers in

  let officials, patches =
    OpamCompiler.Set.fold (fun comp (officials, patches) ->
      let c = OpamFile.Comp.read (OpamPath.compiler t.root comp) in
      let version = OpamFile.Comp.version c in
      if OpamCompiler.Version.to_string version = OpamCompiler.to_string comp then
        comp :: officials, patches
      else
        officials, comp :: patches
    ) descrs ([],[]) in

  let mk l =
    List.fold_left (fun acc comp ->
      let c = OpamCompiler.to_string comp in
      let d =
        let d = descr comp in
        match OpamMisc.cut_at d '\n' with None -> d | Some (d,_) -> d in
      (not_installed_str, not_installed_str, c, d) :: acc
    ) [] l in

  let all =
    if installed then
      installed_s
    else
      installed_s @ mk officials @ mk patches in

  let max_name, max_state, max_compiler =
    List.fold_left (fun (n,s,c) (name, state, compiler, _) ->
      let n = max (String.length name) n in
      let s = max (String.length state) s in
      let c = max (String.length compiler) c in
      (n, s, c)
    ) (0,0,0) all in

  let count = ref (List.length all) in
  let print_compiler (name, state, compiler, descr) =
    decr count;
    if print_short then (
      let name =
        if name = not_installed_str
        then compiler
        else name in
      let sep =
        if !count = 0
        then "\n"
        else " " in
      OpamGlobals.msg "%s%s" name sep
    ) else
      OpamGlobals.msg "%s %s %s  %s\n"
        (OpamMisc.indent_left name max_name)
        (OpamMisc.indent_right state max_state)
        (OpamMisc.indent_left compiler max_compiler)
        descr in

  List.iter print_compiler all

let remove switch =
  log "remove switch=%s" (OpamSwitch.to_string switch);
  let t = OpamState.load_state "switch-remove" in
  let comp_dir = OpamPath.Switch.root t.root switch in
  if not (OpamFilename.exists_dir comp_dir) then (
    OpamGlobals.msg "The compiler switch %s does not exist.\n"
      (OpamSwitch.to_string switch);
    OpamGlobals.exit 1;
  );
  if t.switch = switch then (
    OpamGlobals.msg "Cannot remove %s as it is the current compiler.\n"
      (OpamSwitch.to_string switch);
    OpamGlobals.exit 1;
  );
  let aliases = OpamSwitch.Map.filter (fun a _ -> a <> switch) t.aliases in
  OpamFile.Aliases.write (OpamPath.aliases t.root) aliases;
  OpamFilename.rmdir comp_dir

let update_global_config t ~warning switch =
  OpamState.update_switch_config t switch;
  let t = OpamState.load_state "switch-update-config" in
  if warning then
    OpamState.print_env_warning_at_switch t

let install_with_packages ~quiet ~packages switch compiler =
  log "install %b %s %s" quiet
    (OpamSwitch.to_string switch)
    (OpamCompiler.to_string compiler);

  (* Remember the current switch to be able to roll-back *)
  let t = OpamState.load_state "switch-install-with-packages-1" in

  (* install the new OCaml version *)
  OpamState.install_compiler t ~quiet switch compiler;

  (* install the compiler packages *)
  OpamGlobals.switch := `Command_line (OpamSwitch.to_string switch);
  let t = OpamState.load_state "switch-install-with-packages-2" in

  let to_install, roots = match packages with
    | Some (p, r)  ->
      (OpamSolution.eq_atoms_of_packages p, OpamPackage.names_of_packages r)
    | None         ->
      let to_install = OpamState.get_compiler_packages t compiler in
      let roots = OpamPackage.Name.Set.of_list (List.map fst to_install) in
      to_install, roots in

  let bad_packages =
    OpamMisc.filter_map (fun (n, c) ->
      if OpamState.mem_installed_package_by_name t n then (
        let nv = OpamState.find_installed_package_by_name t n in
        if c = Some (`Eq, OpamPackage.version nv) then
          None
        else
          Some (n, Some (OpamPackage.version nv))
      ) else
        None
    ) to_install in

  let package_error = function
    | n, None   ->
      OpamGlobals.error "%s is an invalid package" (OpamPackage.Name.to_string n)
    | n, Some v ->
      OpamGlobals.error "%s.%s is not available for the current compiler"
        (OpamPackage.Name.to_string n)
        (OpamPackage.Version.to_string v) in

  let remove_compiler () =
    remove switch in

  match bad_packages with
  | [] ->
    let solution = OpamSolution.resolve_and_apply ~force:true t (Switch roots)
        { wish_install = [];
          wish_remove  = [];
          wish_upgrade = to_install } in
    begin try
        OpamSolution.check_solution solution;
      with e ->
        remove_compiler ();
        raise e
    end
  | p::_ ->
    remove_compiler ();
    package_error p

let install ~quiet ~warning ~update_config switch compiler =
  let t = OpamState.load_state "install" in
  let comp_dir = OpamPath.Switch.root t.root switch in
  let comp_f = OpamPath.compiler t.root compiler in
  if not (OpamFilename.exists_dir comp_dir)
  && not (OpamFilename.exists comp_f) then
    OpamCompiler.unknown compiler;
  if not (OpamState.switch_installed t switch) then
    install_with_packages ~quiet ~packages:None switch compiler
  else (
    let a = OpamSwitch.Map.find switch t.aliases in
    if a <> compiler then
      OpamGlobals.error_and_exit
        "The compiler switch %s is already installed as an alias for %s."
        (OpamSwitch.to_string switch)
        (OpamCompiler.to_string a)
  );
  if update_config then
    update_global_config ~warning t switch

let switch ~quiet ~warning switch =
  log "switch switch=%s" (OpamSwitch.to_string switch);
  let t = OpamState.load_state "switch-1" in
  if not (OpamState.switch_installed t switch) then (
    let compiler = OpamCompiler.of_string (OpamSwitch.to_string switch) in
    install ~quiet ~warning ~update_config:true switch compiler
  ) else
    update_global_config ~warning t switch;
  let t = OpamState.load_state "switch-2" in
  OpamState.check_base_packages t

(* Remove from [set] all the packages whose names appear in
   [filter]. *)
let filter_names ~filter set =
  let names = OpamPackage.names_of_packages filter in
  OpamPackage.Set.filter (fun nv ->
    not (OpamPackage.Name.Set.mem (OpamPackage.name nv) names)
  ) set

let import filename =
  log "import switch=%s" (match filename with None -> "<none>"
                                            | Some f -> OpamFilename.to_string f);
  let t = OpamState.load_state "switch-import" in

  let imported, import_roots =
    match filename with
    | None   -> OpamFile.Export.read_from_channel stdin
    | Some f -> OpamFile.Export.read f in

  (* Import only the packages not currently installed at the right version. *)
  let imported = OpamPackage.Set.diff imported t.installed in

  let to_import =
    List.map
      (fun nv -> OpamSolution.eq_atom (OpamPackage.name nv) (OpamPackage.version nv))
      (OpamPackage.Set.elements imported) in

  let to_keep =
    let keep_installed = filter_names ~filter:imported t.installed in
    List.map OpamSolution.atom_of_package (OpamPackage.Set.elements keep_installed) in

  let roots =
    let import_roots = OpamPackage.Set.diff import_roots t.installed_roots in
    let to_keep = filter_names ~filter:import_roots t.installed_roots in
    OpamPackage.names_of_packages (OpamPackage.Set.union import_roots to_keep) in

  let solution = OpamSolution.resolve_and_apply t (Import roots)
      { wish_install = to_keep;
        wish_remove  = [];
        wish_upgrade = to_import } in
  OpamSolution.check_solution solution

let export filename =
  let t = OpamState.load_state "switch-export" in
  let export = (t.installed, t.installed_roots) in
  match filename with
  | None   -> OpamFile.Export.write_to_channel stdout export
  | Some f -> OpamFile.Export.write f export

let show () =
  let t = OpamState.load_state "switch-show" in
  OpamGlobals.msg "%s\n" (OpamSwitch.to_string t.switch)

let reinstall switch =
  log "reinstall switch=%s" (OpamSwitch.to_string switch);
  let t = OpamState.load_state "switch-reinstall" in
  if not (OpamState.switch_installed t switch) then (
    OpamGlobals.msg "The compiler switch %s does not exist.\n"
      (OpamSwitch.to_string switch);
    OpamGlobals.exit 1;
  );
  let ocaml_version = OpamSwitch.Map.find switch t.aliases in
  let packages = Some (t.installed, t.installed_roots) in

  (* Remove the directory *)
  OpamFilename.rmdir (OpamPath.Switch.root t.root switch);

  (* Install the compiler *)
  install_with_packages ~quiet:false ~packages switch ocaml_version

let () =
  OpamState.switch_reinstall_hook := reinstall

(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

let log fmt = OpamGlobals.log "SWITCH" fmt

open OpamTypes
open OpamState.Types

let list () =
  log "list";
  let t = OpamState.load_state () in
  let descrs = OpamState.compilers t in
  let aliases = OpamFile.Aliases.read (OpamPath.aliases t.root) in
  OpamGlobals.msg "--- Installed compilers ---\n";
  OpamSwitch.Map.iter (fun n c ->
    let current = if n = t.switch then "*" else " " in
    let compiler = OpamCompiler.to_string c in
    let switch_name = OpamSwitch.to_string n in
    if switch_name = compiler then
      OpamGlobals.msg " %s %s\n" current switch_name
    else
      OpamGlobals.msg " %s %s [%s]\n" current compiler switch_name
  ) aliases;
  OpamGlobals.msg "\n--- Available compilers ---\n";
  OpamCompiler.Set.iter (fun c ->
    let comp = OpamFile.Comp.read (OpamPath.compiler t.root c) in
    let preinstalled = if OpamFile.Comp.preinstalled comp then "~" else " " in
    let version = OpamFile.Comp.version comp in
    let version, compiler =
      if OpamCompiler.Version.to_string version = OpamCompiler.to_string c then
        OpamCompiler.Version.to_string version, ""
      else
        OpamCompiler.Version.to_string version,
        Printf.sprintf "(%s)" (OpamCompiler.to_string c) in
    OpamGlobals.msg " %s %-8s %s\n" preinstalled version compiler
  ) descrs

let remove switch =
  log "remove switch=%s" (OpamSwitch.to_string switch);
  let t = OpamState.load_state () in
  let comp_dir = OpamPath.Switch.root t.root switch in
  if not (OpamFilename.exists_dir comp_dir) then (
    OpamGlobals.msg "The compiler switch %s does not exist.\n" (OpamSwitch.to_string switch);
    OpamGlobals.exit 1;
  );
  if t.switch = switch then (
    OpamGlobals.msg "Cannot remove %s as it is the current compiler.\n" (OpamSwitch.to_string switch);
    OpamGlobals.exit 1;
  );
  let aliases = OpamSwitch.Map.filter (fun a _ -> a <> switch) t.aliases in
  OpamFile.Aliases.write (OpamPath.aliases t.root) aliases;
  OpamFilename.rmdir comp_dir

let update_config t switch =
  let config = OpamFile.Config.with_switch t.config switch in
  OpamFile.Config.write (OpamPath.config t.root) config;
  OpamState.print_env_warning (OpamState.load_state ())

let install_with_packages ~quiet ~packages ~warning switch compiler =
  log "install %b %s %s" quiet
    (OpamSwitch.to_string switch)
    (OpamCompiler.to_string compiler);

  (* Remember the current switch to be able to roll-back *)
  let t = OpamState.load_state () in
  let old_switch = t.switch in

  (* install the new OCaml version *)
  OpamState.install_compiler t quiet switch compiler;

  (* install the compiler packages *)
  let t = OpamState.load_state () in
  let to_install = match packages with
    | None   -> OpamState.get_compiler_packages t compiler
    | Some p -> OpamSolution.eq_atoms_of_packages p in

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

  let uninstall_compiler () =
    update_config t old_switch;
    remove switch in

  match bad_packages with
  | [] ->
    let solution = OpamSolution.resolve_and_apply ~force:true t Switch
      { wish_install = [];
        wish_remove  = [];
        wish_upgrade = to_install } in
    begin try
      OpamSolution.error_if_no_solution solution;
      if warning then OpamState.print_env_warning t
    with e ->
      uninstall_compiler ();
      raise e
    end
  | p::_ ->
    uninstall_compiler ();
    package_error p

let install ~quiet switch compiler =
  install_with_packages ~quiet ~packages:None ~warning:true switch compiler

let switch ~quiet switch =
  log "sswitch switch=%s" (OpamSwitch.to_string switch);
  let t = OpamState.load_state () in
  let comp_dir = OpamPath.Switch.root t.root switch in
  let compiler = OpamCompiler.of_string (OpamSwitch.to_string switch) in
  let comp_f = OpamPath.compiler t.root compiler in
  if not (OpamFilename.exists_dir comp_dir) && not (OpamFilename.exists comp_f) then (
    OpamGlobals.error "The compiler's description for %s does not exist.\n" (OpamSwitch.to_string switch);
    OpamGlobals.exit 1;
  );
  if not (OpamFilename.exists_dir comp_dir) then
    install quiet switch compiler
  else
    update_config t switch

let import filename =
  log "import switch=%s" (OpamFilename.to_string filename);
  let t = OpamState.load_state () in

  let imported = OpamFile.Installed.read filename in
  let new_packages = OpamPackage.Set.diff imported t.installed in
  let installed =
    OpamPackage.Set.filter (fun nv ->
      let name = OpamPackage.name nv in
      not (OpamPackage.Set.exists (fun nv -> name = OpamPackage.name nv) new_packages)
    ) t.installed in

  let to_install = OpamPackage.Set.union new_packages installed in
  let to_install =
    List.map
      (fun nv -> OpamSolution.eq_atom (OpamPackage.name nv) (OpamPackage.version nv))
      (OpamPackage.Set.elements to_install) in

  let solution = OpamSolution.resolve_and_apply t Switch
    { wish_install = [];
      wish_remove  = [];
      wish_upgrade = to_install } in
  OpamSolution.error_if_no_solution solution

let export filename =
  let t = OpamState.load_state () in
  OpamFile.Installed.write filename t.installed

let current () =
  let t = OpamState.load_state () in
  OpamGlobals.msg "%s\n" (OpamSwitch.to_string t.switch)

let reinstall switch =
  log "reinstall switch=%s" (OpamSwitch.to_string switch);
  let t = OpamState.load_state () in
  if not (OpamSwitch.Map.mem switch t.aliases) then (
    OpamGlobals.msg "The compiler switch %s does not exist.\n" (OpamSwitch.to_string switch);
    OpamGlobals.exit 1;
  );
  let ocaml_version = OpamSwitch.Map.find switch t.aliases in
  let packages = Some (OpamFile.Installed.read (OpamPath.Switch.installed t.root switch)) in

  (* Remove the directory *)
  OpamFilename.rmdir (OpamPath.Switch.root t.root switch);

  (* Install the compiler *)
  install_with_packages ~quiet:false ~packages ~warning:false switch ocaml_version

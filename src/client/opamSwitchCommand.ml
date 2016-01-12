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

module S = OpamFile.State

let log fmt = OpamConsole.log "SWITCH" fmt
let slog = OpamConsole.slog

(* name + state + compiler + description *)
(* TODO: add repo *)
let list ~print_short ~installed ~all =
  log "list";
  let t = OpamSwitchState.load_full_compat "list" (OpamStateConfig.get_switch ()) in
  let descr c =
    if c = OpamCompiler.system then
      let system_version =
        match Lazy.force OpamOCaml.system_ocamlc_version with
        | None   -> "<none>"
        | Some v -> v in
      OpamFile.Descr.create (Printf.sprintf "System compiler (%s)" system_version)
    else
      OpamFile.Descr.safe_read (OpamPath.compiler_descr t.switch_global.root c) in

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
      (OpamCompiler.version comp, n, s, c, d) :: acc
    ) t.switch_global.aliases [] in

  let descrs =
    OpamCompiler.Set.filter (fun comp ->
      OpamSwitch.Map.for_all (fun s c ->
        (* it is either not installed *)
        c <> comp
        (* or it is installed with an alias name. *)
        || OpamSwitch.to_string s <> OpamCompiler.to_string comp
      ) t.switch_global.aliases
    ) t.switch_repos.compilers in

  let officials, patches =
    OpamCompiler.Set.fold (fun comp (officials, patches) ->
        try
          let c = OpamFile.Comp.read (OpamPath.compiler_comp t.switch_global.root comp) in
          let version = OpamFile.Comp.version c in
          if OpamCompiler.Version.to_string version =
             OpamCompiler.to_string comp then
            comp :: officials, patches
          else
            officials, comp :: patches
        with OpamFormat.Bad_format _ -> officials, patches
    ) descrs ([],[]) in

  let mk l =
    List.fold_left (fun acc comp ->
      let c = OpamCompiler.to_string comp in
      let d = descr comp in
      (OpamCompiler.version comp,
       not_installed_str, not_installed_str, c, d) :: acc
    ) [] l in

  let to_show =
    if installed then
      installed_s
    else if all then
      mk officials @ installed_s @ mk patches
    else
      mk officials @ installed_s in

  let to_show =
    List.sort
      (fun (v1,_,_,_,_) (v2,_,_,_,_) -> OpamCompiler.Version.compare v1 v2)
      to_show in

  let max_name, max_state, max_compiler =
    List.fold_left (fun (n,s,c) (_,name, state, compiler, _) ->
      let n = max (String.length name) n in
      let s = max (String.length state) s in
      let c = max (String.length compiler) c in
      (n, s, c)
    ) (0,0,0) to_show in

  let print_compiler (_, name, state, compiler, descr) =
    if print_short then
      let name =
        if name = not_installed_str
        then compiler
        else name in
      OpamConsole.msg "%s\n" name
    else
      let bold_current s =
        if name = OpamSwitch.to_string t.switch
        then OpamConsole.colorise `bold s
        else s in
      let colored_name = bold_current name in
      let colored_state =
        if state = not_installed_str then state else
          bold_current (OpamConsole.colorise `blue state) in
      let colored_compiler =
        bold_current (OpamConsole.colorise `yellow compiler) in
      let colored_descr = bold_current (OpamFile.Descr.synopsis descr) in
      let colored_body =
        if (OpamConsole.verbose ()) then
          match OpamStd.String.strip (OpamFile.Descr.body descr) with
          | "" -> ""
          | d  -> "\n"^d^"\n"
        else "" in
      OpamConsole.msg "%s %s %s  %s%s\n"
        (OpamStd.Format.indent_left colored_name ~visual:name max_name)
        (OpamStd.Format.indent_right colored_state ~visual:state max_state)
        (OpamStd.Format.indent_left colored_compiler ~visual:compiler max_compiler)
        colored_descr colored_body in

  List.iter print_compiler to_show;
  if not installed && not all && not print_short && patches <> [] then
    OpamConsole.msg "# %d more patched or experimental compilers, \
                     use '--all' to show\n"
      (List.length patches);
  if not print_short then
  match OpamStateConfig.(!r.switch_from) with
  | `Env ->
    let sys = OpamFile.Config.switch t.switch_global.config in
    if sys <> Some (OpamStateConfig.get_switch ()) then
      (OpamConsole.msg "\n";
       OpamConsole.note
         "Current switch is set locally through the OPAMSWITCH variable.\n\
          The current global system switch is %s."
         (OpamStd.Option.to_string ~none:"unset"
            (fun s -> OpamConsole.colorise `bold (OpamSwitch.to_string s)) sys))
  | `Default ->
     if not (OpamEnv.is_up_to_date t) then
       (OpamConsole.msg "\n";
        OpamConsole.warning
          "The environment is not in sync with the current switch.\n\
           You should run: %s"
          (OpamEnv.eval_string t.switch_global.root t.switch))
  | _ -> ()

let clear_switch ?(keep_debug=false) t switch =
  let aliases = OpamSwitch.Map.filter (fun a _ -> a <> switch) t.switch_global.aliases in
  OpamFile.Aliases.write (OpamPath.aliases t.switch_global.root) aliases;
  let comp_dir = OpamPath.Switch.root t.switch_global.root switch in
  if keep_debug && (OpamStateConfig.(!r.keep_build_dir) || (OpamConsole.debug ())) then
    OpamConsole.note "Keeping %s despite errors (debug mode), \
                      you may want to remove it by hand"
      (OpamFilename.Dir.to_string comp_dir)
  else
  try OpamFilename.rmdir comp_dir
  with OpamSystem.Internal_error _ -> ()

let remove_t ?(confirm = true) t =
  log "remove switch=%a" (slog OpamSwitch.to_string) t.switch;
  let comp_dir = OpamPath.Switch.root t.switch_global.root t.switch in
  if not (OpamFilename.exists_dir comp_dir) then (
    OpamConsole.msg "The compiler switch %s does not exist.\n"
      (OpamSwitch.to_string t.switch);
    OpamStd.Sys.exit 1;
  );
  if Some t.switch =
     OpamStd.Option.Op.(OpamStateConfig.load t.switch_global.root >>=
                         OpamFile.Config.switch)
  then
    OpamConsole.error_and_exit
      "Cannot remove %s as it is the currently selected switch. You should \
       first run `opam switch <another switch>'\n"
      (OpamSwitch.to_string t.switch);
  if not confirm ||
     OpamConsole.confirm
       "Switch %s and all its packages will be wiped. Are you sure ?"
       (OpamSwitch.to_string t.switch)
  then
    clear_switch t t.switch

let install_compiler ~quiet switch compiler =
  log "install %b %a %a" quiet
    (slog OpamSwitch.to_string) switch
    (slog OpamCompiler.to_string) compiler;

  (* Remember the current switch to be able to roll-back *)
  let t = OpamSwitchState.load_full_compat "switch-install-with-packages-1"
      (OpamStateConfig.get_switch ()) in

  (* install the new OCaml version *)
  try OpamSwitchAction.install_compiler t.switch_global ~quiet switch compiler
  with e ->
    (* OpamConsole.error "%s" (Printexc.to_string e); *)
    clear_switch ~keep_debug:true t switch;
    OpamStateConfig.write t.switch_global.root t.switch_global.config;
    raise e


let install_packages t =
  (* install the compiler packages *)
  (* let t = OpamSwitchState.load_full_compat "switch-install-with-packages-2" switch in *)
  let switch = t.switch in
  let compiler = OpamSwitch.Map.find switch t.switch_global.aliases in

  let comp = OpamFile.Comp.read (OpamPath.compiler_comp t.switch_global.root compiler) in
  let to_install = OpamFormula.atoms (OpamFile.Comp.packages comp) in
  let roots = OpamPackage.Name.Set.of_list (List.map fst to_install) in

  let bad_packages =
    OpamStd.List.filter_map (fun (n, c) ->
      if OpamSwitchState.is_name_installed t n then (
        let nv = OpamSwitchState.find_installed_package_by_name t n in
        if c = Some (`Eq, OpamPackage.version nv) then
          None
        else
          Some (n, Some (OpamPackage.version nv))
      ) else
        None
    ) to_install in

  let package_error = function
    | n, None   ->
      OpamConsole.error "%s is an invalid package" (OpamPackage.Name.to_string n)
    | n, Some v ->
      OpamConsole.error "%s.%s is not available for the current compiler"
        (OpamPackage.Name.to_string n)
        (OpamPackage.Version.to_string v) in

  match bad_packages with
  | p::_ ->
    package_error p;
    OpamStd.Sys.exit 10
  | [] ->
    let solution =
      OpamSolution.resolve t (Switch roots)
        ~orphans:OpamPackage.Set.empty
        { wish_install = [];
          wish_remove  = [];
          wish_upgrade = to_install;
          criteria = `Default;
          extra_attributes = []; } in
    let solution = match solution with
      | Success s -> s
      | _ ->
        OpamConsole.error_and_exit "Could not resolve set of base packages"
    in
    (match OpamSolver.stats solution with
     | { s_install = _; s_reinstall = 0; s_upgrade = 0;
         s_downgrade=0; s_remove = 0 } -> ()
     | _ ->
       OpamConsole.error_and_exit
         "Inconsistent resolution of base package installs");
    let to_install_pkgs = OpamSolver.new_packages solution in
    let to_install_names = OpamPackage.names_of_packages to_install_pkgs in
    if not (OpamStateConfig.(!r.no_base_packages)) &&
       not (OpamPackage.Name.Set.equal to_install_names roots)
    then
      OpamConsole.error_and_exit
        "Inconsistent set of base compiler packages: \
         %s needed but not included / %s extra"
        OpamPackage.Name.Set.(to_string (diff to_install_names roots))
        OpamPackage.Name.Set.(to_string (diff roots to_install_names));
    let result =
      OpamSolution.apply ~ask:false t (Switch roots)
        ~requested:roots
        solution in
    let t = OpamSwitchState.load_full_compat "switch-install-with-packages-3" switch in
    let t = { t with compiler_packages = to_install_pkgs } in
    OpamSwitchAction.write_state_file t;
    OpamSolution.check_solution t result

let install_cont ~quiet ~update_config switch compiler =
  let gt = OpamGlobalState.load () in
  let comp_dir = OpamPath.Switch.root gt.root switch in
  let already_installed = OpamSwitch.Map.mem switch gt.aliases in
  if not already_installed &&
     Sys.file_exists (OpamFilename.Dir.to_string comp_dir) then
    OpamConsole.error_and_exit
      "Directory %S already exists, please choose a different name"
      (OpamFilename.Dir.to_string comp_dir);
  let comp_f = OpamPath.compiler_comp gt.root compiler in
  if not (OpamFilename.exists_dir comp_dir)
  && not (OpamFilename.exists comp_f) then
    if compiler = OpamCompiler.system then (
      OpamConsole.error_and_exit "No OCaml compiler found in path."
    ) else
      OpamConsole.error_and_exit
        "%S is not a valid compiler."
        (OpamCompiler.to_string compiler);
  if already_installed then
    (let a = OpamSwitch.Map.find switch gt.aliases in
     if a <> compiler then
       OpamConsole.error_and_exit
         "The compiler switch %s is already installed as an alias for %s."
         (OpamSwitch.to_string switch)
         (OpamCompiler.to_string a))
  else
    install_compiler ~quiet switch compiler;
  let t =
    if update_config then OpamSwitchAction.set_current_switch gt switch
    else OpamSwitchState.load gt (OpamRepositoryState.load gt) switch
  in
  switch,
  fun () ->
    try install_packages t
    with e ->
      clear_switch ~keep_debug:true t switch;
      OpamStateConfig.write t.switch_global.root t.switch_global.config;
      raise e

let install ~quiet ~update_config switch compiler =
  (snd (install_cont ~quiet ~update_config switch compiler)) ()

let switch_cont ?compiler ~quiet switch =
  log "switch switch=%a" (slog OpamSwitch.to_string) switch;
  let gt = OpamGlobalState.load () in
  let switch, cont =
    if OpamSwitch.Map.mem switch gt.aliases then
      let t = OpamSwitchAction.set_current_switch gt switch in
      OpamEnv.check_and_print_env_warning t;
      switch, fun () -> ()
    else
    let compiler =
      match compiler with
      | None -> OpamCompiler.of_string (OpamSwitch.to_string switch)
      | Some c -> c in
    install_cont ~quiet ~update_config:true switch compiler
  in
  switch,
  cont

let switch ?compiler ~quiet switch =
  (snd (switch_cont ?compiler ~quiet switch)) ()

(* unused ?
(* Remove from [set] all the packages whose names appear in
   [filter]. *)
let filter_names ~filter set =
  let names = OpamPackage.names_of_packages filter in
  OpamPackage.Set.filter (fun nv ->
    not (OpamPackage.Name.Set.mem (OpamPackage.name nv) names)
  ) set
*)

(** !X todo: include overlays in state/export file *)
let import_t importfile t =
  log "import switch";

  let prev_state = OpamSwitchState.state_file t in

  let pinned =
    OpamPackage.Name.Map.merge (fun _ current import ->
        match current, import with
        | _, (Some _ as p) -> p
        | p, None -> p)
      prev_state.S.pinned importfile.S.pinned
  in
  let pinned_version name =
    try
      match OpamPackage.Name.Map.find name pinned with
      | Version v -> Some v
      | _ ->
        Some (OpamPackage.version
                (OpamPackage.max_version importfile.S.installed name))
    with Not_found | Invalid_argument _ -> None
  in
  (* Add the imported pins in case they don't exist already *)
  let available =
    OpamPackage.Set.fold (fun nv available ->
        if OpamPackage.Set.mem nv available then available else
        if OpamPackage.Name.Map.mem (OpamPackage.name nv) importfile.S.pinned
        then OpamPackage.Set.add nv available
        else (
          OpamConsole.warning "%s Skipping."
            (OpamSwitchState.unavailable_reason t
               (OpamSolution.eq_atom
                  (OpamPackage.name nv) (OpamPackage.version nv)));
          available
        )
      )
      importfile.S.installed (Lazy.force t.available_packages)
  in
  let imported = importfile.S.installed %% available in
  let import_roots = importfile.S.installed_roots %% available in
  let revert_pins () =
    if not (OpamStateConfig.(!r.dryrun) || OpamStateConfig.(!r.show)) then
      let switch_state = OpamSwitchState.load_state_file t.switch_global t.switch in
      let state_f = OpamPath.Switch.state t.switch_global.root t.switch in
      S.write state_f { switch_state with S.pinned = prev_state.S.pinned }
  in
  let t =
    {t with available_packages = lazy available;
            packages = t.packages ++ available }
  in
  let t =
    let pinned =
      OpamPackage.Name.Map.mapi (fun name pin ->
         OpamStd.Option.default (OpamPackage.Version.of_string "~dev")
           (pinned_version name),
         pin)
        pinned
    in
    { t with pinned }
  in
  let solution =
    try
      let _ =
        OpamPackage.Name.Map.iter (fun name pin ->
            let overlay_dir =
              OpamPath.Switch.Overlay.package t.switch_global.root t.switch name
            in
            if not (OpamFilename.exists_dir overlay_dir) then
              OpamPinned.add_overlay t.switch_repos ?version:(pinned_version name)
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
          (OpamSwitchAction.write_state_file t;
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
  let st = S.safe_read (OpamPath.Switch.state root switch) in
  match filename with
  | None   -> S.write_to_channel stdout st
  | Some f -> S.write f st

let show () =
  OpamConsole.msg "%s\n"
    (OpamSwitch.to_string (OpamStateConfig.get_switch ()))

let reinstall_gt gt switch =
  log "reinstall switch=%a" (slog OpamSwitch.to_string) switch;
  let ocaml_version =
    try OpamSwitch.Map.find switch gt.aliases
    with Not_found ->
      OpamConsole.error_and_exit
        "The compiler switch %s does not exist.\n"
        (OpamSwitch.to_string switch)
  in
  let export = OpamSwitchState.load_state_file gt switch in

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

  OpamSwitchAction.install_compiler gt ~quiet:false switch ocaml_version;
  let t = OpamSwitchState.load gt (OpamRepositoryState.load gt) switch in
  import_t export t

(* !X unify with OpamClient.with_switch_backup; set a number of backups and just
   rename older versions .1, .2 etc. ? *)
let with_backup gt switch f =
  let state_file = OpamPath.Switch.state gt.root switch in
  let backup_state_file = OpamPath.backup gt.root in
  OpamFilename.copy ~src:state_file ~dst:backup_state_file;
  try
    f gt switch;
    OpamFilename.remove backup_state_file
  with
  | OpamStd.Sys.Exit 0 as e -> raise e
  | err ->
    let new_state = OpamFile.State.safe_read state_file in
    let old_state = OpamFile.State.safe_read backup_state_file in
    if OpamFile.State.(
        OpamPackage.Set.equal new_state.installed old_state.installed &&
        OpamPackage.Set.equal
          new_state.installed_roots old_state.installed_roots &&
        OpamPackage.Set.equal new_state.compiler old_state.compiler
      )
    then OpamFilename.remove backup_state_file
    else
      Printf.eprintf "The former package state can be restored with \
                      %s switch import %S%s\n"
        Sys.argv.(0) (OpamFilename.to_string backup_state_file)
        (if OpamStateConfig.(!r.current_switch <> Some switch ||
                             !r.switch_from = `Command_line)
         then Printf.sprintf " --switch %s" (OpamSwitch.to_string switch)
         else "");
    raise err

let reinstall gt switch =
  with_backup gt switch reinstall_gt

let remove gt switch =
  with_backup gt switch
    (fun gt switch ->
       let st = OpamSwitchState.load gt (OpamRepositoryState.load gt) switch in
       remove_t st)

let import gt switch filename =
  with_backup gt switch
    (fun gt switch ->
       let importfile = match filename with
         | None   -> S.read_from_channel stdin
         | Some f -> S.read f in
       let st = OpamSwitchState.load gt (OpamRepositoryState.load gt) switch in
       import_t importfile st)

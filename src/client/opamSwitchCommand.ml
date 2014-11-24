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
open OpamPackage.Set.Op

let log fmt = OpamGlobals.log "SWITCH" fmt
let slog = OpamGlobals.slog

(* name + state + compiler + description *)
(* TODO: add repo *)
let list ~print_short ~installed ~all =
  log "list";
  let t = OpamState.load_state "switch-list" in
  let descr c =
    if c = OpamCompiler.system then
      let system_version =
        match Lazy.force OpamSystem.system_ocamlc_version with
        | None   -> "<none>"
        | Some v -> v in
      OpamFile.Descr.of_string (Printf.sprintf "System compiler (%s)" system_version)
    else
      OpamFile.Descr.safe_read (OpamPath.compiler_descr t.root c) in

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
      let c = OpamFile.Comp.read (OpamPath.compiler_comp t.root comp) in
      let version = OpamFile.Comp.version c in
      if OpamCompiler.Version.to_string version = OpamCompiler.to_string comp then
        comp :: officials, patches
      else
        officials, comp :: patches
    ) descrs ([],[]) in

  let mk l =
    List.fold_left (fun acc comp ->
      let c = OpamCompiler.to_string comp in
      let d = descr comp in
      (not_installed_str, not_installed_str, c, d) :: acc
    ) [] l in

  let to_show =
    if installed then
      installed_s
    else if all then
      installed_s @ mk officials @ mk patches
    else
      installed_s @ mk officials in

  let max_name, max_state, max_compiler =
    List.fold_left (fun (n,s,c) (name, state, compiler, _) ->
      let n = max (String.length name) n in
      let s = max (String.length state) s in
      let c = max (String.length compiler) c in
      (n, s, c)
    ) (0,0,0) to_show in

  let count = ref (List.length to_show) in
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
      let bold_current s =
        if name = OpamSwitch.to_string t.switch
        then OpamGlobals.colorise `bold s
        else s in
      let colored_name = bold_current name in
      let colored_state =
        if state = not_installed_str then state else
          bold_current (OpamGlobals.colorise `blue state) in
      let colored_compiler =
        bold_current (OpamGlobals.colorise `yellow compiler) in
      let colored_descr = bold_current (OpamFile.Descr.synopsis descr) in
      let colored_body =
        if !OpamGlobals.verbose then
          match OpamMisc.strip (OpamFile.Descr.body descr) with
          | "" -> ""
          | d  -> "\n"^d^"\n"
        else "" in
      OpamGlobals.msg "%s %s %s  %s%s\n"
        (OpamMisc.indent_left colored_name ~visual:name max_name)
        (OpamMisc.indent_right colored_state ~visual:state max_state)
        (OpamMisc.indent_left colored_compiler ~visual:compiler max_compiler)
        colored_descr colored_body in

  List.iter print_compiler to_show;
  if not installed && not all && not print_short && patches <> [] then
    OpamGlobals.msg "# %d more patched or experimental compilers, \
                     use '--all' to show\n"
      (List.length patches);
  match !OpamGlobals.switch with
  | `Env s ->
    let sys = OpamSwitch.to_string (OpamFile.Config.switch t.config) in
    if sys <> s && not print_short then
      (OpamGlobals.msg "\n";
       OpamGlobals.note
         "Current switch is set locally through the OPAMSWITCH variable.\n\
         \       The current global system switch is %s."
         (OpamGlobals.colorise `bold sys))
  | _ -> ()

let remove_t switch ?(confirm = true) t =
  log "remove switch=%a" (slog OpamSwitch.to_string) switch;
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
  if not confirm ||
     OpamGlobals.confirm
       "Switch %s and all its packages will be wiped. Are you sure ?"
       (OpamSwitch.to_string switch)
  then
    let aliases = OpamSwitch.Map.filter (fun a _ -> a <> switch) t.aliases in
    OpamFile.Aliases.write (OpamPath.aliases t.root) aliases;
    OpamFilename.rmdir comp_dir

let update_global_config t ~warning switch =
  OpamState.update_switch_config t switch;
  let t = OpamState.load_state "switch-update-config" in
  if warning then
    OpamState.print_env_warning_at_switch t


let install_compiler ~quiet switch compiler =
  log "install %b %a %a" quiet
    (slog OpamSwitch.to_string) switch
    (slog OpamCompiler.to_string) compiler;

  (* Remember the current switch to be able to roll-back *)
  let t = OpamState.load_state "switch-install-with-packages-1" in

  (* install the new OCaml version *)
  try OpamState.install_compiler t ~quiet switch compiler
    with e ->
      (* in case of reinstall, the switch may still be in t.aliases *)
      let aliases = OpamSwitch.Map.filter (fun a _ -> a <> switch) t.aliases in
      OpamFile.Aliases.write (OpamPath.aliases t.root) aliases;
      let compdir = OpamPath.Switch.root t.root switch in
      (try OpamFilename.rmdir compdir with OpamSystem.Internal_error _ -> ());
      raise e


let install_packages ~packages switch compiler =
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
      if OpamState.is_name_installed t n then (
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
    let t = OpamState.load_state "remove-compiler" in
    remove_t ~confirm:false switch t in

  match bad_packages with
  | [] ->
    let solution = OpamSolution.resolve_and_apply ~ask:false t (Switch roots)
        ~requested:roots
        ~orphans:OpamPackage.Set.empty
        { wish_install = [];
          wish_remove  = [];
          wish_upgrade = to_install;
          criteria = `Default; } in
    begin try
        OpamSolution.check_solution t solution;
      with e ->
        remove_compiler ();
        raise e
    end
  | p::_ ->
    remove_compiler ();
    package_error p

let install_cont ~quiet ~warning ~update_config switch compiler =
  let t = OpamState.load_state "install" in
  let comp_dir = OpamPath.Switch.root t.root switch in
  if Sys.file_exists (OpamFilename.Dir.to_string comp_dir) then
    OpamGlobals.error_and_exit
      "%S already exists, please choose a different name"
      (OpamFilename.Dir.to_string comp_dir);
  let comp_f = OpamPath.compiler_comp t.root compiler in
  if not (OpamFilename.exists_dir comp_dir)
  && not (OpamFilename.exists comp_f) then
    OpamCompiler.unknown compiler;
  let already_installed = OpamState.is_switch_installed t switch in
  if already_installed then
    (let a = OpamSwitch.Map.find switch t.aliases in
     if a <> compiler then
       OpamGlobals.error_and_exit
         "The compiler switch %s is already installed as an alias for %s."
         (OpamSwitch.to_string switch)
         (OpamCompiler.to_string a))
  else
    install_compiler ~quiet switch compiler;
  if update_config then
    update_global_config ~warning t switch;
  switch,
  fun () ->
    install_packages ~packages:None switch compiler

let install ~quiet ~warning ~update_config switch compiler =
  (snd (install_cont ~quiet ~warning ~update_config switch compiler)) ()

let switch_cont ~quiet ~warning switch =
  log "switch switch=%a" (slog OpamSwitch.to_string) switch;
  let t = OpamState.load_state "switch-1" in
  let switch, cont =
    if not (OpamState.is_switch_installed t switch) then (
      let compiler = OpamCompiler.of_string (OpamSwitch.to_string switch) in
      install_cont ~quiet ~warning ~update_config:true switch compiler
    ) else (
      update_global_config ~warning t switch;
      switch, fun () -> ()
    )
  in
  switch,
  fun () ->
    cont ();
    let t = OpamState.load_state "switch-2" in
    OpamState.check_base_packages t

let switch ~quiet ~warning switch =
  (snd (switch_cont ~quiet ~warning switch)) ()

(* Remove from [set] all the packages whose names appear in
   [filter]. *)
let filter_names ~filter set =
  let names = OpamPackage.names_of_packages filter in
  OpamPackage.Set.filter (fun nv ->
    not (OpamPackage.Name.Set.mem (OpamPackage.name nv) names)
  ) set

let import_t importfile t =
  log "import switch";

  let imported, import_roots, import_pins = importfile in

  let pinned =
    OpamPackage.Name.Map.merge (fun _ current import ->
        match current, import with
        | _, (Some _ as p) -> p
        | p, None -> p)
      t.pinned import_pins
  in
  let pinned_version name =
    try
      Some (OpamPackage.version
              (OpamPackage.Set.choose_one
                 (OpamPackage.packages_of_name imported name)))
    with Not_found | Invalid_argument _ -> None
  in
  (* Add the imported pins in case they don't exist already *)
  let available =
    OpamPackage.Set.fold (fun nv available ->
        if OpamPackage.Set.mem nv available then available else
        if OpamPackage.Name.Map.mem (OpamPackage.name nv) import_pins then
          OpamPackage.Set.add nv available
        else (
          OpamGlobals.warning "%s Skipping."
            (OpamState.unavailable_reason t
               (OpamSolution.eq_atom
                  (OpamPackage.name nv) (OpamPackage.version nv)));
          available
        )
      )
      imported (Lazy.force t.available_packages)
  in
  let imported = imported %% available in
  let import_roots = import_roots %% available in
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  let revert_pins () =
    if not (!OpamGlobals.dryrun || !OpamGlobals.show) then
      OpamFile.Pinned.write pin_f t.pinned
  in
  let t = {t with pinned;
                  available_packages = lazy available;
                  packages = t.packages ++ available }
  in
  let solution =
    try
      let _ =
        let dev_pkgs =
          OpamPackage.Name.Map.fold (fun n pin acc ->
              let nv = match pin with
                | Version v -> OpamPackage.create n v
                | _ -> OpamPackage.create n (OpamPackage.Version.of_string "pin")
              in
              OpamPackage.Set.add nv acc)
            import_pins OpamPackage.Set.empty in
        OpamPackage.Name.Map.iter (fun n _ ->
            let overlay_dir = OpamPath.Switch.Overlay.package t.root t.switch n
            in
            if not (OpamFilename.exists_dir overlay_dir) then
              OpamState.add_pinned_overlay t ?version:(pinned_version n) n)
          pinned;
        if not (OpamPackage.Name.Map.is_empty pinned) then (
          OpamGlobals.header_msg "Synchronising pinned packages";
          OpamState.update_dev_packages t dev_pkgs)
        else OpamPackage.Set.empty
      in

      let t =
        if !OpamGlobals.dryrun || !OpamGlobals.show then t
        else
          (OpamFile.Pinned.write pin_f pinned;
           OpamState.load_state "pin-import") in

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
          criteria = `Default; }
    with e ->
      revert_pins ();
      raise e
  in
  (match solution with
   | No_solution | Aborted -> revert_pins ()
   | Error _ | OK _ | Nothing_to_do -> ());
  OpamSolution.check_solution t solution;
  OpamFile.Installed_roots.write
    (OpamPath.Switch.installed_roots t.root t.switch)
    OpamPackage.Set.Op.(t.installed_roots ++ import_roots)

let export filename =
  let t = OpamState.load_state "switch-export" in
  let export = (t.installed, t.installed_roots, t.pinned) in
  match filename with
  | None   -> OpamFile.Export.write_to_channel stdout export
  | Some f -> OpamFile.Export.write f export

let show () =
  let t = OpamState.load_state "switch-show" in
  OpamGlobals.msg "%s\n" (OpamSwitch.to_string t.switch)

let reinstall_t t =
  log "reinstall switch=%a" (slog OpamSwitch.to_string) t.switch;
  if not (OpamState.is_switch_installed t t.switch) then (
    OpamGlobals.msg "The compiler switch %s does not exist.\n"
      (OpamSwitch.to_string t.switch);
    OpamGlobals.exit 1;
  );
  let ocaml_version = t.compiler in
  let export = (t.installed, t.installed_roots, t.pinned) in

  (* Remove the directory (except the overlays) *)
  OpamFilename.with_tmp_dir (fun tmpdir ->
      let tmp dir = OpamFilename.(OP.(tmpdir / Base.to_string (basename_dir dir))) in
      let save dir =
        if OpamFilename.exists_dir dir then
          OpamFilename.move_dir ~src:dir ~dst:(tmp dir) in
      let restore dir =
        if OpamFilename.exists_dir (tmp dir) then (
          OpamFilename.mkdir (OpamFilename.dirname_dir dir);
          OpamFilename.move_dir ~src:(tmp dir) ~dst:dir
        ) in
      let overlays = OpamPath.Switch.Overlay.dir t.root t.switch in
      let backups = OpamPath.Switch.backup_dir t.root t.switch in
      save overlays;
      save backups;
      (try
         OpamFilename.rmdir (OpamPath.Switch.root t.root t.switch);
         OpamState.install_compiler t ~quiet:false t.switch ocaml_version
       with e ->
         restore backups;
         restore overlays;
         raise e);
      restore backups;
      restore overlays
  );

  let t = OpamState.load_state "switch-reinstall-2" in
  import_t export t

let with_backup command f =
  let t = OpamState.load_state command in
  let file = OpamPath.backup t.root in
  OpamFilename.mkdir (OpamPath.backup_dir t.root);
  OpamFile.Export.write file (t.installed, t.installed_roots, t.pinned);
  try
    f t;
    OpamFilename.remove file (* We might want to keep it even if successful ? *)
  with
  | OpamGlobals.Exit 0 as e -> raise e
  | err ->
    let t1 = OpamState.load_state "backup-err" in
    if OpamPackage.Set.equal t.installed t1.installed &&
       OpamPackage.Set.equal t.installed_roots t1.installed_roots then
      OpamFilename.remove file
    else
      Printf.eprintf "The former package state can be restored with \
                      %s switch import %S%s\n"
        Sys.argv.(0) (OpamFilename.to_string file)
        (match !OpamGlobals.switch with
         | `Command_line sw -> Printf.sprintf " --switch %s" sw
         | _ -> "");
    raise err

let reinstall switch =
  OpamGlobals.switch := `Command_line (OpamSwitch.to_string switch);
  with_backup "switch-reinstall" reinstall_t

let remove switch =
  with_backup "switch-remove" (remove_t switch)

let import filename =
  with_backup "switch-import"
    (fun t ->
       let importfile = match filename with
         | None   -> OpamFile.Export.read_from_channel stdin
         | Some f -> OpamFile.Export.read f in
       import_t importfile t)

let () =
  OpamState.switch_reinstall_hook := reinstall

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
open OpamProcess.Job.Op
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "SWACT" fmt
let slog = OpamConsole.slog

let gen_global_config root switch =
  let map f l =
    List.rev_map (fun (s,p) -> OpamVariable.of_string s, S (f p)) l
  in
  let id x = x in
  let vars =
    map id [
      ("user" ,
       try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name
       with Not_found -> "user");
      ("group",
       try (Unix.getgrgid (Unix.getgid ())).Unix.gr_name
       with Not_found -> "group");
      ("make" , OpamStateConfig.(Lazy.force !r.makecmd));
      ("os"   , OpamStd.Sys.os_string ());
    ] @
    map OpamFilename.Dir.to_string
      [
        ("root", root);
        ("prefix", OpamPath.Switch.root root switch);
        ("lib", OpamPath.Switch.Default.lib_dir root switch);
        ("bin", OpamPath.Switch.Default.bin root switch);
        ("sbin", OpamPath.Switch.Default.sbin root switch);
        ("doc", OpamPath.Switch.Default.doc_dir root switch);
        ("stublibs", OpamPath.Switch.Default.stublibs root switch);
        ("toplevel", OpamPath.Switch.Default.toplevel root switch);
        ("man", OpamPath.Switch.Default.man_dir root switch);
        ("share", OpamPath.Switch.Default.share_dir root switch);
        ("etc", OpamPath.Switch.Default.etc_dir root switch);
      ]
  in
  OpamFile.Dot_config.create vars

let install_global_config root switch config =
  log "install_global_config switch=%a" (slog OpamSwitch.to_string) switch;

  OpamFile.Dot_config.write
    (OpamPath.Switch.global_config root switch)
    config

let create_empty_switch gt switch =
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

    let config = gen_global_config root switch in

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

    install_global_config root switch config;

    let root_config =
      OpamFile.Config.with_installed_switches gt.config
        (switch::OpamFile.Config.installed_switches gt.config)
    in
    OpamStateConfig.write root root_config;
    { gt with config = root_config }
  with e ->
    if not (OpamConsole.debug ()) then
      OpamFilename.rmdir switch_dir;
    raise e

(* - compiles and install $opam/compiler/[ocaml_version].comp in $opam/[switch]
   - update $opam/switch
   - update $opam/config

   deprecated *)
let install_compiler gt ~quiet:_ switch compiler =
  log "install_compiler switch=%a compiler=%a"
    (slog OpamSwitch.to_string) switch
    (slog OpamCompiler.to_string) compiler;

  let comp_f = OpamPath.compiler_comp gt.root compiler in
  if not (OpamFile.exists comp_f) then (
    OpamConsole.msg "Cannot find %s: %s is not a valid compiler name.\n"
      (OpamFile.to_string comp_f)
      (OpamCompiler.to_string compiler);
    OpamStd.Sys.exit 1;
  );
  let comp = OpamFile.Comp.read comp_f in

  let switch_dir = OpamPath.Switch.root gt.root switch in
  if OpamFile.Comp.preinstalled comp ||
     OpamFile.Comp.src comp = None
  then ()
  else
  (* Install the compiler *)
  let comp_url = match OpamFile.Comp.src comp with
    | Some f -> f
    | None   ->
      OpamConsole.error_and_exit
        "No source for compiler %s"
        (OpamCompiler.to_string compiler) in
  let build_dir = OpamPath.Switch.build_ocaml gt.root switch in
  let comp_name = OpamCompiler.to_string (OpamFile.Comp.name comp) in
  OpamConsole.header_msg "Installing compiler %s" comp_name;
  (match comp_url.OpamUrl.backend, OpamUrl.local_dir comp_url with
   | `rsync, Some dir -> OpamFilename.link_dir ~src:dir ~dst:build_dir
   | _ ->
     OpamProcess.Job.run @@
     OpamFilename.with_tmp_dir_job (fun download_dir ->
         let fake_pkg = OpamPackage.of_string "compiler.get" in
         let text =
           OpamProcess.make_command_text ~color:`magenta
             comp_name (OpamUrl.string_of_backend comp_url.OpamUrl.backend)
         in
         OpamProcess.Job.with_text text @@
         OpamRepository.pull_url fake_pkg download_dir None [comp_url]
         @@+ function
         | Not_available u ->
           OpamConsole.error_and_exit "%s is not available." u
         | Up_to_date r | Result r ->
           Done (OpamFilename.extract_generic_file r build_dir)
       ));
  let patches = OpamFile.Comp.patches comp in
  let patch_command url =
    let text =
      OpamProcess.make_command_text ~color:`magenta
        comp_name ~args:[OpamUrl.basename url] "download"
    in
    OpamProcess.Job.with_text text @@
    OpamDownload.download ~overwrite:true url build_dir
  in
  let patches =
    OpamParallel.map
      ~jobs:OpamStateConfig.(!r.dl_jobs)
      ~command:patch_command
      patches
  in
  List.iter (fun f -> OpamFilename.patch f build_dir) patches;
  OpamConsole.msg "Now compiling OCaml. This may take a while, \
                   please bear with us...\n";
  let commands =
    if OpamFile.Comp.configure comp @ OpamFile.Comp.make comp <> [] then
      [ ( "./configure" :: OpamFile.Comp.configure comp )
        @ [ "-prefix";  OpamFilename.Dir.to_string switch_dir ]
      (*-bindir %s/bin -libdir %s/lib -mandir %s/man*)
      (* NOTE In case it exists 2 '-prefix', in general the script
         ./configure will only consider the last one, others will be
         discarded. *)
      ; (OpamStateConfig.(Lazy.force !r.makecmd)::OpamFile.Comp.make comp)
      ; [OpamStateConfig.(Lazy.force !r.makecmd); "install" ]
      ]
    else
    let switch_config =
      OpamFile.Dot_config.read (OpamPath.Switch.global_config gt.root switch)
    in
    let env = OpamPackageVar.resolve_switch_raw gt switch switch_config in
    OpamFilter.commands env (OpamFile.Comp.build comp)
  in
  let commands =
    OpamStd.List.filter_map (function
        | [] -> None
        | cmd::args ->
          let text =
            OpamProcess.make_command_text ~color:`magenta comp_name
              ~args cmd
          in
          Some (OpamSystem.make_command
                  ~text
                  ~dir:(OpamFilename.Dir.to_string build_dir)
                  ~verbose:(OpamConsole.verbose ())
                  cmd args))
      commands
  in
  match
    OpamProcess.Job.run (OpamProcess.Job.of_list commands)
  with
  | None ->
    OpamConsole.msg "Done.\n";
    if not OpamStateConfig.(!r.keep_build_dir) then OpamFilename.rmdir build_dir
  | Some (cmd,err) ->
    OpamConsole.error_and_exit "Compiler build failed at %S:\n%s"
      (OpamProcess.string_of_command cmd)
      (OpamProcess.string_of_result err)

let write_selections st =
  if not OpamStateConfig.(!r.dryrun) then
    let f = OpamPath.Switch.selections st.switch_global.root st.switch in
    let env = OpamPath.Switch.environment st.switch_global.root st.switch in
    OpamFile.SwitchSelections.write f (OpamSwitchState.selections st);
    OpamFile.Environment.write env (OpamEnv.compute_updates st)

let add_to_reinstall gt switch switch_selections ~unpinned_only packages =
  log "add-to-reinstall unpinned_only:%b packages:%a" unpinned_only
    (slog OpamPackage.Set.to_string) packages;
  let { sel_installed = installed; sel_pinned = pinned; _ } =
    switch_selections
  in
  let packages =
    if unpinned_only then
      OpamPackage.Name.Map.fold
        (fun name (v,_) -> OpamPackage.Set.remove (OpamPackage.create name v))
        pinned packages
    else packages
  in
  let reinstall_file = OpamPath.Switch.reinstall gt.root switch in
  let reinstall =
    (OpamFile.PkgList.safe_read reinstall_file ++ packages) %% installed
  in
  if OpamPackage.Set.is_empty reinstall then
    OpamFilename.remove (OpamFile.filename reinstall_file)
  else
    OpamFile.PkgList.write reinstall_file reinstall

let set_current_switch gt switch =
  let config = OpamFile.Config.with_switch gt.config switch in
  let gt = { gt with config } in
  OpamStateConfig.write gt.root config;
  let rt = OpamRepositoryState.load gt in
  let st = OpamSwitchState.load gt rt switch in
  OpamEnv.write_dynamic_init_scripts st;
  st

let install_metadata st nv =
  let opam_mirror = OpamPath.opam st.switch_global.root nv in
  if OpamPackage.Name.Map.mem nv.name st.pinned ||
     OpamFile.exists opam_mirror then ()
  else (
    OpamFile.OPAM.write opam_mirror (OpamSwitchState.opam st nv);
    match OpamSwitchState.files st nv with
    | Some src ->
      OpamFilename.copy_dir ~src
        ~dst:(OpamPath.files st.switch_global.root nv)
    | None -> ()
  )

let remove_metadata st packages =
  let all_installed = OpamGlobalState.all_installed st.switch_global in
  let packages = packages -- all_installed in
  OpamPackage.Set.iter (fun nv ->
      let dir = OpamPath.packages st.switch_global.root nv in
      OpamFilename.rmdir dir;
      let parent = OpamFilename.dirname_dir dir in
      if OpamFilename.dir_is_empty parent then OpamFilename.rmdir parent;
(* !X This removed archives a bit too eagerly. To be replaced by e.g. a cleanup done only on [opam update]:
      let archive = OpamPath.archive t.root nv in
      OpamFilename.remove archive; *)
    ) packages

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
  let st =
    { st with
      installed;
      installed_roots = (installed_roots +! st.installed_roots) %% installed;
      reinstall;
      pinned = pinned +! st.pinned;
      compiler_packages; }
  in
  if not OpamStateConfig.(!r.dryrun) then (
    write_selections st;
    if not (OpamPackage.Set.equal reinstall0 reinstall) then
      OpamFile.PkgList.write
        (OpamPath.Switch.reinstall st.switch_global.root st.switch)
        reinstall
  );
  st

let add_to_installed st ?(root=false) nv =
  let name = nv.name in
  let st =
    update_switch_state st
      ~installed:(OpamPackage.Set.add nv st.installed)
      ~reinstall:(OpamPackage.Set.remove nv st.reinstall)
      ~installed_roots:
        (if root
         then OpamPackage.Set.add nv st.installed_roots
         else st.installed_roots)
  in
  let opam = OpamSwitchState.opam st nv in
  if not OpamStateConfig.(!r.dryrun) then (
    install_metadata st nv;
    if OpamFile.OPAM.env opam <> [] &&
       OpamSwitchState.is_switch_globally_set st
    then
      OpamEnv.write_dynamic_init_scripts st;
  );
  if OpamPackage.Set.mem nv st.compiler_packages &&
     List.mem Pkgflag_Compiler (OpamFile.OPAM.flags opam)
  then
    (* Make package variables global for compiler packages *)
    (* /!\ !X this can lead to inconsistencies in the 'available:' field of
       other packages if the values change during e.g. an upgrade of the
       compiler.

       The only way to avoid this would be to forbid upgrading both the compiler
       and other packages in one go. But we could at least check 'available:'
       before preforming the builds and issue a warning when this happens *)
    let switch_vars = OpamFile.Dot_config.bindings st.switch_config in
    let pkg_vars =
      OpamFile.Dot_config.bindings @@
      OpamFile.Dot_config.safe_read
        (OpamPath.Switch.config st.switch_global.root st.switch name)
    in
    let switch_config =
      OpamFile.Dot_config.with_vars st.switch_config (pkg_vars @ switch_vars)
    in
    if not OpamStateConfig.(!r.dryrun) then
      install_global_config st.switch_global.root st.switch switch_config;
    { st with switch_config }
  else
    st

let remove_from_installed st nv =
  let rm = OpamPackage.Set.remove nv in
  let st =
    update_switch_state st
      ~installed:(rm st.installed)
      ~installed_roots:(rm st.installed_roots)
      ~reinstall:(rm st.reinstall)
  in
  let opam = OpamSwitchState.opam st nv in
  if not OpamStateConfig.(!r.dryrun) &&
     OpamFile.OPAM.env (OpamSwitchState.opam st nv) <> [] &&
     OpamSwitchState.is_switch_globally_set st
  then
    (* note: don't remove_metadata just yet *)
    OpamEnv.write_dynamic_init_scripts st;
  if OpamPackage.Set.mem nv st.compiler_packages &&
     List.mem Pkgflag_Compiler (OpamFile.OPAM.flags opam)
  then
    (* Remove gobal variables from this compiler package *)
    let switch_vars = OpamFile.Dot_config.bindings st.switch_config in
    let pkg_vars =
      OpamFile.Dot_config.bindings @@
      OpamFile.Dot_config.safe_read
        (OpamPath.Switch.config st.switch_global.root st.switch
           nv.name)
    in
    let rev_vars, _ =
      List.fold_left (fun (vars,to_remove) (v,_ as binding) ->
          if List.mem_assoc v to_remove then (vars, List.remove_assoc v to_remove)
          else (binding::vars, to_remove))
        ([], pkg_vars) switch_vars
    in
    let switch_config =
      OpamFile.Dot_config.with_vars st.switch_config (List.rev rev_vars)
    in
    if not OpamStateConfig.(!r.dryrun) then
      install_global_config st.switch_global.root st.switch switch_config;
    { st with switch_config }
  else
    st

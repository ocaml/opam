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

let gen_global_config switch =
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
        ("root", OpamPath.root ());
        ("prefix", OpamPath.Switch.root switch);
        ("lib", OpamPath.Switch.Default.lib_dir switch);
        ("bin", OpamPath.Switch.Default.bin switch);
        ("sbin", OpamPath.Switch.Default.sbin switch);
        ("doc", OpamPath.Switch.Default.doc_dir switch);
        ("stublibs", OpamPath.Switch.Default.stublibs switch);
        ("toplevel", OpamPath.Switch.Default.toplevel switch);
        ("man", OpamPath.Switch.Default.man_dir switch);
        ("share", OpamPath.Switch.Default.share_dir switch);
        ("etc", OpamPath.Switch.Default.etc_dir switch);
      ]
  in
  OpamFile.Dot_config.create vars

let install_global_config switch config =
  log "install_global_config switch=%a" (slog OpamSwitch.to_string) switch;

  OpamFile.Dot_config.write
    (OpamPath.Switch.global_config switch)
    config

let create_empty_switch
    ?(compiler = OpamCompiler.of_string "empty") switch =
  log "create_empty_switch at %a" (slog OpamSwitch.to_string) switch;

  let switch_dir = OpamPath.Switch.root switch in

  (* Do some clean-up if necessary *)
  if OpamFilename.exists_dir switch_dir then
    failwith (Printf.sprintf "Directory %s already exists"
                (OpamFilename.Dir.to_string switch_dir));

  try
    (* Create base directories *)
    OpamFilename.mkdir switch_dir;

    let config = gen_global_config switch in

    OpamFilename.mkdir (OpamPath.Switch.lib_dir switch config);
    OpamFilename.mkdir (OpamPath.Switch.stublibs switch config);
    OpamFilename.mkdir (OpamPath.Switch.toplevel switch config);
    OpamFilename.mkdir (OpamPath.Switch.build_dir switch);
    OpamFilename.mkdir (OpamPath.Switch.bin switch config);
    OpamFilename.mkdir (OpamPath.Switch.sbin switch config);
    OpamFilename.mkdir (OpamPath.Switch.doc_dir switch config);
    OpamFilename.mkdir (OpamPath.Switch.man_dir switch config);
    OpamFilename.mkdir (OpamPath.Switch.install_dir switch);
    OpamFilename.mkdir (OpamPath.Switch.config_dir switch);
    List.iter (fun num ->
        OpamFilename.mkdir (OpamPath.Switch.man_dir ~num switch config)
      ) ["1";"1M";"2";"3";"4";"5";"6";"7";"9"];

    install_global_config switch config;

    let aliases_f = OpamPath.aliases () in
    let aliases = OpamFile.Aliases.safe_read aliases_f in
    OpamFile.Aliases.write aliases_f
      (OpamSwitch.Map.add switch compiler aliases);
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

  let comp_f = OpamPath.compiler_comp compiler in
  if not (OpamFilename.exists comp_f) then (
    OpamConsole.msg "Cannot find %s: %s is not a valid compiler name.\n"
      (OpamFilename.to_string comp_f)
      (OpamCompiler.to_string compiler);
    OpamStd.Sys.exit 1;
  );
  let comp = OpamFile.Comp.read comp_f in

  create_empty_switch ~compiler switch;

  let switch_dir = OpamPath.Switch.root switch in
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
  let build_dir = OpamPath.Switch.build_ocaml switch in
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
      OpamFile.Dot_config.read (OpamPath.Switch.global_config switch)
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

let write_state_file st =
  if not OpamStateConfig.(!r.dryrun) then
    let f = OpamPath.Switch.state st.switch in
    let env = OpamPath.Switch.environment st.switch in
    OpamFile.State.write f (OpamSwitchState.state_file st);
    OpamFile.Environment.write env (OpamEnv.compute_updates st)

let add_to_reinstall gt switch switch_state_file ~unpinned_only packages =
  log "add-to-reinstall unpinned_only:%b packages:%a" unpinned_only
    (slog OpamPackage.Set.to_string) packages;
  let { OpamFile.State.installed; pinned; _ } = switch_state_file in
  let packages =
    if unpinned_only then
      OpamPackage.Set.filter
        (fun nv -> not OpamPackage.(Name.Map.mem (name nv) pinned))
        packages
    else packages
  in
  let reinstall_file = OpamPath.Switch.reinstall switch in
  let reinstall =
    (OpamFile.PkgList.safe_read reinstall_file ++ packages) %% installed
  in
  if OpamPackage.Set.is_empty reinstall then
    OpamFilename.remove reinstall_file
  else
    OpamFile.PkgList.write reinstall_file reinstall

let set_current_switch gt switch =
  let config = OpamFile.Config.with_switch gt.config switch in
  let gt = { gt with config } in
  OpamStateConfig.write (OpamPath.config ()) config;
  let rt = OpamRepositoryState.load gt in
  let st = OpamSwitchState.load gt rt switch in
  OpamEnv.update_init_scripts st ~global:None;
  st

let install_metadata st nv =
  let opam_mirror = OpamPath.opam nv in
  if OpamPackage.Name.Map.mem (OpamPackage.name nv) st.pinned ||
     OpamFilename.exists opam_mirror then ()
  else (
    OpamFile.OPAM.write opam_mirror (OpamSwitchState.opam st nv);
    match OpamSwitchState.files st nv with
    | Some src ->
      OpamFilename.copy_dir ~src
        ~dst:(OpamPath.files nv)
    | None -> ()
  )

let remove_metadata st packages =
  let all_installed = OpamGlobalState.all_installed st.switch_global in
  let packages = packages -- all_installed in
  OpamPackage.Set.iter (fun nv ->
    let dir = OpamPath.packages nv in
    OpamFilename.rmdir dir;
    let parent = OpamFilename.dirname_dir dir in
    if OpamFilename.dir_is_empty parent then OpamFilename.rmdir parent;
(* !X This removed archives a bit too eagerly. To be replaced by e.g. a cleanup done only on [opam update]:
    let archive = OpamPath.archive t.root nv in
    OpamFilename.remove archive; *)
  ) packages

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

let log ?level fmt = OpamConsole.log ?level "ACTION" fmt
let slog = OpamConsole.slog

open OpamTypes
open OpamFilename.Op
open OpamStateTypes
open OpamProcess.Job.Op

module PackageActionGraph = OpamSolver.ActionGraph

(* Install the package files *)
let process_dot_install st nv =
  let root = st.switch_global.root in
  if OpamStateConfig.(!r.dryrun) then
      OpamConsole.msg "Installing %s.\n" (OpamPackage.to_string nv)
  else
  let build_dir = OpamPath.Switch.build root st.switch nv in
  if OpamFilename.exists_dir build_dir then OpamFilename.in_dir build_dir (fun () ->

      log "Installing %s.\n" (OpamPackage.to_string nv);
      let name = nv.name in
      let config_f = OpamPath.Switch.build_config root st.switch nv in
      let config = OpamFile.Dot_config.read_opt config_f in
      let install_f = OpamPath.Switch.build_install root st.switch nv in
      let install = OpamFile.Dot_install.safe_read install_f in

      (* .install *)
      let install_f = OpamPath.Switch.install root st.switch name in
      if install <> OpamFile.Dot_install.empty then
        OpamFile.Dot_install.write install_f install;

      (* .config *)
      (match config with
       | Some config ->
         let dot_config = OpamPath.Switch.config root st.switch name in
         OpamFile.Dot_config.write dot_config config
       | None -> ());

      let warnings = ref [] in
      let check ~src ~dst base =
        let src_file = OpamFilename.create src base.c in
        if base.optional && not (OpamFilename.exists src_file) then
          log "Not installing %a is not present and optional."
            (slog OpamFilename.to_string) src_file;
        if not base.optional && not (OpamFilename.exists src_file) then (
          warnings := (dst, base.c) :: !warnings
        );
        OpamFilename.exists src_file in

      (* Install a list of files *)
      let install_files exec dst_fn files_fn =
        let dst_dir = dst_fn root st.switch name in
        let files = files_fn install in
        if not (OpamFilename.exists_dir dst_dir) && files <> [] then (
          log "creating %a" (slog OpamFilename.Dir.to_string) dst_dir;
          OpamFilename.mkdir dst_dir;
        );
        List.iter (fun (base, dst) ->
            let src_file = OpamFilename.create build_dir base.c in
            let dst_file = match dst with
              | None   -> OpamFilename.create dst_dir (OpamFilename.basename src_file)
              | Some d -> OpamFilename.create dst_dir d in
            if check ~src:build_dir ~dst:dst_dir base then
              OpamFilename.install ~exec ~src:src_file ~dst:dst_file ();
          ) files in

      let module P = OpamPath.Switch in
      let module I = OpamFile.Dot_install in
      let instdir_gen fpath r s _ = fpath r s st.switch_config in
      let instdir_pkg fpath r s n = fpath r s st.switch_config n in

      (* bin *)
      install_files true (instdir_gen P.bin) I.bin;

      (* sbin *)
      install_files true (instdir_gen P.sbin) I.sbin;

      (* lib *)
      install_files false (instdir_pkg P.lib) I.lib;
      install_files true (instdir_pkg P.lib) I.libexec;

      (* toplevel *)
      install_files false (instdir_gen P.toplevel) I.toplevel;

      install_files true (instdir_gen P.stublibs) I.stublibs;

      (* Man pages *)
      install_files false (instdir_gen P.man_dir) I.man;

      (* Shared files *)
      install_files false (instdir_pkg P.share) I.share;
      install_files false (instdir_gen P.share_dir) I.share_root;

      (* Etc files *)
      install_files false (instdir_pkg P.etc) I.etc;

      (* Documentation files *)
      install_files false (instdir_pkg P.doc) I.doc;

      (* misc *)
      List.iter
        (fun (src, dst) ->
          let src_file = OpamFilename.create (OpamFilename.cwd ()) src.c in
          if OpamFilename.exists dst
          && OpamConsole.confirm "Overwriting %s ?" (OpamFilename.to_string dst) then
            OpamFilename.install ~src:src_file ~dst ()
          else begin
            OpamConsole.msg "Installing %s to %s.\n"
              (OpamFilename.Base.to_string src.c) (OpamFilename.to_string dst);
            if OpamConsole.confirm "Continue ?" then
              OpamFilename.install ~src:src_file ~dst ()
          end
        ) (I.misc install);

      if !warnings <> [] then (
        let print (dir, base) =
          Printf.sprintf "  - %s to %s\n"
            (OpamFilename.to_string (OpamFilename.create build_dir base))
            (OpamFilename.Dir.to_string dir) in
        OpamConsole.error "Installation of %s failed"
          (OpamPackage.to_string nv);
        let msg =
          Printf.sprintf
            "Some files in %s couldn't be installed:\n%s"
            (OpamFile.to_string install_f)
            (String.concat "" (List.map print !warnings))
        in
        failwith msg
      )
    );
  if not (OpamStateConfig.(!r.keep_build_dir) || (OpamConsole.debug ())) then
    OpamFilename.rmdir build_dir

(* Prepare the package build:
   * apply the patches
   * substitute the files *)
let prepare_package_build st nv =
  let opam = OpamSwitchState.opam st nv in

  (* Substitute the patched files.*)
  let patches = OpamFile.OPAM.patches opam in

  let iter_patches f =
    List.fold_left (fun acc (base, filter) ->
        if OpamFilter.opt_eval_to_bool (OpamPackageVar.resolve ~opam st) filter
        then
          try f base; acc with e ->
            OpamStd.Exn.fatal e; OpamFilename.Base.to_string base :: acc
        else acc
      ) [] patches in

  let print_apply basename =
    log "%s: applying %s.\n" (OpamPackage.name_to_string nv)
      (OpamFilename.Base.to_string basename);
    if OpamConsole.verbose () then
      OpamConsole.msg "[%s: patch] applying %s\n"
        (OpamConsole.colorise `green (OpamPackage.name_to_string nv))
        (OpamFilename.Base.to_string basename)
  in

  if OpamStateConfig.(!r.dryrun) || OpamStateConfig.(!r.fake) then
    ignore (iter_patches print_apply)
  else

  let p_build = OpamPath.Switch.build st.switch_global.root st.switch nv in

  OpamFilename.mkdir p_build;
  OpamFilename.in_dir p_build (fun () ->
    let all = OpamFile.OPAM.substs opam in
    let patches =
      OpamStd.List.filter_map (fun (f,_) ->
        if List.mem f all then Some f else None
      ) patches in
    List.iter
      (OpamFilter.expand_interpolations_in_file (OpamPackageVar.resolve ~opam st))
      patches
  );

  (* Apply the patches *)
  let patching_errors =
    iter_patches (fun base ->
      let root = OpamPath.Switch.build st.switch_global.root st.switch nv in
      let patch = root // OpamFilename.Base.to_string base in
      print_apply base;
      OpamFilename.patch patch p_build)
  in

  (* Substitute the configuration files. We should be in the right
     directory to get the correct absolute path for the
     substitution files (see [substitute_file] and
     [OpamFilename.of_basename]. *)
  OpamFilename.in_dir p_build (fun () ->
    List.iter
      (OpamFilter.expand_interpolations_in_file (OpamPackageVar.resolve ~opam st))
      (OpamFile.OPAM.substs opam)
  );
  if patching_errors <> [] then (
    let msg =
      Printf.sprintf "These patches didn't apply at %s:\n%s"
        (OpamFilename.Dir.to_string
           (OpamPath.Switch.build st.switch_global.root st.switch nv))
        (OpamStd.Format.itemize (fun x -> x) patching_errors)
    in
    failwith msg
  )

let download_package st nv =
  log "download_package: %a" (slog OpamPackage.to_string) nv;
  let name = nv.name in
  if OpamStateConfig.(!r.dryrun) || OpamStateConfig.(!r.fake) then
    Done (`Successful None)
  else
  let dev_dir =
    if OpamSwitchState.is_dev_package st nv then
      Some (OpamPath.Switch.dev_package st.switch_global.root st.switch name)
    else None
  in
  let of_dl = function
    | Some (Up_to_date f | Result f) -> `Successful (Some f)
    | Some (Not_available s) -> `Error s
    | None -> `Successful None
  in
  let job = match dev_dir with
    | Some dir ->
      OpamUpdate.download_upstream st nv dir @@| of_dl
    | None ->
      OpamRepositoryState.download_archive st.switch_repos
        (OpamGlobalState.repos_list st.switch_global)
        nv
      @@+ function
      | Some f ->
        Done (`Successful (Some (F f)))
      | None ->
        let dir =
          OpamPath.Switch.dev_package st.switch_global.root st.switch nv.name
        in
        OpamUpdate.download_upstream st nv dir @@| of_dl
  in
  OpamProcess.Job.catch (fun e -> Done (`Error (Printexc.to_string e))) job

let extract_package st source nv =
  log "extract_package: %a from %a"
    (slog OpamPackage.to_string) nv
    (slog (OpamStd.Option.to_string OpamTypesBase.string_of_generic_file))
    source;
  if OpamStateConfig.(!r.dryrun) then () else
  let build_dir = OpamPath.Switch.build st.switch_global.root st.switch nv in
  OpamFilename.rmdir build_dir;
  let () =
    match source with
    | None -> ()
    | Some (D dir) -> OpamFilename.copy_dir ~src:dir ~dst:build_dir
    | Some (F archive) -> OpamFilename.extract archive build_dir
  in
  let is_repackaged_archive =
    Some (F (OpamPath.archive st.switch_global.root nv)) = source
  in
  if not is_repackaged_archive then (
    (* !X this should be done during the download phase, but at
       the moment it assumes a single file *)
    let opam = OpamSwitchState.opam st nv in
    OpamParallel.iter
      ~jobs:OpamStateConfig.(!r.dl_jobs)
      ~command:(fun (url,checksum,fname) ->
          let fname =
            OpamStd.Option.default
              (OpamFilename.Base.of_string (OpamUrl.basename url))
              fname
          in
          OpamDownload.download_as
            ~overwrite:true
            ~checksum url
            (OpamFilename.create build_dir fname))
      (OpamFile.OPAM.extra_sources opam);

    List.iter (fun (src,base,hash) ->
        if OpamFilename.digest src <> hash then
          OpamConsole.error_and_exit "Bad hash for %s"
            (OpamFilename.to_string src);
        OpamFilename.copy ~src ~dst:(OpamFilename.create build_dir base))
      (OpamFile.OPAM.get_extra_files opam)
  );
  prepare_package_build st nv

(* unused ?
let string_of_commands commands =
  let commands_s = List.map (fun cmd -> String.concat " " cmd)  commands in
  "  "
  ^ if commands_s <> [] then
    String.concat "\n  " commands_s
  else
    "Nothing to do."
*)

let compilation_env t opam =
  let env0 = OpamEnv.get_full ~force_path:true t in
  let env1 = [
    ("MAKEFLAGS", "", None);
    ("MAKELEVEL", "", None);
    ("OPAM_PACKAGE_NAME",
     OpamPackage.Name.to_string (OpamFile.OPAM.name opam),
     None);
    ("OPAM_PACKAGE_VERSION",
     OpamPackage.Version.to_string (OpamFile.OPAM.version opam),
     None);
  ] @ env0 in
  OpamEnv.add env1 (OpamFile.OPAM.build_env opam)

let removal_needs_download st nv =
  match OpamSwitchState.opam_opt st nv with
  | None ->
    OpamConsole.warning
      "No opam file found to remove package %s. Stale files may remain."
      (OpamPackage.to_string nv);
    false
  | Some opam ->
    if OpamFile.OPAM.has_flag Pkgflag_LightUninstall opam then false
    else
    let commands =
      OpamFilter.commands (OpamPackageVar.resolve ~opam st)
        (OpamFile.OPAM.remove opam) in
    (* We use a small hack: if the remove command is simply
       'ocamlfind remove xxx' then, no need to extract the archive
       again. *)
    let use_ocamlfind = function
      | [] -> true
      | "ocamlfind" :: _ -> true
      | "rm" :: _ -> true
      | _ -> false in
    not (List.for_all use_ocamlfind commands)

let cmd_wrapper t opam getter cmd args =
  match
    OpamFilter.commands (OpamPackageVar.resolve ~opam t)
      [getter t.switch_global.config, None]
  with
  | [wrap_cmd::wrap_args] ->
    wrap_cmd, wrap_args @ (cmd :: args)
  | [] | [[]] -> cmd, args
  | _::_::_ -> assert false

(* Remove a given package *)
let remove_package_aux
    t ?(keep_build=false) ?(silent=false) ?changes ?force nv =
  log "Removing %a" (slog OpamPackage.to_string) nv;
  let name = nv.name in

  (* There are three uninstall stages:
     1. execute the package's remove script
     2. remove remaining files listed in the .install file
     3. remove remaining files added in the changes file (or changes parameter)

     The 3. step alone could be sufficient, but:
     - changes only revert additions, not any file changes, so 1. is needed
     - the remove script might take extra actions (stop daemon...)
     - existing installs don't have .changes files yet
     - 1st and 2nd steps may help recover partial/failed states
  *)

  (* Run the remove script *)
  let opam = OpamSwitchState.opam_opt t nv in
  let dot_install =
    OpamPath.Switch.install t.switch_global.root t.switch name
  in
  let changes_file =
    OpamPath.Switch.changes t.switch_global.root t.switch name
  in
  let remove_commands_job =
    match opam with
    | None ->
      log "No opam file was found for removing %a\n"
        (slog OpamPackage.to_string) nv;
      Done ()
    | Some opam ->
      let env = compilation_env t opam in
      let p_build = OpamPath.Switch.build t.switch_global.root t.switch nv in
      let remove =
        OpamFilter.commands (OpamPackageVar.resolve ~opam t)
          (OpamFile.OPAM.remove opam) in
      let name = OpamPackage.Name.to_string name in
      let exec_dir, nameopt =
        if OpamFilename.exists_dir p_build
        then p_build, Some name
        else t.switch_global.root , None in
      let commands =
        OpamStd.List.filter_map (function
            | [] -> None
            | cmd::args ->
              let text = OpamProcess.make_command_text name ~args cmd in
              let cmd, args =
                cmd_wrapper t opam OpamFile.Config.wrap_remove cmd args
              in
              Some
                (OpamSystem.make_command ?name:nameopt ~text cmd args
                   ~env:(OpamTypesBase.env_array env)
                   ~dir:(OpamFilename.Dir.to_string exec_dir)
                   ~verbose:(OpamConsole.verbose ())
                   ~check_existence:false))
          remove
      in
      OpamProcess.Job.of_list ~keep_going:true commands
      @@+ function
      | Some (_,err) ->
        if not silent then
          OpamConsole.warning "package uninstall script failed:\n%s"
            (OpamProcess.string_of_result err);
        Done ()
      | None -> Done ()
  in

  (* handle .install file *)
  let uninstall_files () =
    let install =
      OpamFile.Dot_install.safe_read dot_install
    in
    let remove_files dst_fn files =
      let files = files install in
      let dst_dir = dst_fn t.switch_global.root t.switch t.switch_config in
      List.iter (fun (base, dst) ->
          let dst_file = match dst with
            | None   -> dst_dir // Filename.basename (OpamFilename.Base.to_string base.c)
            | Some b -> OpamFilename.create dst_dir b in
          OpamFilename.remove dst_file
        ) files
    in
    let remove_files_and_dir dst_fn files =
      let dir = dst_fn t.switch_global.root t.switch t.switch_config name in
      remove_files (fun _ _ _ -> dir) files;
      if OpamFilename.rec_files dir = [] then OpamFilename.rmdir dir
    in

    log "Removing files from .install";
    remove_files OpamPath.Switch.sbin OpamFile.Dot_install.sbin;
    remove_files OpamPath.Switch.bin OpamFile.Dot_install.bin;
    remove_files_and_dir
      OpamPath.Switch.lib OpamFile.Dot_install.libexec;
    remove_files_and_dir OpamPath.Switch.lib OpamFile.Dot_install.lib;
    remove_files OpamPath.Switch.stublibs OpamFile.Dot_install.stublibs;
    remove_files_and_dir OpamPath.Switch.share OpamFile.Dot_install.share;
    remove_files OpamPath.Switch.share_dir OpamFile.Dot_install.share_root;
    remove_files_and_dir OpamPath.Switch.etc OpamFile.Dot_install.etc;
    remove_files (OpamPath.Switch.man_dir ?num:None) OpamFile.Dot_install.man;
    remove_files_and_dir OpamPath.Switch.doc OpamFile.Dot_install.doc;

    (* Remove the misc files *)
    log "Removing the misc files";
    List.iter (fun (_,dst) ->
        if OpamFilename.exists dst then begin
          OpamConsole.msg "Removing %s." (OpamFilename.to_string dst);
          if OpamConsole.confirm "Continue ?" then
            OpamFilename.remove dst
        end
      ) (OpamFile.Dot_install.misc install);
  in

  let revert_changes () =
    let changes = match changes with
      | None -> OpamFile.Changes.read_opt changes_file
      | some -> some
    in
    OpamStd.Option.iter
      (OpamDirTrack.revert ~verbose:(not silent) ?force
         (OpamPath.Switch.root t.switch_global.root t.switch))
      changes
  in

  remove_commands_job @@+ fun () ->
  if not OpamStateConfig.(!r.dryrun) then (
    OpamFilename.remove
      (OpamFile.filename
         (OpamPath.Switch.config t.switch_global.root t.switch name));
    if not (keep_build || OpamStateConfig.(!r.keep_build_dir)) then
      OpamFilename.rmdir
        (OpamPath.Switch.build t.switch_global.root t.switch nv);
    uninstall_files ();
    OpamFilename.remove (OpamFile.filename dot_install);
    revert_changes ();
    OpamFilename.remove (OpamFile.filename changes_file);
  );
  if not silent then
    OpamConsole.msg "%s removed   %s.%s\n"
      (if not (OpamConsole.utf8 ()) then "->" else
         OpamActionGraph.(action_color (`Remove ())
                            (action_strings (`Remove ()))))
      (OpamConsole.colorise `bold (OpamPackage.name_to_string nv))
      (OpamPackage.version_to_string nv);
  Done ()


(* Removes build dir and source cache of package if unneeded *)
let cleanup_package_artefacts t nv =
  log "Cleaning up artefacts of %a" (slog OpamPackage.to_string) nv;

  let build_dir = OpamPath.Switch.build t.switch_global.root t.switch nv in
  if not OpamStateConfig.(!r.keep_build_dir) &&
     OpamFilename.exists_dir build_dir then
    OpamFilename.rmdir build_dir;
  let name = nv.name in
  let dev_dir =
    OpamPath.Switch.dev_package t.switch_global.root t.switch name
  in
  if not (OpamPackage.Set.mem nv t.installed) then (
    if OpamFilename.exists_dir dev_dir then (
      log "Cleaning-up the switch repository";
      OpamFilename.rmdir dev_dir );
    log "Removing the local metadata";
    OpamSwitchAction.remove_metadata t (OpamPackage.Set.singleton nv);
  )

let sources_needed st g =
  PackageActionGraph.fold_vertex (fun act acc ->
      match act with
      | `Remove nv ->
        if removal_needs_download st nv
        then OpamPackage.Set.add nv acc else acc
      | `Install nv -> OpamPackage.Set.add nv acc
      | _ -> assert false)
    g OpamPackage.Set.empty

let remove_package t ?keep_build ?silent ?changes ?force nv =
  if OpamStateConfig.(!r.fake) || OpamStateConfig.(!r.show) then
    Done (OpamConsole.msg "Would remove: %s.\n" (OpamPackage.to_string nv))
  else
    remove_package_aux t ?keep_build ?silent ?changes ?force nv

(* Compiles a package.
   Assumes the package has already been downloaded to [source].
*)
let build_package t source nv =
  extract_package t source nv;
  let opam = OpamSwitchState.opam t nv in
  let commands =
    OpamFile.OPAM.build opam @
    (if OpamStateConfig.(!r.build_test)
     then OpamFile.OPAM.build_test opam else []) @
    (if OpamStateConfig.(!r.build_doc)
     then OpamFile.OPAM.build_doc opam else [])
  in
  let commands =
    OpamFilter.commands (OpamPackageVar.resolve ~opam t) commands
  in
  let env = OpamTypesBase.env_array (compilation_env t opam) in
  let name = OpamPackage.name_to_string nv in
  let dir = OpamPath.Switch.build t.switch_global.root t.switch nv in
  let rec run_commands = function
    | (cmd::args)::commands ->
      let text = OpamProcess.make_command_text name ~args cmd in
      let dir = OpamFilename.Dir.to_string dir in
      let cmd, args = cmd_wrapper t opam OpamFile.Config.wrap_build cmd args in
      OpamSystem.make_command ~env ~name ~dir ~text
        ~verbose:(OpamConsole.verbose ()) ~check_existence:false
        cmd args
      @@> fun result ->
      if OpamProcess.is_success result then
        run_commands commands
      else
        (OpamConsole.error
           "The compilation of %s failed at %S."
           name (String.concat " " (cmd::args));
         Done (Some (OpamSystem.Process_error result)))
    | []::commands -> run_commands commands
    | [] -> Done None
  in
  run_commands commands

(* Assumes the package has already been compiled in its build dir.
   Does not register the installation in the metadata ! *)
let install_package t nv =
  let opam = OpamSwitchState.opam t nv in
  let commands = OpamFile.OPAM.install opam in
  let commands = OpamFilter.commands (OpamPackageVar.resolve ~opam t) commands in
  let env = OpamTypesBase.env_array (compilation_env t opam) in
  let name = OpamPackage.name_to_string nv in
  let dir = OpamPath.Switch.build t.switch_global.root t.switch nv in
  let rec run_commands = function
    | (cmd::args)::commands ->
      let text = OpamProcess.make_command_text name ~args cmd in
      let dir = OpamFilename.Dir.to_string dir in
      let cmd, args =
        cmd_wrapper t opam OpamFile.Config.wrap_install cmd args
      in
      OpamSystem.make_command ~env ~name ~dir ~text
        ~verbose:(OpamConsole.verbose ()) ~check_existence:false
        cmd args
      @@> fun result ->
      if OpamFile.OPAM.has_flag Pkgflag_Verbose opam then
        List.iter (OpamConsole.msg "%s\n") result.OpamProcess.r_stdout;
      if OpamProcess.is_success result then
        run_commands commands
      else (
        OpamConsole.error
          "The installation of %s failed at %S."
          name (String.concat " " (cmd::args));
        Done (Some (OpamSystem.Process_error result))
      )
    | []::commands -> run_commands commands
    | [] -> Done None
  in
  let install_job () =
    run_commands commands @@+ function
    | Some _ as err -> Done err
    | None ->
      try
        process_dot_install t nv;
        OpamConsole.msg "%s installed %s.%s\n"
          (if not (OpamConsole.utf8 ()) then "->"
           else OpamActionGraph.
                  (action_color (`Install ()) (action_strings (`Install ()))))
          (OpamConsole.colorise `bold name)
          (OpamPackage.version_to_string nv);
        Done None
      with e -> Done (Some e)
  in
  let root = t.switch_global.root in
  let switch_prefix = OpamPath.Switch.root root t.switch in
  let rel_meta_dir =
    OpamFilename.(Base.of_string (remove_prefix_dir switch_prefix
                                    (OpamPath.Switch.meta root t.switch)))
  in
  OpamDirTrack.track switch_prefix
    ~except:(OpamFilename.Base.Set.singleton rel_meta_dir)
    install_job
  @@+ function
  | Some e, changes ->
    remove_package t ~keep_build:true ~silent:true ~changes nv @@| fun () ->
    OpamStd.Exn.fatal e;
    Some e
  | None, changes ->
    let changes_f = OpamPath.Switch.changes root t.switch nv.name in
    OpamFile.Changes.write changes_f changes;
    Done None

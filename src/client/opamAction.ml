(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let log ?level fmt = OpamConsole.log ?level "ACTION" fmt
let slog = OpamConsole.slog

open OpamTypes
open OpamFilename.Op
open OpamStateTypes
open OpamProcess.Job.Op

module PackageActionGraph = OpamSolver.ActionGraph

(* Preprocess install: returns a list of files to install, and their respective
   install functions *)
let preprocess_dot_install_t st nv build_dir =
  if not (OpamFilename.exists_dir build_dir) then [], None else
  let root = st.switch_global.root in
  let switch_prefix = OpamPath.Switch.root root st.switch in
  let file_wo_prefix f = OpamFilename.remove_prefix switch_prefix f in

  let name = nv.name in
  let install_f = OpamPath.Builddir.install build_dir nv in
  let install = OpamFile.Dot_install.safe_read install_f in

  (* .install *)
  let install_loc = OpamPath.Switch.install root st.switch name in
  if install <> OpamFile.Dot_install.empty then
    OpamFile.Dot_install.write install_loc install;

  (* .config *)
  let (files_and_installs, config) =
    let config_f = OpamPath.Builddir.config build_dir nv in
    let config = OpamFile.Dot_config.read_opt config_f in
    (match config with
     | Some config ->
       let file =
         OpamPath.Switch.config root st.switch name
         |> OpamFile.filename
         |> file_wo_prefix
       in
       let inst _ =
         let dot_config = OpamPath.Switch.config root st.switch name in
         OpamFile.Dot_config.write dot_config config; None
       in
       ([(file, inst)], Some config)
     | None -> ([], None))
  in

  let check ~src ~dst base =
    let src_file = OpamFilename.create src base.c in
    if base.optional && not (OpamFilename.exists src_file) then
      log "Not installing %a is not present and optional."
        (slog OpamFilename.to_string) src_file;
    let exists = OpamFilename.exists src_file in
    let warn =
      if not base.optional && not exists then
        Some (dst, base.c) else None
    in
    exists, warn
  in

  (* Install a list of files *)
  let install_files (exec, dst_fn, files_fn) =
    let dst_dir = dst_fn root st.switch name in
    let files = files_fn install in
    let dir_and_install =
      if OpamFilename.exists_dir dst_dir || files = [] then [] else
      let dir = OpamFilename.remove_prefix_dir switch_prefix dst_dir in
      let inst _ =
        log "creating %a" (slog OpamFilename.Dir.to_string) dst_dir;
        OpamFilename.mkdir dst_dir;
        None
      in
      [dir, inst]
    in
    dir_and_install @ List.rev @@
    List.rev_map (fun (base, dst) ->
        let (base, append) =
          if exec &&
             not (OpamFilename.exists (OpamFilename.create build_dir base.c))
          then
            let base' =
              {base with c = OpamFilename.Base.add_extension base.c "exe"} in
            if OpamFilename.exists (OpamFilename.create build_dir base'.c) then
              (OpamConsole.warning
                 ".install file is missing .exe extension for %s"
                 (OpamFilename.Base.to_string base.c);
               (base', true))
            else
              (base, false)
          else
            (base, false) in
        let src_file = OpamFilename.create build_dir base.c in
        let dst_file = match dst with
          | None   -> OpamFilename.create dst_dir (OpamFilename.basename src_file)
          | Some d ->
            if append && not (OpamFilename.Base.check_suffix d ".exe") then
              OpamFilename.create dst_dir
                (OpamFilename.Base.add_extension d "exe")
            else
              OpamFilename.create dst_dir d in
        let file = file_wo_prefix dst_file in
        let inst warning =
          if append then warning (OpamFilename.to_string src_file) `Add_exe;
          let check, warn = check ~src:build_dir ~dst:dst_dir base in
          if check then
            OpamFilename.install ~warning ~exec ~src:src_file ~dst:dst_file ();
          warn
        in
        file, inst)
      files
  in

  let module P = OpamPath.Switch in
  let module I = OpamFile.Dot_install in
  let instdir_gen fpath r s _ = fpath r s st.switch_config in
  let instdir_pkg fpath r s n = fpath r s st.switch_config n in

  let to_install = [
    (* bin *)
    true, (instdir_gen P.bin), I.bin;

    (* sbin *)
    true, (instdir_gen P.sbin), I.sbin;

    (* lib *)
    false, (instdir_pkg P.lib), I.lib;
    true, (instdir_pkg P.lib), I.libexec;
    false, (instdir_gen P.lib_dir), I.lib_root;
    true, (instdir_gen P.lib_dir), I.libexec_root;

    (* toplevel *)
    false, (instdir_gen P.toplevel), I.toplevel;

    true, (instdir_gen P.stublibs), I.stublibs;

    (* Man pages *)
    false, (instdir_gen P.man_dir), I.man;

    (* Shared files *)
    false, (instdir_pkg P.share), I.share;
    false, (instdir_gen P.share_dir), I.share_root;

    (* Etc files *)
    false, (instdir_pkg P.etc), I.etc;

    (* Documentation files *)
    false, (instdir_pkg P.doc), I.doc;
  ]
  in

  let files_and_installs =
    List.fold_left (fun acc toi -> List.rev_append (install_files toi) acc)
      files_and_installs to_install
  in

  (* misc *)
  let misc_files =
    List.map (fun (src, dst) ->
        let file = file_wo_prefix dst in
        let inst warning =
          let src_file = OpamFilename.create (OpamFilename.cwd ()) src.c in
          if OpamFilename.exists dst
          && OpamConsole.confirm "Overwriting %s?" (OpamFilename.to_string dst) then
            OpamFilename.install ~warning ~src:src_file ~dst ()
          else begin
            OpamConsole.msg "Installing %s to %s.\n"
              (OpamFilename.Base.to_string src.c) (OpamFilename.to_string dst);
            if OpamConsole.confirm "Continue?" then
              OpamFilename.install ~warning ~src:src_file ~dst ()
          end;
          None
        in
        (file, inst)) (I.misc install)
  in

  List.rev_append files_and_installs misc_files, config

(* Returns function to install package files from [.install] *)
let preprocess_dot_install st nv build_dir =
  let files_and_installs, config = preprocess_dot_install_t st nv build_dir in
  let root = st.switch_global.root in
  let files, installs = List.split files_and_installs in
  let really_process_dot_install () =
    if OpamStateConfig.(!r.dryrun) then
      OpamConsole.msg "Installing %s.\n" (OpamPackage.to_string nv)
    else if OpamFilename.exists_dir build_dir then
      let (warning, had_windows_warnings) =
        if OpamFormatConfig.(!r.strict) then
          let had_warnings = ref false in
          let install_warning dst warning =
            let () =
              match warning with
              | `Add_exe | `Install_script | `Cygwin | `Cygwin_libraries ->
                had_warnings := true
              | _ ->
                ()
            in
            OpamSystem.default_install_warning dst warning
          in
          (install_warning, (fun () -> !had_warnings))
        else
          (OpamSystem.default_install_warning, (fun () -> false))
      in
      OpamFilename.in_dir build_dir @@ fun () ->
      log "Installing %s.\n" (OpamPackage.to_string nv);
      let warnings =
        OpamStd.List.filter_map (fun install -> install warning) installs
      in
      if warnings <> [] then
        (let install_f = OpamPath.Switch.install root st.switch nv.name in
         let print (dir, base) =
           Printf.sprintf "  - %s to %s\n"
             (OpamFilename.to_string (OpamFilename.create build_dir base))
             (OpamFilename.Dir.to_string dir)
         in
         OpamConsole.error "Installation of %s failed"
           (OpamPackage.to_string nv);
         let msg =
           Printf.sprintf
             "Some files in %s couldn't be installed:\n%s"
             (OpamFile.to_string install_f)
             (String.concat "" (List.map print warnings))
         in
         failwith msg);
      if had_windows_warnings () then
        failwith "Strict mode is enabled - previous warnings considered fatal"
  in
  files, really_process_dot_install, config

let download_shared_source st url nvs =
  let labelise pkg_str = OpamStd.List.concat_map ", " pkg_str nvs in
  log "download_package: %a%a"
    (slog (fun _ -> labelise OpamPackage.to_string)) ()
    (slog (fun url -> match url, nvs with
         | None, _ | _, [_] -> ""
         | Some url, _ -> " " ^ OpamUrl.to_string (OpamFile.URL.url url))) url;
  if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.fake)
  then Done None else
  let nvs =
    (* filter out version-pinned packages since we already have their source *)
    List.filter (fun nv ->
        let dir = OpamSwitchState.source_dir st nv in
        not (OpamPackage.Set.mem nv st.pinned &&
             OpamFilename.exists_dir dir &&
             OpamStd.Option.Op.(
               OpamPinned.find_opam_file_in_source nv.name dir >>|
               (fun (f, lock) ->
               OpamFile.OPAM.(safe_read f |> with_locked_opt lock)) >>=
               OpamFile.OPAM.version_opt) = Some nv.version))
      nvs
  in
  if nvs = [] then Done None
  else
  let print_action =
    OpamConsole.msg "%s retrieved %s  (%s)\n"
      (if not (OpamConsole.utf8 ()) then "->"
       else OpamActionGraph.
              (action_color (`Fetch []) (action_strings (`Fetch []))))
  in
  let print_single_actions msgs =
    List.iter (fun (nv, msg) ->
        print_action
          (OpamConsole.colorise `bold (OpamPackage.name_to_string nv)
           ^ "." ^ (OpamPackage.version_to_string nv))
          msg)
      msgs
  in
  let print_full_action msg =
    print_action
      (labelise @@ fun nv ->
       (OpamConsole.colorise `bold (OpamPackage.name_to_string nv))
       ^ "." ^ (OpamPackage.version_to_string nv))
      msg
  in
  List.iter (fun nv ->
      OpamUpdate.cleanup_source st
        (OpamPackage.Map.find_opt nv st.installed_opams)
        (OpamSwitchState.opam st nv))
    nvs;
  OpamProcess.Job.catch (fun e ->
      let na =
        match e with
        | OpamDownload.Download_fail (s,l) -> (s,l)
        | e -> (None, Printexc.to_string e)
      in
      Done (Some na))
  @@ fun () ->
  OpamUpdate.download_shared_package_source st url nvs @@| function
  | Some (Not_available (s, l)), _ ->
    let msg = OpamStd.Option.default l s in
    OpamConsole.error "Failed to get sources of %s%s: %s"
      (labelise OpamPackage.to_string)
      (match url, nvs with
       | None, _ | _, [_] -> ""
       | Some url, _ ->
         Printf.sprintf " (%s)" (OpamUrl.to_string (OpamFile.URL.url url)))
      msg;
    Some (s, l)
  | _, ((nv, name, Not_available (s, l)) :: _) ->
    let msg = match s with None -> l | Some s -> s in
    OpamConsole.error "Failed to get extra source \"%s\" of %s: %s"
      name (OpamPackage.to_string nv) msg;
    Some (s, l)
  | Some (Result msg), _ ->
    print_full_action msg; None
  | Some (Up_to_date msg), _ ->
    print_full_action msg; None
  | None, [] -> None
  | None, (e :: es as extras) ->
    if List.for_all (function _, _, Up_to_date _ -> true | _ -> false) extras then
      print_full_action "cached"
    else
      (match e, es with
       | (_, _, Result msg), [] -> print_full_action msg
       | _, _ ->
         print_single_actions
           (List.map (fun (nv, _, _) ->
                nv,
                (Printf.sprintf "%d extra sources"
                   (List.length
                      (List.filter (fun (nv',_,_) ->
                           OpamPackage.compare nv nv' = 0)
                          extras))))
               extras));
    None

let download_package st nv =
  download_shared_source st
    (OpamFile.OPAM.url (OpamSwitchState.opam st nv)) [nv]

(* Prepare the package build:
   * apply the patches
   * substitute the files *)
let prepare_package_build env opam nv dir =
  let patches = OpamFile.OPAM.patches opam in

  let print_apply basename =
    log "%s: applying %s.\n" (OpamPackage.name_to_string nv)
      (OpamFilename.Base.to_string basename);
    if OpamConsole.verbose () then
      OpamConsole.msg "[%s: patch] applying %s\n"
        (OpamConsole.colorise `green (OpamPackage.name_to_string nv))
        (OpamFilename.Base.to_string basename)
  in
  let print_subst basename =
    let file = OpamFilename.Base.to_string basename in
    let file_in = file ^ ".in" in
    log "%s: expanding opam variables in %s, generating %s.\n"
      (OpamPackage.name_to_string nv)
      file_in file;
    if OpamConsole.verbose () then
      OpamConsole.msg
        "[%s: subst] expanding opam variables in %s, generating %s\n"
        (OpamConsole.colorise `green (OpamPackage.name_to_string nv))
        file_in file
  in

  let apply_patches ?(dryrun=false) () =
    let patch base =
      if dryrun then Done None else
        OpamFilename.patch
          (dir // OpamFilename.Base.to_string base) dir
    in
    let rec aux = function
      | [] -> Done []
      | (patchname,filter)::rest ->
        if OpamFilter.opt_eval_to_bool env filter then
          (print_apply patchname;
           patch patchname @@+ function
           | None -> aux rest
           | Some err -> aux rest @@| fun e -> (patchname, err) :: e)
        else aux rest
    in
    aux patches
  in
  let subst_patches, subst_others =
    List.partition (fun f -> List.mem_assoc f patches)
      (OpamFile.OPAM.substs opam)
  in
  if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.fake) then
    (List.iter print_subst (OpamFile.OPAM.substs opam);
     apply_patches ~dryrun:true ()) @@| fun _ -> None
  else
  let subst_errs =
    OpamFilename.in_dir dir  @@ fun () ->
    List.fold_left (fun errs f ->
        try
          print_subst f;
          OpamFilter.expand_interpolations_in_file env f;
          errs
        with e -> (f, e)::errs)
      [] subst_patches
  in

  (* Apply the patches *)
  let text =
    OpamProcess.make_command_text (OpamPackage.Name.to_string nv.name) "patch"
  in
  OpamProcess.Job.with_text text (apply_patches ())
  @@+ fun patching_errors ->

  (* Substitute the configuration files. We should be in the right
     directory to get the correct absolute path for the
     substitution files (see [OpamFilter.expand_interpolations_in_file] and
     [OpamFilename.of_basename]. *)
  let subst_errs =
    OpamFilename.in_dir dir @@ fun () ->
    List.fold_left (fun errs f ->
        try
          print_subst f;
          OpamFilter.expand_interpolations_in_file env f;
          errs
        with e -> (f, e)::errs)
      subst_errs subst_others
  in
  if patching_errors <> [] || subst_errs <> [] then
    let msg =
      (if patching_errors <> [] then
         Printf.sprintf "These patches didn't apply at %s:\n%s"
           (OpamFilename.Dir.to_string dir)
           (OpamStd.Format.itemize
              (fun (f,err) ->
                 Printf.sprintf "%s: %s"
                   (OpamFilename.Base.to_string f) (Printexc.to_string err))
              patching_errors)
       else "") ^
      (if subst_errs <> [] then
         Printf.sprintf "String expansion failed for these files:\n%s"
           (OpamStd.Format.itemize
              (fun (b,err) ->
                 Printf.sprintf "%s.in: %s" (OpamFilename.Base.to_string b)
                   (Printexc.to_string err))
           subst_errs)
       else "")
    in
    Done (Some (Failure msg))
  else
    Done None

let prepare_package_source st nv dir =
  log "prepare_package_source: %a at %a"
    (slog OpamPackage.to_string) nv
    (slog OpamFilename.Dir.to_string) dir;
  if OpamStateConfig.(!r.dryrun) then Done None else
  let opam = OpamSwitchState.opam st nv in
  let get_extra_sources_job =
    (* !X The extra sources have normally been prefetched during the dl phase;
       this is, assuming their metadata contains a hash though. *)
    let dl_file_job (basename, urlf) =
      OpamProcess.Job.catch (fun e -> Done (Some e)) @@ fun () ->
      OpamRepository.pull_file
        ~cache_dir:(OpamRepositoryPath.download_cache st.switch_global.root)
        ~silent_hits:true
        (OpamPackage.to_string nv ^ "/" ^ OpamFilename.Base.to_string basename)
        (OpamFilename.create dir basename)
        (OpamFile.URL.checksum urlf)
        (OpamFile.URL.url urlf :: OpamFile.URL.mirrors urlf)
      @@| function
      | Result () | Up_to_date () -> None
      | Not_available (_,msg) -> Some (Failure msg)
    in
    List.fold_left (fun job dl ->
        job @@+ function
        | None -> dl_file_job dl
        | some_err -> Done some_err)
      (Done None) (OpamFile.OPAM.extra_sources opam)
  in
  let check_extra_files =
    let extra_files =
      let extra_files =
        OpamFile.OPAM.get_extra_files
          ~repos_roots:(OpamRepositoryState.get_root st.switch_repos)
          opam
      in
      if extra_files <> [] then extra_files else
      match OpamFile.OPAM.extra_files opam with
      | None -> []
      | Some xs ->
        (* lookup in switch-local hashmap overlay *)
        let extra_files_dir =
          OpamPath.Switch.extra_files_dir st.switch_global.root st.switch
        in
        OpamStd.List.filter_map (fun (base, hash) ->
            let src =
              OpamFilename.create extra_files_dir
                (OpamFilename.Base.of_string (OpamHash.contents hash))
            in
            if OpamFilename.exists src then
              Some (src, base, hash)
            else None)
          xs
    in
    let bad_hash =
      OpamStd.List.filter_map (fun (src, base, hash) ->
          if OpamHash.check_file (OpamFilename.to_string src) hash then
            (OpamFilename.copy ~src ~dst:(OpamFilename.create dir base); None)
          else
            Some src) extra_files
    in
    if bad_hash = [] then None else
      Some (Failure
              (Printf.sprintf "Bad hash for %s"
                 (OpamStd.Format.itemize OpamFilename.to_string bad_hash)));
  in
  OpamFilename.mkdir dir;
  get_extra_sources_job @@+ function Some _ as err -> Done err | None ->
    check_extra_files |> function Some _ as err -> Done err | None ->
      let opam = OpamSwitchState.opam st nv in
      prepare_package_build (OpamPackageVar.resolve ~opam st) opam nv dir

let compilation_env t opam =
  let open OpamParserTypes in
  let scrub = OpamClientConfig.(!r.scrubbed_environment_variables) in
  OpamEnv.get_full ~scrub ~set_opamroot:true ~set_opamswitch:true
    ~force_path:true t ~updates:([
      "CDPATH", Eq, "", Some "shell env sanitization";
      "MAKEFLAGS", Eq, "", Some "make env sanitization";
      "MAKELEVEL", Eq, "", Some "make env sanitization";
      "OPAM_PACKAGE_NAME", Eq,
      OpamPackage.Name.to_string (OpamFile.OPAM.name opam),
      Some "build environment definition";
      "OPAM_PACKAGE_VERSION", Eq,
      OpamPackage.Version.to_string (OpamFile.OPAM.version opam),
      Some "build environment definition";
      "OPAMCLI", Eq, "2.0", Some "opam CLI version";
    ] @
      OpamFile.OPAM.build_env opam)

let installed_opam_opt st nv =
  OpamStd.Option.Op.(
    OpamPackage.Map.find_opt nv st.installed_opams >>+ fun () ->
    OpamSwitchState.opam_opt st nv
  )

let removal_needs_download st nv =
  match installed_opam_opt st nv with
  | None ->
    if not (OpamFile.exists
              (OpamPath.Switch.changes st.switch_global.root st.switch nv.name))
    then
      OpamConsole.warning
        "No opam or changes file found to remove package %s. Stale files may \
         remain."
        (OpamPackage.to_string nv);
    false
  | Some opam ->
    not (OpamFile.OPAM.has_flag Pkgflag_LightUninstall opam ||
         OpamFilter.commands (OpamPackageVar.resolve ~opam st)
           (OpamFile.OPAM.remove opam)
         = [])

let get_wrappers t =
  OpamFile.Wrappers.add
    ~outer:(OpamFile.Config.wrappers t.switch_global.config)
    ~inner:(OpamFile.Switch_config.wrappers t.switch_config)

let get_wrapper t opam wrappers ?local getter =
  let local_env =
    let hook_env = OpamEnv.hook_env t.switch_global.root in
    match local with
    | Some e -> OpamVariable.Map.union (fun _ v -> v) e hook_env
    | None -> hook_env
  in
  OpamFilter.commands (OpamPackageVar.resolve ~local:local_env ~opam t)
    (getter wrappers) |>
  OpamStd.List.filter_map (function
      | [] -> None
      | cmd::args -> Some (cmd, args))

let cmd_wrapper t opam wrappers getter cmd args =
  match get_wrapper t opam wrappers getter @ [cmd, args] with
  | (cmd, args) :: r -> cmd, args @ List.concat (List.map (fun (c, a) -> c::a) r)
  | [] -> assert false

let opam_local_env_of_status ret =
  OpamVariable.Map.singleton
    (OpamVariable.of_string "error-code")
    (Some (S (match ret with
         | None -> "0"
         | Some r -> string_of_int r.OpamProcess.r_code)))

let make_command st opam ?dir ?text_command (cmd, args) =
  let nv = OpamFile.OPAM.package opam in
  let name = OpamPackage.name_to_string nv in
  let env = OpamTypesBase.env_array (compilation_env st opam) in
  let dir = OpamStd.Option.map OpamFilename.Dir.to_string dir in
  let text =
    let cmd, args = OpamStd.Option.default (cmd, args) text_command in
    OpamProcess.make_command_text name ~args cmd
  in
  let context =
    let open OpamStd.Option.Op in
    String.concat " | " [
      OpamVersion.(to_string current);
      (let env = st.switch_global.global_variables in
       Printf.sprintf "%s/%s"
         (OpamSysPoll.os env +! "unknown")
         (OpamSysPoll.arch env +! "unknown"));
      (OpamStd.List.concat_map " " OpamPackage.to_string
         OpamPackage.Set.(elements @@
                          inter st.compiler_packages st.installed_roots));
      if OpamPackage.Set.mem nv st.pinned then
        match OpamFile.OPAM.url opam with
        | None -> "pinned"
        | Some url ->
          let u = OpamFile.URL.url url in
          let src =
            OpamPath.Switch.pinned_package st.switch_global.root st.switch
              nv.name
          in
          let rev = OpamProcess.Job.run (OpamRepository.revision src u) in
          Printf.sprintf "pinned(%s%s)"
            (OpamUrl.to_string_w_subpath (OpamFile.URL.subpath url) u)
            (OpamStd.Option.to_string
               (fun r -> "#"^OpamPackage.Version.to_string r) rev)
      else
        match
          OpamRepositoryState.find_package_opt st.switch_repos
            (OpamSwitchState.repos_list st) nv
        with
        | None -> "no repo"
        | Some (r, _) ->
          let rt = st.switch_repos in
          let repo = OpamRepositoryName.Map.find r rt.repositories in
          let stamp =
            OpamFile.Repo.stamp
              (OpamRepositoryName.Map.find r rt.repos_definitions)
          in
          OpamUrl.to_string repo.repo_url ^
          OpamStd.Option.to_string (fun s -> "#"^s) stamp
    ]
  in
  OpamSystem.make_command ~env ~name ?dir ~text
    ~resolve_path:OpamStateConfig.(not !r.dryrun)
    ~metadata:["context", context]
    ~verbose:(OpamConsole.verbose ())
    cmd args

let remove_commands t nv =
  match installed_opam_opt t nv with
  | None ->
    log "No opam file was found for removing %a\n"
      (slog OpamPackage.to_string) nv;
    []
  | Some opam ->
    OpamFilter.commands (OpamPackageVar.resolve ~opam t)
      (OpamFile.OPAM.remove opam) |>
    OpamStd.List.filter_map
      (function [] -> None | cmd::args -> Some (cmd,args))

(* Testing wether a package removal will be a NOOP. *)
let noop_remove_package t nv =
  let name = nv.name in
  let has_remove_commands = remove_commands t nv <> [] in
  let has_tracked_files =
    let changes_file =
      OpamPath.Switch.changes t.switch_global.root t.switch name
    in
    match OpamFile.Changes.read_opt changes_file with
    | Some map -> not (OpamStd.String.Map.is_empty map)
    | None ->
      let install_file =
        OpamPath.Switch.install t.switch_global.root t.switch name
      in
      OpamFile.exists install_file in
  not (has_remove_commands || has_tracked_files)


(* Remove a given package *)
let remove_package_aux
    t ?(silent=false) ?changes ?force ?build_dir nv =
  log "Removing %a" (slog OpamPackage.to_string) nv;
  let name = nv.name in
  let root = t.switch_global.root in

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

  let dot_install =
    OpamPath.Switch.install root t.switch name
  in
  let changes_file =
    OpamPath.Switch.changes root t.switch name
  in
  let opam =
    match installed_opam_opt t nv with
    | Some o -> o
    | None -> OpamFile.OPAM.create nv
  in

  (* Remove the installed plugin, if it matches *)
  if OpamFile.OPAM.has_flag Pkgflag_Plugin opam then (
    let link = OpamPath.plugin_bin root name in
    let bin =
      OpamFilename.create
        (OpamPath.Switch.bin root t.switch t.switch_config)
        (OpamFilename.basename link)
    in
    if OpamFilename.exists link && OpamFilename.readlink link = bin then
      OpamFilename.remove link
  );

  (* handle .install file *)
  let uninstall_files () =
    let install =
      OpamFile.Dot_install.safe_read dot_install
    in
    let remove_files dst_fn files =
      let files = files install in
      let dst_dir = dst_fn root t.switch t.switch_config in
      List.iter (fun (base, dst) ->
          let dst_file = match dst with
            | None   -> dst_dir // Filename.basename (OpamFilename.Base.to_string base.c)
            | Some b -> OpamFilename.create dst_dir b in
          OpamFilename.remove dst_file
        ) files
    in
    let remove_files_and_dir dst_fn files =
      let dir = dst_fn root t.switch t.switch_config name in
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
          if OpamConsole.confirm "Continue?" then
            OpamFilename.remove dst
        end
      ) (OpamFile.Dot_install.misc install);
  in

  let revert_changes () =
    let changes = match changes with
      | None -> OpamFile.Changes.read_opt changes_file
      | some -> some
    in
    let title = Printf.sprintf "While removing %s" (OpamPackage.to_string nv) in
    OpamStd.Option.iter
      (OpamDirTrack.revert ~title ~verbose:(not silent) ?force
         (OpamPath.Switch.root root t.switch))
      changes
  in

  (* Run the remove script *)
  let build_dir =
    OpamStd.Option.default_map
      (OpamFilename.opt_dir
        (OpamPath.Switch.remove root t.switch nv))
       build_dir
  in
  let wrappers = get_wrappers t in
  let mk_cmd = make_command t opam ?dir:build_dir in
  OpamProcess.Job.of_fun_list ~keep_going:true
    (List.map (fun cmd () -> mk_cmd cmd)
       (get_wrapper t opam wrappers OpamFile.Wrappers.pre_remove))
  @@+ fun error_pre ->
  OpamProcess.Job.of_fun_list ~keep_going:true
    (List.map (fun ((cmd,args) as ca) () ->
         mk_cmd ~text_command:ca @@
         cmd_wrapper t opam wrappers OpamFile.Wrappers.wrap_remove cmd args)
        (remove_commands t nv))
  @@+ fun error ->

  (* Remove according to the .install file *)
  if not OpamStateConfig.(!r.dryrun) then (
    OpamFilename.remove
      (OpamFile.filename
         (OpamPath.Switch.config root t.switch nv.name));
    uninstall_files ();
    OpamFilename.remove (OpamFile.filename dot_install)
  );

  (* Run the post-remove commands *)
  let local =
    opam_local_env_of_status
      OpamStd.Option.Op.(error_pre ++ error >>| snd)
  in
  OpamProcess.Job.of_fun_list ~keep_going:true
    (List.map (fun cmd () -> mk_cmd cmd)
       (get_wrapper t opam wrappers ~local OpamFile.Wrappers.post_remove))
  @@+ fun error_post ->

  (* Revert remaining changes *)
  if not OpamStateConfig.(!r.dryrun) then (
    revert_changes ();
    OpamFilename.remove (OpamFile.filename changes_file);
  );
  if silent then Done ()
  else
  match OpamStd.Option.Op.(error_pre ++ error ++ error_post) with
  | Some (cmd, e) ->
    OpamConsole.warning "package uninstall script failed at %s:\n%s"
      (OpamProcess.string_of_command cmd)
      (OpamProcess.string_of_result e);
    Done ()
  | None ->
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
  if not OpamClientConfig.(!r.keep_build_dir) then OpamFilename.rmdir build_dir;
  let remove_dir = OpamPath.Switch.remove t.switch_global.root t.switch nv in
  if OpamFilename.exists_dir remove_dir then OpamFilename.rmdir remove_dir;
  let dev_dir = OpamSwitchState.source_dir t nv in
  if OpamPackage.Set.mem nv t.installed then
    (if not (OpamSwitchState.is_dev_package t nv) then
       OpamFilename.rmdir dev_dir)
  else
    (log "Removing the local metadata";
     OpamSwitchAction.remove_metadata t (OpamPackage.Set.singleton nv);
     if not (OpamPackage.Set.mem nv t.pinned) then
       OpamFilename.rmdir dev_dir)

let sources_needed st g =
  PackageActionGraph.fold_vertex (fun act acc ->
      match act with
      | `Remove nv ->
        if removal_needs_download st nv
        then OpamPackage.Set.add nv acc else acc
      | `Install nv -> OpamPackage.Set.add nv acc
      | _ -> assert false)
    g OpamPackage.Set.empty

let remove_package t ?silent ?changes ?force ?build_dir nv =
  if OpamClientConfig.(!r.fake) || OpamClientConfig.(!r.show) then
    Done (OpamConsole.msg "Would remove: %s.\n" (OpamPackage.to_string nv))
  else
    remove_package_aux t ?silent ?changes ?force ?build_dir nv

let local_vars ~test ~doc ~dev_setup =
  OpamVariable.Map.of_list [
    OpamVariable.of_string "with-test", Some (B test);
    OpamVariable.of_string "with-doc", Some (B doc);
    OpamVariable.of_string "with-dev-setup", Some (B dev_setup);
  ]

let build_package t ?(test=false) ?(doc=false) ?(dev_setup=false) build_dir nv =
  let opam = OpamSwitchState.opam t nv in
  let commands =
    OpamFilter.commands
      (OpamPackageVar.resolve ~opam ~local:(local_vars ~test ~doc ~dev_setup) t)
      (OpamFile.OPAM.build opam) @
    (if test then
       OpamFilter.commands (OpamPackageVar.resolve ~opam t)
         (OpamFile.OPAM.run_test opam)
     else []) @
    (if doc then
       OpamFilter.commands (OpamPackageVar.resolve ~opam t)
         (OpamFile.OPAM.deprecated_build_doc opam)
     else [])
    |> OpamStd.List.filter_map (function
        | [] -> None
        | cmd::args -> Some (cmd, args))
  in
  let name = OpamPackage.name_to_string nv in
  let wrappers = get_wrappers t in
  let mk_cmd = make_command t opam ~dir:build_dir in
  let jobs =
    let check_result cmd r =
      if OpamProcess.is_success r then Done None else Done (Some (cmd, r))
    in
    List.map (fun cmd -> function
        | None -> let cmd = mk_cmd cmd in cmd @@> check_result cmd
        | some -> Done some)
      (get_wrapper t opam wrappers OpamFile.Wrappers.pre_build)
    @
    List.map (fun ((cmd,args) as cmd_args) -> function
        | None ->
          let base_cmd = OpamProcess.command cmd args in
          (mk_cmd ~text_command:cmd_args @@
           cmd_wrapper t opam wrappers OpamFile.Wrappers.wrap_build cmd args)
          @@> check_result base_cmd
        | some -> Done some)
      commands
  in
  OpamProcess.Job.seq jobs None @@+
  fun result ->
  let local = opam_local_env_of_status OpamStd.Option.Op.(result >>| snd) in
  OpamProcess.Job.of_fun_list ~keep_going:true
    (List.map (fun cmd () -> mk_cmd cmd)
       (get_wrapper t opam wrappers ~local OpamFile.Wrappers.post_build))
  @@+ fun post_result ->
  match result, post_result with
  | Some (cmd, result), _ | _, Some (cmd, result) ->
    OpamConsole.error
      "The compilation of %s failed at %S."
      (OpamPackage.to_string nv) (OpamProcess.string_of_command cmd);
    Done (Some (OpamSystem.Process_error result))
  | None, None ->
    if commands <> [] && OpamConsole.verbose () then
      OpamConsole.msg "%s compiled  %s.%s\n"
        (if not (OpamConsole.utf8 ()) then "->"
         else OpamActionGraph.
                (action_color (`Build ()) (action_strings (`Build ()))))
        (OpamConsole.colorise `bold name)
        (OpamPackage.version_to_string nv);
    Done None

(* Assumes the package has already been compiled in its build dir.
   Does not register the installation in the metadata! *)
let install_package t ?(test=false) ?(doc=false) ?(dev_setup=false) ?build_dir
    nv =
  let opam = OpamSwitchState.opam t nv in
  let commands =
    OpamFile.OPAM.install opam |>
    OpamFilter.commands
      (OpamPackageVar.resolve ~opam
         ~local:(local_vars ~test ~doc ~dev_setup) t) |>
    OpamStd.List.filter_map
      (function [] -> None | cmd::args -> Some (cmd, args))
  in
  let name = OpamPackage.name_to_string nv in
  let dir = match build_dir with
    | None -> OpamPath.Switch.build t.switch_global.root t.switch nv
    | Some d -> d
  in
  let wrappers = get_wrappers t in
  let mk_cmd = make_command t opam ~dir in
  let rec run_commands = function
    | (cmd,args as ca)::commands ->
      mk_cmd ~text_command:ca
        (cmd_wrapper t opam wrappers OpamFile.Wrappers.wrap_install cmd args)
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
    | [] -> Done None
  in
  let root = t.switch_global.root in
  let switch_prefix = OpamPath.Switch.root root t.switch in
  let pre_install_wrappers =
    get_wrapper t opam wrappers OpamFile.Wrappers.pre_install
  in
  let pre_install () =
    (* let text = OpamProcess.make_command_text name "install" in
      * OpamProcess.Job.with_text text *)
    OpamProcess.Job.of_fun_list
      (List.map (fun cmd () -> mk_cmd cmd) pre_install_wrappers)
  in
  let install_job () =
    pre_install ()
    @@+ function
    | Some (_, result) -> Done (Right (OpamSystem.Process_error result))
    | None ->
      run_commands commands @@| function
      | Some e -> Right e
      | None ->
        try
          let _, process_dot_install, config = preprocess_dot_install t nv dir in
          process_dot_install ();
          Left config
        with e -> Right e
  in
  let install_and_track_job () =
    pre_install ()
    @@+ function
    | Some (_, result) ->
      Done (Right (OpamSystem.Process_error result), OpamStd.String.Map.empty)
    | None ->
      let installed_files, process_dot_install, config =
        preprocess_dot_install t nv dir
      in
      OpamDirTrack.track_files ~prefix:switch_prefix installed_files
        (fun () -> process_dot_install () ; Done None)
      @@+ function
      | _, changes -> Done (Left config, changes)
  in
  let post_install status changes =
    let local =
      let added =
        let open OpamDirTrack in
        OpamStd.List.filter_map (function
            | name, (Added _|Contents_changed _|Kind_changed _) -> Some name
            | _ -> None)
          (OpamStd.String.Map.bindings changes)
      in
      opam_local_env_of_status (match status with
          | Right (OpamSystem.Process_error r) -> Some r
          | _ -> None) |>
      OpamVariable.Map.add
        (OpamVariable.of_string "installed-files")
        (Some (L added))
    in
    let hooks =
      get_wrapper t opam wrappers ~local OpamFile.Wrappers.post_install
    in
    let has_hooks = match hooks with [] -> false | _ -> true in
    OpamProcess.Job.of_fun_list ~keep_going:true
      (List.map (fun cmd () -> mk_cmd cmd) hooks)
    @@+ fun error_post ->
    match status, error_post with
    | Right err, _ -> Done (Right err, changes)
    | _, Some (_cmd, r) -> Done (Right (OpamSystem.Process_error r), changes)
    | Left config, None ->
      let changes =
        if has_hooks then
          OpamDirTrack.update switch_prefix changes
        else
          changes
      in
      Done (Left config, changes)
  in
  let rel_meta_dir =
    OpamFilename.(Base.of_string (remove_prefix_dir switch_prefix
                                    (OpamPath.Switch.meta root t.switch)))
  in
  (if commands = [] && pre_install_wrappers = [] then
     install_and_track_job ()
   else
     OpamDirTrack.track switch_prefix
       ~except:(OpamFilename.Base.Set.singleton rel_meta_dir)
       install_job)
  @@+ fun (status, changes) -> post_install status changes
  @@+ function
  | Right e, changes ->
    remove_package t ~silent:true ~changes ~build_dir:dir nv @@+ fun () ->
    OpamStd.Exn.fatal e;
    Done (Right e)
  | Left config, changes ->
    let changes_f = OpamPath.Switch.changes root t.switch nv.name in
    if OpamStateConfig.(not !r.dryrun) then
      (log "changes recorded for %s: %a"
         (OpamPackage.to_string nv)
         (slog OpamDirTrack.to_summary_string) changes;
       OpamFile.Changes.write changes_f changes);
    OpamConsole.msg "%s installed %s.%s\n"
      (if not (OpamConsole.utf8 ()) then "->"
       else OpamActionGraph.
              (action_color (`Install ()) (action_strings (`Install ()))))
      (OpamConsole.colorise `bold name)
      (OpamPackage.version_to_string nv);
    if OpamFile.OPAM.has_flag Pkgflag_Plugin opam then (
      let link = OpamPath.plugin_bin root (OpamPackage.name nv) in
      let target =
        OpamFilename.create
          (OpamPath.Switch.bin root t.switch t.switch_config)
          (OpamFilename.basename link)
      in
      if OpamFilename.exists target then
        OpamFilename.link
          ~relative:(not (OpamSwitch.is_external t.switch))
          ~target ~link
      else
        OpamConsole.warning "%s claims to be a plugin but no %s file was found"
          name (OpamFilename.to_string target)
    );
    Done (Left config)

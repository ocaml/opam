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

let log fmt = OpamGlobals.log "ACTION" fmt

open OpamTypes
open OpamFilename.OP
open OpamState.Types

(* Install the package files *)
(* IMPORTANT: this function is executed by the children processes,
   thus it is important to NOT modify the global state of OPAM here.
   Thus, the update of ~/.opam/<switch/installed MUST not be done
   here.*)
let install_package t nv =
  if !OpamGlobals.dryrun then
      OpamGlobals.msg "Installing %s.\n" (OpamPackage.to_string nv)
  else
  let build_dir = OpamPath.Switch.build t.root t.switch nv in
  if OpamFilename.exists_dir build_dir then OpamFilename.in_dir build_dir (fun () ->

      OpamGlobals.msg "Installing %s.\n" (OpamPackage.to_string nv);
      let name = OpamPackage.name nv in
      let opam_f = OpamPath.opam t.root nv in
      let opam_ = OpamState.opam t nv in
      let config_f = OpamPath.Switch.build_config t.root t.switch nv in
      let config = OpamFile.Dot_config.safe_read config_f in
      let install_f = OpamPath.Switch.build_install t.root t.switch nv in
      let install = OpamFile.Dot_install.safe_read install_f in

      (* check that libraries and syntax extensions specified in .opam and
         .config are in sync *)
      let check kind config_sections opam_sections =
        List.iter (fun cs ->
          if not (List.mem cs opam_sections) then
            OpamGlobals.error_and_exit "The %s %s does not appear in %s"
              kind (OpamVariable.Section.to_string cs) (OpamFilename.to_string opam_f)
        ) config_sections;
        List.iter (fun os ->
          if not (List.mem os config_sections) then
            OpamGlobals.error_and_exit "The %s %s does not appear in %s"
              kind (OpamVariable.Section.to_string os) (OpamFilename.to_string config_f)
        ) opam_sections in
      if not (OpamFilename.exists config_f)
      && (OpamFile.OPAM.libraries opam_ <> [] || OpamFile.OPAM.syntax opam_ <> []) then
        OpamGlobals.error_and_exit
          "%s does not exist but %s defines some libraries and syntax extensions"
          (OpamFilename.to_string config_f)
          (OpamFilename.to_string opam_f);
      check "library"
        (OpamFile.Dot_config.Library.available config)
        (OpamFile.OPAM.libraries opam_);
      check "syntax"
        (OpamFile.Dot_config.Syntax.available config)
        (OpamFile.OPAM.syntax opam_);

      (* check that depends (in .opam) and requires (in .config) fields
         are in almost in sync *)
      (* NOTES: the check is partial as we don't know which clause is valid
         in depends (XXX there is surely a way to get it from the solver) *)
      let local_sections = OpamFile.Dot_config.Section.available config in
      let libraries_in_opam =
        OpamFormula.fold_left (fun accu (n,_) ->
          if OpamState.is_name_installed t n then (
            let nv = OpamState.find_installed_package_by_name t n in
            let opam = OpamState.opam t nv in
            let libs = OpamFile.OPAM.libraries opam in
            let syntax = OpamFile.OPAM.syntax opam in
            List.fold_right OpamVariable.Section.Set.add (libs @ syntax) accu
          ) else
            accu
        ) OpamVariable.Section.Set.empty (OpamFile.OPAM.depends opam_) in
      let libraries_in_config =
        List.fold_left (fun accu s ->
          List.fold_left (fun accu r ->
            OpamVariable.Section.Set.add r accu
          ) accu (OpamFile.Dot_config.Section.requires config s)
        ) OpamVariable.Section.Set.empty local_sections in
      OpamVariable.Section.Set.iter (fun s ->
        if not (List.mem s local_sections)
        && not (OpamVariable.Section.Set.mem s libraries_in_opam) then
          let config_f =
            OpamFilename.to_string (OpamPath.Switch.build_config t.root t.switch nv) in
          let opam_f = OpamFilename.to_string (OpamPath.opam t.root nv) in
          let local_sections =
            List.map OpamVariable.Section.to_string local_sections in
          let opam_sections =
            List.map OpamVariable.Section.to_string
              (OpamVariable.Section.Set.elements libraries_in_opam) in
          OpamGlobals.error_and_exit
            "%s appears as a library dependency in %s, but:\n\
             - %s defines the libraries {%s}\n\
             - Packages in %s defines the libraries {%s}"
            (OpamVariable.Section.to_string s) config_f
            config_f (String.concat ", " local_sections)
            opam_f (String.concat ", " opam_sections)
      ) libraries_in_config;

      (* .install *)
      OpamFile.Dot_install.write (OpamPath.Switch.install t.root t.switch name) install;

      (* .config *)
      OpamFile.Dot_config.write (OpamPath.Switch.config t.root t.switch name) config;

      let warnings = ref [] in
      let check ~src ~dst base =
        let src_file = OpamFilename.create src base.c in
        if base.optional && not (OpamFilename.exists src_file) then
          log "Not installing %s is not present and optional."
            (OpamFilename.to_string src_file);
        if not base.optional && not (OpamFilename.exists src_file) then (
          warnings := (dst, base.c) :: !warnings
        );
        OpamFilename.exists src_file in

      (* Install a list of files *)
      let install_files exec dst_fn files_fn =
        let dst_dir = dst_fn t.root t.switch name in
        let files = files_fn install in
        if not (OpamFilename.exists_dir dst_dir) then (
          log "creating %s" (OpamFilename.Dir.to_string dst_dir);
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

      (* bin *)
      install_files true (fun r s _ -> OpamPath.Switch.bin r s) OpamFile.Dot_install.bin;

      (* sbin *)
      install_files true (fun r s _ -> OpamPath.Switch.sbin r s) OpamFile.Dot_install.sbin;

      (* lib *)
      install_files false OpamPath.Switch.lib OpamFile.Dot_install.lib;

      (* toplevel *)
      install_files false (fun r s _ -> OpamPath.Switch.toplevel r s)
        OpamFile.Dot_install.toplevel;

      install_files true (fun r s _ -> OpamPath.Switch.stublibs r s)
        OpamFile.Dot_install.stublibs;

      (* Man pages *)
      install_files false (fun r s _ -> OpamPath.Switch.man_dir r s) OpamFile.Dot_install.man;

      (* Shared files *)
      install_files false OpamPath.Switch.share OpamFile.Dot_install.share;
      install_files false (fun r s _ -> OpamPath.Switch.share_dir r s)
        OpamFile.Dot_install.share_root;

      (* Etc files *)
      install_files false OpamPath.Switch.etc OpamFile.Dot_install.etc;

      (* Documentation files *)
      install_files false OpamPath.Switch.doc OpamFile.Dot_install.doc;

      (* misc *)
      List.iter
        (fun (src, dst) ->
          let src_file = OpamFilename.create (OpamFilename.cwd ()) src.c in
          if OpamFilename.exists dst
          && OpamState.confirm "Overwriting %s ?" (OpamFilename.to_string dst) then
            OpamFilename.install ~src:src_file ~dst ()
          else begin
            OpamGlobals.msg "Installing %s to %s.\n"
              (OpamFilename.Base.to_string src.c) (OpamFilename.to_string dst);
            if OpamState.confirm "Continue ?" then
              OpamFilename.install ~src:src_file ~dst ()
          end
        ) (OpamFile.Dot_install.misc install);

      if !warnings <> [] then (
        let print (dir, base) =
          Printf.sprintf " - %s in %s"
            (OpamFilename.Base.to_string base)
            (OpamFilename.Dir.to_string dir) in
        OpamSystem.internal_error
          "While installing the following files:\n%s"
          (String.concat "\n" (List.map print !warnings));
      )
    );
  if not (!OpamGlobals.keep_build_dir || !OpamGlobals.debug) then
    OpamFilename.rmdir build_dir

(* Prepare the package build:
   * apply the patches
   * substitute the files *)
let prepare_package_build t nv =
  let opam = OpamState.opam t nv in

  (* Substitute the patched files.*)
  let patches = OpamFile.OPAM.patches opam in

  let iter_patches f =
    List.iter (fun (base, filter) ->
        if OpamState.eval_filter t ~opam OpamVariable.Map.empty filter
        then f base
      ) patches in

  if !OpamGlobals.dryrun then
    iter_patches (fun base ->
        OpamGlobals.msg "Applying %s.\n" (OpamFilename.Base.to_string base))
  else

  let p_build = OpamPath.Switch.build t.root t.switch nv in

  if not (OpamFilename.exists_dir p_build) then
    OpamFilename.mkdir p_build;

  OpamFilename.in_dir p_build (fun () ->
    let all = OpamFile.OPAM.substs opam in
    let patches =
      OpamMisc.filter_map (fun (f,_) ->
        if List.mem f all then Some f else None
      ) patches in
    List.iter (OpamState.substitute_file t ~opam OpamVariable.Map.empty) patches
  );

  (* Apply the patches *)
  iter_patches (fun base ->
      let root = OpamPath.Switch.build t.root t.switch nv in
      let patch = root // OpamFilename.Base.to_string base in
      OpamGlobals.msg "Applying %s.\n" (OpamFilename.Base.to_string base);
      OpamFilename.patch patch p_build);

  (* Substitute the configuration files. We should be in the right
     directory to get the correct absolute path for the
     substitution files (see [substitute_file] and
     [OpamFilename.of_basename]. *)
  OpamFilename.in_dir p_build (fun () ->
    List.iter (OpamState.substitute_file t ~opam OpamVariable.Map.empty)
      (OpamFile.OPAM.substs opam)
  )

let download_package t nv =
  log "download_package: %s" (OpamPackage.to_string nv);

  if !OpamGlobals.dryrun then ()
  else if OpamState.is_locally_pinned t (OpamPackage.name nv) then
    let dir = OpamPath.Switch.dev_package t.root t.switch nv in
    let _ = OpamState.download_upstream t nv dir in
    ()
  else
  let nv = OpamState.pinning_version t nv in
  match OpamState.download_archive t nv with
  | Some f -> assert (f = OpamPath.archive t.root nv)
  | None   ->
    let dir = OpamPath.dev_package t.root nv in
    let _ = OpamState.download_upstream t nv dir in
    ()

let extract_package t nv =
  log "extract_package: %s" (OpamPackage.to_string nv);

  if !OpamGlobals.dryrun then () else
  let build_dir = OpamPath.Switch.build t.root t.switch nv in
  OpamFilename.rmdir build_dir;

  let extract_and_copy_files dir =
    let () = match OpamFilename.files dir with
      | [] -> ()
      | [f] ->
        log "archive %S => extracting" (OpamFilename.to_string f);
        OpamFilename.extract_generic_file (F f) build_dir
      | _::_::_ ->
        log "multiple files in %S: assuming dev directory & copying"
          (OpamFilename.Dir.to_string dir);
        OpamFilename.extract_generic_file (D dir) build_dir in
    OpamState.copy_files t nv build_dir in

  if OpamState.is_locally_pinned t (OpamPackage.name nv) then
    extract_and_copy_files (OpamPath.Switch.dev_package t.root t.switch nv)
  else if OpamFilename.exists (OpamPath.archive t.root nv) then
    OpamFilename.extract (OpamPath.archive t.root nv) build_dir
  else
    extract_and_copy_files (OpamPath.dev_package t.root nv);

  prepare_package_build t nv

let string_of_commands commands =
  let commands_s = List.map (fun cmd -> String.concat " " cmd)  commands in
  "  "
  ^ if commands_s <> [] then
    String.concat "\n  " commands_s
  else
    "Nothing to do."

let compilation_env t opam =
  let env0 = OpamState.get_full_env ~opam t in
  let env1 = [
    ("OPAM_PACKAGE_NAME", OpamPackage.Name.to_string (OpamFile.OPAM.name opam));
    ("OPAM_PACKAGE_VERSION", OpamPackage.Version.to_string (OpamFile.OPAM.version opam))
  ] @ env0 in
  OpamState.add_to_env t ~opam env1 (OpamFile.OPAM.build_env opam)

let get_metadata t =
  let compiler =
    if t.compiler = OpamCompiler.system then
      let system_version = match OpamCompiler.Version.system () with
        | None   -> "<none>"
        | Some v -> OpamCompiler.Version.to_string v in
      Printf.sprintf "system (%s)" system_version
    else
      OpamCompiler.to_string t.compiler in
  [
    ("compiler", compiler);
  ]

let update_metadata t ~installed ~installed_roots ~reinstall =
  let mark_pinned_versions =
    OpamPackage.Set.map (fun p ->
        let name = OpamPackage.name p in
        if OpamPackage.Name.Map.mem name t.pinned then OpamPackage.pinned name
        else p) in
  let installed       = mark_pinned_versions installed in
  let installed_roots = mark_pinned_versions installed_roots in
  let reinstall       = mark_pinned_versions reinstall in
  let installed_roots = OpamPackage.Set.inter installed_roots installed in
  let reinstall = OpamPackage.Set.inter installed_roots reinstall in
  if not !OpamGlobals.dryrun then (
  OpamFile.Installed.write
    (OpamPath.Switch.installed t.root t.switch)
    installed;
  OpamFile.Installed_roots.write
    (OpamPath.Switch.installed_roots t.root t.switch)
    installed_roots;
  OpamFile.Reinstall.write
    (OpamPath.Switch.reinstall t.root t.switch)
    reinstall
  );
  {t with installed; installed_roots; reinstall}

let dev_opam_opt t nv build_dir =
  let opam () = OpamState.opam_opt t nv in
  if OpamState.is_dev_package t nv then
    let dir =
      if OpamFilename.exists_dir (build_dir / "opam") then
        build_dir / "opam"
      else
        build_dir in
    let overlay = dir // "opam" in
    if OpamFilename.exists overlay then
      try Some (OpamFile.OPAM.read overlay)
      with e -> OpamMisc.fatal e; opam ()
    else opam ()
  else opam ()

let dev_opam t nv build_dir =
  match dev_opam_opt t nv build_dir with
  | None    -> OpamPackage.unknown (OpamPackage.name nv) (Some (OpamPackage.version nv))
  | Some nv' ->
    OpamFile.OPAM.with_name
      (OpamFile.OPAM.with_version nv' (OpamPackage.version nv))
      (OpamPackage.name nv)

let removal_needs_download t nv =
  match OpamState.opam_opt t nv with
  | None ->
    OpamGlobals.warning
      "No opam file found to remove package %s. Stale files may remain."
      (OpamPackage.to_string nv);
    false
  | Some opam ->
    let commands =
      OpamState.filter_commands t ~opam
        OpamVariable.Map.empty (OpamFile.OPAM.remove opam) in
    (* We use a small hack: if the remove command is simply
       'ocamlfind remove xxx' then, no need to extract the archive
       again. *)
    let use_ocamlfind = function
      | [] -> true
      | "ocamlfind" :: _ -> true
      | _ -> false in
    not (List.for_all use_ocamlfind commands)

(* Remove a given package *)
(* This will be done by the parent process, so theoritically we are
   allowed to modify the global state of OPAM here. However, for
   consistency reasons, this is done in the main function only. *)
let remove_package_aux t ~metadata ?(silent=false) nv =
  log "Removing %s (%b)" (OpamPackage.to_string nv) metadata;
  let name = OpamPackage.name nv in

  (* Run the remove script *)
  let opam = OpamState.opam_opt t nv in

  OpamGlobals.msg "Removing %s.\n" (OpamPackage.to_string nv);

  begin match opam with
    | None      -> OpamGlobals.msg "  No OPAM file has been found!\n"
    | Some opam ->
      let env = compilation_env t opam in
      let p_build = OpamPath.Switch.build t.root t.switch nv in
      (* We try to run the remove scripts in the folder where it was
         extracted If it does not exist, we try to download and
         extract the archive again, if that fails, we don't really
         care. *)
      let opam = dev_opam t nv p_build in
      let remove = OpamState.filter_commands t ~opam
          OpamVariable.Map.empty (OpamFile.OPAM.remove opam) in
      let name = OpamPackage.Name.to_string name in
      let exec_dir, name =
        if OpamFilename.exists_dir p_build
        then p_build, Some name
        else t.root , None in
      try
        OpamGlobals.msg "%s\n" (string_of_commands remove);
        let metadata = get_metadata t in
        if not !OpamGlobals.dryrun then
          OpamFilename.exec ~env ?name exec_dir ~metadata ~keep_going:true
            remove
      with OpamSystem.Process_error r ->
        if not silent then
          OpamGlobals.warning
            "failure in package uninstall script, some files may remain:\n%s"
            (OpamProcess.string_of_result r)
  end;

  if not !OpamGlobals.dryrun then begin

  (* Remove the libraries *)
  OpamFilename.rmdir (OpamPath.Switch.lib t.root t.switch name);

  (* Remove the documentation *)
  OpamFilename.rmdir (OpamPath.Switch.doc t.root t.switch name);

  (* Remove build/<package> *)
  if not !OpamGlobals.keep_build_dir then
    OpamFilename.rmdir (OpamPath.Switch.build t.root t.switch nv);

  let install =
    OpamFile.Dot_install.safe_read (OpamPath.Switch.install t.root t.switch name) in

  let remove_files dst_fn files =
    let files = files install in
    List.iter (fun (base, dst) ->
        let dst_dir = dst_fn t.root t.switch in
        let dst_file = match dst with
          | None   -> dst_dir // Filename.basename (OpamFilename.Base.to_string base.c)
          | Some b -> OpamFilename.create dst_dir b in
        OpamFilename.remove dst_file
      ) files in

  (* Remove the binaries *)
  log "Removing the binaries";
  remove_files OpamPath.Switch.bin OpamFile.Dot_install.bin;
  remove_files OpamPath.Switch.sbin OpamFile.Dot_install.sbin;

  (* Remove the C bindings *)
  remove_files OpamPath.Switch.stublibs OpamFile.Dot_install.stublibs;

  (* Remove man pages *)
  remove_files OpamPath.Switch.man_dir OpamFile.Dot_install.man;

  (* Remove the misc files *)
  log "Removing the misc files";
  List.iter (fun (_,dst) ->
      if OpamFilename.exists dst then begin
        OpamGlobals.msg "Removing %s." (OpamFilename.to_string dst);
        if OpamState.confirm "Continue ?" then
          OpamFilename.remove dst
      end
    ) (OpamFile.Dot_install.misc install);

  (* Removing the shared dir *)
  (* This one shouldn't be needed (we remove the dir). Yet some packages
     currently install using '%{share}%/..' to workaround the lack of
     [share_root] in previous versions *)
  remove_files (fun t s -> OpamPath.Switch.share t s name) OpamFile.Dot_install.share;
  OpamFilename.rmdir (OpamPath.Switch.share t.root t.switch name);
  remove_files OpamPath.Switch.share_dir OpamFile.Dot_install.share_root;

  (* Removing the etc dir *)
  OpamFilename.rmdir (OpamPath.Switch.etc t.root t.switch name);

  (* Remove .config and .install *)
  log "Removing config and install files";
  OpamFilename.remove (OpamPath.Switch.install t.root t.switch name);
  OpamFilename.remove (OpamPath.Switch.config t.root t.switch name);

  end;

  (* Update the metadata *)
  if metadata then
    let installed = OpamPackage.Set.remove nv t.installed in
    let installed_roots = OpamPackage.Set.remove nv t.installed_roots in
    let reinstall = OpamPackage.Set.remove nv t.reinstall in
    ignore (update_metadata t ~installed ~installed_roots ~reinstall)

(* Removes build dir and source cache of package if unneeded *)
let cleanup_package_artefacts t nv =
  log "Cleaning up artefacts of %s" (OpamPackage.to_string nv);

  let build_dir = OpamPath.Switch.build t.root t.switch nv in
  if not !OpamGlobals.keep_build_dir && OpamFilename.exists_dir build_dir then
    OpamFilename.rmdir build_dir;

  let dev_dir = OpamPath.Switch.dev_package t.root t.switch nv in
  if not (OpamState.is_package_installed t nv) then (
    if OpamFilename.exists_dir dev_dir then (
      log "Cleaning-up the switch repository";
      OpamFilename.rmdir dev_dir );
    log "Removing the local metadata";
    OpamState.remove_metadata t (OpamPackage.Set.singleton nv);
  );

  (* Remove the dev archive if no switch uses the package anymore *)
  let dev = OpamPath.dev_package t.root nv in
  if OpamFilename.exists_dir dev &&
     not (OpamPackage.Set.mem nv (OpamState.all_installed t)) then (
    log "Removing %S" (OpamFilename.Dir.to_string dev);
    OpamFilename.rmdir dev;
  )

let sources_needed t solution =
  let pkgs =
    OpamPackage.Set.of_list
      (List.filter (removal_needs_download t)
         solution.PackageActionGraph.to_remove) in
  PackageActionGraph.fold_vertex (fun act acc ->
      match act with
      | To_delete nv ->
        if removal_needs_download t nv
        then OpamPackage.Set.add nv acc else acc
      | To_change (None,nv) | To_recompile nv ->
        OpamPackage.Set.add nv acc
      | To_change (Some nv1, nv2) ->
        let acc = OpamPackage.Set.add nv2 acc in
        if removal_needs_download t nv1
        then OpamPackage.Set.add nv1 acc else acc)
    solution.PackageActionGraph.to_process pkgs

let remove_package t ~metadata ?silent nv =
  if !OpamGlobals.fake || !OpamGlobals.show then
    OpamGlobals.msg "Would remove: %s.\n" (OpamPackage.to_string nv)
  else
    remove_package_aux t ~metadata ?silent nv

(* Remove all the packages appearing in a solution (and which need to
   be removed, eg. because of a direct uninstall action or because of
   recompilation. Ensure any possibly partially removed package is
   marked as removed (it's the best we can do) *)
let remove_all_packages t ~metadata sol =
  let deleted = ref [] in
  let update_metadata () =
    let deleted = OpamPackage.Set.of_list !deleted in
    if metadata then (
      let installed = OpamPackage.Set.diff t.installed deleted in
      let installed_roots = OpamPackage.Set.diff t.installed_roots deleted in
      let reinstall = OpamPackage.Set.diff t.reinstall deleted in
      let t = update_metadata t ~installed ~installed_roots ~reinstall in
      t, deleted
    )
    else t, deleted in
  let delete nv =
    if removal_needs_download t nv then extract_package t nv;
    if !deleted = [] then
      OpamGlobals.header_msg "Removing Packages";
    deleted := nv :: !deleted; (* first mark as deleted *)
    try ignore (remove_package t ~metadata:false nv)
    with e -> OpamMisc.fatal e (* ignore individual errors *)
  in
  let action n =
    match n with
    | To_change (Some nv, _) | To_delete nv | To_recompile nv -> delete nv
    | To_change (None, _) -> () in
  try
    List.iter delete PackageActionGraph.(sol.to_remove);
    PackageActionGraph.(Topological.iter action (mirror sol.to_process));
    update_metadata (), `Successful ()
  with e ->
    update_metadata (), `Exception e

(* Build and install a package. In case of error, simply return the
   error traces, and let the repo in a state that the user can
   explore.  Do not try to recover yet.
   Assumes the package has already been downloaded to [p_build].
 *)
let build_and_install_package_aux t ~metadata nv =
  OpamGlobals.header_msg "Installing %s" (OpamPackage.to_string nv);

  let exec =
    extract_package t nv;

    let p_build = OpamPath.Switch.build t.root t.switch nv in

    (* For dev packages, we look for any opam file at the root of the build
       directory *)
    let opam = dev_opam t nv p_build in

    (* Get the env variables set up in the compiler description file *)
    let env = compilation_env t opam in

    (* Exec the given commands. *)
    fun name f ->
      match OpamState.filter_commands t ~opam OpamVariable.Map.empty (f opam) with
      | []       -> ()
      | commands ->
        OpamGlobals.msg "%s:\n%s\n" name (string_of_commands commands);
        if !OpamGlobals.dryrun then () else
        let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
        let metadata = get_metadata t in
        OpamFilename.exec ~env ~name ~metadata p_build commands
  in

    try
      (* First, we build the package. *)
      exec ("Building " ^ OpamPackage.to_string nv) OpamFile.OPAM.build;

      (* If necessary, build and run the test. *)
      if !OpamGlobals.build_test then
        exec "Building and running the test" OpamFile.OPAM.build_test;

      (* If necessary, build the documentation. *)
      if !OpamGlobals.build_doc then
        exec "Generating the documentation" OpamFile.OPAM.build_doc;

      (* If everyting went fine, finally install the package. *)
      install_package t nv;

      (* update the metadata *)
      if metadata then (
        let installed = OpamPackage.Set.add nv t.installed in
        let installed_roots = OpamPackage.Set.add nv t.installed_roots in
        let reinstall = OpamPackage.Set.remove nv t.reinstall in
        let t = update_metadata t ~installed ~installed_roots ~reinstall in
        OpamState.install_metadata t nv;
      )

    with e ->
      let cause = match e with
        | Sys.Break -> "was aborted"
        | _         -> "failed" in
      (* We keep the build dir to help debugging *)
      OpamGlobals.error
        "The compilation of %s %s."
        (OpamPackage.to_string nv) cause;
      ignore (remove_package ~metadata:false t ~silent:true nv);
      raise e

let build_and_install_package t ~metadata nv =
  if not !OpamGlobals.fake then
    build_and_install_package_aux t ~metadata nv
  else
    OpamGlobals.msg "(simulation) Building and installing %s.\n"
      (OpamPackage.to_string nv)

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
      let install_files dst_fn files_fn =
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
              OpamFilename.copy ~src:src_file ~dst:dst_file;
          ) files in

      (* bin *)
      install_files (fun r s _ -> OpamPath.Switch.bin r s) OpamFile.Dot_install.bin;

      (* lib *)
      install_files OpamPath.Switch.lib OpamFile.Dot_install.lib;

      (* toplevel *)
      install_files (fun r s _ -> OpamPath.Switch.toplevel r s)
        OpamFile.Dot_install.toplevel;

      install_files (fun r s _ -> OpamPath.Switch.stublibs r s)
        OpamFile.Dot_install.stublibs;

      (* Man pages *)
      install_files (fun r s _ -> OpamPath.Switch.man_dir r s) OpamFile.Dot_install.man;

      (* Shared files *)
      install_files OpamPath.Switch.share OpamFile.Dot_install.share;

      (* Documentation files *)
      install_files OpamPath.Switch.doc OpamFile.Dot_install.doc;

      (* misc *)
      List.iter
        (fun (src, dst) ->
          let src_file = OpamFilename.create (OpamFilename.cwd ()) src.c in
          if OpamFilename.exists dst
          && OpamState.confirm "Overwriting %s ?" (OpamFilename.to_string dst) then
            OpamFilename.copy ~src:src_file ~dst
          else begin
            OpamGlobals.msg "Installing %s to %s.\n"
              (OpamFilename.Base.to_string src.c) (OpamFilename.to_string dst);
            if OpamState.confirm "Continue ?" then
              OpamFilename.copy ~src:src_file ~dst
          end
        ) (OpamFile.Dot_install.misc install);

      if !warnings <> [] then (
        let print (dir, base) =
          Printf.sprintf " - %s in %s"
            (OpamFilename.Base.to_string base)
            (OpamFilename.Dir.to_string dir) in
        OpamSystem.internal_error
          "Error while installing the following files:\n%s"
          (String.concat "\n" (List.map print !warnings));
      )
    );
  if not (!OpamGlobals.keep_build_dir || !OpamGlobals.debug) then
    OpamFilename.rmdir build_dir

(* Prepare the package build:
   * apply the patches
   * substitute the files *)
let prepare_package_build t nv =
  let p_build = OpamPath.Switch.build t.root t.switch nv in
  let opam = OpamState.opam t nv in

  if not (OpamFilename.exists_dir p_build) then
    OpamFilename.mkdir p_build;

  (* Substitute the patched files.*)
  let patches = OpamFile.OPAM.patches opam in
  OpamFilename.in_dir p_build (fun () ->
    let all = OpamFile.OPAM.substs opam in
    let patches =
      OpamMisc.filter_map (fun (f,_) ->
        if List.mem f all then Some f else None
      ) patches in
    List.iter (OpamState.substitute_file t OpamVariable.Map.empty) patches
  );

  (* Apply the patches *)
  List.iter (fun (base, filter) ->
    let root = OpamPath.Switch.build t.root t.switch nv in
    let patch = root // OpamFilename.Base.to_string base in
    if OpamState.eval_filter t OpamVariable.Map.empty filter then (
      OpamGlobals.msg "Applying %s.\n" (OpamFilename.Base.to_string base);
      OpamFilename.patch patch p_build)
  ) patches;

  (* Substitute the configuration files. We should be in the right
     directory to get the correct absolute path for the
     substitution files (see [substitute_file] and
     [OpamFilename.of_basename]. *)
  OpamFilename.in_dir p_build (fun () ->
    List.iter (OpamState.substitute_file t OpamVariable.Map.empty)
      (OpamFile.OPAM.substs opam)
  )

let extract_package t nv =
  log "extract_package: %s" (OpamPackage.to_string nv);
  let build_dir = OpamPath.Switch.build t.root t.switch nv in
  OpamFilename.rmdir build_dir;

  let extract_and_copy_files = function
    | None   -> ()
    | Some f ->
      OpamFilename.extract_generic_file f build_dir;
      OpamState.copy_files t nv build_dir in

  if OpamState.is_locally_pinned t (OpamPackage.name nv) then
    let dir = OpamPath.Switch.dev_package t.root t.switch nv in
    extract_and_copy_files (OpamState.download_upstream t nv dir)
  else (
    match OpamState.download_archive t nv with
    | Some f -> OpamFilename.extract f build_dir
    | None   ->
      let dir = OpamPath.dev_package t.root nv in
      extract_and_copy_files (OpamState.download_upstream t nv dir)
  );

  prepare_package_build t nv;
  build_dir


let string_of_commands commands =
  let commands_s = List.map (fun cmd -> String.concat " " cmd)  commands in
  "  "
  ^ if commands_s <> [] then
    String.concat "\n  " commands_s
  else
    "Nothing to do."

let compilation_env t opam =
  let env0 = OpamState.get_full_env t in
  let env1 = [
    ("OPAM_PACKAGE_NAME", OpamPackage.Name.to_string (OpamFile.OPAM.name opam));
    ("OPAM_PACKAGE_VERSION", OpamPackage.Version.to_string (OpamFile.OPAM.version opam))
  ] @ env0 in
  OpamState.add_to_env t env1 (OpamFile.OPAM.build_env opam)

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
  let installed_roots = OpamPackage.Set.inter installed_roots installed in
  let reinstall = OpamPackage.Set.inter installed_roots reinstall in
  OpamFile.Installed.write
    (OpamPath.Switch.installed t.root t.switch)
    installed;
  OpamFile.Installed_roots.write
    (OpamPath.Switch.installed_roots t.root t.switch)
    installed_roots;
  OpamFile.Reinstall.write
    (OpamPath.Switch.reinstall t.root t.switch)
    reinstall

(* Remove a given package *)
(* This will be done by the parent process, so theoritically we are
   allowed to modify the global state of OPAM here. However, for
   consistency reasons, this is done in the main function only. *)
let remove_package_aux t ~metadata ~rm_build nv =
  log "Removing %s (%b)" (OpamPackage.to_string nv) metadata;
  let name = OpamPackage.name nv in

  (* Run the remove script *)
  let opam =
    try Some (OpamState.opam t nv)
    with _ -> None in

  OpamGlobals.msg "Removing %s.\n" (OpamPackage.to_string nv);

  begin match opam with
    | None      -> OpamGlobals.msg "  No OPAM file has been found!\n"
    | Some opam ->
      let env = compilation_env t opam in
      let commands =
        OpamState.filter_commands t
          OpamVariable.Map.empty (OpamFile.OPAM.remove opam) in
      match commands with
      | []     -> ()
      | remove ->
        let p_build = OpamPath.Switch.build t.root t.switch nv in
        (* We try to run the remove scripts in the folder where it was
           extracted If it does not exist, we try to download and
           extract the archive again, if that fails, we don't really
           care. *)
        (* We also use a small hack: if the remove command is simply
           'ocamlfind remove xxx' then, no need to extract the archive
           again. *)
        let use_ocamlfind = function
          | [] -> true
          | "ocamlfind" :: _ -> true
          | _ -> false in
        if not (OpamFilename.exists_dir p_build)
        && not (List.for_all use_ocamlfind remove) then (
          try let _ = extract_package t nv in ()
          with _ -> ()
        );
        let name = OpamPackage.Name.to_string name in
        let exec_dir, name =
          if OpamFilename.exists_dir p_build
          then p_build, Some name
          else t.root , None in
        try
          OpamGlobals.msg "%s\n" (string_of_commands remove);
          let metadata = get_metadata t in
          OpamFilename.exec ~env ?name exec_dir ~metadata ~keep_going:true remove
        with OpamSystem.Process_error r ->
          OpamGlobals.error
            "Warning: failure in package uninstall script, some files may remain:\n%s"
            (OpamProcess.string_of_result r)
  end;

  (* Remove the libraries *)
  OpamFilename.rmdir (OpamPath.Switch.lib t.root t.switch name);

  (* Remove the documentation *)
  OpamFilename.rmdir (OpamPath.Switch.doc t.root t.switch name);
  (* XXX: remove the man pages *)

  (* Remove build/<package> if requested *)
  if not !OpamGlobals.keep_build_dir && rm_build then
    OpamFilename.rmdir (OpamPath.Switch.build t.root t.switch nv);

  (* Clean-up the active repository *)
  log "Cleaning-up the active repository";
  let dev_dir = OpamPath.dev_package t.root nv in
  if OpamFilename.exists_dir dev_dir then OpamFilename.rmdir dev_dir;

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

  (* Removing the shared dir if it is empty, overwise keep files for
     future installation. TODO: is it the expected semantics ? *)
  let share = OpamPath.Switch.share t.root t.switch name in
  (match OpamFilename.rec_files share, OpamFilename.rec_dirs share with
   | [], [] -> OpamFilename.rmdir share
   | _      ->
     OpamGlobals.msg
       "WARNING: %s is not empty. We keep its contents for future installations.\n"
       (OpamFilename.Dir.to_string share));

  (* Remove .config and .install *)
  log "Removing config and install files";
  OpamFilename.remove (OpamPath.Switch.install t.root t.switch name);
  OpamFilename.remove (OpamPath.Switch.config t.root t.switch name);

  (* Remove the pinned cache *)
  log "Removing the pinned cache";
  OpamFilename.rmdir (OpamPath.Switch.dev_package t.root t.switch nv);

  (* Update the metadata *)
  if metadata then (
    let installed = OpamPackage.Set.remove nv t.installed in
    let installed_roots = OpamPackage.Set.remove nv t.installed_roots in
    let reinstall = OpamPackage.Set.remove nv t.reinstall in
    update_metadata t ~installed ~installed_roots ~reinstall
  )

let remove_package t ~metadata ~rm_build nv =
  if not !OpamGlobals.fake then
    remove_package_aux t ~metadata ~rm_build nv
  else
    OpamGlobals.msg "(simulation) Removing %s.\n" (OpamPackage.to_string nv)

(* Remove all the packages appearing in a solution (and which need to
   be removed, eg. because of a direct uninstall action or because of
   recompilation.  *)
let remove_all_packages t ~metadata sol =
  let open PackageActionGraph in
  let deleted = ref [] in
  let delete nv =
    if !deleted = [] then
      OpamGlobals.msg "\n=-=-= Removing Packages =-=-=\n";
    deleted := nv :: !deleted;
    try remove_package t ~rm_build:true ~metadata:false nv;
    with _ -> () in
  let action n =
    match n with
    | To_change (Some nv, _)
    | To_recompile nv
    | To_delete nv        -> delete nv
    | To_change (None, _) -> () in
  List.iter delete sol.to_remove;
  PackageActionGraph.iter_vertex action (PackageActionGraph.mirror sol.to_process);
  let deleted = OpamPackage.Set.of_list !deleted in
  if metadata then (
    let installed = OpamPackage.Set.diff t.installed deleted in
    let installed_roots = OpamPackage.Set.diff t.installed_roots deleted in
    let reinstall = OpamPackage.Set.diff t.reinstall deleted in
    update_metadata t ~installed ~installed_roots ~reinstall
  );
  deleted

(* Build and install a package. In case of error, simply return the
   error traces, and let the repo in a state that the user can
   explore.  Do not try to recover yet. *)
let build_and_install_package_aux t ~metadata nv =
  let left, right = match !OpamGlobals.utf8_msgs with
    | true -> "\xF0\x9F\x90\xAB " (* UTF-8 <U+1F42B, U+0020> *), ""
    | false -> "=-=-=", "=-=-="
  in

  let nv =
    let name = OpamPackage.name nv in
    if OpamState.is_locally_pinned t name then OpamPackage.pinned name
    else nv in

  OpamGlobals.msg "\n%s Installing %s %s\n" left (OpamPackage.to_string nv) right;

  let opam = OpamState.opam t nv in

  (* Get the env variables set up in the compiler description file *)
  let env = compilation_env t opam in

  try

    (* This one can raises an exception (for insance an user's CTRL-C
       when the sync takes too long. *)
    let p_build = extract_package t nv in

    (* Exec the given commands. *)
    let exec name f =
      match OpamState.filter_commands t OpamVariable.Map.empty (f opam) with
      | []       -> ()
      | commands ->
        OpamGlobals.msg "%s:\n%s\n" name (string_of_commands commands);
        let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
        let metadata = get_metadata t in
        OpamFilename.exec ~env ~name ~metadata p_build commands in

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
      update_metadata t ~installed ~installed_roots ~reinstall
    )

  with e ->
    let cause = match e with
      | Sys.Break -> "was aborted"
      | _         -> "failed" in
    (* We keep the build dir to help debugging *)
    OpamGlobals.error
      "The compilation of %s %s."
      (OpamPackage.to_string nv) cause;
    remove_package ~rm_build:false ~metadata:false t nv;
    raise e

let build_and_install_package t ~metadata nv =
  if not !OpamGlobals.fake then
    build_and_install_package_aux t ~metadata nv
  else
    OpamGlobals.msg "(simulation) Building and installing %s.\n"
      (OpamPackage.to_string nv)

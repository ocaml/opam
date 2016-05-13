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
open OpamStd.Op

let log fmt = OpamConsole.log "COMMAND" fmt
let slog = OpamConsole.slog

let string_of_pinned opam =
  let bold = OpamConsole.colorise `bold in
  Printf.sprintf "pinned %s (version %s)"
    (OpamStd.Option.to_string ~none:(bold "locally")
       (fun u -> "to " ^ (bold (OpamUrl.to_string (OpamFile.URL.url u))))
       (OpamFile.OPAM.url opam))
    (bold (OpamPackage.Version.to_string (OpamFile.OPAM.version opam)))

let get_source_definition ?version st nv url =
  let root = st.switch_global.root in
  let srcdir = OpamPath.Switch.dev_package root st.switch nv.name in
  let fix opam =
    OpamFile.OPAM.with_url url @@
    (match version with
     | Some v -> OpamFile.OPAM.with_version v
     | None -> fun o -> o) @@
    opam
  in
  let open OpamProcess.Job.Op in
  OpamUpdate.fetch_dev_package url srcdir nv @@| function
  | Not_available _ -> raise Not_found
  | Up_to_date _ | Result _ ->
    match OpamPinned.find_opam_file_in_source nv.name srcdir with
    | None -> None
    | Some f ->
      match OpamFileTools.lint_file f with
      | warns, None ->
        OpamConsole.error
          "Invalid opam file in %s source from %s:"
          (OpamPackage.to_string nv)
          (OpamUrl.to_string (OpamFile.URL.url url));
        OpamConsole.msg "%s\n" (OpamFileTools.warns_to_string warns);
        let dst =
          OpamFile.filename
            (OpamPath.Switch.Overlay.tmp_opam root st.switch nv.name)
        in
        OpamFilename.copy ~src:(OpamFile.filename f) ~dst;
        None
      | [], Some opam -> Some (fix opam)
      | warns, Some opam ->
        OpamConsole.warning
          "Failed checks on %s package definition from source at %s \
           (fix with 'opam pin edit'):"
          (OpamPackage.to_string nv)
          (OpamUrl.to_string (OpamFile.URL.url url));
        OpamConsole.errmsg "%s\n" (OpamFileTools.warns_to_string warns);
        Some (fix opam)

let copy_files st opam =
  let name = OpamFile.OPAM.name opam in
  let files = OpamFile.OPAM.get_extra_files opam in
  if files = [] then
    (match OpamFile.OPAM.extra_files opam with
     | Some [] | None -> ()
     | Some files ->
       OpamConsole.warning
         "Ignoring overlay files of %s (files/*) that were not found: %s"
         (OpamPackage.Name.to_string name)
         (OpamStd.List.to_string (fun (b,_) -> OpamFilename.Base.to_string b)
            files));
  let destdir =
    OpamPath.Switch.Overlay.files st.switch_global.root st.switch name
  in
  let files =
    List.fold_left (fun acc (src, rel_file, hash) ->
        if not (OpamFilename.exists src) then
          (OpamConsole.warning "Overlay file of %s %s not found, ignoring"
             (OpamPackage.Name.to_string name)
             (OpamFilename.to_string src);
           acc)
        else
        let actual_hash = OpamFilename.digest src in
        if actual_hash <> hash then
          (if OpamFormatConfig.(!r.strict) then
             OpamConsole.error_and_exit "Hash mismatch on %s %s (strict mode)"
           else
             OpamConsole.warning
               "Hash doesn't match for overlay file of %s %s, adjusted")
            (OpamPackage.Name.to_string name)
            (OpamFilename.to_string src);
        OpamFilename.copy ~src ~dst:(OpamFilename.create destdir rel_file);
        (rel_file, actual_hash) :: acc)
      [] files
  in
  OpamFile.OPAM.with_extra_files (List.rev files) opam

(* Returns the new opam file, without writing it to disk *)
let edit_raw name temp_file =
  let rec edit () =
    if OpamStd.Sys.tty_in then
      (OpamConsole.msg "Press enter to start \"%s\" (this can be customised by \
                        setting EDITOR or OPAMEDITOR)... "
         (Filename.basename OpamClientConfig.(!r.editor));
       ignore (read_line ()));
    let edited_ok =
      try
        Sys.command
          (Printf.sprintf "%s %s"
             (OpamClientConfig.(!r.editor))
             (OpamFile.to_string temp_file))
        = 0 &&
        match OpamFilename.read (OpamFile.filename temp_file)
        with "" | "\n" -> false | _ -> true
      with _ -> false
    in
    if not edited_ok then
      (OpamConsole.error "Empty file or editor error, aborting.";
       None)
    else
    try
      let warnings, opam_opt =
        OpamFileTools.lint_file temp_file
      in
      let opam = match opam_opt with
        | None ->
          OpamConsole.msg "Invalid opam file:\n%s\n"
            (OpamFileTools.warns_to_string warnings);
          failwith "Syntax errors"
        | Some opam -> opam
      in
      let namecheck = match OpamFile.OPAM.name_opt opam with
        | None ->
          OpamConsole.error "Missing \"name: %S\" field."
            (OpamPackage.Name.to_string name);
          false
        | Some n when n <> name ->
          OpamConsole.error "Bad \"name: %S\" field, package name is %s"
            (OpamPackage.Name.to_string n) (OpamPackage.Name.to_string name);
          false
        | _ -> true
      in
      let versioncheck = match OpamFile.OPAM.version_opt opam with
        | None ->
          OpamConsole.error "Missing \"version\" field.";
          false
        | Some _ -> true
      in
      if not namecheck || not versioncheck then failwith "Bad name/version";
      match warnings with
      | [] -> Some opam
      | ws ->
        OpamConsole.warning "The opam file didn't pass validation:";
        OpamConsole.errmsg "%s\n" (OpamFileTools.warns_to_string ws);
        if OpamConsole.confirm "Continue anyway ('no' will reedit) ?"
        then Some opam
        else edit ()
    with e ->
      OpamStd.Exn.fatal e;
      log "Editing error: %s" (Printexc.to_string e);
      if OpamConsole.confirm "Errors in %s, retry editing ?"
          (OpamFile.to_string temp_file)
      then edit ()
      else None
  in
  match edit () with
  | None -> None
  | Some new_opam -> Some new_opam

let edit st name =
  log "pin-edit %a" (slog OpamPackage.Name.to_string) name;
  let nv =
    try OpamPinned.package st name
    with Not_found ->
      OpamConsole.error_and_exit "%s is not pinned"
        (OpamPackage.Name.to_string name)
  in
  let path f = f st.switch_global.root st.switch name in
  let overlay_file = path OpamPath.Switch.Overlay.opam in
  let temp_file = path OpamPath.Switch.Overlay.tmp_opam in
  let current_opam = OpamSwitchState.opam_opt st nv in
  if not (OpamFile.exists temp_file) then
    (let base_opam = match current_opam with
        | None -> OpamFileTools.template nv
        | Some o -> o
     in
     OpamFile.OPAM.write_with_preserved_format
       ?format_from:(OpamPinned.orig_opam_file base_opam)
       temp_file base_opam);
  match edit_raw name temp_file with
  | None -> st
  | Some opam ->
    OpamConsole.msg "You can edit this file again with \"opam pin edit %s\"\n"
      (OpamPackage.Name.to_string name);
    match current_opam with
    |  Some o when OpamFile.OPAM.equal opam o ->
      (OpamConsole.msg "Package metadata unchanged.\n"; st)
    | _ ->
      (* Remove obsolete auxiliary files, in case *)
      OpamFilename.remove
        (OpamFile.filename (path OpamPath.Switch.Overlay.url));
      OpamFilename.remove
        (OpamFile.filename (path OpamPath.Switch.Overlay.descr));

      let opam_extra =
        OpamStd.Option.default [] @@ OpamFile.OPAM.extra_files opam
      in
      List.iter (fun f ->
          let base =
            OpamFilename.Base.of_string @@
            OpamFilename.remove_prefix (path OpamPath.Switch.Overlay.files) f
          in
          if not (List.mem_assoc base opam_extra) then
            (OpamConsole.note "Removing obsolete overlay file %s"
               (OpamFilename.to_string f);
             OpamFilename.remove f))
        (OpamFilename.rec_files (path OpamPath.Switch.Overlay.files));

      (* Write to overlay *)
      OpamFile.OPAM.write_with_preserved_format ~format_from:temp_file
        overlay_file
        opam;
      OpamFilename.remove (OpamFile.filename temp_file);

      (* Save back to source *)
      ignore OpamStd.Option.Op.(
          OpamFile.OPAM.get_url opam >>= OpamUrl.local_dir >>| fun dir ->
          let src_opam =
            OpamStd.Option.default
              (OpamFile.make OpamFilename.Op.(dir // "opam"))
              (OpamPinned.find_opam_file_in_source name dir)
          in
          let clean_opam =
            OpamFile.OPAM.with_url_opt None @*
            OpamFile.OPAM.with_extra_files []
          in
          if (current_opam >>| fun o ->
              OpamFile.OPAM.equal (clean_opam opam) (clean_opam o))
             <> Some true &&
             OpamConsole.confirm "Save the new opam file back to %S ?"
               (OpamFile.to_string src_opam) then
            OpamFile.OPAM.write_with_preserved_format src_opam
              (clean_opam opam)
        );

      let nv = OpamPackage.create name (OpamFile.OPAM.version opam) in
      let st = OpamSwitchState.update_pin nv opam st in
      OpamSwitchAction.write_selections st;
      st

let version_pin st name version =
  let root = st.switch_global.root in
  let nv = OpamPackage.create name version in
  let repo_opam =
    try OpamPackage.Map.find nv st.repos_package_index
    with Not_found ->
      OpamConsole.error_and_exit
        "Package %s has no known version %s in the repositories"
        (OpamPackage.Name.to_string name)
        (OpamPackage.Version.to_string version)
  in
  begin match OpamPinned.package_opt st name with
    | Some pinned_nv ->
      let opam = OpamSwitchState.opam st pinned_nv in
      if Some opam =
         OpamPackage.Map.find_opt pinned_nv st.repos_package_index
      then (* already version-pinned *)
        (if pinned_nv <> nv then
           OpamConsole.note
             "Package %s used to be pinned to version %s"
             (OpamPackage.Name.to_string name)
             (OpamPackage.Version.to_string version))
      else if OpamConsole.confirm
          "Package %s is already %s. Unpin and continue ?"
          (OpamPackage.Name.to_string name)
          (string_of_pinned opam)
      then
        OpamFilename.rmdir
          (OpamPath.Switch.Overlay.package root st.switch name)
      else
        (OpamConsole.msg "Aborting.\n"; OpamStd.Sys.exit 10)
    | None -> ()
  end;
  let st = OpamSwitchState.update_pin nv repo_opam st in
  OpamSwitchAction.write_selections st;
  st

let source_pin st name ?version ?edit:(need_edit=false) ?(force=false) target_url =
  log "pin %a to %a %a"
    (slog OpamPackage.Name.to_string) name
    (slog (OpamStd.Option.to_string OpamPackage.Version.to_string)) version
    (slog (OpamStd.Option.to_string ~none:"none" OpamUrl.to_string)) target_url;
  let installed_version =
    try
      Some (OpamPackage.version
              (OpamSwitchState.find_installed_package_by_name st name))
    with Not_found -> None
  in

  let cur_version, cur_urlf =
    try
      let cur_version = OpamPinned.version st name in
      let nv = OpamPackage.create name cur_version in
      let cur_opam = OpamSwitchState.opam st nv in
      let cur_urlf = OpamFile.OPAM.url cur_opam in
      let no_changes =
        target_url = OpamStd.Option.map OpamFile.URL.url cur_urlf &&
        (version = Some cur_version || version = None)
      in
      OpamConsole.note
        "Package %s is %s %s."
        (OpamPackage.Name.to_string name)
        (if no_changes then "already" else "currently")
        (string_of_pinned cur_opam);
      if no_changes then
        (OpamConsole.msg "No changes.\n"; OpamStd.Sys.exit 0);
      if OpamConsole.confirm "Proceed and change pinning target ?" then
        OpamFilename.remove
          (OpamFile.filename
             (OpamPath.Switch.Overlay.tmp_opam
                st.switch_global.root st.switch name))
      else OpamStd.Sys.exit 10;
      cur_version, cur_urlf
    with Not_found ->
      if OpamPackage.has_name st.compiler_packages name then (
        OpamConsole.warning
          "Package %s is part of the base packages of this compiler."
          (OpamPackage.Name.to_string name);
        if not @@ OpamConsole.confirm
            "Are you sure you want to override this and pin it anyway ?"
        then OpamStd.Sys.exit 10
      );
      let version =
        try OpamPackage.version (OpamSwitchState.get_package st name)
        with Not_found ->
          OpamStd.Option.Op.(installed_version +!
                             OpamPackage.Version.of_string "~dev")
      in
      version, None
  in

  if OpamPackage.Set.is_empty (OpamPackage.packages_of_name st.packages name) &&
     not (OpamConsole.confirm
            "Package %s does not exist, create as a %s package ?"
            (OpamPackage.Name.to_string name)
            (OpamConsole.colorise `bold "NEW"))
  then
    (OpamConsole.msg "Aborting.\n";
     OpamStd.Sys.exit 10);

  (match OpamStd.Option.map OpamFile.URL.url cur_urlf, target_url with
   | Some u, Some target when OpamUrl.(
       u.transport <> target.transport ||
       u.path <> target.path ||
       u.backend <> target.backend
     ) ->
     OpamFilename.rmdir
       (OpamPath.Switch.dev_package st.switch_global.root st.switch name)
   | _ -> ());

  let pin_version = OpamStd.Option.Op.(version +! cur_version) in

  let nv = OpamPackage.create name pin_version in

  let urlf =
    OpamStd.Option.Op.(
      target_url >>| OpamFile.URL.create >>+ fun () ->
      cur_urlf >>+ fun () ->
      OpamPackage.Map.find_opt nv st.installed_opams >>= OpamFile.OPAM.url
      >>+ fun () ->
      OpamSwitchState.url st nv
    )
  in

  let target_url = OpamStd.Option.map OpamFile.URL.url urlf in

  let temp_file =
    OpamPath.Switch.Overlay.tmp_opam st.switch_global.root st.switch name
  in

  OpamFilename.remove (OpamFile.filename temp_file);

  let opam_opt =
    try
      OpamStd.Option.Op.(
        urlf >>= fun url ->
        OpamProcess.Job.run @@ get_source_definition ?version st nv url
      )
    with Not_found ->
      if force then None else
        OpamConsole.error_and_exit "Error getting source from %s"
          (OpamStd.Option.to_string OpamUrl.to_string target_url)
  in

  let nv =
    match version with
    | Some _ -> nv
    | None ->
      OpamPackage.create name OpamStd.Option.Op.(
          (opam_opt >>= OpamFile.OPAM.version_opt)
          +! cur_version)
  in

  let opam_opt =
    OpamStd.Option.Op.(
      opam_opt >>+ fun () ->
      OpamPackage.Map.find_opt nv st.installed_opams >>+ fun () ->
      OpamSwitchState.opam_opt st nv)
  in

  let need_edit = need_edit || opam_opt = None in

  let opam_opt =
    let opam_base = match opam_opt with
      | None -> OpamFileTools.template nv
      | Some opam -> opam
    in
    let opam_base =
      OpamFile.OPAM.with_url_opt urlf opam_base
    in
    if need_edit then
      (if not (OpamFile.exists temp_file) then
         OpamFile.OPAM.write_with_preserved_format
           ?format_from:(OpamPinned.orig_opam_file opam_base)
           temp_file opam_base;
       edit_raw name temp_file)
    else
      Some (OpamFile.OPAM.with_url_opt urlf opam_base)
  in
  match opam_opt with
  | None ->
    OpamConsole.error_and_exit "No valid package definition found"
  | Some opam ->
    let opam =
      match OpamFile.OPAM.get_url opam with
      | Some _ -> opam
      | None -> OpamFile.OPAM.with_url_opt urlf opam
    in
    let opam =
      opam |>
      OpamFile.OPAM.with_name name |>
      OpamFile.OPAM.with_version
        OpamStd.Option.Op.(OpamFile.OPAM.version_opt opam +! nv.version)
    in
    OpamFilename.rmdir
      (OpamPath.Switch.Overlay.package st.switch_global.root st.switch nv.name);

    let opam = copy_files st opam in

    OpamFile.OPAM.write_with_preserved_format
      ?format_from:(OpamPinned.orig_opam_file opam)
      (OpamPath.Switch.Overlay.opam st.switch_global.root st.switch nv.name)
      opam;

    OpamFilename.remove (OpamFile.filename temp_file);

    let st = OpamSwitchState.update_pin nv opam st in

    OpamSwitchAction.write_selections st;
    OpamConsole.msg "%s is now %s\n"
      (OpamPackage.Name.to_string name)
      (string_of_pinned opam);

    (match target_url with
     | Some ({ OpamUrl.backend = #OpamUrl.version_control;
               transport = "file";
               hash = None;
               path = _; } as url) ->
       (match OpamUrl.local_dir url with
        | Some dir ->
          OpamConsole.note
            "Pinning in mixed mode: OPAM will use tracked files in the current \
             working tree from %s. If this is not what you want, pin to a \
             given branch (e.g. %s#HEAD)"
            (OpamFilename.Dir.to_string dir) (OpamUrl.to_string url)
        | None -> ())
     | _ -> ());
    st

(* pure *)
let unpin_one st nv =
  let st =
    { st with pinned = OpamPackage.Set.remove nv st.pinned }
  in
  (* Restore availability of other versions of this package from the repos *)
  let repo_package =
    OpamPackage.Map.filter (fun nv2 _ -> nv2.name = nv.name)
      st.repos_package_index
  in
  let available_packages = lazy (
    OpamSwitchState.compute_available_packages
      st.switch_global st.switch st.switch_config ~pinned:OpamPackage.Set.empty
      ~opams:repo_package |>
    OpamPackage.Set.union
      (OpamPackage.Set.remove nv (Lazy.force st.available_packages))
  ) in
  match OpamPackage.Map.find_opt nv st.repos_package_index,
        OpamPackage.Map.find_opt nv st.installed_opams with
  | None, None ->
    OpamSwitchState.remove_package_metadata nv st
  | Some opam, _ | None, Some opam -> (* forget about overlay *)
    let st = OpamSwitchState.update_package_metadata nv opam st in
    { st with available_packages }

let unpin st names =
  log "unpin %a"
    (slog @@ OpamStd.List.concat_map " " OpamPackage.Name.to_string) names;
  List.fold_left (fun st name ->
      OpamFilename.rmdir
        (OpamPath.Switch.Overlay.package
           st.switch_global.root st.switch name);
      match OpamPinned.package_opt st name with
      | Some nv ->
        let pin_str =
          OpamStd.Option.to_string ~none:"pinned"
            string_of_pinned (OpamSwitchState.opam_opt st nv)
        in
        let st = unpin_one st nv in
        OpamSwitchAction.write_selections st;
        OpamConsole.msg "%s is no longer %s\n"
          (OpamPackage.Name.to_string name) pin_str;
        st
      | None ->
        OpamConsole.note "%s is not pinned." (OpamPackage.Name.to_string name);
        st)
    st names

let list st ~short =
  log "pin_list";
  if short then
    OpamPackage.Set.iter
      (fun nv -> OpamConsole.msg "%s\n" (OpamPackage.name_to_string nv))
      st.pinned
  else
  let lines nv =
    try
    let opam = OpamSwitchState.opam st nv in
    let url = OpamFile.OPAM.get_url opam in
    let kind, target =
      if Some opam = OpamPackage.Map.find_opt nv st.repos_package_index then
        "version", OpamPackage.Version.to_string nv.version
      else
      match url with
      | Some u -> OpamUrl.string_of_backend u.OpamUrl.backend, OpamUrl.to_string u
      | None -> "local definition", ""
    in
    let state, extra =
      try
        let inst = OpamSwitchState.find_installed_package_by_name st nv.name in
        if inst.version = nv.version then "",[]
        else
          OpamConsole.colorise `red "(not in sync)",
          [Printf.sprintf "(installed:%s)"
             (OpamConsole.colorise `bold (OpamPackage.version_to_string inst))]
      with Not_found -> OpamConsole.colorise `yellow " (uninstalled)", []
    in
    [ OpamPackage.to_string nv;
      state;
      OpamConsole.colorise `blue kind;
      String.concat " " (target::extra) ]
    with Not_found ->
      [ OpamPackage.to_string nv;
        OpamConsole.colorise `red " (no definition found)" ]
  in
  let table = List.map lines (OpamPackage.Set.elements st.pinned) in
  OpamStd.Format.print_table stdout ~sep:"  " (OpamStd.Format.align_table table)

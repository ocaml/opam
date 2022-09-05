(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStateTypes
open OpamStd.Op
open OpamStd.Option.Op

let log fmt = OpamConsole.log "COMMAND" fmt
let slog = OpamConsole.slog

let string_of_pinned ?(subpath_prefix=true) opam =
  let bold = OpamConsole.colorise `bold in
  let url, subpath =
    match OpamFile.OPAM.url opam with
    | None -> None, None
    | Some u -> OpamFile.URL.(Some (url u), subpath u)
  in
  Printf.sprintf "%spinned %s (version %s)"
    (if subpath_prefix && subpath <> None then "subpath-" else "")
    (OpamStd.Option.to_string ~none:(bold "locally")
       (fun u -> Printf.sprintf "to %s"
           (bold (OpamUrl.to_string_w_subpath subpath
                    u)))
       url)
    (bold (OpamPackage.Version.to_string (OpamFile.OPAM.version opam)))

let read_opam_file_for_pinning ?locked ?(quiet=false) name f url =
  let opam0 =
    let dir = OpamFilename.dirname (OpamFile.filename f) in
    (* don't add aux files for [project/opam] *)
    let add_files = OpamUrl.local_dir url = Some dir in
    let opam =
      (OpamFormatUpgrade.opam_file_with_aux ~quiet ~dir ~files:add_files
         ~filename:f) (OpamFile.OPAM.safe_read f)
    in
    if opam = OpamFile.OPAM.empty then None else Some opam
  in
  (match opam0 with
   | None ->
     let warns, _ = OpamFileTools.lint_file f in
     OpamConsole.error
       "Invalid opam file in %s source from %s:"
       (OpamPackage.Name.to_string name)
       (OpamUrl.to_string url);
     OpamConsole.errmsg "%s\n" (OpamFileTools.warns_to_string warns)
   | Some opam ->
     let warns =  OpamFileTools.lint opam in
     if not quiet && warns <> [] then
       (OpamConsole.warning
          "Failed checks on %s package definition from source at %s:"
          (OpamPackage.Name.to_string name)
          (OpamUrl.to_string url);
        OpamConsole.errmsg "%s\n" (OpamFileTools.warns_to_string warns)));
  opam0
  >>| OpamFile.OPAM.with_locked_opt locked


exception Fetch_Fail of string

let get_source_definition ?version ?subpath ?locked st nv url =
  let root = st.switch_global.root in
  let srcdir = OpamPath.Switch.pinned_package root st.switch nv.name in
  let fix opam =
    OpamFile.OPAM.with_url url @@
    (match version with
     | Some v -> OpamFile.OPAM.with_version v
     | None -> fun o -> o) @@
    opam
  in
  let open OpamProcess.Job.Op in
  let url =
    let u = OpamFile.URL.url url in
    match OpamUrl.local_dir u, u.OpamUrl.backend with
    | Some dir, #OpamUrl.version_control ->
      OpamFile.URL.with_url
        (OpamUrl.of_string (OpamFilename.Dir.to_string dir))
        url
    | _, _ -> url
  in
  OpamUpdate.fetch_dev_package url srcdir ?subpath nv @@| function
  | Not_available (_,s) -> raise (Fetch_Fail s)
  | Up_to_date _ | Result _ ->
    let srcdir = OpamFilename.SubPath.(srcdir /? subpath) in
    match OpamPinned.find_opam_file_in_source ?locked nv.name srcdir with
    | None -> None
    | Some (f, locked) ->
      match read_opam_file_for_pinning nv.name ?locked f (OpamFile.URL.url url) with
      | None ->
        let dst =
          OpamFile.filename
            (OpamPath.Switch.Overlay.tmp_opam root st.switch nv.name)
        in
        OpamFilename.copy ~src:(OpamFile.filename f) ~dst;
        None
      | Some opam -> Some (fix opam)

let copy_files st opam =
  let name = OpamFile.OPAM.name opam in
  let files =
    OpamFile.OPAM.get_extra_files
      ~repos_roots:(OpamRepositoryState.get_root st.switch_repos)
      opam
  in
  if files = [] then
    (match OpamFile.OPAM.extra_files opam with
     | Some [] | None -> ()
     | Some files ->
       OpamConsole.warning
         "Ignoring overlay files of %s (files%s*) that were not found: %s"
         (OpamPackage.Name.to_string name) Filename.dir_sep
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
        let hash =
          if not (OpamHash.check_file (OpamFilename.to_string src) hash) then
            if OpamFormatConfig.(!r.strict) then
              OpamConsole.error_and_exit `File_error
                "Hash mismatch on %s %s (strict mode)"
                (OpamPackage.Name.to_string name)
                (OpamFilename.to_string src)
            else
              (OpamConsole.warning
                 "Hash doesn't match for overlay file of %s %s, adjusted"
                 (OpamPackage.Name.to_string name)
                 (OpamFilename.to_string src);
               OpamHash.compute (OpamFilename.to_string src))
          else hash
        in
        OpamFilename.copy ~src ~dst:(OpamFilename.create destdir rel_file);
        (rel_file, hash) :: acc)
      [] files
  in
  OpamFile.OPAM.with_extra_files (List.rev files) opam

(* Returns the new opam file, without writing it to disk *)
let edit_raw name temp_file =
  let rec edit () =
    OpamConsole.pause
      "Press enter to start \"%s\" (this can be customised by setting EDITOR \
       or OPAMEDITOR)..."
      OpamClientConfig.(!r.editor);
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
      (OpamFilename.remove (OpamFile.filename temp_file);
       OpamConsole.error "Empty file or editor error, aborting.";
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
        if OpamConsole.confirm "Proceed anyway ('no' will re-edit)?"
        then Some opam
        else edit ()
    with e ->
      OpamStd.Exn.fatal e;
      (match e with
       | Failure _ -> ()
       | e -> OpamConsole.error "%s" (Printexc.to_string e));
      if OpamStd.Sys.tty_in &&
         OpamConsole.confirm "Errors in %s, edit again?"
           (OpamFile.to_string temp_file)
      then edit ()
      else None
  in
  match edit () with
  | None -> None
  | Some new_opam ->
    OpamConsole.msg
      "You can edit this file again with \"opam pin edit %s\", export it with \
       \"opam show %s --raw\"\n"
      (OpamPackage.Name.to_string name)
      (OpamPackage.Name.to_string name);
    Some new_opam

let edit st ?version name =
  log "pin-edit %a" (slog OpamPackage.Name.to_string) name;
  let nv =
    try OpamPinned.package st name
    with Not_found ->
      OpamConsole.error_and_exit `Bad_arguments "%s is not pinned"
        (OpamPackage.Name.to_string name)
  in
  let new_nv = match version with
    | None -> nv
    | Some v -> OpamPackage.create name v
  in
  let path f = f st.switch_global.root st.switch name in
  let overlay_file = path OpamPath.Switch.Overlay.opam in
  let temp_file = path OpamPath.Switch.Overlay.tmp_opam in
  let current_opam = OpamSwitchState.opam_opt st nv in
  if not (OpamFile.exists temp_file) then
    (let base_opam = match current_opam with
        | None -> OpamFileTools.template new_nv
        | Some o -> OpamFile.OPAM.with_version new_nv.version o
     in
     OpamFile.OPAM.write_with_preserved_format
       ?format_from:(OpamPinned.orig_opam_file st name base_opam)
       temp_file base_opam);
  match edit_raw name temp_file with
  | None -> st
  | Some opam ->
    let opam = match current_opam with
      | Some cur -> OpamFile.OPAM.(with_metadata_dir (metadata_dir cur)) opam
      | None -> opam
    in
    let opam = copy_files st opam in
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
      ignore (
          OpamFile.OPAM.get_url opam >>= OpamUrl.local_dir >>| fun dir ->
          let src_opam =
              (OpamPinned.find_opam_file_in_source name dir >>| fst)
              +! (OpamFile.make OpamFilename.Op.(dir // "opam"))
          in
          let clean_opam =
            OpamFile.OPAM.with_url_opt None @*
            OpamFile.OPAM.with_extra_files []
          in
          if (current_opam >>| fun o ->
              OpamFile.OPAM.equal (clean_opam opam) (clean_opam o))
             <> Some true &&
             OpamConsole.confirm "Save the new opam file back to %S?"
               (OpamFile.to_string src_opam) then
            OpamFile.OPAM.write_with_preserved_format src_opam
              (clean_opam opam)
        );

      let nv = OpamPackage.create name (OpamFile.OPAM.version opam) in
      let st = OpamSwitchState.update_pin nv opam st in
      OpamUpdate.cleanup_source st current_opam opam;
      if not OpamClientConfig.(!r.show) then
        OpamSwitchAction.write_selections st;
      st

let version_pin st name version =
  let root = st.switch_global.root in
  let nv = OpamPackage.create name version in
  let repo_opam =
    try OpamPackage.Map.find nv st.repos_package_index
    with Not_found ->
      OpamConsole.error_and_exit `Not_found
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
           (OpamConsole.note
              "Package %s used to be pinned to version %s"
              (OpamPackage.Name.to_string name)
              (OpamPackage.Version.to_string pinned_nv.version);
            OpamFilename.rmdir
              (OpamPath.Switch.Overlay.package root st.switch name))
         else OpamConsole.note "Pinning unchanged")
      else if OpamConsole.confirm
          "Package %s is already %s. Unpin and continue?"
          (OpamPackage.Name.to_string name)
          (string_of_pinned opam)
      then
        OpamFilename.rmdir
          (OpamPath.Switch.Overlay.package root st.switch name)
      else
        (OpamConsole.msg "Aborting.\n"; OpamStd.Sys.exit_because `Aborted)
    | None -> ()
  end;
  let st = OpamSwitchState.update_pin nv repo_opam st in
  if not OpamClientConfig.(!r.show) then
    OpamSwitchAction.write_selections st;
  OpamConsole.msg "%s is now pinned to version %s\n"
    (OpamPackage.Name.to_string name)
    (OpamPackage.Version.to_string version);
  st

exception Aborted
exception Nothing_to_do

let default_version st name =
  try OpamPackage.version (OpamSwitchState.get_package st name)
  with Not_found -> OpamPackage.Version.default

let fetch_all_pins st ?working_dir pins =
  let root = st.switch_global.root in
  let fetched =
    let cache_dir =
      OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
    in
    let command pinned =
      let { pinned_name = name; pinned_url = url;
            pinned_subpath = subpath; _ } = pinned in
      let srcdir = OpamPath.Switch.pinned_package root st.switch name in
      let name = OpamPackage.Name.to_string name in
      OpamProcess.Job.Op.(
        OpamRepository.pull_tree ~cache_dir ?subpath ?working_dir
          name srcdir [] [url]
        @@| fun r -> (pinned, r))
    in
    OpamParallel.map ~jobs:OpamStateConfig.(!r.dl_jobs) ~command pins
  in
  let errored, to_pin =
    List.fold_left (fun (err, ok) (pinned, result) ->
        match result with
        | Not_available _ -> (* clean dir ? *)
          pinned::err, ok
        | _ -> err, pinned::ok)
      ([],[]) fetched
  in
  if errored = []
  || OpamConsole.confirm
       "Could not retrieve some package sources, they will not be pinned nor \
        installed:%s\n\
        Continue anyway?"
       (OpamStd.Format.itemize (fun p ->
            Printf.sprintf "%s:%s"
              (OpamPackage.Name.to_string p.pinned_name)
              (OpamUrl.to_string_w_subpath p.pinned_subpath
                 p.pinned_url))
           (List.rev errored))
  then
    List.rev to_pin
  else
    OpamStd.Sys.exit_because `Aborted

let rec handle_pin_depends st nv opam =
  let extra_pins = OpamFile.OPAM.pin_depends opam in
  let extra_pins =
    List.filter (fun (nv, url) ->
        not (OpamPackage.Set.mem nv st.pinned &&
             OpamSwitchState.primary_url st nv = Some url))
      extra_pins
  in
  if extra_pins = [] then st else
    (OpamConsole.msg
       "The following additional pinnings are required by %s:\n%s"
       (OpamPackage.to_string nv)
       (OpamStd.Format.itemize
          (fun (nv, url) -> Printf.sprintf "%s at %s"
              (OpamConsole.colorise `bold (OpamPackage.to_string nv))
              (OpamConsole.colorise `underline (OpamUrl.to_string url)))
          extra_pins);
     if OpamConsole.confirm "Pin and install them?" then
       (let extra_pins =
          fetch_all_pins st (List.map (fun (nv, u) ->
              { pinned_name = OpamPackage.name nv;
                pinned_version = Some (OpamPackage.version nv);
                pinned_opam = None;
                pinned_url = u;
                pinned_subpath = None;
                })
              extra_pins)
        in
        List.fold_left (fun st pin ->
            source_pin st pin.pinned_name ?version:pin.pinned_version
              (Some pin.pinned_url) ~ignore_extra_pins:true)
          st extra_pins)
     else if OpamConsole.confirm
         "Try to install anyway, assuming `--ignore-pin-depends'?"
     then st else
       OpamStd.Sys.exit_because `Aborted)

and source_pin
    st name
    ?version ?edit:(need_edit=false) ?opam:opam_opt ?(quiet=false)
    ?(force=false) ?(ignore_extra_pins=OpamClientConfig.(!r.ignore_pin_depends))
    ?subpath ?locked
    target_url
  =
  log "pin %a to %a %a"
    (slog OpamPackage.Name.to_string) name
    (slog (OpamStd.Option.to_string OpamPackage.Version.to_string)) version
    (slog (OpamStd.Option.to_string ~none:"none"
             (OpamUrl.to_string_w_subpath subpath))) target_url;
 (* let installed_version =
    try
      Some (OpamPackage.version
              (OpamSwitchState.find_installed_package_by_name st name))
    with Not_found -> None
  in *)

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
      if not (quiet && no_changes) then
        OpamConsole.note
          "Package %s is %s %s."
          (OpamPackage.Name.to_string name)
          (if no_changes then "already" else "currently")
          (string_of_pinned cur_opam);
      if no_changes then ()
      else (* if OpamConsole.confirm "Proceed and change pinning target?" then *)
        OpamFilename.remove
          (OpamFile.filename
             (OpamPath.Switch.Overlay.tmp_opam
                st.switch_global.root st.switch name))
      (* else raise Exns.Aborted *);
      cur_version, cur_urlf
    with Not_found ->
      let version = default_version st name in
      version, None
  in

  if not (OpamPackage.has_name st.packages name) &&
     not (OpamConsole.confirm
            "Package %s does not exist, create as a %s package?"
            (OpamPackage.Name.to_string name)
            (OpamConsole.colorise `bold "NEW"))
  then raise Aborted;

  (match OpamStd.Option.map OpamFile.URL.url cur_urlf, target_url with
   | Some u, Some target when OpamUrl.(
       u.transport <> target.transport ||
       u.path <> target.path ||
       u.backend <> target.backend
     ) ->
     OpamFilename.rmdir
       (OpamPath.Switch.pinned_package st.switch_global.root st.switch name)
   | _ -> ());

  let pin_version = version +! cur_version in

  let nv = OpamPackage.create name pin_version in

  let urlf = target_url >>| OpamFile.URL.create ?subpath in

  let temp_file =
    OpamPath.Switch.Overlay.tmp_opam st.switch_global.root st.switch name
  in

  let opam_local =
    OpamFile.OPAM.read_opt temp_file
    >>| OpamFormatUpgrade.opam_file
  in
  OpamFilename.remove (OpamFile.filename temp_file);

  let opam_opt =
    try
      opam_opt >>+ fun () ->
      urlf >>= fun url ->
      OpamProcess.Job.run @@ get_source_definition ?version ?subpath ?locked st nv url
    with Fetch_Fail err ->
      if force then None else
        (OpamConsole.error_and_exit `Sync_error
           "Error getting source from %s:\n%s"
           (OpamStd.Option.to_string OpamUrl.to_string target_url)
           (OpamStd.Format.itemize (fun x -> x) [err]));
  in
  let opam_opt = opam_opt >>| OpamFormatUpgrade.opam_file in

  let nv =
    match version with
    | Some _ -> nv
    | None ->
      OpamPackage.create name
        ((opam_opt >>= OpamFile.OPAM.version_opt)
         +! cur_version)
  in

  let opam_opt =
    opam_opt >>+ fun () ->
    OpamPackage.Map.find_opt nv st.installed_opams >>+ fun () ->
    OpamSwitchState.opam_opt st nv
  in

  let opam_opt =
    match opam_local, opam_opt with
    | Some local, None ->
      OpamConsole.warning
        "Couldn't retrieve opam file from versioned source, \
         using the one found locally.";
      Some local
    | Some local, Some vers when
        not OpamFile.(OPAM.effectively_equal
                        (OPAM.with_url URL.empty local)
                        (OPAM.with_url URL.empty vers)) ->
      OpamConsole.warning
        "%s's opam file has uncommitted changes, using the versioned one"
        (OpamPackage.Name.to_string name);
      opam_opt
    | _ -> opam_opt
  in

  if not need_edit && opam_opt = None then
    OpamConsole.note
      "No package definition found for %s: please complete the template"
      (OpamConsole.colorise `bold (OpamPackage.to_string nv));

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
           ?format_from:(OpamPinned.orig_opam_file st name opam_base)
           temp_file opam_base;
       edit_raw name temp_file >>|
       (* Preserve metadata_dir so that copy_files below works *)
       OpamFile.OPAM.(with_metadata_dir (metadata_dir opam_base))
      )
    else
      Some opam_base
  in
  match opam_opt with
  | None ->
    OpamConsole.error_and_exit `Not_found
      "No valid package definition found"
  | Some opam ->
    let opam =
      match OpamFile.OPAM.get_url opam with
      | Some _ -> opam
      | None -> OpamFile.OPAM.with_url_opt urlf opam
    in
    let version = version +! (OpamFile.OPAM.version_opt opam +! nv.version) in
    let nv = OpamPackage.create nv.name version in
    let st =
      if ignore_extra_pins then st
      else handle_pin_depends st nv opam
    in
    let opam =
      opam |>
      OpamFile.OPAM.with_name name |>
      OpamFile.OPAM.with_version version
    in
    OpamFilename.rmdir
      (OpamPath.Switch.Overlay.package st.switch_global.root st.switch nv.name);

    let opam = copy_files st opam in

    OpamFile.OPAM.write_with_preserved_format
      ?format_from:(OpamPinned.orig_opam_file st name opam)
      (OpamPath.Switch.Overlay.opam st.switch_global.root st.switch nv.name)
      opam;

    OpamFilename.remove (OpamFile.filename temp_file);

    let st = OpamSwitchState.update_pin nv opam st in

    if not OpamClientConfig.(!r.show) then
      OpamSwitchAction.write_selections st;
    OpamConsole.msg "%s is now %s\n"
      (OpamPackage.Name.to_string name)
      (string_of_pinned opam);

    st

let pin_current st nv =
  let root = st.switch_global.root in
  let opam =
    try OpamPackage.Map.find nv st.installed_opams
    with Not_found ->
      OpamConsole.error_and_exit `Not_found
        "No metadata found for %s"
        (OpamPackage.to_string nv)
  in
  let opam =
    opam |>
    OpamFile.OPAM.with_name nv.name |>
    OpamFile.OPAM.with_version nv.version
  in
  let overlay =
    OpamPath.Switch.Overlay.package root st.switch nv.name
  in
  OpamFilename.cleandir overlay;
  OpamFile.OPAM.write_with_preserved_format
    ~format_from:(OpamPath.Switch.installed_opam root st.switch nv)
    (OpamPath.Switch.Overlay.opam st.switch_global.root st.switch nv.name)
    opam;
  let src = OpamPath.Switch.installed_opam_files_dir root st.switch nv in
  if OpamFilename.exists_dir src then
    OpamFilename.copy_dir ~src
      ~dst:(OpamPath.Switch.Overlay.files root st.switch nv.name);
  let st = OpamSwitchState.update_pin nv opam st in
  OpamSwitchAction.write_selections st;
  OpamConsole.msg "%s is now %s\n"
    (OpamPackage.Name.to_string nv.name)
    (string_of_pinned opam);
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
        (OpamPath.Switch.pinned_package st.switch_global.root st.switch name);
      OpamFilename.rmdir
        (OpamPath.Switch.Overlay.package
           st.switch_global.root st.switch name);
      match OpamPinned.package_opt st name with
      | Some nv ->
        let pin_str =
          OpamStd.Option.to_string ~none:"pinned"
            (string_of_pinned ~subpath_prefix:false)
            (OpamSwitchState.opam_opt st nv)
        in
        let st = unpin_one st nv in
        if OpamClientConfig.(!r.show) then
          OpamConsole.msg "Ok, %s would no longer be %s\n"
            (OpamPackage.Name.to_string name) pin_str
        else
          (OpamSwitchAction.write_selections st;
           OpamConsole.msg "Ok, %s is no longer %s\n"
             (OpamPackage.Name.to_string name) pin_str);
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
      let url = OpamFile.OPAM.url opam in
      let kind, target =
        if OpamSwitchState.is_version_pinned st nv.name then
          "version", OpamPackage.Version.to_string nv.version
        else
        match url with
        | Some url ->
          let u = OpamFile.URL.url url in
          OpamUrl.string_of_backend u.OpamUrl.backend,
          OpamUrl.to_string_w_subpath (OpamFile.URL.subpath url) u
        | None -> "local definition", ""
      in
      let state, extra =
        try
          let inst =
            OpamSwitchState.find_installed_package_by_name st nv.name
          in
          if inst.version = nv.version then "",[]
          else
            OpamConsole.colorise `red "(not in sync)",
            [Printf.sprintf "(installed:%s)"
               (OpamConsole.colorise `bold
                  (OpamPackage.version_to_string inst))]
        with Not_found -> OpamConsole.colorise `yellow "(uninstalled)", []
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
  OpamConsole.print_table stdout ~sep:"  " (OpamStd.Format.align_table table)

(* Must not be contained in a package name, version, nor url *)
let scan_sep = '^'

let scan ~normalise ~recurse ?subpath url =
  let pins_of_dir dir =
    OpamPinned.files_in_source_w_target
      ?locked:OpamStateConfig.(!r.locked)
      ~recurse ?subpath url dir
    |> OpamStd.List.filter_map (fun nf ->
        let opam =
          OpamFile.OPAM.(safe_read nf.pin.pin_file
                         |> with_locked_opt nf.pin.pin_locked)
        in
        match (nf.pin_name ++ OpamFile.OPAM.name_opt opam) with
        | Some name ->
          Some { pinned_name = name;
                 pinned_version = OpamFile.OPAM.version_opt opam;
                 pinned_url = nf.pin.pin_url;
                 pinned_subpath = nf.pin.pin_subpath;
                 pinned_opam = None; }
        | None ->
          OpamConsole.warning "Can not retrieve a package name from %s"
            (OpamFilename.to_string (OpamFile.filename nf.pin.pin_file));
          None)
  in
  let pins, cleanup =
    match OpamUrl.local_dir url with
    | Some dir -> pins_of_dir dir, None
    | None ->
      let pin_cache_dir = OpamRepositoryPath.pin_cache url in
      let cleanup = fun () ->
        OpamFilename.rmdir @@ OpamRepositoryPath.pin_cache_dir ()
      in
      let basename =
        match OpamStd.String.split (OpamUrl.basename url) '.' with
        | [] ->
          OpamConsole.error_and_exit `Bad_arguments
            "Can not retrieve a path from '%s'"
            (OpamUrl.to_string url)
        | b::_ -> b
      in
      try
        let open OpamProcess.Job.Op in
        OpamProcess.Job.run @@
        OpamRepository.pull_tree
          ~cache_dir:(OpamRepositoryPath.download_cache
                        OpamStateConfig.(!r.root_dir))
          basename pin_cache_dir [] [url] @@| function
        | Not_available (_,u) ->
          OpamConsole.error_and_exit `Sync_error
            "Could not retrieve %s" u
        | Result _ | Up_to_date _ ->
          pins_of_dir pin_cache_dir, Some cleanup
      with e -> OpamStd.Exn.finalise e cleanup
  in
  let finalise = OpamStd.Option.default (fun () -> ()) cleanup in
  OpamStd.Exn.finally finalise @@ fun () ->
  if normalise then
    OpamConsole.msg "%s"
      (OpamStd.List.concat_map "\n"
         (fun p ->
            Printf.sprintf "%s%s%c%s%s"
              (OpamPackage.Name.to_string p.pinned_name)
              (OpamStd.Option.to_string
                 (fun v -> "." ^OpamPackage.Version.to_string v)
                 p.pinned_version)
              scan_sep
              (OpamUrl.to_string p.pinned_url)
              (OpamStd.Option.to_string (fun sb ->
                   (String.make 1 scan_sep)
                   ^ OpamFilename.SubPath.to_string sb)
                  p.pinned_subpath))
         pins)
  else
    ["# Name"; "# Version"; "# Url" ; "# Subpath"] ::
    List.map (fun p ->
        [ OpamPackage.Name.to_string p.pinned_name;
          (p.pinned_version >>| OpamPackage.Version.to_string) +! "-";
          OpamUrl.to_string p.pinned_url;
          (p.pinned_subpath >>| OpamFilename.SubPath.normalised_string) +! "-"
        ])
      pins
    |> OpamStd.Format.align_table
    |> OpamConsole.print_table stdout ~sep:"  "

let looks_like_normalised args =
    List.for_all (fun s -> OpamStd.String.contains_char s scan_sep) args

let parse_pins pins =
  let separator = Re.char scan_sep in
  let re =
    Re.(compile @@ whole_string @@ seq [
        (* package name *)
        group @@
        rep1 @@ alt [ alnum; diff punct (alt [char '.'; char scan_sep]) ];
        (* optional version *)
        opt @@ seq [ char '.';
                     group @@
                     rep1 @@ alt [ alnum; diff punct separator ]];
        separator;
        (* url *)
        group @@ rep1 @@ diff any separator;
        (* optional subpath *)
        opt @@ seq [ separator; group @@ rep1 any ];
      ])
  in
  let get s =
    try
      let groups = Re.exec re s in
      Some Re.Group.{
          pinned_name = OpamPackage.Name.of_string @@ get groups 1;
          pinned_version =
            OpamStd.Option.map OpamPackage.Version.of_string
            @@ OpamStd.Option.of_Not_found (get groups) 2;
          pinned_url = OpamUrl.parse @@ get groups 3;
          pinned_subpath =
            OpamStd.Option.map OpamFilename.SubPath.of_string
            @@ OpamStd.Option.of_Not_found (get groups) 4;
          pinned_opam = None;
        }
    with Not_found | Failure _ -> None
  in
  OpamStd.List.filter_map (fun str ->
      let pin = get str in
      if pin = None then
        (OpamConsole.warning "Argument %S is not correct" str;
         None)
      else pin) pins

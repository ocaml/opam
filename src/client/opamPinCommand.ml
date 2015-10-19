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
open OpamTypesBase
open OpamState.Types
open OpamStd.Op

let log fmt = OpamConsole.log "COMMAND" fmt
let slog = OpamConsole.slog

let cleanup_dev_dirs t name =
  let packages = OpamPackage.packages_of_name t.packages name in
  OpamPackage.Set.iter (fun nv ->
      OpamFilename.rmdir (OpamPath.Switch.build t.root t.switch nv);
      OpamFilename.rmdir (OpamPath.Switch.dev_package t.root t.switch name);
    ) packages

let edit t name =
  log "pin-edit %a" (slog OpamPackage.Name.to_string) name;
  let pin =
    try OpamPackage.Name.Map.find name t.pinned
    with Not_found ->
      OpamConsole.error_and_exit "%s is not pinned."
        (OpamPackage.Name.to_string name)
  in
  let installed_nv =
    try Some (OpamState.find_installed_package_by_name t name)
    with Not_found -> None
  in
  let file = OpamPath.Switch.Overlay.opam t.root t.switch name in
  let temp_file = OpamPath.Switch.Overlay.tmp_opam t.root t.switch name in
  let orig_opam =
    try Some (OpamFile.OPAM.read file) with e -> OpamStd.Exn.fatal e; None
  in
  let version,empty_opam =
    match orig_opam with
    | None -> None,true
    | Some opam ->
      Some (OpamFile.OPAM.version opam),
      try
        opam = OpamFile.OPAM.create
          (OpamPackage.create name (OpamFile.OPAM.version opam))
      with OpamSystem.Internal_error _ -> true
  in
  if empty_opam && not (OpamFilename.exists temp_file) then
    OpamState.add_pinned_overlay ~template:true ?version t name;
  if not (OpamFilename.exists temp_file) then
    OpamFilename.copy ~src:file ~dst:temp_file;
  let rec edit () =
    try
      ignore @@ Sys.command
        (Printf.sprintf "%s %s"
           (OpamClientConfig.(!r.editor))
           (OpamFilename.to_string temp_file));
      let warnings,opam_opt =
        OpamFile.OPAM.validate_file temp_file
      in
      let opam = match opam_opt with
        | None ->
          OpamConsole.msg "Invalid opam file:\n%s\n"
            (OpamFile.OPAM.warns_to_string warnings);
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
      let versioncheck = match pin, OpamFile.OPAM.version_opt opam with
        | _, None ->
          OpamConsole.error "Missing \"version\" field.";
          false
        | Version vpin, Some v when v <> vpin ->
          OpamConsole.error "Bad \"version: %S\" field, package is pinned to %s"
            (OpamPackage.Version.to_string v) (OpamPackage.Version.to_string vpin);
          false
        | _ -> true
      in
      if not namecheck || not versioncheck then failwith "Bad name/version";
      match warnings with
      | [] -> Some opam
      | ws ->
        OpamConsole.warning "The opam file didn't pass validation:";
        OpamConsole.errmsg "%s\n" (OpamFile.OPAM.warns_to_string ws);
        if OpamConsole.confirm "Continue anyway ('no' will reedit) ?"
        then Some opam
        else edit ()
    with e ->
      OpamStd.Exn.fatal e;
      if OpamConsole.confirm "Errors in %s, retry editing ?"
          (OpamFilename.to_string file)
      then edit ()
      else None
  in
  match edit () with
  | None -> if empty_opam then raise Not_found else None
  | Some new_opam ->
    OpamFilename.move ~src:temp_file ~dst:file;
    OpamConsole.msg "You can edit this file again with \"opam pin edit %s\"\n"
      (OpamPackage.Name.to_string name);
    if Some new_opam = orig_opam then (
      OpamConsole.msg "Package metadata unchanged.\n";
      None
    ) else
    let () =
      let dir = match pin with
        | Source url -> OpamUrl.local_dir url
        | Version _ -> None
      in
      match dir with
      | Some dir ->
        let src_opam =
          OpamStd.Option.default OpamFilename.Op.(dir // "opam")
            (OpamState.find_opam_file_in_source name dir)
        in
        if OpamConsole.confirm "Save the new opam file back to %S ?"
            (OpamFilename.to_string src_opam) then
          OpamFilename.copy ~src:file ~dst:src_opam
      | _ -> ()
    in
    match installed_nv with
    | None -> None
    | Some nv -> Some (OpamPackage.version nv = OpamFile.OPAM.version new_opam)
(* unused ?
let update_set set old cur save =
  if OpamPackage.Set.mem old set then
    save (OpamPackage.Set.add cur (OpamPackage.Set.remove old set))
*)
let update_config t name pins =
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  cleanup_dev_dirs t name;
  OpamFile.Pinned.write pin_f pins

let pin name ?version pin_option =
  log "pin %a to %a"
    (slog OpamPackage.Name.to_string) name
    (slog string_of_pin_option) pin_option;
  let t = OpamState.load_state "pin" OpamStateConfig.(!r.current_switch) in
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  let pin_kind = kind_of_pin_option pin_option in
  let pins = OpamFile.Pinned.safe_read pin_f in
  let installed_version =
    try
      Some (OpamPackage.version
              (OpamState.find_installed_package_by_name t name))
    with Not_found -> None in

  let _check = match pin_option with
    | Version v ->
      if not (OpamPackage.Set.mem (OpamPackage.create name v) t.packages) then
        OpamConsole.error_and_exit "Package %s has no version %s"
          (OpamPackage.Name.to_string name) (OpamPackage.Version.to_string v);
      if version <> None && version <> Some v then
        OpamConsole.error_and_exit "Inconsistent version request for %s"
          (OpamPackage.Name.to_string name);
    | _ -> ()
  in

  let no_changes =
    try
      let current = OpamPackage.Name.Map.find name pins in
      let no_changes = pin_option = current in
      if no_changes then
        OpamConsole.note
          "Package %s is already %s-pinned to %s.\n\
           This will erase any previous custom definition."
          (OpamPackage.Name.to_string name)
          (string_of_pin_kind (kind_of_pin_option current))
          (string_of_pin_option current)
      else
        OpamConsole.note
          "%s is currently %s-pinned to %s."
          (OpamPackage.Name.to_string name)
          (string_of_pin_kind (kind_of_pin_option current))
          (string_of_pin_option current);
      if OpamConsole.confirm "Proceed ?" then
        (OpamFilename.remove
           (OpamPath.Switch.Overlay.tmp_opam t.root t.switch name);
         no_changes)
      else OpamStd.Sys.exit 0
    with Not_found ->
      if OpamPackage.Name.Set.mem name (OpamState.base_package_names t) then (
        OpamConsole.warning
          "Package %s is part of the base packages of this compiler."
          (OpamPackage.Name.to_string name);
        if not @@ OpamConsole.confirm
            "Are you sure you want to override this and pin it anyway ?"
        then OpamStd.Sys.exit 0);
      false
  in
  let pins = OpamPackage.Name.Map.remove name pins in
  if OpamPackage.Set.is_empty (OpamState.find_packages_by_name t name) &&
     not (OpamConsole.confirm
            "Package %s does not exist, create as a %s package ?"
            (OpamPackage.Name.to_string name)
            (OpamConsole.colorise `bold "NEW"))
  then
    (OpamConsole.msg "Aborting.\n";
     OpamStd.Sys.exit 0);

  log "Adding %a => %a"
    (slog string_of_pin_option) pin_option
    (slog OpamPackage.Name.to_string) name;

  let pinned = OpamPackage.Name.Map.add name pin_option pins in
  update_config t name pinned;
  let t = { t with pinned } in
  OpamState.add_pinned_overlay t ?version name;

  if not no_changes then
    OpamConsole.msg "%s is now %a-pinned to %s\n"
      (OpamPackage.Name.to_string name)
      (OpamConsole.acolor `bold)
      (string_of_pin_kind pin_kind)
      (string_of_pin_option pin_option);

  (match pin_option with
   | Source ({ OpamUrl.backend = #OpamUrl.version_control;
               transport = "file";
               hash = None;
               path = _; } as url) ->
     (match OpamUrl.local_dir url with
      | Some dir ->
        OpamConsole.note
          "Pinning in mixed mode: OPAM will use tracked files in the current \
           working tree from %s. If this is not what you want, pin to a given \
           branch (e.g. %s#HEAD)"
          (OpamFilename.Dir.to_string dir) (OpamUrl.to_string url)
      | None -> ())
   | _ -> ());

  if not no_changes && installed_version <> None then
    let nv_v = OpamState.pinned t name in
    let pin_version = OpamPackage.version nv_v in
    if installed_version = Some pin_version then
      if pin_kind = `version then None
      else Some true
    else Some false
  else None

let unpin ?state names =
  log "unpin %a"
    (slog @@ OpamStd.List.concat_map " " OpamPackage.Name.to_string) names;
  let t = match state with
    | None -> OpamState.load_state "pin" OpamStateConfig.(!r.current_switch)
    | Some t -> t
  in
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  let pins, needs_reinstall =
    List.fold_left (fun (pins, needs_reinstall) name ->
        try
          let current = OpamPackage.Name.Map.find name pins in
          let is_installed = OpamState.is_name_installed t name in
          let pins = OpamPackage.Name.Map.remove name pins in
          let needs_reinstall = match current with
            | Version _ -> needs_reinstall
            | _ when is_installed -> name::needs_reinstall
            | _ -> needs_reinstall
          in
          if not is_installed then
            OpamState.remove_overlay t name;
          OpamConsole.msg "%s is now %a from %s %s\n"
            (OpamPackage.Name.to_string name)
            (OpamConsole.acolor `bold) "unpinned"
            (string_of_pin_kind (kind_of_pin_option current))
            (string_of_pin_option current);
          pins, needs_reinstall
        with Not_found ->
          OpamConsole.note "%s is not pinned."
            (OpamPackage.Name.to_string name);
          pins, needs_reinstall)
      (OpamFile.Pinned.safe_read pin_f, [])
      names
  in
  OpamFile.Pinned.write pin_f pins;
  List.iter (cleanup_dev_dirs t) names;
  needs_reinstall

let list ~short () =
  log "pin_list";
  let t = OpamState.load_state "pin-list"
      OpamStateConfig.(!r.current_switch) in
  let pins =
    OpamFile.Pinned.safe_read (OpamPath.Switch.pinned t.root t.switch)
  in
  if short then
    OpamPackage.Name.Map.iter
      (fun n _ -> OpamConsole.msg "%s\n" (OpamPackage.Name.to_string n))
      pins
  else
  let lines (n,a) =
    let kind = string_of_pin_kind (kind_of_pin_option a) in
    let state, extra =
      try
        let nv = OpamState.find_installed_package_by_name t n in
        match OpamState.pinned_opt t n with
        | None -> OpamConsole.colorise `red " (invalid)",[]
        | Some nvp when nvp = nv -> "",[]
        | _ -> OpamConsole.colorise `red " (not in sync)",
               [Printf.sprintf " (installed:%s)"
                  (OpamPackage.version_to_string nv)]
      with Not_found -> OpamConsole.colorise `yellow " (uninstalled)", []
    in
    [ OpamPackage.to_string (OpamState.pinned t n);
      state;
      OpamConsole.colorise `blue kind;
      string_of_pin_option a ]
    @ extra
  in
  let table = List.map lines (OpamPackage.Name.Map.bindings pins) in
  OpamStd.Format.print_table stdout ~sep:"  " (OpamStd.Format.align_table table)

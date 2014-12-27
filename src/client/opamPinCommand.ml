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
open OpamTypesBase
open OpamState.Types
open OpamMisc.OP

let log fmt = OpamGlobals.log "COMMAND" fmt
let slog = OpamGlobals.slog

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
      OpamGlobals.error_and_exit "%s is not pinned."
        (OpamPackage.Name.to_string name)
  in
  let installed_nv =
    try Some (OpamState.find_installed_package_by_name t name)
    with Not_found -> None
  in
  let file = OpamPath.Switch.Overlay.opam t.root t.switch name in
  let temp_file = OpamPath.Switch.Overlay.tmp_opam t.root t.switch name in
  let orig_opam =
    try Some (OpamFile.OPAM.read file) with e -> OpamMisc.fatal e; None
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
           (Lazy.force OpamGlobals.editor) (OpamFilename.to_string temp_file));
      let opam = OpamFile.OPAM.read temp_file in
      if OpamFile.OPAM.name_opt opam <> Some name then
        (OpamGlobals.error
           "Inconsistent or missing \"name\" field, it must be %S"
           (OpamPackage.Name.to_string name);
         failwith "bad name");
      match pin, OpamFile.OPAM.version_opt opam with
      | Version vpin, Some v when v <> vpin ->
        OpamGlobals.error
          "Bad \"version: %S\" field: the package is version-pinned to %s"
          (OpamPackage.Version.to_string v)
          (OpamPackage.Version.to_string vpin);
        failwith "bad version"
      | _, None ->
        OpamGlobals.error "The \"version\" field is mandatory";
        failwith "missing field"
      | _ ->
        match OpamFile.OPAM.validate opam with
        | [] -> Some opam
        | warns ->
          OpamGlobals.warning "The opam file didn't pass validation:\n  - %s\n"
            (String.concat "\n  - " warns);
          if OpamGlobals.confirm "Continue anyway ('no' will reedit) ?"
          then Some opam
          else edit ()
    with e ->
      OpamMisc.fatal e;
      if OpamGlobals.confirm "Errors in %s, retry editing ?"
          (OpamFilename.to_string file)
      then edit ()
      else None
  in
  match edit () with
  | None -> if empty_opam then raise Not_found else None
  | Some new_opam ->
    OpamFilename.move ~src:temp_file ~dst:file;
    OpamGlobals.msg "You can edit this file again with \"opam pin edit %s\"\n"
      (OpamPackage.Name.to_string name);
    if Some new_opam = orig_opam then None else
    let () =
      let dir = match pin with
        | Local dir -> Some dir
        | Git (a,None) | Darcs (a,None) | Hg (a,None) ->
          Some (OpamFilename.Dir.of_string a)
        | Version _ | Git _ | Darcs _ | Hg _ | Http _ ->
          None
      in
      match dir with
      | Some dir when OpamFilename.exists_dir dir ->
        let src_opam =
          OpamFilename.OP.(
            if OpamFilename.exists_dir (dir / "opam")
            then dir / "opam" // "opam"
            else dir // "opam")
        in
        if OpamGlobals.confirm "Save the new opam file back to %S ?"
            (OpamFilename.to_string src_opam) then
          OpamFilename.copy ~src:file ~dst:src_opam
      | _ -> ()
    in
    match installed_nv with
    | None -> None
    | Some nv -> Some (OpamPackage.version nv = OpamFile.OPAM.version new_opam)

let update_set set old cur save =
  if OpamPackage.Set.mem old set then
    save (OpamPackage.Set.add cur (OpamPackage.Set.remove old set))

let update_config t name pins =
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  cleanup_dev_dirs t name;
  OpamFile.Pinned.write pin_f pins

let pin name ?version pin_option =
  log "pin %a to %a"
    (slog OpamPackage.Name.to_string) name
    (slog string_of_pin_option) pin_option;
  let t = OpamState.load_state "pin" in
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
        OpamGlobals.error_and_exit "Package %s has no version %s"
          (OpamPackage.Name.to_string name) (OpamPackage.Version.to_string v);
      if version <> None && version <> Some v then
        OpamGlobals.error_and_exit "Inconsistent version request for %s"
          (OpamPackage.Name.to_string name);
    | _ -> ()
  in

  let no_changes =
    try
      let current = OpamPackage.Name.Map.find name pins in
      let no_changes = pin_option = current in
      if no_changes then
        OpamGlobals.note
          "Package %s is already pinned to %s.\n\
           This will erase any previous custom definition."
          (OpamPackage.Name.to_string name)
          (string_of_pin_option current)
      else
        OpamGlobals.note
          "%s is already pinned to %s."
          (OpamPackage.Name.to_string name)
          (string_of_pin_option current);
      if OpamGlobals.confirm "Proceed ?" then
        (OpamFilename.remove
           (OpamPath.Switch.Overlay.tmp_opam t.root t.switch name);
         no_changes)
      else OpamGlobals.exit 0
    with Not_found -> false
  in
  let pins = OpamPackage.Name.Map.remove name pins in
  if OpamState.find_packages_by_name t name = None &&
     not (OpamGlobals.confirm
            "Package %s does not exist, create as a %s package ?"
            (OpamPackage.Name.to_string name)
            (OpamGlobals.colorise `bold "NEW"))
  then
    (OpamGlobals.msg "Aborting.\n";
     OpamGlobals.exit 0);

  log "Adding %a => %a"
    (slog string_of_pin_option) pin_option
    (slog OpamPackage.Name.to_string) name;

  let pinned = OpamPackage.Name.Map.add name pin_option pins in
  update_config t name pinned;
  let t = { t with pinned } in
  OpamState.add_pinned_overlay t ?version name;

  if not no_changes then
    OpamGlobals.msg "%s is now %a-pinned to %s\n"
      (OpamPackage.Name.to_string name)
      (OpamGlobals.acolor `bold)
      (string_of_pin_kind pin_kind)
      (string_of_pin_option pin_option);

  (match pin_option with
   | Git (dir, None) | Hg (dir, None) | Darcs (dir, None)
     when OpamFilename.exists_dir (OpamFilename.Dir.of_string dir) ->
     OpamGlobals.note
       "Pinning in mixed mode: OPAM will use tracked files in the current \
        working\ntree from %s. If this is not what you want, pin to a given \
        branch (e.g.\n%s#HEAD)"
       dir dir
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
    (slog @@ String.concat " " @* List.map OpamPackage.Name.to_string) names;
  let t = match state with
    | None -> OpamState.load_state "pin"
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
          OpamGlobals.msg "%s is now %a from %s %s\n"
            (OpamPackage.Name.to_string name)
            (OpamGlobals.acolor `bold) "unpinned"
            (string_of_pin_kind (kind_of_pin_option current))
            (string_of_pin_option current);
          pins, needs_reinstall
        with Not_found ->
          OpamGlobals.note "%s is not pinned."
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
  let t = OpamState.load_state "pin-list" in
  let pins = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned t.root t.switch) in
  let print_short n _ =
    OpamGlobals.msg "%s\n" (OpamPackage.Name.to_string n)
  in
  let print n a =
    let kind = string_of_pin_kind (kind_of_pin_option a) in
    OpamGlobals.msg "%-25s %s %s\n"
      (OpamPackage.to_string (OpamState.pinned t n))
      (OpamGlobals.colorise `blue (Printf.sprintf "%-8s" kind))
      (string_of_pin_option a) in
  OpamPackage.Name.Map.iter (if short then print_short else print) pins

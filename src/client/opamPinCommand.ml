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
  if not (OpamFilename.exists file) then
    OpamState.add_pinned_overlay ~template:true t name;
  let backup =
    OpamFilename.OP.(OpamPath.backup_dir t.root //
                     (OpamPackage.Name.to_string name ^ "-opam.bak")) in
  OpamFilename.copy ~src:file ~dst:backup;
  let orig_opam = OpamFile.OPAM.read file in
  let rec edit () =
    try
      ignore @@ Sys.command
        (Printf.sprintf "%s %s"
           (Lazy.force OpamGlobals.editor) (OpamFilename.to_string file));
      let opam = OpamFile.OPAM.read file in
      let opam =
        if OpamFile.OPAM.name_opt opam = None then
          OpamFile.OPAM.with_name opam name
        else opam in
      if OpamFile.OPAM.version_opt opam = None then
        (OpamGlobals.error "The \"version\" field is mandatory";
         failwith "missing field");
      if OpamFile.OPAM.name_opt opam <> Some name then
        (OpamGlobals.error "Inconsistent \"name\" field, it must be %s"
           (OpamPackage.Name.to_string name);
         failwith "bad name");
      opam
    with e ->
      (try OpamMisc.fatal e with e ->
        OpamFilename.move ~src:backup ~dst:file;
        raise e);
      if OpamState.confirm "Errors in %s, retry editing ?"
          (OpamFilename.to_string file)
      then edit ()
      else (OpamFilename.move ~src:backup ~dst:file;
            OpamFile.OPAM.read file)
  in
  let opam = edit () in
  OpamFilename.remove backup;
  if opam = orig_opam then None else
  let () = match pin with
    | Local dir ->
      let src_opam =
        OpamFilename.OP.(
          if OpamFilename.exists_dir (dir / "opam") then dir / "opam" // "opam"
          else dir // "opam")
       in
       if OpamState.confirm "Save the new opam file back to %S ?"
           (OpamFilename.to_string src_opam) then
         OpamFilename.copy ~src:file ~dst:src_opam
    | Version _ | Git _ | Darcs _ | Hg _ -> ()
  in
  match installed_nv with
  | None -> None
  | Some nv -> Some (OpamPackage.version nv = OpamFile.OPAM.version opam)

let update_set set old cur save =
  if OpamPackage.Set.mem old set then
    save (OpamPackage.Set.add cur (OpamPackage.Set.remove old set))

let update_config t name pins =
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  cleanup_dev_dirs t name;
  OpamFile.Pinned.write pin_f pins

let pin name pin_option =
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
          (OpamPackage.Name.to_string name) (OpamPackage.Version.to_string v)
    | _ -> ()
  in

  let no_changes =
    try
      let current = OpamPackage.Name.Map.find name pins in
      let no_changes = pin_option = current in
      if no_changes then
        OpamGlobals.note
          "Package %s is already pinned to %s. This will reset metadata."
          (OpamPackage.Name.to_string name)
          (string_of_pin_option current)
      else
        OpamGlobals.note
          "%s is already pinned to %s."
          (OpamPackage.Name.to_string name)
          (string_of_pin_option current);
      if OpamState.confirm "Proceed ?" then no_changes
      else OpamGlobals.exit 0
    with Not_found -> false
  in
  let pins = OpamPackage.Name.Map.remove name pins in
  if OpamState.find_packages_by_name t name = None &&
     not (OpamState.confirm
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
  OpamState.add_pinned_overlay t name;

  if not no_changes then
    OpamGlobals.msg "%s is now %a-pinned to %s\n"
      (OpamPackage.Name.to_string name)
      (OpamGlobals.acolor `bold)
      (string_of_pin_kind pin_kind)
      (string_of_pin_option pin_option);

  if not no_changes && installed_version <> None then
    let nv_v = OpamState.pinned t name in
    let pin_version = OpamPackage.version nv_v in
    if installed_version = Some pin_version then
      if pin_kind = `version then None
      else Some true
    else Some false
  else None

let unpin name =
  log "unpin %a" (slog OpamPackage.Name.to_string) name;
  let t = OpamState.load_state "pin" in
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  let pins = OpamFile.Pinned.safe_read pin_f in

  try
    let current = OpamPackage.Name.Map.find name pins in
    let needs_reinstall = match current with
      | Version _ -> false
      | _ -> OpamState.is_name_installed t name
    in
    OpamState.remove_overlay t name;
    let pins = OpamPackage.Name.Map.remove name pins in
    update_config t name pins;

    OpamGlobals.msg "%s is now %a from %s\n"
      (OpamPackage.Name.to_string name)
      (OpamGlobals.acolor `bold) "unpinned"
      (string_of_pin_option current);

    needs_reinstall

  with Not_found ->
    OpamGlobals.note "%s is not pinned." (OpamPackage.Name.to_string name);
    false


let list () =
  log "pin_list";
  let t = OpamState.load_state "pin-list" in
  let pins = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned t.root t.switch) in
  let print n a =
    let kind = string_of_pin_kind (kind_of_pin_option a) in
    OpamGlobals.msg "%-25s %s %s\n"
      (OpamPackage.to_string (OpamState.pinned t n))
      (OpamGlobals.colorise `blue (Printf.sprintf "%-8s" kind))
      (string_of_pin_option a) in
  OpamPackage.Name.Map.iter print pins

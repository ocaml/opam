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

let log fmt = OpamGlobals.log "COMMAND" fmt
let slog = OpamGlobals.slog

let edit_t t name =
  log "pin-edit %a" (slog OpamPackage.Name.to_string) name;
  if not (OpamState.is_pinned t name) then
    OpamGlobals.error_and_exit "%s is not pinned."
      (OpamPackage.Name.to_string name);
  let nv = OpamPackage.pinned name in
  let file = OpamPath.Switch.Overlay.opam t.root t.switch nv in
  if not (OpamFilename.exists file) then OpamState.add_pinned_overlay t name;
  ignore
    (Sys.command
       (Printf.sprintf "%s %s"
          (Lazy.force OpamGlobals.editor) (OpamFilename.to_string file)));
  OpamState.is_name_installed t name

let edit name =
  let t = OpamState.load_state "pin" in
  edit_t t name

let update_set set old cur save =
  if OpamPackage.Set.mem old set then
    save (OpamPackage.Set.add cur (OpamPackage.Set.remove old set))

let update_config t name pins =
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  let packages = OpamPackage.packages_of_name t.packages name in
  OpamPackage.Set.iter (fun nv ->
      OpamFilename.rmdir (OpamPath.Switch.build t.root t.switch nv);
      OpamFilename.rmdir (OpamPath.Switch.dev_package t.root t.switch nv);
    ) packages;
  OpamFile.Pinned.write pin_f pins


let pin name ?(edit=false) pin_option =
  log "pin %a to %a"
    (slog OpamPackage.Name.to_string) name
    (slog string_of_pin_option) pin_option;
  let t = OpamState.load_state "pin" in
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  let pins = OpamFile.Pinned.safe_read pin_f in

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
          "%s was pinned to %s."
          (OpamPackage.Name.to_string name)
          (string_of_pin_option current);
      no_changes
    with Not_found -> false
  in
  let pins = OpamPackage.Name.Map.remove name pins in

  let needs_reinstall =
    not no_changes &&
    match pin_option with
    | Git _ | Darcs _ | Local _ | Hg _ ->
      OpamState.is_name_installed t name
    | Version version ->
      try
        let nv = OpamState.find_installed_package_by_name t name in
        OpamPackage.version nv <> version
      with Not_found -> false
  in

  if OpamState.find_packages_by_name t name = None then
    OpamGlobals.error_and_exit
      "%s is not a valid package name."
      (OpamPackage.Name.to_string name);

  log "Adding %a => %a"
    (slog string_of_pin_option) pin_option
    (slog OpamPackage.Name.to_string) name;

  let pinned = OpamPackage.Name.Map.add name pin_option pins in
  update_config t name pinned;
  let t = { t with pinned } in
  OpamState.add_pinned_overlay t name;

  (* In case the package is installed *)
  let nv_pin = OpamPackage.pinned name in
  let nv_v = OpamState.pinning_version t nv_pin in
  update_set t.installed nv_v nv_pin
    (OpamFile.Installed.write
       (OpamPath.Switch.installed t.root t.switch));
  update_set t.installed_roots nv_v nv_pin
    (OpamFile.Installed_roots.write
       (OpamPath.Switch.installed_roots t.root t.switch));

  if not no_changes then
    OpamGlobals.msg "%s is now %a-pinned to %s\n"
      (OpamPackage.Name.to_string name)
      (OpamGlobals.acolor `bold)
      (string_of_pin_kind (kind_of_pin_option pin_option))
      (string_of_pin_option pin_option);

  if edit then edit_t t name || needs_reinstall
  else needs_reinstall

let unpin name =
  log "unpin %a" (slog OpamPackage.Name.to_string) name;
  let t = OpamState.load_state "pin" in
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  let pins = OpamFile.Pinned.safe_read pin_f in

  if not (OpamPackage.Name.Map.mem name pins) then
    (OpamGlobals.note "%s is not pinned." (OpamPackage.Name.to_string name);
     false)
  else
  let needs_reinstall =
    match OpamPackage.Name.Map.find name pins with
    | Version _ -> false
    | _ -> OpamState.is_name_installed t name
  in
  let nv_pin = OpamPackage.pinned name in
  let nv_v = OpamState.pinning_version t nv_pin in
  update_set t.installed nv_pin nv_v
    (OpamFile.Installed.write
       (OpamPath.Switch.installed t.root t.switch));
  update_set t.installed_roots nv_pin nv_v
    (OpamFile.Installed_roots.write
       (OpamPath.Switch.installed_roots t.root t.switch));
  update_config t name (OpamPackage.Name.Map.remove name pins);
  OpamState.remove_overlay t nv_pin;

  OpamGlobals.msg "%s is now unpinned\n"
    (OpamPackage.Name.to_string name);

  needs_reinstall

let list () =
  log "pin_list";
  let t = OpamState.load_state "pin-list" in
  let pins = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned t.root t.switch) in
  let print n a =
    let kind = string_of_pin_kind (kind_of_pin_option a) in
    OpamGlobals.msg "%-20s %-8s %s\n"
      (OpamPackage.Name.to_string n)
      kind
      (string_of_pin_option a) in
  OpamPackage.Name.Map.iter print pins

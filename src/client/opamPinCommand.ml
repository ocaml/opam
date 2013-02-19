(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012-2013 OCamlPro                                     *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open OpamTypes
open OpamState.Types

let log fmt = OpamGlobals.log "COMMAND" fmt

let pin ~force action =
  log "pin %s" (string_of_pin action);
  let t = OpamState.load_state "pin" in
  let pin_f = OpamPath.Switch.pinned t.root t.switch in
  let pins = OpamFile.Pinned.safe_read pin_f in
  let name = action.pin_package in
  let update_config pins =
    let packages = OpamPackage.packages_of_name t.packages name in
    OpamPackage.Set.iter (fun nv ->
      OpamFilename.rmdir (OpamPath.Switch.build t.root t.switch nv);
      OpamFilename.rmdir (OpamPath.Switch.pinned_dir t.root t.switch (OpamPackage.name nv));
    ) packages;
    if force then OpamState.add_to_reinstall t ~all:false packages;
    OpamFile.Pinned.write pin_f pins in

  match action.pin_option with
  | Unpin ->
    if not (OpamPackage.Name.Map.mem name pins) then
      OpamGlobals.error_and_exit "%s is not pinned." (OpamPackage.Name.to_string name);
    begin match OpamPackage.Name.Map.find name pins with
      | Version _ -> ()
      | _         ->
        if not force && OpamState.mem_installed_package_by_name t name then
          OpamGlobals.error_and_exit "You must uninstall the package before unpinning it (or use --force).";
    end;
    update_config (OpamPackage.Name.Map.remove name pins);
  | _     ->
    if not force && OpamPackage.Name.Map.mem name pins then (
      let current = OpamPackage.Name.Map.find name pins in
      OpamGlobals.error_and_exit "Cannot pin %s to %s as it is already associated to %s. Use 'opam pin %s none' and retry (or use --force)."
        (OpamPackage.Name.to_string name)
        (path_of_pin_option action.pin_option)
        (path_of_pin_option current)
        (OpamPackage.Name.to_string name);
    );
    let pins = OpamPackage.Name.Map.remove name pins in

    begin match action.pin_option with
      | Unpin           -> ()
      | Version version ->
        if not force && not (OpamState.mem_installed_package_by_name t name) then
          OpamGlobals.error_and_exit
            "Cannot pin %s to %s, you must install the package first (or use --force)."
            (OpamPackage.Name.to_string name)
            (OpamPackage.Version.to_string version);
        if OpamState.mem_installed_package_by_name t name then
          let nv = OpamState.find_installed_package_by_name t name in
          if not force && OpamPackage.version nv <> version then
            OpamGlobals.error_and_exit
              "Cannot pin %s as its current version is %s. You must install the version %s first (or use --force)."
              (OpamPackage.Name.to_string name)
              (OpamPackage.Version.to_string (OpamPackage.version nv))
              (OpamPackage.Version.to_string version);
      | Git _ | Darcs _ | Local _ ->
        if not force && OpamState.mem_installed_package_by_name t name then
          OpamGlobals.error_and_exit
            "Cannot pin %s to a dev version as it is already installed. You must uninstall it first (or use --force)."
            (OpamPackage.Name.to_string name);
    end;

    match OpamState.find_packages_by_name t name with
    | None   ->
      OpamGlobals.error_and_exit
        "%s is not a valid package name."
        (OpamPackage.Name.to_string name)
    | Some _ ->
      log "Adding %s(%s) => %s"
        (path_of_pin_option action.pin_option)
        (string_of_pin_kind (kind_of_pin_option action.pin_option))
        (OpamPackage.Name.to_string name);
      update_config (OpamPackage.Name.Map.add name action.pin_option pins)

let list () =
  log "pin_list";
  let t = OpamState.load_state "pin-list" in
  let pins = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned t.root t.switch) in
  let print n a =
    OpamGlobals.msg "%-20s %-8s %s\n"
      (OpamPackage.Name.to_string n)
      (string_of_pin_kind (kind_of_pin_option a))
      (path_of_pin_option a) in
  OpamPackage.Name.Map.iter print pins

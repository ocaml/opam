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
open OpamState.Types

let log fmt = OpamGlobals.log "COMMAND" fmt

let string_of_pin_kind_o o =
  match kind_of_pin_option o with
  | Some k -> string_of_pin_kind k
  | None   -> "none"

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
      OpamFilename.rmdir (OpamPath.Switch.dev_package t.root t.switch nv);
    ) packages;
    if force then OpamState.add_to_reinstall t ~all:false packages;
    OpamFile.Pinned.write pin_f pins in

  match action.pin_option with
  | Edit  ->
    if not (OpamState.is_locally_pinned t name) then
      OpamGlobals.error_and_exit "%s is not locally pinned."
        (OpamPackage.Name.to_string name);
    let editor =
      try OpamMisc.getenv "OPAM_EDITOR"
      with Not_found ->
        try OpamMisc.getenv "VISUAL"
        with Not_found ->
          try OpamMisc.getenv "EDITOR"
          with Not_found -> "nano" in
    let nv = OpamPackage.pinned name in
    let file = OpamPath.Switch.opam t.root t.switch nv in
    if not (OpamFilename.exists file) then OpamState.add_pinned_overlay t name;
    ignore (Sys.command (Printf.sprintf "%s %s" editor (OpamFilename.to_string file)))

  | Unpin ->
    if not (OpamPackage.Name.Map.mem name pins) then
      OpamGlobals.error_and_exit "%s is not pinned." (OpamPackage.Name.to_string name);
    begin match OpamPackage.Name.Map.find name pins with
      | Version _ -> ()
      | _         ->
        if not force && OpamState.is_name_installed t name then
          OpamGlobals.error_and_exit
            "You must remove the package before unpinning it (or use --force).";
    end;
    update_config (OpamPackage.Name.Map.remove name pins);
    let nv = OpamPackage.pinned name in
    OpamState.remove_overlay t nv

  | _     ->
    if not force && OpamPackage.Name.Map.mem name pins then (
      let current = OpamPackage.Name.Map.find name pins in
      OpamGlobals.error_and_exit
        "Cannot pin %s to %s as it is already associated to %s. Use 'opam pin %s \
         none' and retry (or use --force)."
        (OpamPackage.Name.to_string name)
        (string_of_pin_option action.pin_option)
        (string_of_pin_option current)
        (OpamPackage.Name.to_string name);
    );
    let pins = OpamPackage.Name.Map.remove name pins in

    begin match action.pin_option with
      | Edit            -> ()
      | Unpin           -> ()
      | Version version ->
        if not force && not (OpamState.is_name_installed t name) then
          OpamGlobals.error_and_exit
            "Cannot pin %s to %s, you must install the package first (or use --force)."
            (OpamPackage.Name.to_string name)
            (OpamPackage.Version.to_string version);
        if OpamState.is_name_installed t name then
          let nv = OpamState.find_installed_package_by_name t name in
          if not force && OpamPackage.version nv <> version then
            OpamGlobals.error_and_exit
              "Cannot pin %s as its current version is %s. You must install the \
               version %s first (or use --force)."
              (OpamPackage.Name.to_string name)
              (OpamPackage.Version.to_string (OpamPackage.version nv))
              (OpamPackage.Version.to_string version);
      | Git _ | Darcs _ | Local _ | Hg _ ->
        if not force && OpamState.is_name_installed t name then
          OpamGlobals.error_and_exit
            "Cannot pin %s to a dev version as it is already installed. You must \
             remove it first (or use --force)."
            (OpamPackage.Name.to_string name);
    end;

    match OpamState.find_packages_by_name t name with
    | None   ->
      OpamGlobals.error_and_exit
        "%s is not a valid package name."
        (OpamPackage.Name.to_string name)
    | Some _ ->
      log "Adding %s(%s) => %s"
        (string_of_pin_option action.pin_option)
        (string_of_pin_kind_o action.pin_option)
        (OpamPackage.Name.to_string name);
      let pinned = OpamPackage.Name.Map.add name action.pin_option pins in
      update_config pinned;
      let t = { t with pinned } in
      OpamState.add_pinned_overlay t name

let list () =
  log "pin_list";
  let t = OpamState.load_state "pin-list" in
  let pins = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned t.root t.switch) in
  let print n a =
    let kind = match kind_of_pin_option a with
      | None   -> ""
      | Some k -> string_of_pin_kind k in
    OpamGlobals.msg "%-20s %-8s %s\n"
      (OpamPackage.Name.to_string n)
      kind
      (string_of_pin_option a) in
  OpamPackage.Name.Map.iter print pins

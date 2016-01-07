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
open OpamStd.Op
open OpamFilename.Op

open OpamStateTypes

let log fmt = OpamConsole.log "GSTATE" fmt

module Format_upgrade = struct

  let v1_1 = OpamVersion.of_string "1.1"

  let from_1_0_to_1_1 root =
    OpamConsole.error_and_exit
      "You appear to have an opam setup dating back to opam 1.0, which is no \
       longer supported since opam 1.3. Please remove \"%s\" and run \
       `opam init`"
      (OpamFilename.Dir.to_string root)

  let v1_2 = OpamVersion.of_string "1.2"

  let from_1_1_to_1_2 root =
    log "Upgrade pinned packages format to 1.2";
    let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
    let remove_pinned_suffix d =
      let s = OpamFilename.Dir.to_string d in
      if Filename.check_suffix s ".pinned" then
        OpamFilename.move_dir ~src:d
          ~dst:(OpamFilename.Dir.of_string (Filename.chop_suffix s ".pinned"))
    in
    let packages = lazy (
      OpamPackage.Set.of_list
        (OpamPackage.Map.keys
           (OpamFile.Package_index.safe_read (OpamPath.package_index root)))
    ) in
    OpamSwitch.Map.iter (fun switch _ ->
        let pinned_version name =
          try
            let f = OpamPath.Switch.Overlay.opam root switch name in
            match OpamFile.OPAM.version_opt (OpamFile.OPAM.read f) with
            | None -> raise Not_found
            | Some v -> v
          with e ->
            OpamStd.Exn.fatal e;
            try OpamPackage.version
                  (OpamPackage.max_version (Lazy.force packages) name)
            with Not_found -> OpamPackage.Version.of_string "0" in
        let fix_version nv =
          let obsolete_pinned_v = OpamPackage.Version.of_string "pinned" in
          if OpamPackage.version nv = obsolete_pinned_v then
            let name = OpamPackage.name nv in
            OpamPackage.create name (pinned_version name)
          else nv in
        List.iter remove_pinned_suffix
          (OpamFilename.dirs (OpamPath.Switch.dev_packages_dir root switch));
        List.iter remove_pinned_suffix
          (OpamFilename.dirs (OpamPath.Switch.Overlay.dir root switch));
        let switch_prefix = OpamPath.Switch.root root switch in
        let installed_f = OpamFilename.Op.(switch_prefix // "installed") in
        let installed = OpamFile.PkgList.safe_read installed_f in
        OpamFile.PkgList.write installed_f
          (OpamPackage.Set.map fix_version installed);
        let installed_roots_f =
          OpamFilename.Op.(switch_prefix // "installed.roots")
        in
        let installed_roots = OpamFile.PkgList.safe_read installed_roots_f in
        OpamFile.PkgList.write installed_roots_f
          (OpamPackage.Set.map fix_version installed_roots);
        (* Move .config files *)
        List.iter (fun f ->
            let name =
              OpamFilename.Base.to_string @@
              OpamFilename.basename @@
              OpamFilename.chop_extension f in
            if name <> "global-config" then
              let dst =
                OpamPath.Switch.Default.lib root switch
                  (OpamPackage.Name.of_string name)
                // "opam.config"
              in
              OpamFilename.mkdir (OpamFilename.dirname dst);
              OpamFilename.move ~src:f ~dst
          )
          (OpamFilename.files (OpamPath.Switch.config_dir root switch))
      ) aliases

  let v1_3_dev2 = OpamVersion.of_string "1.3~dev2"

  let from_1_2_to_1_3_dev2 root =
    log "Upgrade switch state files format to 1.3";
    let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
    OpamSwitch.Map.iter (fun switch c ->
        let switch_dir = OpamPath.Switch.root root switch in
        let open OpamFilename.Op in
        let installed_f = switch_dir // "installed" in
        let installed_roots_f = switch_dir // "installed.roots" in
        let pinned_f = switch_dir // "pinned" in
        let installed = OpamFile.PkgList.safe_read installed_f in
        let installed_roots = OpamFile.PkgList.safe_read installed_roots_f in
        let pinned = OpamFile.Pinned_legacy.safe_read pinned_f in
        let compiler =
          let comp = OpamFile.Comp.read (OpamPath.compiler_comp root c) in
          let atoms = OpamFormula.atoms (OpamFile.Comp.packages comp) in
          List.fold_left (fun acc (name,_) ->
              let nv =
                try
                  match OpamPackage.Name.Map.find name pinned with
                  | Version v -> OpamPackage.create name v
                  | Source _ ->
                    let overlay = OpamPath.Switch.Overlay.opam root switch name in
                    let opam = OpamFile.OPAM.read overlay in
                    match OpamFile.OPAM.version_opt opam with
                    | Some v -> OpamPackage.create name v
                    | None -> raise Not_found
                with Not_found ->
                try OpamPackage.max_version installed name with Not_found ->
                  OpamPackage.create name
                    (OpamPackage.Version.of_string "~unknown")
              in
              OpamPackage.Set.add nv acc)
            OpamPackage.Set.empty atoms
        in
        OpamFile.State.write
          (OpamPath.Switch.state root switch)
          { OpamFile.State.
            installed; installed_roots; pinned; compiler };
        OpamFilename.remove installed_f;
        OpamFilename.remove installed_roots_f;
        OpamFilename.remove pinned_f;
        (* Move .config files back *)
        OpamPackage.Set.iter (fun nv ->
            let name = OpamPackage.name nv in
            let src =
              OpamPath.Switch.Default.lib root switch name // "opam.config"
            in
            let dst = OpamPath.Switch.config root switch name in
            if OpamFilename.exists src then
              OpamFilename.move ~src ~dst)
          installed)
      aliases

  let as_necessary root config =
    let config_version = OpamFile.Config.opam_version config in
    let cmp = OpamVersion.(compare current_nopatch config_version) in
    if cmp = 0 then config
    else if cmp < 0 then
      if OpamFormatConfig.(!r.skip_version_checks) then config else
        OpamConsole.error_and_exit
          "%s reports a newer OPAM version, aborting."
          (OpamFilename.Dir.to_string (OpamStateConfig.(!r.root_dir)))
    else
    if OpamVersion.compare config_version v1_3_dev2 < 0 then
      let config = OpamFile.Config.with_opam_version config v1_3_dev2 in
      if OpamVersion.compare config_version v1_1 < 0 then from_1_0_to_1_1 root;
      if OpamVersion.compare config_version v1_2 < 0 then from_1_1_to_1_2 root;
      from_1_2_to_1_3_dev2 root;
      OpamStateConfig.write root config;
      config
    else
      config;
end

let load_config root =
  let config = match OpamStateConfig.load root with
    | Some c -> c
    | None ->
      if OpamFilename.exists_dir (root / "opam") then
        OpamFile.Config.(with_opam_version empty (OpamVersion.of_string "1.1"))
      else OpamFile.Config.empty
  in
  Format_upgrade.as_necessary root config

let load ?(lock=Lock_readonly) () =
  log "LOAD-GLOBAL-STATE";
  let root = OpamStateConfig.(!r.root_dir) in
  let config = load_config root in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  { global_lock=lock; root; config; aliases }

let fold_switches f gt acc =
  OpamSwitch.Map.fold (fun switch _ acc ->
      f switch
        (OpamFile.State.safe_read (OpamPath.Switch.state gt.root switch))
        acc
    ) gt.aliases acc

let all_installed gt =
  fold_switches (fun _ state acc ->
      OpamPackage.Set.union acc state.OpamFile.State.installed)
    gt  OpamPackage.Set.empty

let installed_versions gt name =
  fold_switches (fun switch state acc ->
      let installed =
        OpamPackage.packages_of_name state.OpamFile.State.installed name
      in
      try
        let nv = OpamPackage.Set.choose installed in
        try OpamPackage.Map.add nv (switch::OpamPackage.Map.find nv acc) acc
        with Not_found -> OpamPackage.Map.add nv [switch] acc
      with Not_found -> acc)
    gt OpamPackage.Map.empty

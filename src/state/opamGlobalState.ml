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
          if nv.version = obsolete_pinned_v then
            let name = nv.name in
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
        let pinned =
          OpamPackage.Name.Map.mapi (fun name pin ->
              let v =
                match pin with
                | Version v -> v
                | Source _ ->
                  let overlay = OpamPath.Switch.Overlay.opam root switch name in
                  let opam = OpamFile.OPAM.safe_read overlay in
                    OpamStd.Option.default (OpamPackage.Version.of_string "0")
                      (OpamFile.OPAM.version_opt opam)
              in
              v, pin)
            pinned
        in
        let compiler =
          let comp = OpamFile.Comp.read (OpamPath.compiler_comp root c) in
          let atoms = OpamFormula.atoms (OpamFile.Comp.packages comp) in
          List.fold_left (fun acc (name,_) ->
              let nv =
                try
                  let v, _ = OpamPackage.Name.Map.find name pinned in
                  OpamPackage.create name v
                with Not_found ->
                try OpamPackage.max_version installed name
                with Not_found ->
                  OpamPackage.create name
                    (OpamPackage.Version.of_string "~unknown")
              in
              OpamPackage.Set.add nv acc)
            OpamPackage.Set.empty atoms
        in
        OpamFile.State.write
          (OpamPath.Switch.state root switch)
          { sel_installed = installed;
            sel_roots = installed_roots;
            sel_pinned = pinned;
            sel_compiler = compiler };
        OpamFilename.remove installed_f;
        OpamFilename.remove installed_roots_f;
        OpamFilename.remove pinned_f;
        (* Move .config files back *)
        OpamPackage.Set.iter (fun nv ->
            let name = nv.name in
            let src =
              OpamPath.Switch.Default.lib root switch name // "opam.config"
            in
            let dst = OpamPath.Switch.config root switch name in
            if OpamFilename.exists src then
              OpamFilename.move ~src ~dst)
          installed)
      aliases

  let v1_3_dev5 = OpamVersion.of_string "1.3~dev5"

  let from_1_3_dev2_to_1_3_dev5 root conf =
    log "Upgrade switch state files format to 1.3 step 2";
    let aliases_f = OpamPath.aliases root in
    let aliases = OpamFile.Aliases.safe_read aliases_f in
    OpamSwitch.Map.iter (fun switch comp_name ->
        (* Convert state-file table format to selections file, opam syntax
           format *)
        let state_f = OpamPath.Switch.state root switch in
        let selections = OpamFile.State.safe_read state_f in
        let selections_f = OpamPath.Switch.selections root switch in
        (* Change comp file to a package *)
        let selections =
          if comp_name <> OpamCompiler.of_string "empty" then
            let name = OpamPackage.Name.of_string "ocaml" in
            let comp_f = OpamPath.compiler_comp root comp_name in
            let comp = OpamFile.Comp.read comp_f in
            let descr_f = OpamPath.compiler_descr root comp_name in
            let descr =
              if OpamFilename.exists descr_f then OpamFile.Descr.read descr_f
              else
                OpamFile.Descr.create
                  "Switch relying on a system-wide installation of OCaml"
            in
            let comp_opam =
              OpamFile.Comp.to_package name comp (Some descr)
            in
            let nv = OpamFile.OPAM.package comp_opam in
            let switch_config_f = OpamPath.Switch.global_config root switch in
            let switch_config = OpamFile.Dot_config.safe_read switch_config_f in
            let config =
              if OpamFile.Comp.preinstalled comp then
                let config =
                  OpamFile.Dot_config.create @@
                  List.map (fun (v,c) -> OpamVariable.of_string v, c) @@
                  [ "ocaml-version",
                    S (OpamStd.Option.default "unknown"
                         (Lazy.force OpamOCaml.system_ocamlc_version));
                    "compiler", S (OpamCompiler.to_string comp_name);
                    "preinstalled", B true;
                    "ocaml-native", B (Lazy.force OpamOCaml.ocaml_native_available);
                    "ocaml-native-tools", B (Lazy.force OpamOCaml.ocaml_opt_available);
                    "ocaml-native-dynlink", B (Lazy.force OpamOCaml.ocaml_natdynlink_available);
                    "ocaml-stubsdir",
                    S (Filename.concat
                         (OpamStd.Option.default "/usr/lib/ocaml"
                            (Lazy.force OpamOCaml.system_ocamlc_where))
                         "stublibs")
                   ]
                in
                match Lazy.force OpamOCaml.where_is_ocamlc with
                | Some ocamlc ->
                  let f = OpamFilename.Dir.of_string ocamlc // "ocamlc" in
                  OpamFile.Dot_config.with_file_depends config
                    [f, OpamFilename.digest f]
                | None -> config
              else
                OpamFile.Dot_config.create @@
                List.map (fun (v,c) -> OpamVariable.of_string v, c) @@
                [ "ocaml-version",
                  S (OpamCompiler.Version.to_string
                       (OpamFile.Comp.version comp));
                  "compiler", S (OpamCompiler.to_string comp_name);
                  "preinstalled", B false;
                  "ocaml-native",
                  B (OpamFilename.exists
                       (OpamPath.Switch.bin root switch switch_config
                        // "ocamlopt"));
                  "ocaml-native-tools",
                  B (OpamFilename.exists
                       (OpamPath.Switch.bin root switch switch_config
                        // "ocamlc.opt"));
                  "ocaml-native-dynlink",
                  B (OpamFilename.exists
                       (OpamPath.Switch.lib_dir root switch switch_config
                        / "ocaml" // "dynlink.cmxa"));
                  "ocaml-stubsdir",
                  S (OpamFilename.Dir.to_string
                       (OpamPath.Switch.stublibs root switch switch_config));
                ]
            in
            let config_f = OpamPath.Switch.config root switch name in
            OpamFile.OPAM.write (OpamPath.opam root nv) comp_opam;
            OpamFile.Dot_config.write config_f config;
            (* Also export compiler variables as globals *)
            OpamFile.Dot_config.write switch_config_f
              (OpamFile.Dot_config.with_vars switch_config
                 (OpamFile.Dot_config.bindings switch_config @
                  OpamFile.Dot_config.bindings config));
            {selections with
             sel_installed = OpamPackage.Set.add nv selections.sel_installed;
             sel_compiler = OpamPackage.Set.add nv selections.sel_compiler;
             sel_roots = OpamPackage.Set.add nv selections.sel_roots; }
          else selections
        in
        OpamFile.SwitchSelections.write selections_f selections;
        OpamFilename.remove state_f)
      aliases;
    let conf =
      OpamFile.Config.with_installed_switches conf (OpamSwitch.Map.keys aliases)
    in
    OpamFilename.remove aliases_f;
    OpamConsole.note "Opam root update successful. You should run 'opam \
                      update' to sync with the new format repositories.";
    conf

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
    if OpamVersion.compare config_version v1_3_dev5 < 0 then
      if OpamVersion.git () <> None &&
         OpamConsole.confirm
           "This dev version of opam requires an update to the layout of %s, \
            which can't be reverted.\n\
            You may want to back it up before going further. Abort ?"
           (OpamFilename.Dir.to_string (OpamStateConfig.(!r.root_dir)))
      then OpamConsole.error_and_exit "Aborted"
      else
      let config = OpamFile.Config.with_opam_version config v1_3_dev5 in
      if OpamVersion.compare config_version v1_1 < 0 then
        from_1_0_to_1_1 root;
      if OpamVersion.compare config_version v1_2 < 0 then
        from_1_1_to_1_2 root;
      if OpamVersion.compare config_version v1_3_dev2 < 0 then
        from_1_2_to_1_3_dev2 root;
      let config =
        if OpamVersion.compare config_version v1_3_dev5 < 0 then
          from_1_3_dev2_to_1_3_dev5 root config
        else config
      in
      OpamStateConfig.write root config;
      config
    else
      config

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
  { global_lock=lock; root; config; }

let fold_switches f gt acc =
  List.fold_left (fun acc switch ->
      f switch
        (OpamFile.SwitchSelections.safe_read
           (OpamPath.Switch.selections gt.root switch))
        acc
    ) acc (OpamFile.Config.installed_switches gt.config)

let all_installed gt =
  fold_switches (fun _ sel acc ->
      OpamPackage.Set.union acc sel.sel_installed)
    gt  OpamPackage.Set.empty

let installed_versions gt name =
  fold_switches (fun switch sel acc ->
      let installed =
        OpamPackage.packages_of_name sel.sel_installed name
      in
      try
        let nv = OpamPackage.Set.choose installed in
        try OpamPackage.Map.add nv (switch::OpamPackage.Map.find nv acc) acc
        with Not_found -> OpamPackage.Map.add nv [switch] acc
      with Not_found -> acc)
    gt OpamPackage.Map.empty

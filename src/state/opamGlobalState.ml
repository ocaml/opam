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
    let aliases = OpamFile.Aliases.safe_read (OpamFile.make (root // "aliases")) in
    let remove_pinned_suffix d =
      let s = OpamFilename.Dir.to_string d in
      if Filename.check_suffix s ".pinned" then
        OpamFilename.move_dir ~src:d
          ~dst:(OpamFilename.Dir.of_string (Filename.chop_suffix s ".pinned"))
    in
    let packages = lazy (
      OpamPackage.Set.of_list
        (OpamPackage.Map.keys
           (OpamFile.Package_index.safe_read
              (OpamFile.make (root / "repo" // "package-index"))))
    ) in
    OpamSwitch.Map.iter (fun switch _ ->
        let switch_root = root / OpamSwitch.to_string switch in
        let pinned_version name =
          try
            let f =
              OpamFile.make (switch_root / "overlay" /
                             OpamPackage.Name.to_string name // "opam")
            in
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
          (OpamFilename.dirs (switch_root / "packages.dev"));
        List.iter remove_pinned_suffix
          (OpamFilename.dirs (switch_root / "overlay"));
        let switch_prefix = switch_root in
        let installed_f =
          OpamFile.make OpamFilename.Op.(switch_prefix // "installed")
        in
        let installed = OpamFile.PkgList.safe_read installed_f in
        OpamFile.PkgList.write installed_f
          (OpamPackage.Set.map fix_version installed);
        let installed_roots_f =
          OpamFile.make OpamFilename.Op.(switch_prefix // "installed.roots")
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
              let dst = switch_root / "lib" / name // "opam.config" in
              OpamFilename.mkdir (OpamFilename.dirname dst);
              OpamFilename.move ~src:f ~dst
          )
          (OpamFilename.files (switch_root / "config"))
      ) aliases

  let v1_3_dev2 = OpamVersion.of_string "1.3~dev2"

  let from_1_2_to_1_3_dev2 root =
    log "Upgrade switch state files format to 1.3";
    let aliases =
      OpamFile.Aliases.safe_read (OpamFile.make (root // "aliases"))
    in
    OpamSwitch.Map.iter (fun switch c ->
        let switch_dir = root / OpamSwitch.to_string switch in
        let installed_f = switch_dir // "installed" in
        let installed_roots_f = switch_dir // "installed.roots" in
        let pinned_f = switch_dir // "pinned" in
        let installed =
          OpamFile.PkgList.safe_read (OpamFile.make installed_f)
        in
        let installed_roots =
          OpamFile.PkgList.safe_read (OpamFile.make installed_roots_f)
        in
        let pinned =
          OpamFile.Pinned_legacy.safe_read (OpamFile.make pinned_f)
        in
        let pinned =
          OpamPackage.Name.Map.mapi (fun name pin ->
              let v =
                match pin with
                | Version v -> v
                | Source _ ->
                  let overlay =
                    OpamFile.make (switch_dir / "overlay" /
                                   OpamPackage.Name.to_string name // "opam")
                  in
                  let opam = OpamFile.OPAM.safe_read overlay in
                    OpamStd.Option.default (OpamPackage.Version.of_string "0")
                      (OpamFile.OPAM.version_opt opam)
              in
              v, pin)
            pinned
        in
        let compiler =
          let version = match OpamStd.String.cut_at c '+' with
            | Some (v,_) -> v
            | None -> c
          in
          let comp =
            OpamFile.Comp.read
              (OpamFile.make (root / "compilers" / version / c // (c ^".comp")))
          in
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
        OpamFile.State.write (OpamFile.make (switch_dir // "state"))
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
              switch_dir / "lib" / OpamPackage.Name.to_string name //
              "opam.config"
            in
            let dst =
              switch_dir / "config" //
              (OpamPackage.Name.to_string name ^ ".config")
            in
            if OpamFilename.exists src then
              OpamFilename.move ~src ~dst)
          installed)
      aliases

  let v1_3_dev5 = OpamVersion.of_string "1.3~dev5"

  let from_1_3_dev2_to_1_3_dev5 root conf =
    log "Upgrade switch state files format to 1.3 step 2";
    let aliases_f = OpamFile.make (root // "aliases") in
    let aliases = OpamFile.Aliases.safe_read aliases_f in
    OpamSwitch.Map.iter (fun switch comp_name ->
        (* Convert state-file table format to selections file, opam syntax
           format *)
        let switch_dir = root / OpamSwitch.to_string switch in
        let state_f = OpamFile.make (switch_dir // "state") in
        let selections = OpamFile.State.safe_read state_f in
        let selections_f = OpamFile.make (switch_dir // "switch-state") in
        let comp_version = match OpamStd.String.cut_at comp_name '+' with
          | Some (v,_) -> v
          | None -> comp_name
        in
        (* Change comp file to a package *)
        let selections =
          if comp_name <> "empty" then
            let name = OpamPackage.Name.of_string "ocaml" in
            let comp_f =
              OpamFile.make (root / "compilers" / comp_version /
                             comp_name // (comp_name ^ ".comp"))
            in
            let comp = OpamFile.Comp.read comp_f in
            let descr_f =
              OpamFile.make (root / "compilers" / comp_version /
                             comp_name // (comp_name ^ ".descr"))
            in
            let descr =
              OpamStd.Option.default
                (OpamFile.Descr.create
                   "Switch relying on a system-wide installation of OCaml")
                (OpamFile.Descr.read_opt descr_f)
            in
            let comp_opam =
              OpamFile.Comp.to_package name comp (Some descr)
            in
            let nv = OpamFile.OPAM.package comp_opam in
            let switch_config_f =
              OpamFile.make
                (switch_dir / "config" // "global-config.config")
            in
            let switch_config = OpamFile.Dot_config.safe_read switch_config_f in
            let config =
              if OpamFile.Comp.preinstalled comp then
                let config =
                  OpamFile.Dot_config.create @@
                  List.map (fun (v,c) -> OpamVariable.of_string v, c) @@
                  [ "ocaml-version",
                    S (OpamStd.Option.default "unknown"
                         (Lazy.force OpamOCaml.system_ocamlc_version));
                    "compiler", S comp_name;
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
                  S (OpamFile.Comp.version comp);
                  "compiler", S comp_name;
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
            let config_f =
              OpamFile.make
                (switch_dir / "config" //
                 (OpamPackage.Name.to_string name ^".config"))
            in
            OpamFile.OPAM.write
              (OpamFile.make
                 (root / "packages" / OpamPackage.Name.to_string name
                  / OpamPackage.to_string nv // "opam"))
              comp_opam;
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
        OpamFilename.remove (OpamFile.filename state_f))
      aliases;
    let conf =
      OpamFile.Config.with_installed_switches conf (OpamSwitch.Map.keys aliases)
    in
    OpamFilename.remove (OpamFile.filename aliases_f);
    OpamConsole.note "Opam root update successful. You should run 'opam \
                      update' to sync with the new format repositories.";
    conf

  let v1_3_dev6 = OpamVersion.of_string "1.3~dev6"

  let from_1_3_dev5_to_1_3_dev6 root conf =
    log "Upgrade switch state files format to 1.3 step 3";
    (* Move switch internals to [switch/.opam-switch] *)
    List.iter (fun switch ->
        let switch_dir = root / OpamSwitch.to_string switch in
        let meta_dir =  switch_dir / ".opam-switch" in
        OpamFilename.mkdir meta_dir;
        List.iter (fun f ->
            let src = switch_dir // f in
            let dst = meta_dir // f in
            if OpamFilename.exists src then OpamFilename.move ~src ~dst)
          ["lock"; "switch-state"; "reinstall"; "environment"];
        List.iter (fun d ->
            let src = switch_dir / d in
            let dst = meta_dir / d in
            if OpamFilename.exists_dir src then OpamFilename.move_dir ~src ~dst)
          ["backup"; "build"; "install"; "config"; "packages.dev"; "overlay"]
      )
      (OpamFile.Config.installed_switches conf)

  let v1_3_dev7 = OpamVersion.of_string "1.3~dev7"

  let from_1_3_dev6_to_1_3_dev7 root conf =
    log "Upgrade switch state files format to 1.3 step 4";
    (* Get mirrors of the metadata of all installed packages into
       switch_meta_dir/packages *)
    List.iter (fun switch ->
        let switch_dir = root / OpamSwitch.to_string switch in
        let meta_dir =  switch_dir / ".opam-switch" in
        let installed =
          (OpamFile.SwitchSelections.safe_read
             (OpamFile.make (switch_dir // "switch-state")))
          .sel_installed
        in
        OpamFilename.mkdir (meta_dir / "packages");
        OpamPackage.Set.iter (fun nv ->
            let dst = meta_dir / "packages" // (OpamPackage.to_string nv ^ ".opam") in
            try
              let src =
                List.find OpamFilename.exists [
                  meta_dir / "overlay" / OpamPackage.Name.to_string nv.name // "opam";
                  root / "packages" / OpamPackage.Name.to_string nv.name /
                  OpamPackage.to_string nv // "opam";
                ]
              in
              OpamFilename.copy ~src ~dst
            with Not_found ->
              OpamFile.OPAM.write (OpamFile.make dst) (OpamFile.OPAM.create nv)
          )
          installed)
      (OpamFile.Config.installed_switches conf);
    OpamFilename.rmdir (root / "packages")

  let latest_version = v1_3_dev7

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
    if OpamVersion.compare config_version latest_version < 0 then
      if OpamVersion.git () <> None &&
         OpamConsole.read
           "This dev version of opam requires an update to the layout of %s \
            from version %s to version %s, which can't be reverted.\n\
            You may want to back it up before going further.\n\
            Type \"yes\" to perform the update and continue:"
           (OpamFilename.Dir.to_string (OpamStateConfig.(!r.root_dir)))
           (OpamVersion.to_string config_version)
           (OpamVersion.to_string latest_version)
         <> Some "yes"
      then OpamConsole.error_and_exit "Aborted"
      else
      let config = OpamFile.Config.with_opam_version config latest_version in
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
      if OpamVersion.compare config_version v1_3_dev6 < 0 then
        from_1_3_dev5_to_1_3_dev6 root config;
      if OpamVersion.compare config_version v1_3_dev7 < 0 then
        from_1_3_dev6_to_1_3_dev7 root config;
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

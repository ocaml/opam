(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStateTypes

let log fmt = OpamConsole.log "AUXCMD" fmt
let slog = OpamConsole.slog

let package_file_changes st packages =
  OpamPackage.Set.fold (fun nv acc ->
      let f =
        OpamPath.Switch.changes st.switch_global.root st.switch nv.name
      in
      match OpamFile.Changes.read_opt f with
      | None -> acc
      | Some ch -> OpamStd.String.Map.union (fun _ x -> x) acc ch)
    packages
    OpamStd.String.Map.empty

let copy_files_to_destdir st pfx packages =
  let switch_pfx = OpamPath.Switch.root st.switch_global.root st.switch in
  package_file_changes st packages |>
  OpamDirTrack.check switch_pfx |>
  List.iter @@ function
  | src, `Unchanged ->
    let relf = OpamFilename.remove_prefix switch_pfx src in
    let dst = OpamFilename.Op.(pfx // relf) in
    if OpamConsole.verbose () then
      OpamConsole.msg "%-40s %s %s\n"
        relf (OpamConsole.colorise `blue "=>")
        (OpamFilename.to_string dst);
    if not OpamStateConfig.(!r.dryrun) then
      if OpamFilename.exists src then OpamFilename.copy ~src ~dst else
      let as_dir f = OpamFilename.(Dir.of_string (to_string f)) in
      if OpamFilename.exists_dir (as_dir src)
      then OpamFilename.mkdir (as_dir dst)
  | src, (`Changed | `Removed as ch) ->
    OpamConsole.warning "Not installing %s, which was %s since"
      (OpamConsole.colorise `bold (OpamFilename.to_string src))
      (match ch with `Changed -> "changed" | `Removed -> "removed")

let remove_files_from_destdir st pfx packages =
  package_file_changes st packages |>
  OpamStd.String.Map.bindings |>
  List.rev |> (* Get the longer names first, their parent folders last *)
  List.iter @@ fun (rel_file, change) ->
  match change with
  | OpamDirTrack.Added _
  | OpamDirTrack.Contents_changed _
  | OpamDirTrack.Kind_changed _ ->
    let f = OpamFilename.Op.(pfx // rel_file) in
    let d = OpamFilename.Op.(pfx / rel_file) in
    if OpamFilename.exists f then
      (if OpamConsole.verbose () then
         OpamConsole.msg "Removing %s\n"
           (OpamConsole.colorise `bold (OpamFilename.to_string f));
       if not OpamStateConfig.(!r.dryrun) then OpamFilename.remove f)
    else if OpamFilename.exists_dir d then
      if OpamFilename.dir_is_empty d then
        (if OpamConsole.verbose () then
           OpamConsole.msg "Removing %s\n"
             (OpamConsole.colorise `bold (OpamFilename.Dir.to_string d));
         if not OpamStateConfig.(!r.dryrun) then OpamFilename.rmdir d)
      else
        OpamConsole.note "Not removing non-empty directory %s"
          (OpamConsole.colorise `bold (OpamFilename.Dir.to_string d))
  | _ -> ()

let name_from_project_dirname d =
  try
    Some (OpamFilename.(Base.to_string (basename_dir d)) |>
          Re.(replace_string (compile (seq [char '.'; any]))) ~by:"" |>
          OpamPackage.Name.of_string)
  with Failure _ -> None

let url_with_local_branch = function
  | { OpamUrl.backend = #OpamUrl.version_control; hash = None; _ } as url ->
    (match OpamProcess.Job.run (OpamRepository.current_branch url) with
     | Some b -> { url with OpamUrl.hash = Some b }
     | None -> url)
  | url -> url

let opams_of_dir_t files_in_source dir =
  let files = files_in_source dir in
  List.fold_left (fun acc p ->
      let name =
        let open OpamStd.Option.Op in
        p.pin_name >>+ fun () ->
        OpamFile.OPAM.(name_opt (safe_read p.pin.pin_file))
        >>+ fun () ->
        match files with
        | [] | _::_::_ -> None
        | [_] -> name_from_project_dirname dir
      in
      match name with
      | Some n -> { p with pin_name = n} :: acc
      | None ->
        OpamConsole.warning
          "Ignoring file at %s: could not infer package name"
          (OpamFile.to_string p.pin.pin_file);
        acc)
    [] files


let opams_of_dir ?locked ?recurse ?subpath dir =
  opams_of_dir_t (OpamPinned.files_in_source ?locked ?recurse ?subpath)
    dir

let opams_of_dir_w_target ?locked ?recurse ?subpath
    ?(same_kind=fun _ -> OpamClientConfig.(!r.pin_kind_auto)) url dir =
  opams_of_dir_t
    (OpamPinned.files_in_source_w_target
       ?locked ?recurse ?subpath ~same_kind url)
    dir

let name_and_dir_of_opam_file ?locked f =
  let srcdir = OpamFilename.dirname f in
  let srcdir =
    if OpamFilename.dir_ends_with ".opam" srcdir &&
       OpamUrl.guess_version_control (OpamFilename.Dir.to_string srcdir)
       = None
    then OpamFilename.dirname_dir srcdir
    else srcdir
  in
  let name =
    let open OpamStd.Option.Op in
    OpamPinned.name_of_opam_filename ?locked srcdir f >>+ fun () ->
    OpamFile.OPAM.(name_opt (safe_read (OpamFile.make f))) >>+ fun () ->
    name_from_project_dirname srcdir
  in
  name, srcdir

let resolve_locals_pinned st ?(recurse=false) ?subpath atom_or_local_list =
  let pinned_packages_of_dir st dir =
    OpamPackage.Set.filter
      (fun nv ->
         let open OpamStd.Option.Op in
         match subpath with
         | Some sp ->
           let dir_sp = OpamFilename.SubPath.(dir / sp) in
           let url_sp_dir =
             OpamSwitchState.primary_url_with_subpath st nv >>= OpamUrl.local_dir
           in
           if recurse then
             (url_sp_dir >>| OpamFilename.dir_starts_with dir_sp) +! false
           else
             url_sp_dir = Some dir_sp
         | None ->
           if recurse then
             (OpamSwitchState.primary_url st nv >>= OpamUrl.local_dir)
             = Some dir
           else
             (OpamSwitchState.primary_url_with_subpath st nv >>= OpamUrl.local_dir)
             = Some dir
      )
      st.pinned
  in
  let atoms =
    List.fold_left (fun acc -> function
        | `Atom a -> a::acc
        | `Dirname d ->
          let pkgs = pinned_packages_of_dir st d in
          if OpamPackage.Set.is_empty pkgs then
            OpamConsole.warning "No pinned packages found at %s"
              (OpamFilename.Dir.to_string d);
          List.rev_append (OpamSolution.atoms_of_packages pkgs) acc
        | `Filename f ->
          OpamConsole.error_and_exit `Bad_arguments
            "This command doesn't support specifying a file name (%S)"
            (OpamFilename.to_string f))
      [] atom_or_local_list
  in
  List.rev atoms

let resolve_locals ?(quiet=false) ?locked ?recurse ?subpath
    atom_or_local_list =
  let target_dir dir =
    let d = OpamFilename.Dir.to_string dir in
    let backend = OpamUrl.guess_version_control d in
    OpamUrl.parse ?backend ~from_file:false d |>
    url_with_local_branch
  in
  let to_pin, atoms =
    List.fold_left (fun (to_pin, atoms) -> function
        | `Atom a -> to_pin, a :: atoms
        | `Dirname d ->
          let target = target_dir d in
          let names_files =
            opams_of_dir_w_target ?recurse ?subpath ?locked target d
          in
          if names_files = [] && not quiet then
            OpamConsole.warning "No package definitions found at %s"
              (OpamFilename.Dir.to_string d);
          let to_pin = names_files @ to_pin in
          let atoms =
            List.map (fun nf -> nf.pin_name, None) names_files @ atoms
          in
          to_pin, atoms
        | `Filename f ->
          let f, locked =
            match locked with
            | None -> f, None
            | Some ext ->
              let flocked = OpamFilename.add_extension f ext in
              if OpamFilename.exists flocked then flocked, locked else f, None
          in
          match name_and_dir_of_opam_file ?locked f with
          | Some n, srcdir ->
            { pin_name = n;
              pin = { pin_file = OpamFile.make f;
                      pin_locked = locked;
                      pin_url = target_dir srcdir;
                      pin_subpath = None;
                    }} :: to_pin,
            (n, None) :: atoms
          | None, _ ->
            OpamConsole.error_and_exit `Not_found
              "Could not infer package name from package definition file %s"
              (OpamFilename.to_string f))
      ([], [])
      atom_or_local_list
  in
  let duplicates =
    List.filter (fun nf ->
        List.exists (fun nf' ->
            nf.pin_name = nf'.pin_name && nf.pin.pin_file <> nf'.pin.pin_file)
          to_pin)
      to_pin
  in
  match duplicates with
  | [] -> List.rev to_pin, List.rev atoms
  | _ ->
    OpamConsole.error_and_exit `Bad_arguments
      "Multiple files for the same package name were specified:\n%s"
      (OpamStd.Format.itemize (fun nf ->
           Printf.sprintf "Package %s with %s definition %s %s %s"
             (OpamConsole.colorise `bold
                (OpamPackage.Name.to_string nf.pin_name))
             (if nf.pin.pin_locked = None then "" else "locked")
             (OpamFile.to_string nf.pin.pin_file)
             (OpamConsole.colorise `blue "=>")
             (OpamUrl.to_string nf.pin.pin_url))
          duplicates)

let autopin_aux st ?quiet ?(for_view=false) ?recurse ?subpath ?locked
    atom_or_local_list =
  let to_pin, atoms =
    resolve_locals ?quiet ?recurse ?subpath ?locked atom_or_local_list
  in
  if to_pin = [] then
    atoms, to_pin, OpamPackage.Set.empty, OpamPackage.Set.empty
  else
  let pinning_dirs =
    OpamStd.List.filter_map (function
        | `Dirname d -> Some OpamFilename.SubPath.(d /? subpath)
        | _ -> None)
      atom_or_local_list
  in
  log "autopin: %a"
    (slog @@ OpamStd.List.to_string (fun pin ->
         Printf.sprintf "%s%s => %s%s"
           (OpamPackage.Name.to_string pin.pin_name)
           (if pin.pin.pin_locked = None then "" else "[locked]")
           (OpamUrl.to_string pin.pin.pin_url)
           (OpamStd.Option.to_string
              OpamFilename.SubPath.pretty_string pin.pin.pin_subpath)))
    to_pin;
  let obsolete_pins =
    (* Packages not current but pinned to the same dirs *)
    OpamPackage.Set.filter (fun nv ->
        not (List.exists (fun nf -> nf.pin_name = nv.name) to_pin) &&
        let primary_url =
          if recurse = Some true then
            OpamSwitchState.primary_url
          else
            OpamSwitchState.primary_url_with_subpath
        in
        match OpamStd.Option.Op.( primary_url st nv >>= OpamUrl.local_dir) with
        | Some d ->
          List.mem d pinning_dirs
        | None -> false)
      st.pinned
  in
  let already_pinned, to_pin =
    List.partition (fun nf ->
        try
          (* check of the target to avoid repin of pin to update with `opam
             install .` and loose edited opams *)
          let pinned_pkg = OpamPinned.package st nf.pin_name in
          OpamSwitchState.primary_url st pinned_pkg = Some nf.pin.pin_url
          &&
          (match OpamSwitchState.opam_opt st pinned_pkg with
           | Some opam ->
             (match locked, OpamFile.OPAM.locked opam with
              | Some ext , Some ext' -> String.equal ext ext'
              | None, None -> true
              | _ -> false)
           | None -> false)
          &&
          (* For `opam show`, we need to check does the opam file changed to
             perform a simulated pin if so *)
          (not for_view ||
           match
             OpamSwitchState.opam_opt st pinned_pkg,
             OpamFile.OPAM.read_opt nf.pin.pin_file
           with
           | Some opam0, Some opam ->
             let opam = OpamFile.OPAM.with_locked_opt nf.pin.pin_locked opam in
             OpamFile.OPAM.equal opam0 opam
           | _, _ -> false)
        with Not_found -> false)
      to_pin
  in
  let already_pinned_set =
    List.fold_left (fun acc nf ->
        OpamPackage.Set.add (OpamPinned.package st nf.pin_name) acc)
      OpamPackage.Set.empty already_pinned
  in
  atoms, to_pin, obsolete_pins, already_pinned_set

let simulate_local_pinnings ?quiet ?(for_view=false) st to_pin =
  assert (not (for_view &&
               OpamSystem.get_lock_flag st.switch_lock = `Lock_write));
  let local_names =
    List.fold_left (fun set nf ->
        OpamPackage.Name.Set.add nf.pin_name set)
      OpamPackage.Name.Set.empty to_pin
  in
  let local_opams =
    List.fold_left (fun map pin ->
        let { pin_name = name;
              pin = { pin_file = file; pin_locked = locked;
                      pin_subpath = subpath; pin_url = target }} = pin
        in
        match
          OpamPinCommand.read_opam_file_for_pinning
            ?locked ?quiet name file target
        with
        | None -> map
        | Some opam ->
          let opam = OpamFile.OPAM.with_name name opam in
          let opam =
            if for_view then opam else
              OpamFile.OPAM.with_url (OpamFile.URL.create ?subpath target) opam
          in
          let opam, version = match OpamFile.OPAM.version_opt opam with
            | Some v -> opam, v
            | None ->
              let v = OpamPinCommand.default_version st name in
              OpamFile.OPAM.with_version v opam, v
          in
          OpamPackage.Map.add (OpamPackage.create name version) opam map)
      OpamPackage.Map.empty to_pin
  in
  let local_packages = OpamPackage.keys local_opams in
  let pinned =
    let open OpamPackage.Set.Op in
    st.pinned
    -- OpamPackage.packages_of_names st.pinned
      (OpamPackage.names_of_packages local_packages)
    ++ local_packages
  in
  let st = {
    st with
    opams =
      OpamPackage.Map.union (fun _ o -> o) st.opams local_opams;
    packages =
      OpamPackage.Set.union st.packages local_packages;
    available_packages = lazy (
      OpamPackage.Set.union
        (OpamPackage.Set.filter
           (fun nv -> not (OpamPackage.Name.Set.mem nv.name local_names))
           (Lazy.force st.available_packages))
        (OpamSwitchState.compute_available_packages
           st.switch_global st.switch st.switch_config ~pinned ~opams:local_opams)
    );
    pinned;
  } in
  st, local_packages

let simulate_autopin st ?quiet ?(for_view=false) ?locked ?recurse ?subpath
    atom_or_local_list =
  let atoms, to_pin, obsolete_pins, already_pinned_set =
    autopin_aux st ?quiet ~for_view ?recurse ?subpath ?locked atom_or_local_list
  in
  if to_pin = [] then st, atoms else
  let st =
    OpamPackage.Set.fold (fun nv st -> OpamPinCommand.unpin_one st nv)
      obsolete_pins st
  in
  let st, pins = simulate_local_pinnings ?quiet ~for_view st to_pin in
  if not for_view then
    (let pins = OpamPackage.Set.union pins already_pinned_set in
     let pin_depends =
       OpamPackage.Set.fold (fun nv acc ->
           List.fold_left (fun acc (nv,target) ->
               OpamPackage.Map.add nv target acc)
             acc
             (OpamFile.OPAM.pin_depends (OpamSwitchState.opam st nv)))
         pins OpamPackage.Map.empty
     in
     if not (OpamPackage.Map.is_empty pin_depends) then
       (OpamConsole.msg "Would pin the following:\n%s"
          (OpamStd.Format.itemize (fun (nv, url) ->
               Printf.sprintf "%s to %s"
                 (OpamConsole.colorise `bold (OpamPackage.to_string nv))
                 (OpamConsole.colorise `underline (OpamUrl.to_string url)))
              (OpamPackage.Map.bindings pin_depends));
        OpamConsole.note "The following may not reflect the above pinnings (their \
                          package definitions are not available at this stage)";
        OpamConsole.msg "\n"));
  st, atoms

let autopin st ?(simulate=false) ?quiet ?locked ?recurse ?subpath
    atom_or_local_list =
  if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show) then
    simulate_autopin st ?quiet ?locked atom_or_local_list
  else
  let atoms, to_pin, obsolete_pins, already_pinned_set =
    autopin_aux st ?quiet ?recurse ?subpath ?locked atom_or_local_list
  in
  if to_pin = [] && OpamPackage.Set.is_empty obsolete_pins &&
     OpamPackage.Set.is_empty already_pinned_set
  then st, atoms else
  let st =
    if simulate then
      OpamPackage.Set.fold (fun nv st -> OpamPinCommand.unpin_one st nv)
        obsolete_pins st
    else
      OpamPinCommand.unpin st
        (OpamPackage.Name.Set.elements
           (OpamPackage.names_of_packages obsolete_pins))
  in
  let already_pinned_diff_url =
    (* is pinned but no in already pinned because not same url *)
    List.fold_left (fun set nf ->
        match
          OpamStd.Option.map
            (fun nv -> OpamPackage.Set.mem nv already_pinned_set)
            (OpamPinned.package_opt st nf.pin_name)
        with
        | Some false -> OpamPackage.Name.Set.add nf.pin_name set
        | _ -> set
      ) OpamPackage.Name.Set.empty to_pin
  in

  let st, pins =
    if simulate then simulate_local_pinnings ?quiet st to_pin else
    try
      List.fold_left (fun (st, pins) pin ->
          let { pin_name = name;
                pin = { pin_file = file; pin_locked = locked;
                        pin_subpath = subpath; pin_url = target }} = pin
          in
          match
            OpamPinCommand.read_opam_file_for_pinning
              ?locked ?quiet name file target
          with
          | None -> st, pins
          | Some opam ->
            let st =
              try
                OpamPinCommand.source_pin st name ~quiet:true ~opam ?subpath
                  (Some target)
              with OpamPinCommand.Nothing_to_do -> st
            in
            st, OpamPackage.Set.add (OpamPinned.package st name) pins)
        (st, OpamPackage.Set.empty) to_pin
    with OpamPinCommand.Aborted ->
      OpamStd.Sys.exit_because `Aborted
  in
  let _result, st, _updated =
    let already_pinned =
      OpamPackage.Set.union already_pinned_set
        (OpamPackage.packages_of_names pins already_pinned_diff_url)
    in
    if OpamClientConfig.(!r.working_dir || !r.inplace_build) then
      OpamUpdate.dev_packages st ~working_dir:pins pins
    else
      OpamUpdate.dev_packages st ~working_dir:OpamPackage.Set.empty already_pinned
  in
  let st =
    if OpamClientConfig.(!r.ignore_pin_depends) then st else
      OpamPackage.Set.fold (fun nv st ->
          OpamPinCommand.handle_pin_depends st nv (OpamSwitchState.opam st nv))
        (OpamPackage.Set.union pins already_pinned_set) st
  in
  st, atoms

let check_and_revert_sandboxing root config =
  let sdbx_wrappers =
    let w = OpamFile.Config.wrappers config in
    let init_sdbx_cmds =
      List.map (function `build cmd | `install cmd | `remove cmd -> cmd)
        OpamInitDefaults.sandbox_wrappers
      |> List.flatten
    in
    List.filter (fun cmd -> List.mem cmd init_sdbx_cmds)
      OpamFile.Wrappers.(wrap_build w @ wrap_install w @ wrap_remove w)
  in
  let env = fun v ->
    let fv = OpamVariable.Full.variable v in
    match OpamVariable.Map.find_opt fv (OpamEnv.hook_env root) with
    | Some c -> c
    | None ->
      OpamStd.Option.Op.(OpamStd.Option.of_Not_found (List.assoc fv)
                           OpamSysPoll.variables >>= Lazy.force)
  in
  match OpamFilter.commands env sdbx_wrappers with
  | [] -> config
  | cmd::_ ->
    let test_cmd =
      [ "sh"; "-c"; "echo SUCCESS | tee check-write" ]
    in
    let working_or_noop =
      let env =
        Array.append [| "OPAM_SWITCH_PREFIX=/dev/null" |] (Unix.environment ())
      in
      try
        (* Don't assume that we can mount the CWD *)
        OpamSystem.in_tmp_dir @@ fun () ->
          OpamSystem.read_command_output ~env ~allow_stdin:false (cmd @ test_cmd)
        = ["SUCCESS"]
      with e ->
        (OpamConsole.error "Sandboxing is not working on your platform%s:\n%s"
           (OpamStd.Option.to_string (fun os -> " "^os)
              (OpamSysPoll.os_distribution OpamVariable.Map.empty))
           (Printexc.to_string e);
         not (OpamConsole.confirm ~default:false
                "Do you want to disable it?  Note that this will result in \
                less secure package builds, so please ensure that you have \
                some other isolation mechanisms in place (such as running \
                within a container or virtual machine)."))
    in
    if working_or_noop then config else
    let wrappers =
      let filter sdbx_cmd =
        List.filter (fun cmd_l -> not (List.mem cmd_l sdbx_cmd))
      in
      List.fold_left OpamFile.Wrappers.(fun w -> function
          | `build sdbx_build ->
            { w with wrap_build = filter sdbx_build w.wrap_build }
          | `install sdbx_install ->
            { w with wrap_install = filter sdbx_install w.wrap_install }
          | `remove sdbx_remove ->
            { w with wrap_remove = filter sdbx_remove w.wrap_remove })
        (OpamFile.Config.wrappers config) OpamInitDefaults.sandbox_wrappers
    in
    OpamFile.Config.with_wrappers wrappers config

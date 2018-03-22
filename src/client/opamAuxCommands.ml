(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
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

let opams_of_dir ?(locked=false) d =
  let files = OpamPinned.files_in_source d in
  List.fold_left (fun acc (n, f) ->
      let f =
        let locked_f =
          OpamFilename.add_extension (OpamFile.filename f) "locked"
        in
        if locked && OpamFilename.exists locked_f then OpamFile.make locked_f
        else f
      in
      let name =
        let open OpamStd.Option.Op in
        n >>+ fun () ->
        OpamFile.OPAM.(name_opt (safe_read f))
        >>+ fun () ->
        match files with
        | [] | _::_::_ -> None
        | [_] -> name_from_project_dirname d
      in
      match name with
      | Some n -> (n, f) :: acc
      | None ->
        OpamConsole.warning
          "Ignoring file at %s: could not infer package name"
          (OpamFile.to_string f);
        acc)
    [] files

let name_and_dir_of_opam_file f =
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
    OpamPinned.name_of_opam_filename srcdir f >>+ fun () ->
    OpamFile.OPAM.(name_opt (safe_read (OpamFile.make f))) >>+ fun () ->
    name_from_project_dirname srcdir
  in
  name, srcdir

let resolve_locals_pinned st atom_or_local_list =
  let pinned_packages_of_dir st dir =
    OpamPackage.Set.filter
      (fun nv ->
         OpamStd.Option.Op.(OpamSwitchState.primary_url st nv >>=
                            OpamUrl.local_dir)
         = Some dir)
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
          List.rev_append (OpamSolution.eq_atoms_of_packages pkgs) acc
        | `Filename f ->
          OpamConsole.error_and_exit `Bad_arguments
            "This command doesn't support specifying a file name (%S)"
            (OpamFilename.to_string f))
      [] atom_or_local_list
  in
  List.rev atoms

let resolve_locals ?(quiet=false) ?locked atom_or_local_list =
  let target_dir dir =
    let d = OpamFilename.Dir.to_string dir in
    let backend = OpamUrl.guess_version_control d in
    OpamUrl.parse ?backend d |>
    url_with_local_branch
  in
  let to_pin, atoms =
    List.fold_left (fun (to_pin, atoms) -> function
        | `Atom a -> to_pin, a :: atoms
        | `Dirname d ->
          let names_files = opams_of_dir ?locked d in
          if names_files = [] && not quiet then
            OpamConsole.warning "No package definitions found at %s"
              (OpamFilename.Dir.to_string d);
          let target = target_dir d in
          let to_pin =
            List.map (fun (n,f) -> n, target, f) names_files @ to_pin
          in
          let atoms =
            List.map (fun (n,_) -> n, None) names_files @ atoms
          in
          to_pin, atoms
        | `Filename f ->
          match name_and_dir_of_opam_file f with
          | Some n, srcdir ->
            (n, target_dir srcdir, OpamFile.make f) :: to_pin,
            (n, None) :: atoms
          | None, _ ->
            OpamConsole.error_and_exit `Not_found
              "Could not infer package name from package definition file %s"
              (OpamFilename.to_string f))
      ([], [])
      atom_or_local_list
  in
  let duplicates =
    List.filter (fun (n, _, f) ->
        List.exists (fun (n1, _, f1) -> n = n1 && f <> f1) to_pin)
      to_pin
  in
  match duplicates with
  | [] -> List.rev to_pin, List.rev atoms
  | _ ->
    OpamConsole.error_and_exit `Bad_arguments
      "Multiple files for the same package name were specified:\n%s"
      (OpamStd.Format.itemize (fun (n, t, f) ->
         Printf.sprintf "Package %s with definition %s %s %s"
           (OpamConsole.colorise `bold @@ OpamPackage.Name.to_string n)
           (OpamFile.to_string f)
           (OpamConsole.colorise `blue "=>")
           (OpamUrl.to_string t))
          duplicates)

let autopin_aux st ?quiet ?locked atom_or_local_list =
  let to_pin, atoms = resolve_locals ?quiet ?locked atom_or_local_list in
  if to_pin = [] then
    atoms, to_pin, OpamPackage.Set.empty, OpamPackage.Set.empty
  else
  let pinning_dirs =
    OpamStd.List.filter_map (function
        | `Dirname d -> Some d
        | _ -> None)
      atom_or_local_list
  in
  log "autopin: %a"
    (slog @@ OpamStd.List.to_string (fun (name, target, _) ->
         Printf.sprintf "%s => %s"
           (OpamPackage.Name.to_string name)
           (OpamUrl.to_string target)))
    to_pin;
  let obsolete_pins =
    (* Packages not current but pinned to the same dirs *)
    OpamPackage.Set.filter (fun nv ->
        not (List.exists (fun (n,_,_) -> n = nv.name) to_pin) &&
        match OpamStd.Option.Op.(OpamSwitchState.primary_url st nv >>=
                                 OpamUrl.local_dir)
        with
        | Some d -> List.mem d pinning_dirs
        | None -> false)
      st.pinned
  in
  let already_pinned, to_pin =
    List.partition (fun (name, target, _) ->
        try
          OpamSwitchState.primary_url st (OpamPinned.package st name)
          = Some target
        with Not_found -> false)
      to_pin
  in
  let already_pinned_set =
    List.fold_left (fun acc (name, _, _) ->
        OpamPackage.Set.add (OpamPinned.package st name) acc)
      OpamPackage.Set.empty already_pinned
  in
  atoms, to_pin, obsolete_pins, already_pinned_set

let simulate_local_pinnings ?quiet ?(keep_url=false) st to_pin =
  let local_names =
    List.fold_left (fun set (name, _, _) ->
        OpamPackage.Name.Set.add name set)
      OpamPackage.Name.Set.empty to_pin
  in
  let local_opams =
    List.fold_left (fun map (name, target, file) ->
        match
          OpamPinCommand.read_opam_file_for_pinning ?quiet name file target
        with
        | None -> map
        | Some opam ->
          let opam = OpamFile.OPAM.with_name name opam in
          let opam =
            if keep_url then opam
            else OpamFile.OPAM.with_url (OpamFile.URL.create target) opam
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
           st.switch_global st.switch st.switch_config ~pinned:st.pinned
           ~opams:local_opams)
    );
    pinned = local_packages;
  } in
  st, local_packages

let simulate_autopin st ?quiet ?keep_url ?locked atom_or_local_list =
  let atoms, to_pin, obsolete_pins, already_pinned_set =
    autopin_aux st ?quiet ?locked atom_or_local_list
  in
  if to_pin = [] then st, atoms else
  let st =
    OpamPackage.Set.fold (fun nv st -> OpamPinCommand.unpin_one st nv)
      obsolete_pins st
  in
  let st, pins = simulate_local_pinnings ?quiet ?keep_url st to_pin in
  let pins = OpamPackage.Set.union pins already_pinned_set in
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
     OpamConsole.note "The following may not reflect the above pinnings \
                       (package definitions are not available yet)";
     OpamConsole.msg "\n");
  st, atoms

let autopin st ?(simulate=false) ?quiet ?locked atom_or_local_list =
  if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show) then
    simulate_autopin st ?quiet ?locked atom_or_local_list
  else
  let atoms, to_pin, obsolete_pins, already_pinned_set =
    autopin_aux st ?quiet ?locked atom_or_local_list
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
  let st =
    let working_dir =
      if OpamClientConfig.(!r.working_dir) then already_pinned_set
      else OpamPackage.Set.empty
    in
    let _result, st, _updated =
      OpamUpdate.dev_packages st ~working_dir already_pinned_set
    in
    st
  in
  let st, pins =
    if simulate then simulate_local_pinnings ?quiet st to_pin else
    try
      List.fold_left (fun (st, pins) (name, target, file) ->
          match OpamPinCommand.read_opam_file_for_pinning ?quiet name file target with
          | None -> st, pins
          | Some opam ->
            let st =
              try
                OpamPinCommand.source_pin st name ~quiet:true ~opam
                  (Some target)
              with OpamPinCommand.Nothing_to_do -> st
            in
            st, OpamPackage.Set.add (OpamPinned.package st name) pins)
        (st, OpamPackage.Set.empty) to_pin
    with OpamPinCommand.Aborted ->
      OpamStd.Sys.exit_because `Aborted
  in
  let st =
    OpamPackage.Set.fold (fun nv st ->
        OpamPinCommand.handle_pin_depends st nv (OpamSwitchState.opam st nv))
      (OpamPackage.Set.union pins already_pinned_set) st
  in
  st, atoms

let get_compatible_compiler ?repos ?locked rt dir =
  let gt = rt.repos_global in
  let virt_st =
    OpamSwitchState.load_virtual ?repos_list:repos gt rt
  in
  let local_files = opams_of_dir ?locked dir in
  let local_opams =
    List.fold_left (fun acc (name, f) ->
        let opam = OpamFile.OPAM.safe_read f in
        let opam = OpamFormatUpgrade.opam_file ~filename:f opam in
        let nv, opam =
          match OpamFile.OPAM.version_opt opam with
          | Some v -> OpamPackage.create name v, opam
          | None ->
            let v = OpamPinCommand.default_version virt_st name in
            OpamPackage.create name v,
            OpamFile.OPAM.with_version v opam
        in
        OpamPackage.Map.add nv opam acc)
      OpamPackage.Map.empty
      local_files
  in
  let local_packages = OpamPackage.keys local_opams in
  let pin_depends =
    OpamPackage.Map.fold (fun _nv opam acc ->
        List.fold_left (fun acc (nv,_) -> OpamPackage.Set.add nv acc)
          acc (OpamFile.OPAM.pin_depends opam))
      local_opams OpamPackage.Set.empty
  in
  let local_atoms =
    OpamSolution.eq_atoms_of_packages local_packages
  in
  let virt_st =
    let opams =
      OpamPackage.Map.union (fun _ x -> x) virt_st.opams local_opams
    in
    let available = lazy (
      OpamPackage.Map.filter (fun package opam ->
          OpamFilter.eval_to_bool ~default:false
            (OpamPackageVar.resolve_switch_raw ~package gt
               (OpamSwitch.of_dirname dir)
               OpamFile.Switch_config.empty)
            (OpamFile.OPAM.available opam))
        opams
      |> OpamPackage.keys
    ) in
    let open OpamPackage.Set.Op in
    { virt_st with
      opams =
        OpamPackage.Set.fold (fun nv acc ->
            OpamPackage.Map.add nv (OpamFile.OPAM.create nv) acc)
          pin_depends opams;
      packages =
         virt_st.packages ++ local_packages ++ pin_depends;
      available_packages =
        lazy (Lazy.force available ++ local_packages ++ pin_depends);
    }
  in
  let univ =
    OpamSwitchState.universe virt_st
      ~requested:(OpamPackage.names_of_packages local_packages)
      Query
  in
  (* Find if there is a single possible dependency having Pkgflag_Compiler *)
  let alldeps =
    OpamSolver.dependencies
      ~depopts:false ~build:true ~post:true ~installed:false
      univ local_packages
  in
  let compilers =
    OpamPackage.Set.filter (fun nv ->
        OpamFile.OPAM.has_flag Pkgflag_Compiler
          (OpamSwitchState.opam virt_st nv))
      (OpamPackage.Set.of_list alldeps)
  in
  let installable =
    OpamSolver.installable_subset
      {univ with u_base = local_packages; u_installed = local_packages}
      (OpamPackage.Set.union local_packages compilers)
  in
  if OpamPackage.Set.is_empty installable then
    (OpamConsole.error
       "The following local packages don't appear to be installable:\n%s"
       (OpamStd.Format.itemize OpamPackage.to_string
          (OpamPackage.Set.elements local_packages));
     if OpamConsole.confirm "Do you want to create an empty switch nonetheless ?"
     then []
     else OpamStd.Sys.exit_because `Aborted)
  else
  let compilers = OpamPackage.Set.inter compilers installable in
  try
    [OpamSolution.eq_atom_of_package
       (OpamPackage.Set.choose_one compilers)]
  with
  | Not_found ->
    OpamConsole.warning
      "No possible installation was found including a compiler and the \
       selected packages.";
    if OpamClientConfig.(!r.show) ||
       OpamConsole.confirm
         "Create the switch with no specific compiler selected, and attempt to \
          continue anyway ?"
    then []
    else OpamStd.Sys.exit_because `Aborted
 | Failure _ ->
   (* Find a matching compiler from the default selection *)
   let default_compiler =
     OpamFile.Config.default_compiler gt.config
   in
   if default_compiler = Empty then
     (OpamConsole.warning "No compiler selected"; [])
   else
   let candidates = OpamFormula.to_dnf default_compiler in
   try
     List.find
       (fun atoms ->
          OpamSolver.atom_coinstallability_check univ
            (local_atoms @ atoms))
       candidates
   with Not_found ->
      OpamConsole.warning
        "The default compiler selection: %s\n\
         is not compatible with the local packages found at %s, and the \
         packages don't specify an unambiguous compiler.\n\
         You can use `--compiler` to manually select one."
        (OpamFormula.to_string default_compiler)
        (OpamFilename.Dir.to_string dir);
      if OpamConsole.confirm
          "Continue anyway, with no specific compiler selected ?"
      then []
      else OpamStd.Sys.exit_because `Aborted

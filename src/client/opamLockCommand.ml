(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes


let select_packages atom_locs st =
  let st, atoms =
    OpamAuxCommands.simulate_autopin ~quiet:true ~for_view:true st atom_locs
  in
  let packages =
    OpamFormula.packages_of_atoms OpamPackage.Set.Op.(st.packages ++ st.installed) atoms
  in
  if OpamPackage.Set.is_empty packages then
    OpamConsole.error_and_exit `Not_found "No package matching %s"
      (OpamFormula.string_of_atoms atoms)
  else
    (let names = OpamPackage.names_of_packages packages in
     let missing =
       OpamStd.List.filter_map (fun (n,vc) ->
           if OpamPackage.Name.Set.mem n names then None
           else Some (n,vc)) atoms
     in
     if missing <> [] then
       OpamConsole.error "No package matching %s"
         (OpamFormula.string_of_atoms missing);
     (* we keep only one version of each package, the pinned or installed one,
        the latest version otherwise ; and the one that have their dependencies \
        installed *)
     let packages =
       OpamPackage.Name.Set.fold (fun name acc ->
           let pkgs = OpamPackage.packages_of_name packages name in
           let pkg, is_pinned =
             let open OpamPackage.Set.Op in
             let pinned = pkgs %% st.pinned in
             if OpamPackage.Set.is_empty pinned then
               pkgs %% st.installed, false
             else pinned, true
           in
           let nv =
             match OpamPackage.Set.elements pkg with
             | [nv] -> nv
             | _ ->
               (let nv = OpamPackage.Set.max_elt pkgs in
                OpamConsole.note "Package %s is not installed nor pinned, generating lock \
                                  file for its latest version %s"
                  (OpamConsole.colorise `underline (OpamPackage.Name.to_string name))
                  (OpamConsole.colorise `underline (OpamPackage.version_to_string nv));
                nv)
           in
           let opam =
             if is_pinned then
               let open OpamStd.Option.Op in
               match
                 OpamSwitchState.opam st nv
                 |> OpamFile.OPAM.url
                 >>| OpamFile.URL.url
                 >>= OpamUrl.local_dir
                 >>= OpamPinned.find_opam_file_in_source name
                 >>| fst
                 >>| OpamFile.OPAM.read
               with
               | Some opam ->
                 (* we add the name/version because of an [OpamFile.OPAM.package] in all depends *)
                 let opam =
                   if OpamFile.OPAM.name_opt opam = None then
                     OpamFile.OPAM.with_name name opam
                   else opam
                 in
                 if OpamFile.OPAM.version_opt opam  = None then
                   OpamFile.OPAM.with_version (OpamPackage.version nv) opam
                 else opam
               | None -> OpamSwitchState.opam st nv
             else OpamSwitchState.opam st nv
           in
           let atoms =
             OpamFormula.atoms
               (OpamPackageVar.all_depends ~depopts:false st opam)
           in
           let missing =
             List.filter (fun (n,vc) ->
                 let pkgs =
                   OpamPackage.packages_of_name
                     (OpamPackage.Set.union st.installed packages)
                     n
                 in
                 OpamPackage.Set.is_empty pkgs ||
                 not (OpamPackage.Set.exists (OpamFormula.check (n,vc)) pkgs))
               atoms
           in
           if missing <> [] then
             (OpamConsole.error
                "Skipping %s, dependencies are not satisfied in this switch, \
                 not installed packages are:\n%s"
                (OpamPackage.to_string nv)
                (OpamStd.Format.itemize OpamFormula.string_of_atom missing);
              acc)
           else
             OpamPackage.Set.add nv acc)
         names OpamPackage.Set.empty
     in
     st, packages)

let get_git_url url nv dir =
  let module VCS =
    (val OpamRepository.find_backend_by_kind url.OpamUrl.backend)
  in
  let open OpamProcess.Job.Op in
  OpamProcess.Job.run @@
  VCS.get_remote_url ?hash:url.OpamUrl.hash dir @@| function
  | Some u ->
    (if u.OpamUrl.hash = None then
       OpamConsole.warning
         "Referenced git branch for %s is not available in remote: %s, \
          use default branch instead."
         (OpamConsole.colorise `underline (OpamPackage.to_string nv))
         (OpamUrl.to_string u);
     Some u)
  | _ ->
    (OpamConsole.error "Can't retrieve remote informations for %s"
       (OpamPackage.to_string nv);
     None)

let lock_opam ?(only_direct=false) st opam =
  let nv = OpamFile.OPAM.package opam in
  let univ =
    OpamSwitchState.universe st
      ~requested:(OpamPackage.Set.singleton nv)
      Query
  in
  (* Depends *)
  let all_depends =
    OpamSwitchState.dependencies
      ~depopts:true ~build:true ~post:true ~installed:true
      st univ (OpamPackage.Set.singleton nv) |>
    OpamPackage.Set.remove nv
  in
  let depends =
    if only_direct then
      let names =
        OpamFilter.filter_formula ~default:true (fun _ -> None)
          (OpamFile.OPAM.depends opam) |>
        OpamFormula.fold_left (fun acc (n,_) -> OpamPackage.Name.Set.add n acc)
          OpamPackage.Name.Set.empty
      in
      OpamPackage.packages_of_names all_depends names
    else all_depends
  in
  let map_of_set x set =
    OpamPackage.Map.of_list (List.map (fun nv -> nv, x)
                               (OpamPackage.Set.elements set))
  in
  let depends_map = map_of_set `version depends in
  (* others: dev, test, doc, dev-setup *)
  let open OpamPackage.Set.Op in
  let select ?(build=false) ?(test=false) ?(doc=false) ?(dev_setup=false)
      ?(dev=false) ?(default=false) ?(post=false) () =
    OpamFormula.packages st.packages
      (OpamFilter.filter_deps
         ~build ~test ~doc ~dev ~dev_setup ~default ~post
         (OpamFile.OPAM.depends opam))
  in
  let default = select () in
  let select_depends typ selection =
    let depends = selection -- default in
    let installed = depends %% st.installed in
    let uninstalled =
      OpamPackage.(Name.Set.diff
                     (names_of_packages depends)
                     (names_of_packages installed))
    in
    if OpamPackage.Name.Set.is_empty uninstalled then
      let depends_map = map_of_set `other installed in
      if only_direct then depends_map
      else
        (OpamSwitchState.dependencies
           ~depopts:false ~build:true ~post:true ~installed:true
           st univ installed
         -- all_depends)
        |> map_of_set (`other_dep typ)
        |> OpamPackage.Map.union (fun _v _o -> `other_dep typ) depends_map
    else
      (OpamConsole.msg "Not all dependencies of %s are satisfied, not \
                        including these: %s\n"
         (OpamPackage.to_string nv)
         (OpamStd.List.concat_map ", " OpamPackage.Name.to_string
            (OpamPackage.Name.Set.elements uninstalled));
       OpamPackage.Map.empty)
  in
  (* variables are set here as a string *)
  let dev_depends_map =
    select_depends "dev" (select ~dev:true ~build:true () -- select ~build:true ())
  in
  let test_depends_map = select_depends "with-test" (select ~test:true ()) in
  let doc_depends_map = select_depends "with-doc" (select ~doc:true ()) in
  let dev_setup_depends_map =
    select_depends "with-dev-setup" (select ~dev_setup:true ())
  in
  let depends =
    let f a b =
      match a,b with
      | _, (`other_dep _ as ot)
      | (`other_dep _ as ot), _ -> ot
      | _, `other
      | `other, _ -> `other
      |  `version, `version -> `version
    in
    OpamPackage.Map.(
      depends_map
      |> union f dev_depends_map
      |> union f test_depends_map
      |> union f doc_depends_map
      |> union f dev_setup_depends_map
    )
  in
  (* formulas *)
  let filters =
    OpamFormula.fold_left (fun acc form ->
        let n, vc = form in
        OpamPackage.Name.Map.add n vc acc)
      OpamPackage.Name.Map.empty (OpamFile.OPAM.depends opam)
  in
  let depends_formula =
    OpamFormula.ands
      (List.rev (OpamPackage.Map.fold (fun nv cst acc ->
           let filter =
             let cst_v =
               Atom (
                 Constraint (`Eq, FString (OpamPackage.version_to_string nv)))
             in
             match cst with
             | `version -> cst_v
             | `other_dep typ ->
               And (cst_v,
                    Atom (Filter
                            (FIdent ([],
                                     OpamVariable.of_string typ, None))))
             | `other ->
               let orig_deps_formula =
                 OpamPackage.Name.Map.find (OpamPackage.name nv) filters
               in
               let new_formula =
                 OpamFormula.map (function
                     | Constraint _ -> cst_v
                     | Filter _ as f ->  Atom f
                   ) orig_deps_formula
               in
               if new_formula = orig_deps_formula then
                 And (cst_v, new_formula)
               else new_formula
           in
           Atom (nv.name, filter)::acc)
           depends []))
  in
  (* keep installed depopts in depends and set as conflicting uninstalled ones *)
  let all_depopts =
    OpamFormula.packages st.packages
      (OpamFilter.filter_deps
         ~build:true ~test:true ~doc:true ~dev_setup:true ~default:true
         ~post:false (OpamFile.OPAM.depopts opam))
  in
  let installed_depopts = OpamPackage.Set.inter all_depopts st.installed in
  let uninstalled_depopts =
    OpamPackage.(Name.Set.diff
                   (names_of_packages all_depopts)
                   (names_of_packages installed_depopts))
  in
  let conflicts =
    OpamFormula.ors
      (OpamFile.OPAM.conflicts opam ::
       List.map (fun n -> Atom (n, Empty))
         (OpamPackage.Name.Set.elements uninstalled_depopts))
  in
  let pin_depends =
    OpamPackage.Set.fold (fun nv acc ->
        if not (OpamSwitchState.is_pinned st nv.name) then acc else
        match OpamSwitchState.primary_url st nv with
        | None -> acc
        | Some u ->
          match OpamUrl.local_dir u with
          | Some d ->
            let local_warn () =
              OpamConsole.warning "Dependency %s is pinned to local target %s"
                (OpamPackage.to_string nv) (OpamUrl.to_string u);
              acc
            in
            (match u.OpamUrl.backend with
             | #OpamUrl.version_control ->
               (match get_git_url u nv d with
                | Some resolved_u ->
                  OpamConsole.note "Local pin %s resolved to %s"
                    (OpamUrl.to_string u) (OpamUrl.to_string resolved_u);
                  (nv, resolved_u) :: acc
                | None -> local_warn ())
             | _ -> local_warn ())
          | None -> (nv, u) :: acc)
      all_depends []
    |> List.rev
  in
  opam |>
  OpamFile.OPAM.with_depopts OpamFormula.Empty |>
  OpamFile.OPAM.with_depends depends_formula |>
  OpamFile.OPAM.with_conflicts conflicts |>
  OpamFile.OPAM.with_pin_depends pin_depends

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
open OpamStateTypes
open OpamTypesBase
open OpamProcess.Job.Op
open OpamFilename.Op
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "UPDATE" fmt
let slog = OpamConsole.slog

let fetch_dev_package url srcdir nv =
  let remote_url = OpamFile.URL.url url in
  let mirrors = remote_url :: OpamFile.URL.mirrors url in
  let checksum = OpamFile.URL.checksum url in
  log "updating %a" (slog OpamUrl.to_string) remote_url;
  let text =
    OpamProcess.make_command_text
      (OpamPackage.Name.to_string (OpamPackage.name nv))
      (OpamUrl.string_of_backend remote_url.OpamUrl.backend) in
  OpamProcess.Job.with_text text @@
  OpamRepository.pull_url nv srcdir checksum mirrors
  @@| function
  | Not_available _ ->
    (* OpamConsole.error "Upstream %s of %s is unavailable" u *)
    (*   (OpamPackage.to_string nv); *)
    false
  | Up_to_date _    -> false
  | Result _        -> true


(* Used to get the different versions of metadata for pinned packages.
   Returns [opam, descr_file, files_dir]. We don't consider [url] since
   this is for pinned packages. if [root], don't look for a subdir [opam]
   to find [files] and [descr]. *)
let local_opam ?(root=false) ?fixed_version ?(check=false) ?copy_invalid_to
    name dir =
  let metadir =
    if root then dir else
    match
      OpamFilename.opt_dir (dir / (OpamPackage.Name.to_string name ^ ".opam"))
    with
    | Some d -> d
    | None -> dir / "opam"
  in
  let opam_file =
    if root then OpamFilename.opt_file (dir // "opam")
    else OpamPinned.find_opam_file_in_source name dir
  in
  let opam_opt = match opam_file with
    | None -> None
    | Some local_opam ->
      let warns, opam_opt = OpamFile.OPAM.validate_file local_opam in
      if check && warns <> [] then
        (OpamConsole.warning
           "%s opam file from upstream of %s (fix with 'opam pin edit'):"
           (if opam_opt = None then "Fatal errors, not using"
            else "Failed checks in")
           (OpamConsole.colorise `bold (OpamPackage.Name.to_string name));
         OpamConsole.errmsg "%s\n"
           (OpamFile.OPAM.warns_to_string warns));
      (match opam_opt, copy_invalid_to with
       | None, Some dst ->
         if not check then
           OpamConsole.warning
             "Errors in opam file from %s upstream, ignored (fix with \
              'opam pin edit')"
             (OpamPackage.Name.to_string name);
         OpamFilename.copy ~src:local_opam ~dst:dst
       | _ -> ());
      OpamStd.Option.map
        (fun opam ->
           let opam = OpamFile.OPAM.with_name opam name in
           let opam = match fixed_version with
             | None -> opam
             | Some v -> OpamFile.OPAM.with_version opam v
           in
           opam)
        opam_opt
  in
  OpamStd.Option.Op.(
    (opam_opt >>| fun o -> OpamFile.OPAM.with_descr_opt o None),
    (OpamFilename.opt_file (metadir // "descr") >>| OpamFile.Descr.read) ++
    (opam_opt >>= OpamFile.OPAM.descr),
    OpamFilename.opt_dir (metadir / "files")
  )

(* todo: implement a separate diff2, diff3 for package metadata with either all
   data, or only recompilation-triggering changes: this could be used also after
   normal update *)
(* !X rewrite to better use switch_state rather than reload overlay *)
let pinned_package st ?fixed_version name =
  let overlay = OpamPath.Switch.Overlay.package st.switch name in
  let url_f = OpamPath.Switch.Overlay.url st.switch name in
  if not (OpamFilename.exists url_f) then Done false else
  let url = OpamFile.URL.read url_f in
  let srcdir = OpamPath.Switch.dev_package st.switch name in
  let pinning_kind =
    kind_of_pin_option (snd (OpamPackage.Name.Map.find name st.pinned)) in
  (* Four versions of the metadata: from the old and new versions
     of the package, from the current overlay, and also the original one
     from the repo *)
  let hash_meta (opam, descr, files_dir) =
    (match opam with None -> [] | Some o ->
      ["opam",
       `Opam OpamFile.OPAM.(with_metadata_dir (with_url_opt o None) None)]) @
    (match descr with None -> [] | Some d ->
      ["descr", `Digest (Digest.string (OpamFile.Descr.full d))]) @
    (match files_dir with None -> [] | Some files_dir ->
      List.map (fun f ->
          OpamFilename.remove_prefix (OpamFilename.dirname_dir files_dir) f,
          `Digest (OpamFilename.digest f))
        (OpamFilename.rec_files files_dir))
  in
  let old_meta = (* Version previously present in the source *)
    if pinning_kind = `version then [] else
      hash_meta @@ local_opam ?fixed_version name srcdir
  in
  let old_opam_file =
    try Some (List.find OpamFilename.exists
                [srcdir // "opam"; srcdir / "opam" // "opam"])
    with Not_found -> None
  in
  let was_single_opam_file = (old_opam_file = Some (srcdir // "opam")) in
  let old_opam_digest =
    OpamStd.Option.map OpamFilename.digest old_opam_file
  in
  let just_opam = List.filter (function (_, `Opam _) -> true | _ -> false) in
  let user_meta, empty_user_meta, user_version =
    (* Installed version (overlay) *)
    let opam,_,_ as files = local_opam ~root:true ?fixed_version name overlay in
    hash_meta files,
    (match opam with
     | Some o ->
       OpamFile.OPAM.(empty = with_name_opt (with_version_opt o None) None)
     | None -> true),
    OpamStd.Option.map OpamFile.OPAM.version opam
  in
  let repo_meta = (* Version from the repo *)
    let nv_opam =
      let packages =
        OpamPackage.Map.filter (fun nv _ -> OpamPackage.name nv = name)
          st.switch_repos.repo_opams
      in
      match user_version with
      | None ->
        (try Some (OpamPackage.Map.max_binding packages) with
         | Not_found -> None)
      | Some v ->
        let nv = OpamPackage.create name v in
        (* get the latest version below v *)
        match OpamPackage.Map.split nv packages with
        | _, (Some opam), _ -> Some (nv, opam)
        | below, None, _ when not (OpamPackage.Map.is_empty below) ->
          Some (OpamPackage.Map.max_binding below)
        | _, None, above when not (OpamPackage.Map.is_empty above) ->
          Some (OpamPackage.Map.min_binding above)
        | _ -> None
    in
    let meta = match nv_opam with
      | None ->
        Some (OpamFile.OPAM.with_name OpamFile.OPAM.empty name), None, None
      | Some (_, opam) ->
        Some (OpamFile.OPAM.with_version_opt
                (OpamFile.OPAM.with_descr_opt opam None)
                user_version),
        OpamFile.OPAM.descr opam,
        OpamStd.Option.Op.(
          OpamFile.OPAM.metadata_dir opam >>|
          fun d -> OpamFilename.Op.(d / "files")
        )
    in
    hash_meta @@ meta
  in
  let fake_nv = OpamPackage.create name (OpamPackage.Version.of_string "") in
  (* Do the update *)
  fetch_dev_package url srcdir fake_nv @@+ fun result ->
  let check = (* only on upstream changes *)
    try
      old_opam_digest <> Some (
        OpamFilename.digest
          (List.find OpamFilename.exists
             [srcdir // "opam"; srcdir / "opam" // "opam"]))
    with Not_found -> false
  in
  let new_meta = (* New version from the source *)
    hash_meta @@
    local_opam ?fixed_version
      ~check
      ~copy_invalid_to:(OpamPath.Switch.Overlay.tmp_opam st.switch name)
      name srcdir
  in
  let user_meta, old_meta, repo_meta =
    match OpamPinned.find_opam_file_in_source name srcdir with
    | Some f when OpamFilename.dirname f = srcdir ->
      (* Single opam file directly at the project root:
         don't override other files, restrict to 'opam' *)
      just_opam user_meta, just_opam old_meta, just_opam repo_meta
    | _ -> user_meta, old_meta, repo_meta
  in
  let rec diff a b = match a,b with
    | (f1,h1)::r1, (f2,h2)::r2 ->
      if f1 < f2 then `Removed f1 :: diff r1 b
      else if f1 > f2 then `Added f2 :: diff a r2
      else if h1 = h2 then diff r1 r2
      else `Changed f1 :: diff r1 r2
    | l, [] -> List.map (fun (f,_) -> `Removed f) l
    | [], l -> List.map (fun (f,_) -> `Added f) l
  in
  let diff_to_string = function
    | `Removed f -> Printf.sprintf "%S was removed" f
    | `Added f -> Printf.sprintf "%S was added" f
    | `Changed f -> Printf.sprintf "The contents of %S changed" f
  in
  let install_meta dir rm_hash hash =
    let root =
      let d = dir / (OpamPackage.Name.to_string name ^ ".opam") in
      if OpamFilename.exists_dir d then d else
      let d = dir / "opam" in
      if OpamFilename.exists_dir d then d else
        dir
    in
    List.iter (fun (f, _) -> OpamFilename.remove (overlay // f)) rm_hash;
    List.iter (fun (f,kind) -> match kind with
        | `Opam o ->
          let vo =
            OpamStd.Option.Op.(OpamFile.OPAM.version_opt o ++ user_version)
          in
          OpamFile.OPAM.write (overlay // f)
            (OpamFile.OPAM.with_version_opt o vo)
        | `Digest _ -> OpamFilename.copy_in ~root (root // f) overlay)
      hash
  in
  (* Metadata from the package changed *)
  if result && new_meta <> [] &&
     new_meta <> old_meta && new_meta <> user_meta
  then
    if old_meta = user_meta || repo_meta = user_meta || empty_user_meta ||
       was_single_opam_file && old_meta = just_opam user_meta
    then
      (* No manual changes *)
      (OpamConsole.formatted_msg
         "[%s] Installing new package description from %s\n"
         (OpamConsole.colorise `green (OpamPackage.Name.to_string name))
         (OpamUrl.to_string (OpamFile.URL.url url));
       OpamFilename.remove
         (OpamPath.Switch.Overlay.tmp_opam st.switch name);
       install_meta srcdir user_meta new_meta)
    else if
      OpamConsole.formatted_msg
        "[%s] Conflicting update of the metadata from %s:\n%s"
        (OpamConsole.colorise `green (OpamPackage.Name.to_string name))
        (OpamUrl.to_string (OpamFile.URL.url url))
        (OpamStd.Format.itemize diff_to_string (diff user_meta new_meta));
      OpamConsole.confirm "\nOverride files in %s (there will be a backup) ?"
        (OpamFilename.Dir.to_string overlay)
    then (
      let bak =
        OpamPath.backup_dir () / (OpamPackage.Name.to_string name ^ ".bak") in
      OpamFilename.mkdir (OpamPath.backup_dir ());
      OpamFilename.rmdir bak;
      OpamFilename.copy_dir ~src:overlay ~dst:bak;
      OpamConsole.formatted_msg "User metadata backed up in %s\n"
        (OpamFilename.Dir.to_string bak);
      install_meta srcdir user_meta new_meta;
    );
  Done result

let dev_package st nv =
  log "update-dev-package %a" (slog OpamPackage.to_string) nv;
  let name = OpamPackage.name nv in
  if OpamPinned.package_opt st name = Some nv then
    pinned_package st name
  else
  match OpamSwitchState.url st nv with
  | None     -> Done false
  | Some url ->
    if (OpamFile.URL.url url).OpamUrl.backend = `http then Done false else
      fetch_dev_package url (OpamPath.dev_package nv) nv

let dev_packages st packages =
  log "update-dev-packages";
  let command nv =
    OpamProcess.Job.ignore_errors ~default:OpamPackage.Set.empty @@
    dev_package st nv @@| function
    | true -> OpamPackage.Set.singleton nv
    | false -> OpamPackage.Set.empty
  in
  let updates =
    OpamParallel.reduce ~jobs:OpamStateConfig.(!r.dl_jobs)
      ~command
      ~merge:OpamPackage.Set.union
      ~nil:OpamPackage.Set.empty
      (OpamPackage.Set.elements packages)
  in
  let pinned =
    OpamPackage.Set.filter
      (fun nv -> OpamPackage.Name.Map.mem (OpamPackage.name nv) st.pinned)
      packages
  in
  OpamSwitchAction.add_to_reinstall st.switch_global st.switch
    (OpamSwitchState.state_file st)
    ~unpinned_only:false updates;
  let unpinned_updates = updates -- pinned in
  OpamGlobalState.fold_switches (fun switch state_file () ->
      if switch <> st.switch then
        OpamSwitchAction.add_to_reinstall st.switch_global switch state_file
          ~unpinned_only:true unpinned_updates)
    st.switch_global ();
  updates

let pinned_packages st names =
  log "update-pinned-packages";
  let command name =
    OpamProcess.Job.ignore_errors ~default:OpamPackage.Name.Set.empty @@
    pinned_package st name @@| function
    | true -> OpamPackage.Name.Set.singleton name
    | false -> OpamPackage.Name.Set.empty
  in
  let updates =
    OpamParallel.reduce
      ~jobs:(OpamFile.Config.jobs st.switch_global.config)
      ~command
      ~merge:OpamPackage.Name.Set.union
      ~nil:OpamPackage.Name.Set.empty
      (OpamPackage.Name.Set.elements names)
  in
  let updates =
    OpamPackage.Name.Set.fold (fun name acc ->
        OpamPackage.Set.add (OpamPinned.package st name) acc)
      updates OpamPackage.Set.empty
  in
  OpamSwitchAction.add_to_reinstall st.switch_global st.switch
    (OpamSwitchState.state_file st) ~unpinned_only:false updates;
  updates

(* Download a package from its upstream source, using 'cache_dir' as cache
   directory. *)
let download_upstream st nv dirname =
  match OpamSwitchState.url st nv with
  | None   -> Done None
  | Some u ->
    let remote_url = OpamFile.URL.url u in
    let mirrors = remote_url :: OpamFile.URL.mirrors u in
    let checksum = OpamFile.URL.checksum u in
    let text =
      OpamProcess.make_command_text (OpamPackage.name_to_string nv)
        (OpamUrl.string_of_backend remote_url.OpamUrl.backend)
    in
    OpamProcess.Job.with_text text @@
    OpamRepository.pull_url nv dirname checksum mirrors
    @@| OpamStd.Option.some

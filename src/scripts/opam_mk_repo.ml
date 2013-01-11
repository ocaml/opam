(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
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

open OpamFilename.OP

let () =
  OpamHTTP.register ();
  OpamGit.register ();
  OpamDarcs.register ();
  OpamLocal.register ()

let log fmt = OpamGlobals.log "OPAM-MK-REPO" fmt

let all, index, packages, gener_digest, dryrun, recurse =
  let usage = Printf.sprintf "%s [-all] [<package>]*" (Filename.basename Sys.argv.(0)) in
  let all = ref true in
  let index = ref false in
  let packages = ref [] in
  let gener_digest = ref false in
  let dryrun = ref false in
  let recurse = ref false in
  let specs = Arg.align [
    ("-v"       , Arg.Unit OpamVersion.message, " Display version information");
    ("--version", Arg.Unit OpamVersion.message, " Display version information");

    ("-a"   , Arg.Set all, "");
    ("--all", Arg.Set all  , Printf.sprintf " Build all package archives (default is %b)" !all);

    ("-i"     , Arg.Set index, "");
    ("--index", Arg.Set index, Printf.sprintf " Build indexes only (default is %b)" !index);

    ("-g"                  , Arg.Set gener_digest, "");
    ("--generate-checksums", Arg.Set gener_digest,
     Printf.sprintf " Automatically correct the wrong archive checksums (default is %b)" !gener_digest);

    ("-d"      , Arg.Set dryrun, "");
    ("--dryrun", Arg.Set dryrun, " Simply display the possible actions instead of executing them");

    ("-r", Arg.Set recurse, " Recurse among the transitive dependencies");
  ] in
  let ano p = packages := p :: !packages in
  Arg.parse specs ano usage;
  !all, !index, !packages, !gener_digest, !dryrun, !recurse

let process () =
  let local_path = OpamFilename.cwd () in
  log "local_path=%s" (OpamFilename.Dir.to_string local_path);

  OpamGlobals.root_dir := OpamFilename.Dir.to_string local_path;

  let mk_packages str =
    let dir = OpamPath.Repository.packages_dir local_path / str in
    if OpamFilename.exists_dir dir then
      let nv = OpamPackage.of_string str in
      OpamPackage.Set.singleton nv
    else
      let n = OpamPackage.Name.of_string str in
      match OpamPackage.Version.Set.elements (OpamRepository.versions local_path n) with
      | []       ->
        OpamGlobals.msg "Skipping unknown package %s.\n" str;
        OpamPackage.Set.empty
      | versions ->
        OpamPackage.Set.of_list (List.map (OpamPackage.create n) versions) in
  let packages =
    List.fold_left
      (fun accu str -> OpamPackage.Set.union accu (mk_packages str))
      OpamPackage.Set.empty packages in

  if OpamPackage.Set.is_empty packages then (
    OpamGlobals.msg "No package to process.\n";
    exit 0
  );

  (* Read urls.txt *)
  log "Reading urls.txt";
  let local_index_file = OpamFilename.of_string "urls.txt" in
  let old_index = OpamFile.Urls_txt.safe_read local_index_file in
  let new_index = OpamHTTP.make_urls_txt local_path in

  let to_remove = OpamFilename.Attribute.Set.diff old_index new_index in
  let to_add = OpamFilename.Attribute.Set.diff new_index old_index in

  (* Compute the transitive closure of packages *)
  let get_dependencies nv =
    let opam_f = OpamPath.Repository.opam local_path nv in
    if OpamFilename.exists opam_f then (
      let opam = OpamFile.OPAM.read opam_f in
      let deps = OpamFile.OPAM.depends opam in
      let depopts = OpamFile.OPAM.depopts opam in
      OpamFormula.fold_left (fun accu (n,_) ->
        OpamPackage.Set.union (mk_packages (OpamPackage.Name.to_string n)) accu
      ) OpamPackage.Set.empty (OpamFormula.And (deps, depopts))
    ) else
      OpamPackage.Set.empty in
  let rec get_transitive_dependencies packages =
    let new_packages =
      OpamPackage.Set.fold
        (fun nv set -> OpamPackage.Set.union (get_dependencies nv) set)
        packages OpamPackage.Set.empty in
    if OpamPackage.Set.cardinal packages = OpamPackage.Set.cardinal new_packages then
      packages
    else
      get_transitive_dependencies new_packages in
  let packages =
    if recurse then
      get_transitive_dependencies packages
    else
      packages in

  let nv_set_of_remotes remotes =
    let aux r = OpamFilename.create (OpamFilename.cwd ()) (OpamFilename.Attribute.base r) in
    let list = List.map aux (OpamFilename.Attribute.Set.elements remotes) in
    OpamPackage.Set.of_list (OpamMisc.filter_map OpamPackage.of_filename list) in
  let new_index = nv_set_of_remotes new_index in
  let missing_archive =
    OpamPackage.Set.filter (fun nv ->
      let archive = OpamPath.Repository.archive local_path nv in
      not (OpamFilename.exists archive)
    ) new_index in
  let to_remove = nv_set_of_remotes to_remove in
  let to_add = OpamPackage.Set.union (nv_set_of_remotes to_add) missing_archive in
  let to_add =
    if OpamPackage.Set.is_empty packages then
      to_add
    else
      OpamPackage.Set.inter packages to_add in
  let to_remove = OpamPackage.Set.diff to_remove to_add in

  let errors = ref [] in
  if not index then (

    (* Remove the old archive files *)
    if not (OpamPackage.Set.is_empty to_remove) then
      OpamGlobals.msg "Packages to remove: %s\n" (OpamPackage.Set.to_string to_remove);
    OpamPackage.Set.iter (fun nv ->
      let archive = OpamPath.Repository.archive local_path nv in
      OpamGlobals.msg "Removing %s ...\n" (OpamFilename.to_string archive);
      if not dryrun then
        OpamFilename.remove archive
    ) to_remove;

    (* build the new archives *)
    if not (OpamPackage.Set.is_empty to_add) then
      OpamGlobals.msg "Packages to build: %s\n" (OpamPackage.Set.to_string to_add);
    OpamPackage.Set.iter (fun nv ->
      let archive = OpamPath.Repository.archive local_path nv in
      try
        if not dryrun then (
          OpamFilename.remove archive;
          OpamRepository.make_archive ~gener_digest nv
        ) else
          OpamGlobals.msg "Building %s\n" (OpamFilename.to_string archive)
      with e ->
        OpamFilename.remove archive;
        errors := (nv, e) :: !errors;
    ) to_add;
  );

  (* Create index.tar.gz *)
  if not (OpamFilename.exists (local_path // "index.tar.gz"))
  || not (OpamPackage.Set.is_empty to_add)
  || not (OpamPackage.Set.is_empty to_remove) then (
    OpamGlobals.msg "Rebuilding index.tar.gz ...\n";
    if not dryrun then
      OpamHTTP.make_index_tar_gz local_path;
  ) else
    OpamGlobals.msg "OPAM Repository already up-to-date.\n";

  OpamSystem.remove "log";
  OpamSystem.remove "tmp";

  (* Rebuild urls.txt now the archives have been updated *)
  if dryrun then
    OpamGlobals.msg "Rebuilding urls.txt\n"
  else
    let _index = OpamHTTP.make_urls_txt local_path in

    if !errors <> [] then
      let display_error (nv, error) =
        OpamGlobals.error "==== ERROR [%s] ====\n" (OpamPackage.to_string nv);
        match error with
        | OpamSystem.Process_error r  -> OpamGlobals.error "%s" (OpamProcess.string_of_result r)
        | OpamSystem.Internal_error s -> OpamGlobals.error "  %s" s
        | _ -> OpamGlobals.error "%s" (Printexc.to_string error) in
      let all_errors = List.map fst !errors in
      OpamGlobals.error "Got some errors while processing: %s"
        (String.concat ", " (List.map OpamPackage.to_string all_errors));
      List.iter display_error !errors

let () =
  try process ()
  with e ->
    Printf.eprintf "%s\n" (Printexc.to_string e)

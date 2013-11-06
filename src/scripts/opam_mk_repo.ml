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
open OpamFilename.OP

let log fmt = OpamGlobals.log "OPAM-MK-REPO" fmt

type args = {
  index: bool;
  names: string list;
  gener_digest: bool;
  dryrun: bool;
  recurse: bool;
  debug: bool;
}

let args =
  let open Cmdliner in
  (*{[
       let all =
         let doc = "Build all package archives (this is the default unless -i)" in
         Arg.(value & flag & info ["a";"all"] ~doc)
       in
     ]}*)
  let index =
    let doc = "Only build indexes, not package archives." in
    Arg.(value & flag & info ["i";"index"] ~doc)
  in
  let gener_digest =
    let doc = "Automatically correct the wrong archive checksums." in
    Arg.(value & flag & info ["g";"generate-checksums"] ~doc)
  in
  let dryrun =
    let doc = "Simply display the possible actions instead of executing them." in
    Arg.(value & flag & info ["d";"dryrun"] ~doc)
  in
  let recurse =
    let doc = "Recurse among the transitive dependencies." in
    Arg.(value & flag & info ["r";"recursive"] ~doc)
  in
  let names =
    let doc = "Names of the packages to include in the repo." in
    Arg.(value & pos_all string [] & info [] ~docv:"PKG" ~doc)
  in
  let debug =
    let doc = "Display debug messages." in
    Arg.(value & flag & info ["debug"] ~doc)
  in
  Term.(
    pure
      (fun index gener_digest dryrun recurse names debug ->
         {index; gener_digest; dryrun; recurse; names; debug})
      $index $gener_digest $dryrun $recurse $names $debug
  )

let process {index; gener_digest; dryrun; recurse; names; debug} =
  let () =
    OpamHTTP.register ();
    OpamGit.register ();
    OpamDarcs.register ();
    OpamLocal.register ();
    OpamHg.register () in
  OpamGlobals.debug := !OpamGlobals.debug || debug;

  let tmp_dirs = [ "tmp"; "log" ] in

  List.iter (fun dir ->
      if Sys.file_exists dir then (
        OpamGlobals.error
          "The subdirectory '%s' already exists in the current directory. \n\
           Please remove or rename it or run %s in a different folder.\n"
          dir Sys.argv.(0);
        exit 1;
      )
    ) tmp_dirs;

  let () =
    let checkdir dir = Sys.file_exists dir && Sys.is_directory dir in
    if not (checkdir "packages" || checkdir "compilers") then
      OpamGlobals.error_and_exit
        "No repository found in current directory.\n\
         Please make sure there is a \"packages\" or \"compilers\" directory" in

  let repo = OpamRepository.local (OpamFilename.cwd ()) in

  let prefixes = OpamRepository.packages_with_prefixes repo in
  let packages = OpamRepository.packages repo in

  let mk_packages str =
    match OpamPackage.of_string_opt str with
    | Some nv -> OpamPackage.Set.singleton nv
    | None    ->
      let n = OpamPackage.Name.of_string str in
      match OpamPackage.Version.Set.elements
              (OpamPackage.versions_of_name packages n)
      with
      | []       ->
        OpamGlobals.msg "Skipping unknown package %s.\n" str;
        OpamPackage.Set.empty
      | versions ->
        OpamPackage.Set.of_list (List.map (OpamPackage.create n) versions) in
  let new_packages =
    List.fold_left
      (fun accu str -> OpamPackage.Set.union accu (mk_packages str))
      OpamPackage.Set.empty names in

  if names <> [] && OpamPackage.Set.is_empty new_packages then (
    OpamGlobals.msg "No package to process.\n";
    exit 0
  );

  (* Read urls.txt *)
  log "Reading urls.txt";
  let local_index_file = OpamFilename.of_string "urls.txt" in
  let old_index = OpamFile.File_attributes.safe_read local_index_file in
  let new_index = OpamHTTP.make_urls_txt ~write:(not dryrun) repo.repo_root in
  let to_remove = OpamFilename.Attribute.Set.diff old_index new_index in
  let to_add = OpamFilename.Attribute.Set.diff new_index old_index in

  (* Compute the transitive closure of packages *)
  let get_dependencies nv =
    let prefix = OpamPackage.Map.find nv prefixes in
    let opam_f = OpamPath.Repository.opam repo prefix nv in
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
        (fun nv set ->
           if OpamPackage.Set.mem nv set then
             set
           else
             OpamPackage.Set.(add nv (union (get_dependencies nv) set)))
        packages OpamPackage.Set.empty in
    if OpamPackage.Set.cardinal packages = OpamPackage.Set.cardinal new_packages then
      packages
    else
      get_transitive_dependencies new_packages in
  let packages =
    if recurse then
      get_transitive_dependencies new_packages
    else
      new_packages in

  let nv_set_of_remotes remotes =
    let aux r =
      OpamFilename.create (OpamFilename.cwd ()) (OpamFilename.Attribute.base r) in
    let list = List.map aux (OpamFilename.Attribute.Set.elements remotes) in
    OpamPackage.Set.of_list (OpamMisc.filter_map OpamPackage.of_filename list) in

  let packages_of_attrs attrs =
    OpamFilename.Attribute.Set.fold (fun attr nvs ->
        let f =
          OpamFilename.raw_dir
            (OpamFilename.Base.to_string (OpamFilename.Attribute.base attr))
        in
        let path = OpamFilename.to_list_dir f in
        let rec get_nv = function
          | [_] | [] -> nvs
          | pkgdir::(f::_ as subpath)-> match OpamPackage.of_dirname pkgdir with
            | None -> get_nv subpath
            | Some nv -> match OpamFilename.Dir.to_string f with
              | "url" | "files" -> OpamPackage.Set.add nv nvs
              | _ -> nvs
        in
        get_nv path)
      attrs OpamPackage.Set.empty in
  let new_index = nv_set_of_remotes new_index in
  let missing_archive =
    OpamPackage.Set.filter (fun nv ->
        let archive = OpamPath.Repository.archive repo nv in
        not (OpamFilename.exists archive)
      ) new_index in
  let to_add =
    let added_changed = packages_of_attrs to_add in
    let files_removed = OpamPackage.Set.inter new_index (packages_of_attrs to_remove) in
    let (++) = OpamPackage.Set.union in
    missing_archive ++ added_changed  ++ files_removed in
  let to_remove = nv_set_of_remotes to_remove in
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
        let archive = OpamPath.Repository.archive repo nv in
        OpamGlobals.msg "Removing %s ...\n" (OpamFilename.to_string archive);
        if not dryrun then
          OpamFilename.remove archive
      ) to_remove;

    (* build the new archives *)
    if not (OpamPackage.Set.is_empty to_add) then
      OpamGlobals.msg "Packages to build: %s\n" (OpamPackage.Set.to_string to_add);
    OpamPackage.Set.iter (fun nv ->
        let prefix = OpamPackage.Map.find nv prefixes in
        let local_archive = OpamPath.Repository.archive repo nv in
        try
          OpamGlobals.msg "Building %s\n" (OpamFilename.to_string local_archive);
          if not dryrun then (
            OpamFilename.remove local_archive;
            OpamRepository.make_archive ~gener_digest repo prefix nv;
          )
        with e ->
          OpamFilename.remove local_archive;
          errors := (nv, e) :: !errors;
      ) to_add;
  );

  (* Create index.tar.gz *)
  if not (OpamFilename.exists (repo.repo_root // "index.tar.gz"))
  || not (OpamPackage.Set.is_empty to_add)
  || not (OpamPackage.Set.is_empty to_remove) then (
    OpamGlobals.msg "Rebuilding index.tar.gz ...\n";
    if not dryrun then (
      OpamHTTP.make_index_tar_gz repo.repo_root;
    )
  ) else
    OpamGlobals.msg "OPAM Repository already up-to-date.\n";

  if not dryrun then
    List.iter OpamSystem.remove tmp_dirs;

  (* Rebuild urls.txt now the archives have been updated *)
  OpamGlobals.msg "Rebuilding urls.txt\n";
  let _index = OpamHTTP.make_urls_txt ~write:(not dryrun) repo.repo_root in
  if !errors <> [] then (
    let display_error (nv, error) =
      let disp = OpamGlobals.header_error "%s" (OpamPackage.to_string nv) in
      match error with
      | OpamSystem.Process_error r  ->
        disp "%s" (OpamProcess.string_of_result ~color:`red r)
      | OpamSystem.Internal_error s -> OpamGlobals.error "  %s" s
      | _ -> disp "%s" (Printexc.to_string error) in
    let all_errors = List.map fst !errors in
    OpamGlobals.error "Got some errors while processing: %s"
      (String.concat ", " (List.map OpamPackage.to_string all_errors));
    List.iter display_error !errors
  )

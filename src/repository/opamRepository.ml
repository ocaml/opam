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
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "REPOSITORY" fmt
let slog = OpamConsole.slog


let find_backend_by_kind = function
  | `http -> (module OpamHTTP.B: OpamRepositoryBackend.S)
  | `local -> (module OpamLocal.B: OpamRepositoryBackend.S)
  | `git -> (module OpamGit.B: OpamRepositoryBackend.S)
  | `hg -> (module OpamHg.B: OpamRepositoryBackend.S)
  | `darcs -> (module OpamDarcs.B: OpamRepositoryBackend.S)

let find_backend r = find_backend_by_kind r.repo_kind

(* initialize the current directory *)
let init repo =
  log "init %a" (OpamConsole.slog OpamRepositoryBackend.to_string) repo;
  (* let module B = (val find_backend repo: OpamRepositoryBackend.S) in *)
  OpamFilename.rmdir repo.repo_root;
  OpamFilename.mkdir repo.repo_root;
  OpamFile.Repo_config.write (OpamRepositoryPath.config repo) repo;
  OpamFilename.mkdir (OpamRepositoryPath.packages_dir repo);
  OpamFilename.mkdir (OpamRepositoryPath.archives_dir repo);
  OpamFilename.mkdir (OpamRepositoryPath.compilers_dir repo);
  Done ()

let pull_url kind package local_dirname checksum remote_url =
  if OpamRepositoryConfig.(!r.force_checksums = Some true) && checksum = None
  then
    OpamConsole.error_and_exit
      "Checksum required for %s, but not found in package description.\n\
       This may be due to an outdated package description, try running `opam update`.\n\
       In case an update does not fix the problem, you can bypass the check by not\n\
       passing the `--require-checksums` command line option."
      (OpamPackage.to_string package);
  let pull url =
    let module B = (val find_backend_by_kind kind: OpamRepositoryBackend.S) in
    B.pull_url package local_dirname checksum url in
  let rec attempt = function
    | [] -> assert false
    | [url] -> pull url
    | url::mirrors ->
      pull url @@+ function
      | Not_available s ->
        OpamConsole.warning "download of %s failed, trying mirror" s;
        attempt mirrors
      | r -> Done r
  in
  attempt remote_url

let revision repo =
  let kind = repo.repo_kind in
  let module B = (val find_backend_by_kind kind: OpamRepositoryBackend.S) in
  B.revision repo

let pull_url_and_fix_digest kind package dirname checksum file url =
  pull_url kind package dirname None url @@+ function
  | Not_available _
  | Up_to_date _
  | Result (D _) as r -> Done r
  | Result (F f) as r ->
    let actual = OpamFilename.digest f in
    if checksum <> actual then (
      OpamConsole.msg
        "Fixing wrong checksum for %s: current value is %s, setting it to %s.\n"
        (OpamPackage.to_string package) checksum actual;
      let u = OpamFile.URL.read file in
      OpamFile.URL.write file (OpamFile.URL.with_checksum u actual)
    );
    Done r

let pull_archive repo nv =
  let module B = (val find_backend_by_kind repo.repo_kind: OpamRepositoryBackend.S) in
  let filename = OpamRepositoryPath.remote_archive repo nv in
  B.pull_archive repo filename

let check_version repo =
  let repo_version =
    repo
    |> OpamRepositoryPath.repo
    |> OpamFile.Repo.safe_read
    |> OpamFile.Repo.opam_version
  in
  if not OpamFormatConfig.(!r.skip_version_checks) &&
     OpamVersion.compare repo_version OpamVersion.current > 0 then
    OpamConsole.error_and_exit
      "The current version of OPAM cannot read the repository %S. \n\
       You should upgrade to at least version %s.\n"
      (OpamRepositoryName.to_string repo.repo_name)
      (OpamVersion.to_string repo_version)
  else Done ()
(* XXX unused ?
let extract_prefix repo dir nv =
  let prefix =
    let prefix = OpamFilename.Dir.to_string (OpamRepositoryPath.packages_dir repo) in
    prefix ^ Filename.dir_sep in
  let suffix =
    let suffix = OpamPackage.to_string nv in
    Filename.dir_sep ^ suffix in
  let dir = OpamFilename.Dir.to_string dir in
  OpamStd.String.remove_prefix ~prefix (OpamStd.String.remove_suffix ~suffix dir)
*)
let file f =
  if OpamFilename.exists f then [f] else []

let dir d =
  if OpamFilename.exists_dir d then OpamFilename.rec_files d else []

(* Compiler updates *)

let compilers_with_prefixes r =
  OpamCompiler.prefixes (OpamRepositoryPath.compilers_dir r)

let compilers repo =
OpamCompiler.list (OpamRepositoryPath.compilers_dir repo)

let compiler_files repo prefix c =
  let comp = OpamRepositoryPath.compiler_comp repo prefix c in
  let descr = OpamRepositoryPath.compiler_descr repo prefix c in
  file comp @ file descr

let compiler_state repo prefix c =
  let fs = compiler_files repo prefix c in
  List.flatten (List.map OpamFilename.checksum fs)

let packages r =
  OpamPackage.list (OpamRepositoryPath.packages_dir r)

let packages_with_prefixes r =
  OpamPackage.prefixes (OpamRepositoryPath.packages_dir r)

(* Returns the meaningful checksum of a url file. Uses the hash of the remote
   archive if present, or its address, not the hash of the url file itself which
   doesn't really matter *)
let url_checksum url =
  let u = OpamFile.URL.safe_read url in
  if u = OpamFile.URL.empty then []
  else match OpamFile.URL.checksum u with
    | Some cksum -> [cksum]
    | None ->
      [Digest.string (string_of_address (OpamFile.URL.url u))]

let package_files repo prefix nv ~archive =
  let opam = OpamRepositoryPath.opam repo prefix nv in
  let descr = OpamRepositoryPath.descr repo prefix nv in
  let url = OpamRepositoryPath.url repo prefix nv in
  let files = OpamRepositoryPath.files repo prefix nv in
  let archive =
    if archive then file (OpamRepositoryPath.archive repo nv)
    else [] in
  file opam @ file descr @ file url @ dir files @ archive

let package_important_files repo prefix nv ~archive =
  let url = OpamRepositoryPath.url repo prefix nv in
  let files = OpamRepositoryPath.files repo prefix nv in
  if archive then
    let archive = OpamRepositoryPath.archive repo nv in
    file url @ dir files @ file archive
  else
    file url @ dir files

let package_state repo prefix nv all =
  let fs = match all with
    | `all       -> package_files repo prefix nv ~archive:true
    | `partial b -> package_important_files repo prefix nv ~archive:b in
  let url = OpamRepositoryPath.url repo prefix nv in
  let l =
    List.map (fun f ->
        if all <> `all && f = url then url_checksum f
        else OpamFilename.checksum f)
      fs in
  List.flatten l

let compare_repo r1 r2 =
  let r = compare r1.repo_priority r2.repo_priority in
  if r = 0 then compare r1 r2 else ~- r

(* Sort repositories by priority *)
let sort repositories =
  let repositories = OpamRepositoryName.Map.values repositories in
  List.sort compare_repo repositories

let package_index repositories =
  log "package-index";
  let repositories = sort repositories in
  List.fold_left (fun map repo ->
      let packages = packages_with_prefixes repo in
      OpamPackage.Map.fold (fun nv prefix map ->
          if OpamPackage.Map.mem nv map then map
          else OpamPackage.Map.add nv (repo.repo_name, prefix) map
        ) packages map
    ) OpamPackage.Map.empty repositories

let compiler_index repositories =
  log "compiler-index";
  let repositories = sort repositories in
  List.fold_left (fun map repo ->
      let comps = compilers_with_prefixes repo in
      OpamCompiler.Map.fold (fun comp prefix map ->
          if OpamCompiler.Map.mem comp map then map
          else OpamCompiler.Map.add comp (repo.repo_name, prefix) map
        ) comps map
    ) OpamCompiler.Map.empty repositories

let update repo =
  log "update %a" (slog OpamRepositoryBackend.to_string) repo;
  let module B = (val find_backend repo: OpamRepositoryBackend.S) in
  B.pull_repo repo

let make_archive ?(gener_digest=false) repo prefix nv =
  let url_file = OpamRepositoryPath.url repo prefix nv in
  let files_dir = OpamRepositoryPath.files repo prefix nv in
  let archive = OpamRepositoryPath.archive repo nv in
  let archive_dir = OpamRepositoryPath.archives_dir repo in
  if not (OpamFilename.exists_dir archive_dir) then
    OpamFilename.mkdir archive_dir;

  (* Download the remote file / fetch the remote repository *)
  let download download_dir =
    if OpamFilename.exists url_file then (
      let url = OpamFile.URL.read url_file in
      let checksum = OpamFile.URL.checksum url in
      let remote_url = OpamFile.URL.url url in
      let mirrors = remote_url :: OpamFile.URL.mirrors url in
      let kind = OpamFile.URL.kind url in
      log "downloading %a:%a"
        (slog (string_of_address ~kind)) remote_url
        (slog string_of_repository_kind) kind;
      if not (OpamFilename.exists_dir download_dir) then
        OpamFilename.mkdir download_dir;
      match checksum with
      | Some c when gener_digest ->
        pull_url_and_fix_digest kind nv download_dir c url_file mirrors
        @@+ fun f -> Done (Some f)
      | _ ->
        pull_url kind nv download_dir checksum mirrors
        @@+ fun f -> Done (Some f)
    ) else
      Done None
  in

  (* if we've downloaded a file, extract it, otherwise just copy it *)
  let extract local_filename extract_dir =
    match local_filename with
    | None                   -> ()
    | Some (Not_available u) -> OpamConsole.error_and_exit "%s is not available" u
    | Some ( Result r
           | Up_to_date r )  -> OpamFilename.extract_generic_file r extract_dir in

  (* Eventually add <package>/files/* into the extracted dir *)
  let copy_files extract_dir =
    if OpamFilename.exists_dir files_dir then (
      if not (OpamFilename.exists_dir extract_dir) then
        OpamFilename.mkdir extract_dir;
      OpamFilename.copy_files ~src:files_dir ~dst:extract_dir;
      OpamFilename.Set.of_list (OpamFilename.rec_files extract_dir)
    ) else
      OpamFilename.Set.empty in

    (* Finally create the final archive *)
  let create_archive files extract_root =
    if not (OpamFilename.Set.is_empty files) || OpamFilename.exists url_file then (
      OpamConsole.msg "Creating %s.\n" (OpamFilename.to_string archive);
      OpamFilename.exec extract_root [
        [ "tar" ; "czf" ;
          OpamFilename.to_string archive ;
          OpamPackage.to_string nv ]
      ];
      Some archive
    ) else
      None in

  OpamFilename.with_tmp_dir_job (fun extract_root ->
      OpamFilename.with_tmp_dir_job (fun download_dir ->
          download download_dir @@+ fun local_filename ->
          let extract_dir = extract_root / OpamPackage.to_string nv in
          extract local_filename extract_dir;
          let files = copy_files extract_dir in
          match create_archive files extract_root with
          | None | Some _ -> Done ()
        )
    )

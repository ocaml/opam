(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamFilename.Op
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "REPOSITORY" fmt
let slog = OpamConsole.slog


let find_backend_by_kind = function
  | `http -> (module OpamHTTP.B: OpamRepositoryBackend.S)
  | `rsync -> (module OpamLocal.B: OpamRepositoryBackend.S)
  | `git -> (module OpamGit.B: OpamRepositoryBackend.S)
  | `hg -> (module OpamHg.B: OpamRepositoryBackend.S)
  | `darcs -> (module OpamDarcs.B: OpamRepositoryBackend.S)

let url_backend url = find_backend_by_kind url.OpamUrl.backend

let find_backend r = url_backend r.repo_url

(* initialize the current directory *)
let init root name =
  log "init local repo mirror at %s" (OpamRepositoryName.to_string name);
  (* let module B = (val find_backend repo: OpamRepositoryBackend.S) in *)
  let dir = OpamRepositoryPath.create root name in
  OpamFilename.cleandir dir;
  Done ()

let pull_url package local_dirname checksum remote_url =
  if OpamRepositoryConfig.(!r.force_checksums = Some true) && checksum = None
  then
    OpamConsole.error_and_exit
      "Checksum required for %s, but not found in package description.\n\
       This may be due to an outdated package description, try running `opam update`.\n\
       In case an update does not fix the problem, you can bypass the check by not\n\
       passing the `--require-checksums` command line option."
      (OpamPackage.to_string package);
  let pull url =
    let module B = (val url_backend url: OpamRepositoryBackend.S) in
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
  let kind = repo.repo_url.OpamUrl.backend in
  let module B = (val find_backend_by_kind kind: OpamRepositoryBackend.S) in
  B.revision repo.repo_root

let pull_url_and_fix_digest package dirname checksum file url =
  pull_url package dirname None url @@+ function
  | Not_available _
  | Up_to_date _
  | Result (D _) as r -> Done r
  | Result (F f) as r ->
    if not (OpamHash.check_file (OpamFilename.to_string f) checksum) then (
      let actual = OpamHash.compute (OpamFilename.to_string f) in
      OpamConsole.msg
        "Fixing wrong checksum for %s: current value is %s, setting it to %s.\n"
        (OpamPackage.to_string package) (OpamHash.to_string checksum)
        (OpamHash.to_string actual);
      let u = OpamFile.URL.read file in
      OpamFile.URL.write file (OpamFile.URL.with_checksum actual u)
    );
    Done r

let pull_archive repo nv =
  let module B =
    (val find_backend_by_kind repo.repo_url.OpamUrl.backend:
      OpamRepositoryBackend.S)
  in
  let url = OpamRepositoryPath.Remote.archive repo.repo_url nv in
  B.pull_archive repo.repo_name repo.repo_root url

let packages r =
  OpamPackage.list (OpamRepositoryPath.packages_dir r.repo_root)

let packages_with_prefixes r =
  OpamPackage.prefixes (OpamRepositoryPath.packages_dir r.repo_root)

let update repo =
  log "update %a" (slog OpamRepositoryBackend.to_string) repo;
  let module B = (val find_backend repo: OpamRepositoryBackend.S) in
  B.pull_repo repo.repo_name repo.repo_root repo.repo_url

let make_archive ?(gener_digest=false) repo prefix nv =
  let url_file = OpamRepositoryPath.url repo.repo_root prefix nv in
  let files_dir = OpamRepositoryPath.files repo.repo_root prefix nv in
  let archive = OpamRepositoryPath.archive repo.repo_root nv in
  let archive_dir = OpamRepositoryPath.archives_dir repo.repo_root in
  if not (OpamFilename.exists_dir archive_dir) then
    OpamFilename.mkdir archive_dir;

  (* Download the remote file / fetch the remote repository *)
  let download download_dir =
    match OpamFile.URL.read_opt url_file with
    | None -> Done None
    | Some url ->
      let checksum = OpamFile.URL.checksum url in
      let remote_url = OpamFile.URL.url url in
      let mirrors = remote_url :: OpamFile.URL.mirrors url in
      log "downloading %a" (slog OpamUrl.to_string) remote_url;
      if not (OpamFilename.exists_dir download_dir) then
        OpamFilename.mkdir download_dir;
      match checksum with
      | Some c when gener_digest ->
        pull_url_and_fix_digest nv download_dir c url_file mirrors
        @@+ fun f -> Done (Some f)
      | _ ->
        pull_url nv download_dir checksum mirrors
        @@+ fun f -> Done (Some f)
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
      OpamFilename.copy_dir ~src:files_dir ~dst:extract_dir;
      OpamFilename.Set.of_list (OpamFilename.rec_files extract_dir)
    ) else
      OpamFilename.Set.empty in

    (* Finally create the final archive *)
  let create_archive files extract_root =
    if not (OpamFilename.Set.is_empty files) ||
       OpamFile.exists url_file then (
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

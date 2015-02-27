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
open OpamTypesBase
open OpamMisc.OP
open OpamFilename.OP
open OpamProcess.Job.Op

let log fmt = OpamGlobals.log "REPOSITORY" fmt
let slog = OpamGlobals.slog

let compare r1 r2 =
  match compare r2.repo_priority r1.repo_priority with
  | 0 -> compare r2.repo_name r1.repo_name
  | x -> x

let to_string r =
  Printf.sprintf "%s(%d %s %s)"
    (OpamRepositoryName.to_string r.repo_name)
    r.repo_priority
    (string_of_repository_kind r.repo_kind)
    (string_of_address r.repo_address)

let default_address =
  OpamGlobals.default_repository_address, None

let default () = {
  repo_name     = OpamRepositoryName.default;
  repo_kind     = `http;
  repo_address  = default_address;
  repo_priority = 0;
  repo_root     =
    OpamPath.Repository.create (OpamPath.root()) OpamRepositoryName.default;
}

let local dirname = {
  repo_name     = OpamRepositoryName.of_string "local";
  repo_root     = dirname;
  repo_address  = ("<none>", None);
  repo_kind     = `local;
  repo_priority = 0;
}

let to_json r =
  `O  [ ("name", OpamRepositoryName.to_json r.repo_name);
        ("kind", `String (string_of_repository_kind r.repo_kind));
      ]

module O = struct
  type tmp = repository
  type t = tmp
  let compare = compare
  let hash t = Hashtbl.hash (t.repo_name, t.repo_priority)
  let equal t1 t2 = compare t1 t2 = 0
  let to_string = to_string
  let to_json = to_json
end

let of_string _ =
  failwith "TOTO"

module Set = OpamMisc.Set.Make(O)

module Map = OpamMisc.Map.Make(O)

module type BACKEND = sig
  val pull_url: package -> dirname -> string option -> address -> generic_file download OpamProcess.job
  val pull_repo: repository -> unit OpamProcess.job
  val pull_archive: repository -> filename -> filename download OpamProcess.job
  val revision: repository -> version option OpamProcess.job
end

exception Unknown_backend

let backends = Hashtbl.create 8

let find_backend r =
  try
    Hashtbl.find backends r.repo_kind
  with Not_found -> raise Unknown_backend

let find_backend_by_kind k =
  try
    Hashtbl.find backends k
  with Not_found -> raise Unknown_backend

let register_backend name backend =
  Hashtbl.replace backends name backend

(* initialize the current directory *)
let init repo =
  log "init %a" (OpamGlobals.slog to_string) repo;
  let module B = (val find_backend repo: BACKEND) in
  OpamFilename.rmdir repo.repo_root;
  OpamFilename.mkdir repo.repo_root;
  OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
  OpamFilename.mkdir (OpamPath.Repository.packages_dir repo);
  OpamFilename.mkdir (OpamPath.Repository.archives_dir repo);
  OpamFilename.mkdir (OpamPath.Repository.compilers_dir repo);
  Done ()

open OpamProcess.Job.Op

let pull_url kind package local_dirname checksum remote_url =
  if !OpamGlobals.req_checksums && checksum = None then
    OpamGlobals.error_and_exit
      "Checksum required for %s, but not found in package description.\n\
       This may be due to an outdated package description, try running `opam update`.\n\
       In case an update does not fix the problem, you can bypass the check by not\n\
       passing the `--require-checksums` command line option."
      (OpamPackage.to_string package);
  let pull url =
    let module B = (val find_backend_by_kind kind: BACKEND) in
    B.pull_url package local_dirname checksum url in
  let rec attempt = function
    | [] -> assert false
    | [url] -> pull url
    | url::mirrors ->
      pull url @@+ function
      | Not_available s ->
        OpamGlobals.warning "download of %s failed, trying mirror" s;
        attempt mirrors
      | r -> Done r
  in
  attempt remote_url

let revision repo =
  let kind = repo.repo_kind in
  let module B = (val find_backend_by_kind kind: BACKEND) in
  B.revision repo

let pull_url_and_fix_digest kind package dirname checksum file url =
  pull_url kind package dirname None url @@+ function
  | Not_available _
  | Up_to_date _
  | Result (D _) as r -> Done r
  | Result (F f) as r ->
    let actual = OpamFilename.digest f in
    if checksum <> actual then (
      OpamGlobals.msg
        "Fixing wrong checksum for %s: current value is %s, setting it to %s.\n"
        (OpamPackage.to_string package) checksum actual;
      let u = OpamFile.URL.read file in
      OpamFile.URL.write file (OpamFile.URL.with_checksum u actual)
    );
    Done r

let check_digest filename = function
  | Some expected when not (!OpamGlobals.no_checksums) ->
    let actual = OpamFilename.digest filename in
    if actual = expected then true
    else
      (OpamGlobals.error
         "Bad checksum for %s:\n\
         \  - %s [expected result]\n\
         \  - %s [actual result]\n\
          This may be fixed by running `opam update`.\n"
         (OpamFilename.to_string filename) expected actual;
       false)
  | _ -> true

let pull_archive repo nv =
  let module B = (val find_backend_by_kind repo.repo_kind: BACKEND) in
  let filename = OpamPath.Repository.remote_archive repo nv in
  B.pull_archive repo filename

let check_version repo =
  let repo_version =
    repo
    |> OpamPath.Repository.repo
    |> OpamFile.Repo.safe_read
    |> OpamFile.Repo.opam_version
  in
  if not !OpamGlobals.skip_version_checks &&
     OpamVersion.compare repo_version OpamVersion.current > 0 then
    OpamGlobals.error_and_exit
      "The current version of OPAM cannot read the repository %S. \n\
       You should upgrade to at least version %s.\n"
      (OpamRepositoryName.to_string repo.repo_name)
      (OpamVersion.to_string repo_version)
  else Done ()

let extract_prefix repo dir nv =
  let prefix =
    let prefix = OpamFilename.Dir.to_string (OpamPath.Repository.packages_dir repo) in
    prefix ^ Filename.dir_sep in
  let suffix =
    let suffix = OpamPackage.to_string nv in
    Filename.dir_sep ^ suffix in
  let dir = OpamFilename.Dir.to_string dir in
  OpamMisc.remove_prefix ~prefix (OpamMisc.remove_suffix ~suffix dir)

let file f =
  if OpamFilename.exists f then [f] else []

let dir d =
  if OpamFilename.exists_dir d then OpamFilename.rec_files d else []

(* Compiler updates *)

let compilers_with_prefixes r =
  OpamCompiler.prefixes (OpamPath.Repository.compilers_dir r)

let compilers repo =
OpamCompiler.list (OpamPath.Repository.compilers_dir repo)

let compiler_files repo prefix c =
  let comp = OpamPath.Repository.compiler_comp repo prefix c in
  let descr = OpamPath.Repository.compiler_descr repo prefix c in
  file comp @ file descr

let compiler_state repo prefix c =
  let fs = compiler_files repo prefix c in
  List.flatten (List.map OpamFilename.checksum fs)

let packages r =
  OpamPackage.list (OpamPath.Repository.packages_dir r)

let packages_with_prefixes r =
  OpamPackage.prefixes (OpamPath.Repository.packages_dir r)

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
  let opam = OpamPath.Repository.opam repo prefix nv in
  let descr = OpamPath.Repository.descr repo prefix nv in
  let url = OpamPath.Repository.url repo prefix nv in
  let files = OpamPath.Repository.files repo prefix nv in
  let archive =
    if archive then file (OpamPath.Repository.archive repo nv)
    else [] in
  file opam @ file descr @ file url @ dir files @ archive

let package_important_files repo prefix nv ~archive =
  let url = OpamPath.Repository.url repo prefix nv in
  let files = OpamPath.Repository.files repo prefix nv in
  if archive then
    let archive = OpamPath.Repository.archive repo nv in
    file url @ dir files @ file archive
  else
    file url @ dir files

let package_state repo prefix nv all =
  let fs = match all with
    | `all       -> package_files repo prefix nv ~archive:true
    | `partial b -> package_important_files repo prefix nv ~archive:b in
  let url = OpamPath.Repository.url repo prefix nv in
  let l = List.map OpamFilename.checksum fs in
  List.flatten l

(* Sort repositories by priority *)
let sort repositories =
  let repositories = OpamRepositoryName.Map.values repositories in
  List.sort compare repositories

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
  log "update %a" (slog to_string) repo;
  let module B = (val find_backend repo: BACKEND) in
  B.pull_repo repo

let make_archive ?(gener_digest=false) repo prefix nv =
  let url_file = OpamPath.Repository.url repo prefix nv in
  let files_dir = OpamPath.Repository.files repo prefix nv in
  let archive = OpamPath.Repository.archive repo nv in
  let archive_dir = OpamPath.Repository.archives_dir repo in
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
        (slog string_of_address) remote_url
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
    | Some (Not_available u) -> OpamGlobals.error_and_exit "%s is not available" u
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
      OpamGlobals.msg "Creating %s.\n" (OpamFilename.to_string archive);
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

module Graph = OpamParallel.MakeGraph (O)

module Parallel = Graph.Parallel

let find_backend = find_backend_by_kind

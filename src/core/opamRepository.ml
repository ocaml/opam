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
open OpamMisc.OP
open OpamFilename.OP

let log fmt = OpamGlobals.log "REPOSITORY" fmt

let compare r1 r2 =
  match compare r2.repo_priority r1.repo_priority with
  | 0 -> compare r2.repo_name r1.repo_name
  | x -> x

let to_string r =
  Printf.sprintf "%s(%d %s %s)"
    (OpamRepositoryName.to_string r.repo_name)
    r.repo_priority
    (OpamTypes.string_of_repository_kind r.repo_kind)
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
  let to_string = to_string
  let to_json = to_json
end

let of_string _ =
  failwith "TOTO"

module Set = OpamMisc.Set.Make(O)

module Map = OpamMisc.Map.Make(O)

module type BACKEND = sig
  val pull_url: package -> dirname -> address -> generic_file download
  val pull_repo: repository -> unit
  val pull_archive: repository -> filename -> filename download
  val revision: repository -> version option
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
  log "init %s" (to_string repo);
  let module B = (val find_backend repo: BACKEND) in
  OpamFilename.rmdir repo.repo_root;
  OpamFilename.mkdir repo.repo_root;
  OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
  OpamFilename.mkdir (OpamPath.Repository.packages_dir repo);
  OpamFilename.mkdir (OpamPath.Repository.archives_dir repo);
  OpamFilename.mkdir (OpamPath.Repository.compilers_dir repo);
  ignore (B.pull_repo repo)

type pull_fn = repository_kind -> package -> dirname -> address -> generic_file download

let pull_url kind package local_dirname remote_url =
  let module B = (val find_backend_by_kind kind: BACKEND) in
  B.pull_url package local_dirname remote_url

let revision repo =
  let kind = repo.repo_kind in
  let module B = (val find_backend_by_kind kind: BACKEND) in
  B.revision repo

let pull_and_check_digest ~checksum kind package dirname url =
  let filename = OpamFilename.of_string (string_of_address url) in
  if OpamFilename.exists filename
  && not (Sys.is_directory (OpamFilename.to_string filename))
  && OpamFilename.digest filename = checksum then
    Up_to_date (F filename)
  else match pull_url kind package dirname url with
    | Not_available _
    | Up_to_date _
    | Result (D _) as r -> r
    | Result (F f)      ->
      let actual = OpamFilename.digest f in
      if !OpamGlobals.no_checksums
      || actual = checksum then
        Result (F f)
      else
        OpamGlobals.error_and_exit
          "Wrong checksum for %s:\n\
          \  - %s [expected result]\n\
          \  - %s [actual result]\n\
           This is surely due to outdated package descriptions and should be   \
           fixed by running `opam update`. In case an update does not fix that \
           problem, you can  use the `--no-checksums` command-line option to   \
           bypass any checksum checks."
          (OpamFilename.to_string filename)
          checksum
          actual

let pull_and_fix_digest ~file ~checksum kind package dirname url =
  match pull_url kind package dirname url with
  | Not_available _
  | Up_to_date _
  | Result (D _) as r -> r
  | Result (F f) as r ->
    let actual = OpamFilename.digest f in
    if checksum <> actual then (
      OpamGlobals.msg
        "Fixing wrong checksum for %s: current value is %s, setting it to %s.\n"
        (OpamPackage.to_string package) checksum actual;
      let u = OpamFile.URL.read file in
      OpamFile.URL.write file (OpamFile.URL.with_checksum u actual)
    );
    r

let pull_archive repo nv =
  let module B = (val find_backend_by_kind repo.repo_kind: BACKEND) in
  let filename = OpamPath.Repository.remote_archive repo nv in
  B.pull_archive repo filename

let check_version repo =
  let repo_version =
    try
      (OpamPath.Repository.version |>
       OpamFilename.read |>
       OpamMisc.strip |>
       OpamVersion.of_string
      ) repo
    with _ ->
      OpamVersion.of_string "0.7.5" in
  if OpamVersion.compare repo_version OpamVersion.current > 0 then
    OpamGlobals.error_and_exit
      "\nThe current version of OPAM cannot read the repository. \
       You should upgrade to at least version %s.\n"
      (OpamVersion.to_string repo_version)

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
  List.flatten (List.map OpamFilename.checksum fs)

let update repo =
  log "update %s" (to_string repo);
  let module B = (val find_backend repo: BACKEND) in
  B.pull_repo repo;
  check_version repo


let make_archive ?(gener_digest=false) repo prefix nv =
  let url_file = OpamPath.Repository.url repo prefix nv in
  let files_dir = OpamPath.Repository.files repo prefix nv in
  let archive = OpamPath.Repository.archive repo nv in

  (* Download the remote file / fetch the remote repository *)
  let download download_dir =
    if OpamFilename.exists url_file then (
      let url = OpamFile.URL.read url_file in
      let checksum = OpamFile.URL.checksum url in
      let remote_url = OpamFile.URL.url url in
      let kind = guess_repository_kind (OpamFile.URL.kind url) remote_url in
      log "downloading %s:%s"
        (string_of_address remote_url) (string_of_repository_kind kind);
      if not (OpamFilename.exists_dir download_dir) then
        OpamFilename.mkdir download_dir;
      OpamFilename.in_dir download_dir (fun () ->
          match checksum with
          | None   -> Some (pull_url kind nv download_dir remote_url)
          | Some c ->
            if gener_digest then
              Some (pull_and_fix_digest ~file:url_file ~checksum:c
                      kind nv download_dir remote_url)
            else
              Some (pull_and_check_digest ~checksum:c
                      kind nv download_dir remote_url)
        )
    ) else
      None
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

  OpamFilename.with_tmp_dir (fun extract_root ->
      OpamFilename.with_tmp_dir (fun download_dir ->
          let local_filename = download download_dir in
          let extract_dir = extract_root / OpamPackage.to_string nv in
          extract local_filename extract_dir;
          let files = copy_files extract_dir in
          match create_archive files extract_root with
          | None | Some _ -> ()
        )
    )

module Graph = struct
  module Vertex =  struct
    type t = repository
    let compare = compare
    let hash = Hashtbl.hash
    let equal r1 r2 = compare r1 r2 = 0
  end
  module PG = Graph.Imperative.Digraph.ConcreteBidirectional (Vertex)
  module Topological = Graph.Topological.Make (PG)
  module Traverse = Graph.Traverse.Dfs(PG)
  module Components = Graph.Components.Make(PG)
  module Parallel = OpamParallel.Make(struct
    let string_of_vertex = to_string
    include PG
    include Topological
    include Traverse
    include Components
  end)
end

module Parallel = Graph.Parallel

let find_backend = find_backend_by_kind

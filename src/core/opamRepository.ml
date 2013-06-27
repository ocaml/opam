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
    (OpamFilename.Dir.to_string r.repo_address)

let default_address =
  OpamFilename.raw_dir OpamGlobals.default_repository_address

let default () = {
  repo_name     = OpamRepositoryName.default;
  repo_kind     = `http;
  repo_address  = default_address;
  repo_priority = 0;
  repo_root     = OpamPath.Repository.create OpamRepositoryName.default;
}

let local dirname = {
  repo_name     = OpamRepositoryName.of_string "local";
  repo_root     = dirname;
  repo_address  = dirname;
  repo_kind     = `local;
  repo_priority = 0;
}

let repository_address address =
  if Sys.file_exists address
  then OpamFilename.Dir.of_string address
  else OpamFilename.raw_dir address

module O = struct
  type tmp = repository
  type t = tmp
  let compare = compare
  let to_string = to_string
end

module Set = OpamMisc.Set.Make(O)

module Map = OpamMisc.Map.Make(O)

module type BACKEND = sig
  val pull_file: dirname -> filename -> filename download
  val pull_dir: dirname -> dirname -> dirname download
  val pull_repo: repository -> unit
  val pull_archive: repository -> filename -> filename download
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

let nv_set_of_files ~all files =
  OpamPackage.Set.of_list
    (OpamMisc.filter_map
       (OpamPackage.of_filename ~all)
       (OpamFilename.Set.elements files)
    )

let compiler_set_of_files files =
  let files = OpamFilename.Set.filter (OpamFilename.ends_with ".comp") files in
  OpamFilename.Set.fold (fun f set ->
      match OpamCompiler.of_filename f with
      | None   -> set
      | Some f -> OpamCompiler.Set.add f set
  ) files OpamCompiler.Set.empty

let read_tmp dir =
  let dirs =
    if OpamFilename.exists_dir dir then
      OpamFilename.Dir.Set.of_list (OpamFilename.sub_dirs dir)
    else
      OpamFilename.Dir.Set.empty in
  OpamPackage.Set.of_list
    (OpamMisc.filter_map OpamPackage.of_dirname (OpamFilename.Dir.Set.elements dirs))

let pull_file kind local_dirname remote_filename =
  let module B = (val find_backend_by_kind kind: BACKEND) in
  B.pull_file local_dirname remote_filename

let pull_dir kind local_dirname remote_dirname =
  let module B = (val find_backend_by_kind kind: BACKEND) in
  B.pull_dir local_dirname remote_dirname

let pull_file_and_check_digest kind dirname filename checksum =
  if OpamFilename.exists filename
  && OpamFilename.digest filename = checksum then
    Up_to_date filename
  else match pull_file kind dirname filename with
    | Not_available -> Not_available
    | Up_to_date f  -> Up_to_date f
    | Result f      ->
      let actual = OpamFilename.digest f in
      if !OpamGlobals.no_checksums
      || actual = checksum then
        Result f
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

let pull_file_and_fix_digest ~url_file nv kind dirname filename checksum =
  match pull_file kind dirname filename with
  | Not_available -> Not_available
  | Up_to_date f  -> Up_to_date f
  | Result f      ->
    let actual = OpamFilename.digest f in
    if checksum <> actual then (
      OpamGlobals.msg
        "Fixing wrong checksum for %s: current value is %s, setting it to %s.\n"
        (OpamPackage.to_string nv) checksum actual;
      let url = OpamFile.URL.read url_file in
      OpamFile.URL.write url_file (OpamFile.URL.with_checksum url actual)
    );
    Result f

(* Infer the url kind: when the url is a local directory, use the
   `local` backend, otherwise use the `http` one. This function is
   only used when the user hasn't specified a repository kind. *)
let kind_of_url url =
  if Sys.file_exists url
  then `local
  else `http

let pull_archive repo nv =
  let module B = (val find_backend_by_kind repo.repo_kind: BACKEND) in
  let filename = OpamPath.Repository.remote_archive repo nv in
  B.pull_archive repo filename

let read_prefix local_repo =
  OpamFile.Prefix.safe_read (OpamPath.Repository.prefix local_repo)

let find_prefix prefix nv =
  let name = OpamPackage.name nv in
  if not (OpamPackage.Name.Map.mem name prefix) then None
  else Some (OpamPackage.Name.Map.find name prefix)

(* Copy the file in local_repo in current dir *)
let copy_files repo nv dst =
  let prefix = find_prefix (read_prefix repo) nv in
  (* Eventually add the <package>/files/* to the extracted dir *)
  log "Adding the files to the archive";
  let files_dir = OpamPath.Repository.files repo prefix nv in
  let files = OpamFilename.rec_files files_dir in
  if files <> [] then (
    if not (OpamFilename.exists_dir dst) then
      OpamFilename.mkdir dst;
    List.iter (fun file ->
      let basename = OpamFilename.remove_prefix files_dir file in
      let dst_file = dst // basename in
      if OpamFilename.exists dst_file then
        OpamGlobals.warning
          "Skipping %s as it already exists in %s."
          basename
          (OpamFilename.Dir.to_string dst)
      else (
        OpamGlobals.msg "Copying %s.\n" basename;
        OpamFilename.copy_in file dst
      )
    ) files;
  );
  OpamFilename.Set.of_list files

let map fn = function
  | Up_to_date f  -> Up_to_date (fn f)
  | Result  f     -> Result (fn f)
  | Not_available -> Not_available
let dir = map (fun d -> D d)
let file = map (fun f -> F f)

let pull kind download address =
  match pull_file kind download (OpamFilename.raw address) with
  | Not_available -> dir (pull_dir kind download (OpamFilename.raw_dir address))
  | x             -> file x

let make_archive ?(gener_digest=false) repo nv =
  let prefix = find_prefix (read_prefix repo) nv in
  let url_file = OpamPath.Repository.url repo prefix nv in

  let download_dir = OpamPath.Repository.tmp_dir repo nv in
  OpamFilename.mkdir download_dir;

  OpamFilename.with_tmp_dir (fun extract_root ->
    let extract_dir = extract_root / OpamPackage.to_string nv in

    if OpamFilename.exists url_file then (
      let url = OpamFile.URL.read url_file in
      let checksum = OpamFile.URL.checksum url in
      let address = OpamFile.URL.url url in
      let kind = match OpamFile.URL.kind url with
        | None   -> kind_of_url address
        | Some k -> k in
      log "downloading %s:%s" address (string_of_repository_kind kind);

      let local_filename =
        OpamFilename.in_dir download_dir (fun () ->
          match checksum with
          | None   -> pull kind download_dir address
          | Some c ->
            let filename = OpamFilename.raw address in
            if gener_digest then
              file (pull_file_and_fix_digest ~url_file nv kind download_dir filename c)
            else
              file (pull_file_and_check_digest kind download_dir filename c)
        ) in

      match local_filename with
      | Not_available -> OpamGlobals.error_and_exit "Cannot get %s" address
      | Result (F f) | Up_to_date (F f) ->
        log "extracting %s to %s"
          (OpamFilename.to_string f)
          (OpamFilename.Dir.to_string extract_dir);
        OpamFilename.extract f extract_dir
      | Result (D d) | Up_to_date (D d) ->
        if d <> extract_dir then (
          log "copying %s to %s"
            (OpamFilename.Dir.to_string d)
            (OpamFilename.Dir.to_string extract_dir);
          OpamFilename.copy_dir ~src:d ~dst:extract_dir
        )
    );

    (* Eventually add the <package>/files/* to the extracted dir *)
    let files =
      if not (OpamFilename.exists_dir extract_dir) then
        OpamFilename.mkdir extract_dir;
      copy_files repo nv extract_dir in

    (* Finally create the final archive *)
    if not (OpamFilename.Set.is_empty files) || OpamFilename.exists url_file then (
      OpamFilename.mkdir (OpamPath.Repository.archives_dir repo);
      let local_archive = OpamPath.Repository.archive repo nv in
      OpamGlobals.msg "Creating %s.\n" (OpamFilename.to_string local_archive);
      OpamFilename.exec extract_root [
        [ "tar" ; "czf" ;
          OpamFilename.to_string local_archive ;
          OpamPackage.to_string nv ]
      ]
    );
  )

(* Download the archive on the OPAM server.
   If it is not there, then:
   * download the original archive upstream
   * add eventual patches
   * create a new tarball *)
let download repo nv =
  log "download %s %s" (to_string repo) (OpamPackage.to_string nv);
  (* If the archive is on the server, download it directly *)
  match pull_archive repo nv with
  | Up_to_date _ ->
    OpamGlobals.msg "The archive for %s is in the local cache.\n"
      (OpamPackage.to_string nv);
    log "The archive for %s is already downloaded and up-to-date"
      (OpamPackage.to_string nv)
  | Result local_file ->
    log "Downloaded %s successfully" (OpamFilename.to_string local_file)
  | Not_available ->
    log "The archive for %s is not available, need to build it"
      (OpamPackage.to_string nv);
    make_archive repo nv

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

(* Used on upgrade to get the list of available packages in the repository *)
let packages repo =
  log "repository-package %s" (to_string repo);
  let dir = OpamPath.Repository.packages_dir repo in
  let empty = OpamPackage.Name.Map.empty, OpamPackage.Set.empty in
  if OpamFilename.exists_dir dir then (
    let files = OpamFilename.rec_files dir in
    List.fold_left (fun (prefix, packages as acc) file ->
      if OpamFilename.basename file = OpamFilename.Base.of_string "opam" then (
        let dir  = OpamFilename.dirname file in
        let base = OpamFilename.basename_dir dir in
        let base = OpamFilename.Base.to_string base in
        match OpamPackage.of_string_opt base with
        | None    -> acc
        | Some nv ->
          let packages = OpamPackage.Set.add nv packages in
          let prefix =
            if dir = OpamPath.Repository.package repo None nv then prefix
            else
              OpamPackage.Name.Map.add
                (OpamPackage.name nv)
                (extract_prefix repo dir nv)
                prefix in
          prefix, packages
      ) else
        acc
    ) empty files
  ) else
    empty

let compilers r =
  OpamCompiler.list (OpamPath.Repository.compilers_dir r)

(* XXX: not very efficient *)
let compiler_state repo comp =
  let comps = compilers repo in
  let comp_repo = repo in
  let comp_file, comp_descr = OpamCompiler.Map.find comp comps in
  let comp_checksums =
    OpamFilename.digest comp_file ::
      match comp_descr with
      | None   -> []
      | Some f -> [OpamFilename.digest f] in
  { comp_repo; comp_file; comp_descr; comp_checksums }

let package_state repo_p prefix nv =
  let pkg_repo = repo_p in
  let pkg_opam = OpamPath.Repository.opam repo_p prefix nv in
  if not (OpamFilename.exists pkg_opam) then
    raise Not_found;

  (* files *)
  let aux exists fn =
    let f = fn repo_p nv in
    if exists f then Some f
    else None in
  let file = aux OpamFilename.exists in
  let dir  = aux OpamFilename.exists_dir in
  let pkg_descr   = file (fun r -> OpamPath.Repository.descr r prefix) in
  let pkg_url     = file (fun r -> OpamPath.Repository.url r prefix) in
  let pkg_files   = dir  (fun r -> OpamPath.Repository.files r prefix) in
  let pkg_archive = file OpamPath.Repository.archive in

  (* checksums *)
  let aux digest = function
    | None   -> []
    | Some f -> [digest f] in
  let file  = aux OpamFilename.digest in
  let dir d = List.flatten (aux (fun dir ->
    let files = OpamFilename.rec_files dir in
    List.map OpamFilename.digest files
  ) d) in
  let sort = List.sort String.compare in
  let pkg_metadata = sort (OpamFilename.digest pkg_opam :: file pkg_descr) in
  let pkg_contents = sort (file pkg_archive @ file pkg_url @ dir pkg_files) in
  { pkg_repo; pkg_opam; pkg_descr; pkg_metadata;
    pkg_archive; pkg_url; pkg_files; pkg_contents; }


(* Clean-up archives and tmp files on URL changes *)
let clean repo active_packages =
  log "cleanup";
  let prefix, packages = packages repo in
  OpamPackage.Set.iter (fun nv ->
    let cleanup () =
      let tmp_dir = OpamPath.Repository.tmp_dir repo nv in
      OpamFilename.rmdir tmp_dir;
      OpamFilename.remove (OpamPath.Repository.archive repo nv) in
    if not (OpamPackage.Map.mem nv active_packages) then cleanup ()
    else
      let old_state = OpamPackage.Map.find nv active_packages in
      let prefix = find_prefix prefix nv in
      let new_state = package_state repo prefix nv in
      if old_state.pkg_contents <> new_state.pkg_contents then cleanup ()
  ) packages

(* [packages] is the state of currently 'active' packages in this
   repository. *)
let update repo =
  log "update %s" (to_string repo);
  let module B = (val find_backend repo: BACKEND) in
  B.pull_repo repo;
  check_version repo

let get_upstream_updates repo packages =
  log "get-upstream-updates %s %s" (to_string repo)
    (OpamMisc.string_of_list OpamPackage.to_string (OpamPackage.Map.keys packages));

  clean repo packages;
  let prefix = read_prefix repo in

  (* For each package in the cache, look at what changed upstream *)
  let cached_packages = read_tmp (OpamPath.Repository.tmp repo) in
  log "cached_packages: %s" (OpamPackage.Set.to_string cached_packages);
  let cached_packages = OpamPackage.Set.filter (fun nv ->
    let prefix = find_prefix prefix nv in
    let state = package_state repo prefix nv in
    match state.pkg_url with
    | None       -> true
    | Some url_f ->
      let url = OpamFile.URL.read url_f in
      let kind = match OpamFile.URL.kind url with
        | None   -> kind_of_url (OpamFile.URL.url url)
        | Some k -> k in
      let filename = OpamFile.URL.url url in
      log "updating %s:%s" filename (string_of_repository_kind kind);
      let dirname = OpamPath.Repository.tmp_dir repo nv in
      match pull kind dirname filename with
      | Not_available -> OpamGlobals.error_and_exit "%s is not available." filename
      | Up_to_date _  -> false
      | Result _      -> true
  ) cached_packages in

  let upstream_changes = OpamPackage.Map.fold (fun nv old_state set ->
    let prefix = find_prefix prefix nv in
    let new_state = package_state repo prefix nv in
    if old_state.pkg_contents = new_state.pkg_contents then set
    else OpamPackage.Set.add nv set
  ) packages OpamPackage.Set.empty in

  OpamPackage.Set.union cached_packages upstream_changes

let files r nv =
  let prefix = find_prefix (read_prefix r) nv in
  let l =
    if OpamFilename.exists_dir (OpamPath.Repository.files r prefix nv) then
      OpamFilename.rec_files (OpamPath.Repository.files r prefix nv)
    else
      [] in
  OpamFilename.Set.of_list l

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

let map_reduce jobs map merge init = function
  | []           -> init
  | [repository] -> merge (map repository) init
  | repositories ->
    if jobs = 1 then
      List.fold_left (fun acc repo -> merge (map repo) acc) init repositories
    else
      let g = Graph.Parallel.create repositories in
      Graph.Parallel.map_reduce jobs g ~map ~merge ~init

let parallel_iter jobs fn = function
  | []           -> ()
  | [repository] -> fn repository
  | repositories ->
    if jobs = 1 then List.iter fn repositories
    else
      let g = Graph.Parallel.create repositories in
      let pre _ = () in
      let post _ = () in
      let child = fn in
      Graph.Parallel.iter jobs g ~pre ~post ~child

let find_backend = find_backend_by_kind

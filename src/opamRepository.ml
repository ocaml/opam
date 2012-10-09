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

open OpamTypes
open OpamMisc.OP
open OpamFilename.OP

let log fmt = OpamGlobals.log "REPOSITORY" fmt

type t = repository

let to_string r =
  Printf.sprintf "%s(%s %s)"
    r.repo_name
    (OpamFilename.Dir.to_string r.repo_address)
    r.repo_kind

let of_string _ =
  failwith "Use Repository.create instead"

let default = {
  repo_name    = OpamGlobals.default_repository_name;
  repo_kind    = OpamGlobals.default_repository_kind;
  repo_address = OpamFilename.raw_dir OpamGlobals.default_repository_address;
}

let create = create_repository

let with_kind r repo_kind = { r with repo_kind }

module O = struct
  type tmp = repository
  type t = tmp
  let compare = compare
  let to_string = to_string
end

module Set = OpamMisc.Set.Make(O)

module Map = OpamMisc.Map.Make(O)

module type BACKEND = sig
  val init: address -> unit
  val update: address -> OpamFilename.Set.t
  val download_archive: address -> package -> filename download
  val download_file: package -> filename -> filename download
  val download_dir: package -> ?dst:dirname -> dirname -> dirname download
  val upload_dir: address:address -> dirname -> OpamFilename.Set.t
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

let local_repo () =
  OpamPath.Repository.raw (OpamFilename.cwd ())

let remote_repo remote_address =
  OpamPath.Repository.raw remote_address

let repo r =
  OpamPath.Repository.create (OpamPath.default ()) r

(* initialize the current directory *)
let init r =
  log "init %s" (to_string r);
  let module B = (val find_backend r: BACKEND) in
  let open OpamPath.Repository in
  let repo = repo r in
  OpamFilename.mkdir (root repo);
  OpamFile.Repo_config.write (config repo) r;
  OpamFilename.mkdir (packages_dir repo);
  OpamFilename.mkdir (archives_dir repo);
  OpamFilename.mkdir (compilers_dir repo);
  OpamFilename.mkdir (upload_dir repo);
  OpamFilename.in_dir (root repo) (fun () -> B.init r.repo_address)

let nv_set_of_files files =
  OpamPackage.Set.of_list
    (OpamMisc.filter_map
       OpamPackage.of_filename
       (OpamFilename.Set.elements files))

let read_tmp dir =
  let files = if OpamFilename.exists_dir dir then
      OpamFilename.Set.of_list (OpamFilename.list_files dir)
    else
      OpamFilename.Set.empty in
  OpamPackage.Set.of_list (OpamMisc.filter_map OpamPackage.of_filename (OpamFilename.Set.elements files))

(* upload the content of ./upload to the given OPAM repository *)
let upload r =
  log "upload %s" (to_string r);
  let repo = repo r in
  let local_dir = OpamPath.Repository.root repo in
  let upload_dir = OpamPath.Repository.upload_dir repo in
  let address = r.repo_address in
  let module B = (val find_backend r: BACKEND) in
  let files = OpamFilename.in_dir local_dir (fun () -> B.upload_dir ~address upload_dir) in
  let packages = nv_set_of_files files in
  OpamGlobals.msg "The following packages have been uploaded:\n";
  OpamPackage.Set.iter (fun nv ->
    OpamGlobals.msg "  - %s\n" (OpamPackage.to_string nv)
  ) packages

(* Download file f in the current directory *)
let map fn = function
  | Result x      -> Result (fn x)
  | Up_to_date x  -> Up_to_date (fn x)
  | Not_available -> Not_available

let download_file ~gener_digest k nv f c =
  log "download_file %s %s %s" k (OpamPackage.to_string nv) (OpamFilename.to_string f);
  let module B = (val find_backend_by_kind k: BACKEND) in
  let check file = match c with
    | None   -> true
    | Some c -> OpamFilename.digest file = c in
  let rename file =
    if not gener_digest && !OpamGlobals.verify_checksums && not (check file) then
      OpamGlobals.error_and_exit "Wrong checksum for %s (waiting for %s, got %s)"
        (OpamFilename.to_string file)
        (match c with Some c -> c | None -> "<none>")
        (OpamFilename.digest file);
    if k = "curl" && not (OpamSystem.is_tar_archive (OpamFilename.to_string f)) then
      let new_file = OpamFilename.raw_file (OpamFilename.to_string file ^ ".tar.gz") in
      OpamFilename.move file new_file;
      new_file
    else
      file in
  map rename (B.download_file nv f)

(* Download directory d in the current directory *)
let download_dir k nv ?dst d =
  log "download_dir %s %s %s" k (OpamPackage.to_string nv) (OpamFilename.Dir.to_string d);
  let module B = (val find_backend_by_kind k: BACKEND) in
  B.download_dir nv ?dst d

(* Download either a file or a directory in the current directory *)
let download_one ?(gener_digest = false) k nv url checksum =
  let f x = F x in
  let d x = D x in
  if k = "curl" || OpamSystem.is_tar_archive url then
    map f (download_file ~gener_digest k nv (OpamFilename.raw_file url) checksum)
  else
    map d (download_dir k nv (OpamFilename.raw_dir url))

(* Infer the url kind: when the url is a local directory, use the
   rsync backend, otherwise use the curl one. This function is only
   used when the user hasn't specified a repository kind. *)
let kind_of_url url =
  if Sys.file_exists url then
  "rsync"
  else
  "curl"

let download_archive r nv =
  let module B = (val find_backend r: BACKEND) in
  B.download_archive r.repo_address nv

(* Copy the file in local_repo in current dir *)
let copy_files local_repo nv =
  let local_dir = OpamFilename.cwd () in
  (* Eventually add the <package>/files/* to the extracted dir *)
  log "Adding the files to the archive";
  let files = OpamFilename.list_files (OpamPath.Repository.files local_repo nv) in
  if files <> [] then (
    if not (OpamFilename.exists_dir local_dir) then
      OpamFilename.mkdir local_dir;
    List.iter (fun f ->
      let dst = local_dir // OpamFilename.Base.to_string (OpamFilename.basename f) in
      if OpamFilename.exists dst then
        OpamGlobals.warning
          "Skipping %s as it already exists in %s"
          (OpamFilename.to_string f)
          (OpamFilename.Dir.to_string local_dir)
      else
        OpamFilename.copy_in f local_dir) files;
  );
  OpamFilename.Set.of_list files

let make_archive ?(gener_digest=false) ?local_path nv =
  (* download the archive upstream if the upstream address is
     specified *)
  let local_repo = local_repo () in
  let local_dir = OpamPath.Repository.root local_repo in
  let url_f = OpamPath.Repository.url local_repo nv in

  let download_dir = OpamPath.Repository.tmp_dir local_repo nv in
  OpamFilename.mkdir download_dir;

  OpamFilename.with_tmp_dir (fun extract_root ->
    let extract_dir = extract_root / OpamPackage.to_string nv in

    if local_path = None && OpamFilename.exists url_f then (
      let url_file = OpamFile.URL.read url_f in
      let checksum = OpamFile.URL.checksum url_file in
      let kind = match OpamFile.URL.kind url_file with
      | None   -> kind_of_url (OpamFile.URL.url url_file)
      | Some k -> k in
      let url = OpamFile.URL.url url_file in
      log "downloading %s:%s" url kind;

      match OpamFilename.in_dir local_dir (fun () -> download_one ~gener_digest kind nv url checksum) with
      | Not_available -> OpamGlobals.error_and_exit "Cannot get %s" url
      | Up_to_date (F local_archive)
      | Result (F local_archive) ->
          if gener_digest then (
            let digest = OpamFilename.digest local_archive in
            begin match checksum with
            | Some c when c <> digest ->
              OpamGlobals.msg
                "Wrong checksum for %s (waiting: %s, got: %s)\nFixing %s ...\n"
                (OpamFilename.to_string local_archive) c digest (OpamFilename.to_string url_f);
            | _ -> ();
            end;
            OpamFile.URL.write url_f (OpamFile.URL.with_checksum url_file digest);
          );
          log "extracting %s to %s"
            (OpamFilename.to_string local_archive)
            (OpamFilename.Dir.to_string extract_dir);
          OpamFilename.extract local_archive extract_dir
      | Up_to_date (D dir)
      | Result (D dir) ->
          log "copying %s to %s"
            (OpamFilename.Dir.to_string dir)
            (OpamFilename.Dir.to_string extract_dir);
          if dir <> extract_dir then
          OpamFilename.copy_dir download_dir extract_dir
    );

    let extract_dir = match local_path with
      | None   -> extract_dir
      | Some p -> p in

    (* Eventually add the <package>/files/* to the extracted dir *)
    let files =
      if not (OpamFilename.exists_dir extract_dir) then
        OpamFilename.mkdir extract_dir;
      OpamFilename.in_dir extract_dir (fun () -> copy_files local_repo nv) in

    (* And finally create the final archive *)
    if local_path <> None || not (OpamFilename.Set.is_empty files) || OpamFilename.exists url_f then (
      OpamFilename.mkdir (OpamPath.Repository.archives_dir local_repo);
      let local_archive = OpamPath.Repository.archive local_repo nv in
      OpamGlobals.msg "Creating the archive file in %s\n" (OpamFilename.to_string local_archive);
      OpamFilename.exec extract_root [
        [ "tar" ; "czf" ; OpamFilename.to_string local_archive ; OpamPackage.to_string nv ]
      ]
    );
  )

(* Download the archive on the OPAM server.
   If it is not there, then:
   * download the original archive upstream
   * add eventual patches
   * create a new tarball *)
let download r nv =
  log "download %s %s" (to_string r) (OpamPackage.to_string nv);
  let repo = repo r in
  let dir = OpamPath.Repository.root repo in
  (* If the archive is on the server, download it directly *)
  match OpamFilename.in_dir dir (fun () -> download_archive r nv) with
  | Up_to_date local_file ->
      OpamGlobals.msg "The archive for %s is in the local cache.\n" (OpamPackage.to_string nv);
      log "The archive for %s is already downloaded and up-to-date"
        (OpamPackage.to_string nv)
  | Result local_file ->
      log "Downloaded %s successfully" (OpamFilename.to_string local_file)
  | Not_available ->
      log "The archive for %s is not available, need to build it"
        (OpamPackage.to_string nv);
      OpamFilename.in_dir dir (fun () -> make_archive nv)

let check_version repo =
  let repo_version =
    try
      (OpamPath.Repository.version |>
       OpamFilename.read |>
       OpamMisc.strip |>
       OpamVersion.of_string
      ) repo
    with e ->
      OpamVersion.of_string "0.7.5" in
  if OpamVersion.compare repo_version OpamVersion.current >= 0 then
    OpamGlobals.error_and_exit
      "\nThe current version of OPAM cannot read the repository. \
       You should upgrade to at least the version %s.\n" (OpamVersion.to_string repo_version)

let update r =
  log "update %s" (to_string r);
  let repo = repo r in
  let dir = OpamPath.Repository.root repo in
  let module B = (val find_backend r: BACKEND) in
  let updated_files = OpamFilename.in_dir dir (fun () -> B.update r.repo_address) in

  check_version repo;

  let updated_packages = nv_set_of_files updated_files in

  (* Clean-up archives and tmp files on URL changes *)
  OpamPackage.Set.iter (fun nv ->
    let url_f = OpamPath.Repository.url repo nv in
    if OpamFilename.Set.mem url_f updated_files then begin
      let tmp_dir = OpamPath.Repository.tmp_dir repo nv in
      OpamFilename.rmdir tmp_dir;
      OpamFilename.remove (OpamPath.Repository.archive repo nv);
    end
  ) updated_packages;

  (* For each package in the cache, look at what changed upstream *)
  let cached_packages = read_tmp (OpamPath.Repository.tmp repo) in
  let updated_cached_packages = OpamPackage.Set.filter (fun nv ->
    let url_f = OpamPath.Repository.url repo nv in
    let url = OpamFile.URL.read url_f in
    let kind = match OpamFile.URL.kind url with
      | None   -> kind_of_url (OpamFile.URL.url url)
      | Some k -> k in
    let checksum = OpamFile.URL.checksum url in
    let url = OpamFile.URL.url url in
    log "updating %s:%s" url kind;
    match OpamFilename.in_dir dir (fun () -> download_one kind nv url checksum) with
    | Not_available -> OpamGlobals.error_and_exit "Cannot get %s" url
    | Up_to_date _  -> false
    | Result _      -> true
  ) cached_packages in

  let updated = OpamPackage.Set.union updated_packages updated_cached_packages in
  OpamFile.Updated.write (OpamPath.Repository.updated repo) updated

let find_backend = find_backend_by_kind

let packages r =
  let dir = OpamPath.Repository.packages_dir r in
  if OpamFilename.exists_dir dir then (
    let all = OpamFilename.list_dirs dir in
    let basenames = List.map OpamFilename.basename_dir all in
    OpamPackage.Set.of_list
      (OpamMisc.filter_map
         (OpamFilename.Base.to_string |> OpamPackage.of_string_opt)
         basenames)
  ) else
    OpamPackage.Set.empty

let versions r n =
  OpamPackage.versions_of_packages
    (OpamPackage.Set.filter
       (fun nv -> OpamPackage.name nv = n)
       (packages r))

let archives r =
  let d = OpamPath.Repository.archives_dir r in
  if OpamFilename.exists_dir d then
    OpamFilename.Set.of_list (OpamFilename.list_files d)
  else
    OpamFilename.Set.empty

let compilers r =
  OpamCompiler.list (OpamPath.Repository.compilers_dir r)

let files r nv =
  let l =
    if OpamFilename.exists_dir (OpamPath.Repository.files r nv) then
      OpamFilename.list_files (OpamPath.Repository.files r nv)
    else
      [] in
  OpamFilename.Set.of_list l

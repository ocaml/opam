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

open Types

let log fmt = Globals.log "REPOSITORIES" fmt

type kind = string
type address = dirname

module type BACKEND = sig
  val init: address -> unit
  val update: address -> Filename.Set.t
  val download_archive: address -> nv -> filename download
  val download_file: nv -> filename -> filename download
  val download_dir: nv -> dirname -> dirname download
  val upload_dir: address:address -> dirname -> Filename.Set.t
end

exception Unknown_backend

let backends = Hashtbl.create 8

let find_backend r =
  try
    Hashtbl.find backends (Repository.kind r)
  with Not_found -> raise Unknown_backend

let find_backend_by_kind k =
  try
    Hashtbl.find backends k
  with Not_found -> raise Unknown_backend

let register_backend name backend =
  Hashtbl.replace backends name backend

(* initialize the current directory *)
let init r =
  log "init %s" (Repository.to_string r);
  let root = Path.R.create r in
  let module B = (val find_backend r: BACKEND) in
  Dirname.mkdir (Path.R.root root);
  File.Repo_config.write (Path.R.config root) r;
  Dirname.mkdir (Path.R.packages_dir root);
  Dirname.mkdir (Path.R.archives_dir root);
  Dirname.mkdir (Path.R.compilers_dir root);
  Dirname.mkdir (Path.R.upload_dir root);
  Dirname.in_dir (Path.R.root root) (fun () -> B.init (Repository.address r))

let nv_set_of_files files =
  NV.Set.of_list (Utils.filter_map NV.of_filename (Filename.Set.elements files))

(* upload the content of ./upload to the given OPAM repository *)
let upload r =
  log "upload %s" (Repository.to_string r);
  let local_repo = Path.R.create r in
  let local_dir = Path.R.root local_repo in
  let upload_dir = Path.R.upload_dir local_repo in
  let address = Repository.address r in
  let module B = (val find_backend r: BACKEND) in
  let files = Dirname.in_dir local_dir (fun () -> B.upload_dir ~address upload_dir) in
  let packages = nv_set_of_files files in
  Globals.msg "The following packages have been uploaded:\n";
  NV.Set.iter (fun nv ->
    Globals.msg "  - %s\n" (NV.to_string nv)
  ) packages

(* Download file f in the current directory *)
let map fn = function
  | Result x      -> Result (fn x)
  | Up_to_date x  -> Up_to_date (fn x)
  | Not_available -> Not_available

let download_file ~gener_digest k nv f c =
  log "download_file %s %s %s" k (NV.to_string nv) (Filename.to_string f);
  let module B = (val find_backend_by_kind k: BACKEND) in
  let check file = match c with
    | None   -> true
    | Some c -> Filename.digest file = c in
  let rename file =
    if not gener_digest && !Globals.verify_checksums && not (check file) then
      Globals.error_and_exit "Wrong checksum for %s (waiting for %s, got %s)"
        (Filename.to_string file)
        (match c with Some c -> c | None -> "<none>")
        (Filename.digest file);
    if k = "curl" && not (Run.is_tar_archive (Filename.to_string f)) then
      let new_file = Filename.raw (Filename.to_string file ^ ".tar.gz") in
      Filename.move file new_file;
      new_file
    else
      file in
  map rename (B.download_file nv f)

(* Download directory d in the current directory *)
let download_dir k nv d =
  log "download_dir %s %s %s" k (NV.to_string nv) (Dirname.to_string d);
  let module B = (val find_backend_by_kind k: BACKEND) in
  B.download_dir nv d

(* Download either a file or a directory in the current directory *)
let download_one ?(gener_digest = false) k nv url checksum =
  let f x = F x in
  let d x = D x in
  if k = "curl" || Run.is_tar_archive url then
    map f (download_file ~gener_digest k nv (Filename.raw url) checksum)
  else
    map d (download_dir k nv (Dirname.raw url))

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
  B.download_archive (Repository.address r) nv

let make_archive ?(gener_digest = false) nv =
  (* download the archive upstream if the upstream address is
     specified *)
  let local_repo = Path.R.cwd () in
  let local_dir = Path.R.root local_repo in
  let url_f = Path.R.url local_repo nv in

  let download_dir = Path.R.tmp_dir local_repo nv in
  Dirname.mkdir download_dir;

  Dirname.with_tmp_dir (fun extract_root ->
    let extract_dir = extract_root / NV.to_string nv in

    if Filename.exists url_f then (
      let url_file = File.URL.read url_f in
      let checksum = File.URL.checksum url_file in
      let kind = match File.URL.kind url_file with
      | None   -> kind_of_url (File.URL.url url_file)
      | Some k -> k in
      let url = File.URL.url url_file in
      log "downloading %s:%s" url kind;

      match Dirname.in_dir local_dir (fun () -> download_one ~gener_digest kind nv url checksum) with
      | Not_available -> Globals.error_and_exit "Cannot get %s" url
      | Up_to_date (F local_archive)
      | Result (F local_archive) ->
          if gener_digest then (
            let digest = Filename.digest local_archive in
            begin match checksum with
            | Some c when c <> digest ->
              Globals.warning
                "Wrong checksum for %s (in cache: %s, new downloaded: %s). \
                 Update by keeping the downloaded digest..."
                (Filename.to_string local_archive) c digest
            | _ -> ();
              File.URL.write url_f (File.URL.with_checksum url_file (Some digest));
            end;
          );
          log "extracting %s to %s"
            (Filename.to_string local_archive)
            (Dirname.to_string extract_dir);
          Filename.extract local_archive extract_dir
      | Up_to_date (D dir)
      | Result (D dir) ->
          log "copying %s to %s"
            (Dirname.to_string dir)
            (Dirname.to_string extract_dir);
          if dir <> extract_dir then
          Dirname.copy download_dir extract_dir
    );

    (* Eventually add the <package>/files/* to the extracted dir *)
    log "Adding the files to the archive";
    let files = Path.R.available_files local_repo nv in
    if files <> [] then (
      if not (Dirname.exists extract_dir) then
        Dirname.mkdir extract_dir;
      List.iter (fun f ->
        let dst = extract_dir // Basename.to_string (Filename.basename f) in
        if Filename.exists dst then
          Globals.warning
            "Skipping %s as it already exists in %s\n"
            (Filename.to_string f)
            (Dirname.to_string extract_dir)
        else
          Filename.copy_in f extract_dir) files;
    );

    (* And finally create the final archive *)
    (* XXX: we should add a suffix to the version to show that
       the archive has been repacked by opam *)
    if files <> [] || Filename.exists url_f then (
      Dirname.mkdir (Path.R.archives_dir local_repo);
      let local_archive = Path.R.archive local_repo nv in
      log "Creating the archive files in %s" (Filename.to_string local_archive);
      Dirname.exec extract_root [
        [ "tar" ; "czf" ; Filename.to_string local_archive ; NV.to_string nv ]
      ]
    );
  )

(* Download the archive on the OPAM server.
   If it is not there, then:
   * download the original archive upstream
   * add eventual patches
   * create a new tarball *)
let download r nv =
  log "download %s %s" (Repository.to_string r) (NV.to_string nv);
  let local_repo = Path.R.create r in
  let local_dir = Path.R.root local_repo in
  (* If the archive is on the server, download it directly *)
  match Dirname.in_dir local_dir (fun () -> download_archive r nv) with
  | Up_to_date local_file ->
      Globals.msg "The archive for %s is in the local cache.\n" (NV.to_string nv);
      log "The archive for %s is already downloaded and up-to-date"
        (NV.to_string nv)
  | Result local_file ->
      log "Downloaded %s successfully" (Filename.to_string local_file)
  | Not_available ->
      log "The archive for %s is not available, need to build it"
        (NV.to_string nv);
      Dirname.in_dir local_dir (fun () -> make_archive nv)

let update r =
  log "update %s" (Repository.to_string r);
  let local_repo = Path.R.create r in
  let local_dir = Path.R.root local_repo in
  let module B = (val find_backend r: BACKEND) in
  let updated_files = Dirname.in_dir local_dir (fun () -> B.update (Repository.address r)) in
  let updated_packages = nv_set_of_files updated_files in

  (* Clean-up archives and tmp files on URL changes *)
  NV.Set.iter (fun nv ->
    let url_f = Path.R.url local_repo nv in
    if Filename.Set.mem url_f updated_files then begin
      let tmp_dir = Path.R.tmp_dir local_repo nv in
      Dirname.rmdir tmp_dir;
      Filename.remove (Path.R.archive local_repo nv);
    end
  ) updated_packages;

  (* For each package in the cache, look at what changed upstream *)
  let cached_packages = Path.R.available_tmp local_repo in
  let updated_cached_packages = NV.Set.filter (fun nv ->
    let url_f = Path.R.url local_repo nv in
    let url = File.URL.read url_f in
    let kind = match File.URL.kind url with
      | None   -> kind_of_url (File.URL.url url)
      | Some k -> k in
    let checksum = File.URL.checksum url in
    let url = File.URL.url url in
    log "updating %s:%s" url kind;
    match Dirname.in_dir local_dir (fun () -> download_one kind nv url checksum) with
    | Not_available -> Globals.error_and_exit "Cannot get %s" url
    | Up_to_date _  -> false
    | Result _      -> true
  ) cached_packages in

  let updated = NV.Set.union updated_packages updated_cached_packages in
  File.Updated.write (Path.R.updated local_repo) updated

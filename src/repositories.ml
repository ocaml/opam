(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
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

let backends = Hashtbl.create 8

let find_backend r =
  Hashtbl.find backends (Repository.kind r)

let find_backend_by_kind k =
  Hashtbl.find backends k

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

let download_file k nv f =
  log "download_file %s %s %s" k (NV.to_string nv) (Filename.to_string f);
  let module B = (val find_backend_by_kind k: BACKEND) in
  let rename file =
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
let download_one k nv url =
  let f x = F x in
  let d x = D x in
  if k = "curl" || Run.is_tar_archive url then
    map f (download_file k nv (Filename.raw url))
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

let make_archive nv =
  (* download the archive upstream if the upstream address is
     specified *)
  let local_repo = Path.R.cwd () in
  let local_dir = Path.R.root local_repo in
  let url_f = Path.R.url local_repo nv in

  let download_dir = Path.R.tmp_dir local_repo nv in
  Dirname.mkdir download_dir;

  Dirname.with_tmp_dir (fun extract_root ->
    let extract_dir = extract_root / NV.to_string nv in

    if Filename.exists url_f then begin
      let url = File.URL.read url_f in
      let kind = match File.URL.kind url with
      | None   -> kind_of_url (File.URL.url url)
      | Some k -> k in
      let url = File.URL.url url in
      log "downloading %s:%s" url kind;

      match Dirname.in_dir local_dir (fun () -> download_one kind nv url) with
      | Not_available -> Globals.error_and_exit "Cannot get %s" url
      | Up_to_date (F local_archive)
      | Result (F local_archive) ->
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
    end;
    
    (* Eventually add the files/<package>/* to the extracted dir *)
    log "Adding the files to the archive";
    let files = Path.R.available_files local_repo nv in
    if not (Dirname.exists extract_dir) then
    Dirname.mkdir extract_dir;
    List.iter (fun f -> Filename.copy_in f extract_dir) files;

    (* And finally create the final archive *)
    (* XXX: we should add a suffix to the version to show that
       the archive has been repacked by opam *)
    Dirname.mkdir (Path.R.archives_dir local_repo);
    let local_archive = Path.R.archive local_repo nv in
    log "Creating the archive files in %s" (Filename.to_string local_archive);
    let err = Dirname.exec extract_root [
      [ "tar" ; "czf" ; Filename.to_string local_archive ; NV.to_string nv ]
    ] in
    if err <> 0 then
      Globals.error_and_exit "Cannot compress %s" (Dirname.to_string extract_dir)
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
  let files = Dirname.in_dir local_dir (fun () -> B.update (Repository.address r)) in
  let packages = nv_set_of_files files in
  
  let cached_packages = Path.R.available_tmp local_repo in

  (* Clean-up outdated archives *)
  NV.Set.iter (fun nv ->
    let archive = Path.R.archive local_repo nv in
    if Filename.Set.mem archive files then
      Filename.remove archive
  ) cached_packages;

  (* Clean-up tmp files when the URL change *)
  NV.Set.iter (fun nv ->
    let url_f = Path.R.url local_repo nv in
    if Filename.Set.mem url_f files then begin
      let tmp_dir = Path.R.tmp_dir local_repo nv in
      Dirname.rmdir tmp_dir;
    end
  ) cached_packages;

  (* For each URL file, look at changes upstream *)
  let updated_cached_packages = NV.Set.filter (fun nv ->
    let archive = Path.R.archive local_repo nv in
    if not (Filename.exists archive) then
      let url_f = Path.R.url local_repo nv in
      let url = File.URL.read url_f in
      let kind = match File.URL.kind url with
      | None   -> kind_of_url (File.URL.url url)
      | Some k -> k in
      let url = File.URL.url url in
      log "updating %s:%s" url kind;
      match Dirname.in_dir local_dir (fun () -> download_one kind nv url) with
      | Not_available -> Globals.error_and_exit "Cannot get %s" url
      | Up_to_date _  -> false
      | Result _      -> true
    else
      false
  ) cached_packages in

  let updated = NV.Set.union packages updated_cached_packages in
  File.Updated.write (Path.R.updated local_repo) updated

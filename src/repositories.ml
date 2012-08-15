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

let log fmt = Globals.log "REPO" fmt

type kind = string
module type BACKEND = sig
  val init: repository -> unit
  val update: repository -> Filename.Set.t
  val download_archive: repository -> nv -> filename download
  val download_file: repository -> nv -> filename -> filename download
  val download_dir: repository -> nv -> dirname  -> dirname download
  val upload_dir: repository -> dirname -> Filename.Set.t
end

let backends = Hashtbl.create 8

let find_backend r =
  Hashtbl.find backends (Repository.kind r)

let register_backend name backend =
  Hashtbl.replace backends name backend

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
  B.init r

let nv_set_of_files files =
  NV.Set.of_list (Utils.filter_map NV.of_filename (Filename.Set.elements files))

let upload r =
  log "upload %s" (Repository.to_string r);
  let root = Path.R.create r in
  let module B = (val find_backend r: BACKEND) in
  let files = B.upload_dir r (Path.R.upload_dir root) in
  let packages = nv_set_of_files files in
  Globals.msg "The following packages have been uploaded:\n";
  NV.Set.iter (fun nv ->
    Globals.msg "  - %s\n" (NV.to_string nv)
  ) packages

let download_file r nv f =
  log "download_file %s %s %s"
    (Repository.to_string r)
    (NV.to_string nv)
    (Filename.to_string f);
  let module B = (val find_backend r: BACKEND) in
  B.download_file r nv f

let download_dir r nv d =
  log "download_dir %s %s %s"
    (Repository.to_string r)
    (NV.to_string nv)
    (Dirname.to_string d);
  let module B = (val find_backend r: BACKEND) in
  B.download_dir r nv d

let download_one r nv url =
  let map fn = function
    | Result x      -> Result (fn x)
    | Up_to_date x  -> Up_to_date (fn x)
    | Not_available -> Not_available in
  let f x = F x in
  let d x = D x in
  if Run.is_tar_archive url then
    map f (download_file r nv (Filename.raw url))
  else
    map d (download_dir r nv (Dirname.raw url))

let kind_of_repository r =
  match Repository.kind r with
  | "server" -> "server"
  | x ->
      if Dirname.exists (Repository.address r) then
        "rsync"
      else
        "curl"

(* Download the archive on the OPAM server.
   If it is not there, then:
   * download the original archive upstream
   * add eventual patches
   * create a new tarball *)
let download r nv =
  log "download %s %s" (Repository.to_string r) (NV.to_string nv);
  let local_repo = Path.R.create r in
  let module B = (val find_backend r: BACKEND) in

  (* If the archive is on the server, download it directly *)

  match B.download_archive r nv with
  | Up_to_date local_file ->
      log "The archive for %s is already downloaded and up-to-date"
        (NV.to_string nv)
  | Result local_file ->
      log "Downloaded %s successfully" (Filename.to_string local_file)
  | Not_available ->
      log "The archive for %s is not available, need to build it"
        (NV.to_string nv);

      (* download the archive upstream if the upstream address
         is specified *)
      let url_f = Path.R.url local_repo nv in
      let download_dir = Path.R.tmp_dir local_repo nv in
      Dirname.mkdir download_dir;

      Dirname.with_tmp_dir (fun extract_root ->
        let extract_dir = extract_root / NV.to_string nv in

        if Filename.exists url_f then begin
          let url = File.URL.read url_f in
          let kind = match File.URL.kind url with
            | None   -> kind_of_repository r
            | Some k -> k in
          let url = File.URL.url url in
          log "downloading %s:%s" url kind;
          let r2 = Repository.with_kind r kind in

          match Dirname.in_dir download_dir (fun () -> download_one r2 nv url) with
          | Not_available -> Globals.error_and_exit "Cannot get %s" url
          | Up_to_date (F local_archive)
          | Result (F local_archive) ->
              log "extracting %s to %s"
                (Filename.to_string local_archive)
                (Dirname.to_string extract_dir);
              Filename.extract local_archive extract_dir
          | Up_to_date (D local_dir)
          | Result (D local_dir) ->
              log "copying %s to %s"
                (Dirname.to_string local_dir)
                (Dirname.to_string extract_dir);
              if local_dir <> extract_dir then
                Dirname.copy local_dir extract_dir
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
        let local_archive = Path.R.archive local_repo nv in
        log "Creating the archive files in %s" (Filename.to_string local_archive);
        let err = Dirname.exec extract_root [
          [ "tar" ; "czf" ; Filename.to_string local_archive ; NV.to_string nv ]
        ] in
        if err <> 0 then
          Globals.error_and_exit "Cannot compress %s" (Dirname.to_string extract_dir)
      )

(* XXX: clean-up + update when the url change *)
(* XXX: update when the thing pointed by the url change *)
let update r =
  log "update %s" (Repository.to_string r);
  let root = Path.R.create r in
  let module B = (val find_backend r: BACKEND) in
  let files = B.update r in
  let packages = nv_set_of_files files in
  File.Updated.write (Path.R.updated root) packages

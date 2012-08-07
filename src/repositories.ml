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

let run cmd repo args =
  log "opam-%s: %s %s" cmd (Repository.to_string repo) (String.concat " " args);
  let path = Path.R.root (Path.R.create repo) in
  let cmd =  Printf.sprintf "opam-%s-%s" (Repository.kind repo) cmd in
  let i = Run.in_dir (Dirname.to_string path) (fun () ->
    Run.command ( cmd :: Dirname.to_string (Repository.address repo) :: args );
  ) in
  if i <> 0 then
    Globals.error_and_exit "%s failed" cmd

let init r =
  let root = Path.R.create r in
  Dirname.mkdir (Path.R.root root);
  run "init" r [];
  File.Repo_config.write (Path.R.config root) r;
  Dirname.mkdir (Path.R.packages_dir root);
  Dirname.mkdir (Path.R.archives_dir root);
  Dirname.mkdir (Path.R.compilers_dir root);
  Dirname.mkdir (Path.R.upload_dir root)

let update r =
  run "update" r []

let upload r =
  run "upload" r []

type download_info = {
  filename: filename;
  nv      : nv;
  force   : bool;
}

let download_info d = (d.nv, d.filename)

let string_of_download_info d =
  Printf.sprintf "<filename=%s nv=%s force=%s>" 
    (Filename.to_string d.filename) (NV.to_string d.nv) (string_of_bool d.force)

let read_download_info () =
  let filename = Filename.of_string Sys.argv.(1) in
  let nv = NV.of_string Sys.argv.(2) in
  let force = bool_of_string Sys.argv.(3) in
  { filename; force; nv }

type file = D of dirname | F of filename

let file filename =
  if Sys.is_directory filename then
    F (Filename.of_string filename)
  else
    D (Dirname.of_string filename)

let download_one d kind dirname =
  let cmd = Printf.sprintf "opam-%s-download" kind in
  let output = Dirname.in_dir dirname (fun () ->
    Run.read_command_output [
      cmd; Filename.to_string d.filename; NV.to_string d.nv; string_of_bool d.force
    ]) in
  match output with
  | None        -> None
  | Some []     -> failwith "download error"
  | Some (f::_) -> Some (file f)

let rec download_iter nv dirname = function
  | []       -> None
  | (f,k)::t ->
      let d = {
        filename = f;
        force    = false;
        nv;
      } in
      match download_one d k dirname with
      | None -> download_iter nv dirname t
      | r    -> r

let kind_of_file filename =
  if Filename.exists filename then "rsync" else "curl"

let download r nv =
  let remote_repo = Path.R.of_dirname (Repository.address r) in
  let local_repo = Path.R.create r in
  Dirname.with_tmp_dir (fun tmp_download_dir ->
    Dirname.with_tmp_dir (fun tmp_extract_root ->
      let tmp_extract_dir = tmp_extract_root / NV.to_string nv in

      (* If the archive is on the server, download it directly *)
      let remote_archive = Path.R.archive remote_repo nv in
      let kind = kind_of_file remote_archive in
      let dirname = Path.R.archives_dir local_repo in 
      let download_info = {
        filename = remote_archive;
        force    = true;
        nv;
      } in
      match download_one download_info kind dirname with
      | Some f -> ()
      | None   ->
          log
            "%s is not on the server, need to build it"
            (Filename.to_string remote_archive);
          let url_f = Path.R.url local_repo nv in

          if Filename.exists url_f then begin
            (* download the archive upstream if the upstream address
               is specified *)
            let urls = File.URL.read url_f in
            let urls =
              List.map (function
                | (f,Some k) -> (f,k)
                | (f,None) -> (f, kind_of_file f)
              ) urls in
            let urls_s =
              String.concat " "
                (List.map
                   (fun (f,k) -> Printf.sprintf "%s:%s" (Filename.to_string f) k)
                   urls) in
            log "downloading %s" urls_s;
            match download_iter nv tmp_download_dir urls with
            | None -> Globals.error_and_exit "Cannot get %s" urls_s
            | Some (F local_archive) ->
                log "extracting %s to %s"
                  (Filename.to_string local_archive)
                  (Dirname.to_string tmp_extract_dir);
                Filename.extract local_archive tmp_extract_dir
            | Some (D local_dir) ->
                Dirname.move local_dir tmp_extract_dir
          end;
            
          (* Eventually add the files/<package>/* to the extracted dir *)
          log "Adding the files to the archive";
          let files = Path.R.available_files local_repo nv in
          List.iter (fun f -> Filename.copy_in f tmp_extract_dir) files;

          (* And finally create the final archive *)
          (* XXX: we should add a suffix to the version to show that
             the archive has been repacked by opam *)
          let local_archive = Path.R.archive local_repo nv in
          log "Creating the archive files in %s" (Filename.to_string local_archive);
          log "Files in tmp_extract_root: %s"
            (Filename.Set.to_string (Filename.Set.of_list (Filename.list tmp_extract_root)));
          let err = Dirname.exec tmp_extract_root [
            [ "tar" ; "czf" ; Filename.to_string local_archive ; NV.to_string nv ]
          ] in
          if err <> 0 then
            Globals.error_and_exit "Cannot compress %s" (Dirname.to_string tmp_extract_dir);
    )
  )

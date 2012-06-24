(* Download scrip for curl-ed repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-address> <package>" Sys.argv.(0);
    exit 1
  )

let package = Sys.argv.(2)

module F = Filename
open Types
open Misc

let tmp_download_dir = Dirname.of_string (Run.mk_temp_dir "curl-download")
let tmp_extract_root = Dirname.of_string (Run.mk_temp_dir "curl-extract")
let tmp_extract_dir  = tmp_extract_root / package

let nv = NV.of_string package

let archive_name src =
  let name = F.basename src in
  if F.check_suffix name ".tar.gz"
  || F.check_suffix name ".tar.bz2"
  || F.check_suffix name ".tgz"
  || F.check_suffix name ".tbz" then
    name
  else    
    Printf.sprintf "%s.tar.gz" name

let raw_mv src =
  let name = archive_name src in
  if (F.basename src) = name then
    []
  else
    [ "mv" ; F.basename src ; name ]

let download_external_archive url =
  Dirname.exec tmp_download_dir [raw_wget url]

let () =
  Dirname.mkdir tmp_download_dir;
  Dirname.mkdir tmp_extract_dir;

  (* If the archive is there, download it directly *)
  let remote_archive = Path.R.archive remote_repo nv in
  match download_remote_file remote_archive with
  | Some _ -> ()
  | None   ->
      log
        "%s is not on the server, need to build it"
        (Filename.to_string remote_archive);
      (* Otherwise, need to download it from the given url *)
      begin match FileUrl.read (Path.R.url local_repo nv) with
      | None     -> ()
      | Some url ->
          log "downloading %s" url;
          let err = Dirname.exec tmp_download_dir [
            raw_wget url;
            raw_mv url;
          ] in
          if err <> 0 then
            Globals.error_and_exit "Cannot get %s" url;
          let local_archive = tmp_download_dir // F.basename (archive_name url) in
          log "extracting %s to %s"
            (Filename.to_string local_archive)
            (Dirname.to_string tmp_extract_dir);
          Filename.extract local_archive tmp_extract_dir;
      end;
      (* Also add the files/<package>/* to the extracted dir *)
      log "Adding the files to the archive";
      let files =
        let _files = download_remote_dir (Path.R.files remote_repo nv) in
        Path.R.available_files local_repo nv in
      List.iter (fun f -> Filename.copy_in f tmp_extract_dir) files;
      let local_archive = Path.R.archive local_repo nv in
      log "Creating the archive files in %s" (Filename.to_string local_archive);
      let err = Dirname.exec tmp_extract_root [
        [ "tar" ; "czf" ; Filename.to_string local_archive ; package ]
      ] in
      if err <> 0 then
        Globals.error_and_exit "Cannot compress %s" (Dirname.to_string tmp_extract_dir)

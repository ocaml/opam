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
open OpamStd.Op
open OpamProcess.Job.Op

let log msg = OpamConsole.log "CURL" msg
let slog = OpamConsole.slog

let index_archive_name = "index.tar.gz"

let local_index_archive repo_root =
  OpamFilename.Op.(repo_root // index_archive_name)
let remote_index_archive url = OpamUrl.Op.(url / index_archive_name)

let sync_state name repo_root url =
  OpamDownload.download_as ~overwrite:true
    (remote_index_archive url)
    (local_index_archive repo_root)
  @@+ fun () ->
  List.iter OpamFilename.rmdir (OpamFilename.dirs repo_root);
  OpamFilename.extract_in (local_index_archive repo_root) repo_root;
  OpamConsole.msg "[%s] synchronized from %s\n"
    (OpamConsole.colorise `blue
       (OpamRepositoryName.to_string name))
    (OpamUrl.to_string url);
  Done ()

module B = struct

  let name = `http

  let pull_repo repo_name repo_root url =
    log "pull-repo";
    sync_state repo_name repo_root url

  let pull_url dirname checksum remote_url =
    log "pull-file into %a: %a"
      (slog OpamFilename.Dir.to_string) dirname
      (slog OpamUrl.to_string) remote_url;
    OpamProcess.Job.catch
      (fun e ->
         OpamStd.Exn.fatal e;
         let msg =
           Printf.sprintf "%s (%s)" (OpamUrl.to_string remote_url)
             (match e with
              | Failure msg -> msg
              | _ -> "download failed")
         in
         Done (Not_available msg))
    @@ fun () ->
    OpamDownload.download ~overwrite:true ?checksum remote_url dirname
    @@+ fun local_file -> Done (Result (F local_file))

  let revision _ =
    Done None

end

(* Helper functions used by opam-admin *)

let make_index_tar_gz repo_root =
  OpamFilename.in_dir repo_root (fun () ->
    let to_include = [ "version"; "packages"; "repo" ] in
    match List.filter Sys.file_exists to_include with
    | [] -> ()
    | d  -> OpamSystem.command ("tar" :: "czhf" :: "index.tar.gz" :: d)
  )

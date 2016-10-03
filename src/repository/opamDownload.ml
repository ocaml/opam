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
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "CURL" fmt

let user_agent =
  FString (Printf.sprintf "opam/%s" (OpamVersion.(to_string current)))

let curl_args = [
  CString "--write-out", None;
  CString "%%{http_code}\\n", None;
  CString "--retry", None; CIdent "retry", None;
  CString "--retry-delay", None; CString "2", None;
  CString "--compressed",
  Some (FIdent (OpamFilter.ident_of_string "compressed"));
  CString "--user-agent", Some user_agent;
  CString "-L", None;
  CString "-o", None; CIdent "out", None;
  CIdent "url", None;
]

let wget_args = [
  CString "--content-disposition", None;
  CString "-t", None; CIdent "retry", None;
  CString "-O", None; CIdent "out", None;
  CIdent "url", None;
  CString "-U", Some user_agent
]

let download_args ~url ~out ~retry ?checksum ~compress =
  let cmd, _ = Lazy.force OpamRepositoryConfig.(!r.download_tool) in
  let cmd =
    match cmd with
    | [(CIdent "wget"), _] -> cmd @ wget_args
    | [_] -> cmd @ curl_args (* Assume curl if the command is a single arg *)
    | _ -> cmd
  in
  OpamFilter.single_command (fun v ->
      if not (OpamVariable.Full.is_global v) then None else
      match OpamVariable.to_string (OpamVariable.Full.variable v) with
      | "curl" -> Some (S "curl")
      | "wget" -> Some (S "wget")
      | "url" -> Some (S (OpamUrl.to_string url))
      | "out" -> Some (S out)
      | "retry" -> Some (S (string_of_int retry))
      | "compress" -> Some (B compress)
      | "checksum" ->
        OpamStd.Option.map (fun c -> S (OpamHash.to_string c)) checksum
      | _ -> None)
    cmd

let tool_return url ret =
  match Lazy.force OpamRepositoryConfig.(!r.download_tool) with
  | _, `Default -> Done (OpamSystem.raise_on_process_error ret)
  | _, `Curl ->
    OpamSystem.raise_on_process_error ret;
    match ret.OpamProcess.r_stdout with
    | [] ->
      OpamSystem.internal_error "curl: empty response while downloading %s"
        (OpamUrl.to_string url)
    | l  ->
      let code = List.hd (List.rev l) in
      let num = try int_of_string code with Failure _ -> 999 in
      if num >= 400 then
        OpamSystem.internal_error "curl: code %s while downloading %s" code
          (OpamUrl.to_string url)
      else Done ()

let download_command ~compress ?checksum ~url ~dst =
  let cmd, args =
    match
      download_args
        ~url
        ~out:dst
        ~retry:OpamRepositoryConfig.(!r.retries)
        ?checksum
        ~compress
    with
    | cmd::args -> cmd, args
    | [] -> OpamConsole.error_and_exit "Empty custom download command"
  in
  OpamSystem.make_command cmd args @@> tool_return url

let really_download ~overwrite ?(compress=false) ?checksum ~url ~dst =
  assert (url.OpamUrl.backend = `http);
  let tmp_dst = dst ^ ".part" in
  if Sys.file_exists tmp_dst then OpamSystem.remove tmp_dst;
  OpamProcess.Job.catch
    (function
      | OpamSystem.Internal_error s as e ->
        OpamSystem.remove tmp_dst;
        OpamConsole.error "%s" s;
        raise e
      | e ->
        OpamSystem.remove tmp_dst;
        OpamStd.Exn.fatal e;
        log "Could not download file at %s." (OpamUrl.to_string url);
        raise e)
    (download_command ~compress ?checksum ~url ~dst:tmp_dst
     @@+ fun () ->
     if not (Sys.file_exists tmp_dst) then
       OpamSystem.internal_error "Downloaded file not found"
     else if Sys.file_exists dst && not overwrite then
       OpamSystem.internal_error "The downloaded file will overwrite %s." dst;
     if OpamRepositoryConfig.(!r.force_checksums <> Some false) then
       OpamStd.Option.iter (fun cksum ->
         if not (OpamHash.check_file tmp_dst cksum) then
           failwith (Printf.sprintf "Bad checksum for %s (expected %s)"
                       (OpamUrl.to_string url) (OpamHash.to_string cksum)))
         checksum;
     OpamSystem.mv tmp_dst dst;
     Done ())

let download_as ~overwrite ?compress ?checksum url dst =
  match OpamUrl.local_file url with
  | Some src ->
    if src = dst then Done () else
      (if OpamFilename.exists dst then
         if overwrite then OpamFilename.remove dst else
           OpamSystem.internal_error "The downloaded file will overwrite %s."
             (OpamFilename.to_string dst);
       OpamFilename.copy ~src ~dst;
       Done ())
  | None ->
    OpamFilename.(mkdir (dirname dst));
    really_download ~overwrite ?compress ?checksum
      ~url
      ~dst:(OpamFilename.to_string dst)

let download ~overwrite ?compress ?checksum url dstdir =
  let dst =
    OpamFilename.(create dstdir (Base.of_string (OpamUrl.basename url)))
  in
  download_as ~overwrite ?compress ?checksum url dst @@| fun () -> dst

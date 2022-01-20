(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
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

exception Download_fail of string option * string
let fail (s,l) = raise (Download_fail (s,l))

let user_agent =
  CString (Printf.sprintf "opam/%s" (OpamVersion.(to_string current)))

let curl_args = [
  CString "--write-out", None;
  CString "%%{http_code}\\n", None;
  CString "--retry", None; CIdent "retry", None;
  CString "--retry-delay", None; CString "2", None;
  CString "--compressed",
  Some (FIdent (OpamFilter.ident_of_string "compress"));
  CString "--user-agent", None; user_agent, None;
  CString "-L", None;
  CString "-o", None; CIdent "out", None;
  CString "--", None; (* End list of options *)
  CIdent "url", None;
]

let wget_args = [
  CString "--content-disposition", None;
  CString "-t", None; CIdent "retry", None;
  CString "-O", None; CIdent "out", None;
  CString "-U", None; user_agent, None;
  CString "--", None; (* End list of options *)
  CIdent "url", None;
]

let fetch_args = [
  CString "-o", None; CIdent "out", None;
  CString "--user-agent", None; user_agent, None;
  CString "--", None; (* End list of options *)
  CIdent "url", None;
]

let ftp_args = [
  CString "-o", None; CIdent "out", None;
  CString "-U", None; user_agent, None;
  CString "--", None; (* End list of options *)
  CIdent "url", None;
]

let download_args ~url ~out ~retry ?checksum ~compress () =
  let cmd, _ = Lazy.force OpamRepositoryConfig.(!r.download_tool) in
  let cmd =
    match cmd with
    | [(CIdent "wget"), _] -> cmd @ wget_args
    | [(CIdent "fetch"), _] -> cmd @ fetch_args
    | [(CIdent "ftp"), _] -> cmd @ ftp_args
    | [_] -> cmd @ curl_args (* Assume curl if the command is a single arg *)
    | _ -> cmd
  in
  OpamFilter.single_command (fun v ->
      if not (OpamVariable.Full.is_global v) then None else
      match OpamVariable.to_string (OpamVariable.Full.variable v) with
      | ("curl" | "wget" | "fetch" | "ftp") as dl_tool-> Some (S dl_tool)
      | "url" -> Some (S (OpamUrl.to_string url))
      | "out" -> Some (S out)
      | "retry" -> Some (S (string_of_int retry))
      | "compress" -> Some (B compress)
      | "opam-version" -> Some (S OpamVersion.(to_string current))
      | "checksum" ->
        OpamStd.Option.map (fun c -> S OpamHash.(to_string c)) checksum
      | "hashalgo" ->
        OpamStd.Option.map (fun c -> S OpamHash.(string_of_kind (kind c)))
          checksum
      | "hashpath" ->
        OpamStd.Option.map
          (fun c -> S (String.concat Filename.dir_sep OpamHash.(to_path c)))
          checksum
      | "hashvalue" ->
        OpamStd.Option.map (fun c -> S OpamHash.(contents c)) checksum
      | _ -> None)
    cmd

let tool_return url ret =
  match Lazy.force OpamRepositoryConfig.(!r.download_tool) with
  | _, `Default ->
    if OpamProcess.is_failure ret then
      fail (Some "Download command failed",
                Printf.sprintf "Download command failed: %s"
                  (OpamProcess.result_summary ret))
    else Done ()
  | _, `Curl ->
    if OpamProcess.is_failure ret then
      fail (Some "Curl failed", Printf.sprintf "Curl failed: %s"
                  (OpamProcess.result_summary ret));
    match ret.OpamProcess.r_stdout with
    | [] ->
      fail (Some "curl empty response",
                Printf.sprintf "curl: empty response while downloading %s"
                  (OpamUrl.to_string url))
    | l  ->
      let code = List.hd (List.rev l) in
      let num = try int_of_string code with Failure _ -> 999 in
      if num >= 400 then
        fail (Some ("curl error code " ^ code),
                  Printf.sprintf "curl: code %s while downloading %s"
                    code (OpamUrl.to_string url))
      else Done ()

let download_command ~compress ?checksum ~url ~dst () =
  let cmd, args =
    match
      download_args
        ~url
        ~out:dst
        ~retry:OpamRepositoryConfig.(!r.retries)
        ?checksum
        ~compress
        ()
    with
    | cmd::args -> cmd, args
    | [] ->
      OpamConsole.error_and_exit `Configuration_error
        "Empty custom download command"
  in
  OpamSystem.make_command ~allow_stdin:false cmd args @@> tool_return url

let really_download
    ?(quiet=false) ~overwrite ?(compress=false) ?checksum ?(validate=true)
    ~url ~dst () =
  assert (url.OpamUrl.backend = `http);
  let tmp_dst = dst ^ ".part" in
  if Sys.file_exists tmp_dst then OpamSystem.remove tmp_dst;
  OpamProcess.Job.catch
    (function
      | Failure s as e ->
        OpamSystem.remove tmp_dst;
        if not quiet then OpamConsole.error "%s" s;
        raise e
      | e ->
        OpamSystem.remove tmp_dst;
        OpamStd.Exn.fatal e;
        log "Could not download file at %s." (OpamUrl.to_string url);
        raise e)
  @@ fun () ->
  download_command ~compress ?checksum ~url ~dst:tmp_dst ()
  @@+ fun () ->
  if not (Sys.file_exists tmp_dst) then
    fail (Some "Downloaded file not found",
          "Download command succeeded, but resulting file not found")
  else if Sys.file_exists dst && not overwrite then
    OpamSystem.internal_error "The downloaded file will overwrite %s." dst;
  if validate &&
     OpamRepositoryConfig.(!r.force_checksums <> Some false) then
    OpamStd.Option.iter (fun cksum ->
        if not (OpamHash.check_file tmp_dst cksum) then
          fail (Some "Bad checksum",
                    Printf.sprintf "Bad checksum, expected %s"
                      (OpamHash.to_string cksum)))
      checksum;
  OpamSystem.mv tmp_dst dst;
  Done ()

let download_as ?quiet ?validate ~overwrite ?compress ?checksum url dst =
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
    really_download ?quiet ~overwrite ?compress ?checksum ?validate
      ~url
      ~dst:(OpamFilename.to_string dst)
      ()

let download ?quiet ?validate ~overwrite ?compress ?checksum url dstdir =
  let dst =
    OpamFilename.(create dstdir (Base.of_string (OpamUrl.basename url)))
  in
  download_as ?quiet ?validate ~overwrite ?compress ?checksum url dst @@|
  fun () -> dst


(** Stdout output retrieval and post requests management *)

let post_tools = ["wget"; "curl"]
let check_post_tool () =
  match Lazy.force OpamRepositoryConfig.(!r.download_tool) with
  | [(CIdent cmd), _], _ -> List.mem cmd post_tools
  | _ -> false

let get_output ~post ?(args=[]) url =
  let cmd_args =
    download_args ~url ~out:"-" ~retry:OpamRepositoryConfig.(!r.retries)
      ~compress:false ()
    @ args
  in
  let cmd_args =
    if post then
      match cmd_args with
      | ("wget"|"curl" as cmd)::args -> Some (cmd :: ["-X"; "POST"] @ args)
      | _ -> None
    else Some cmd_args
  in
  Done (OpamStd.Option.map
          (OpamSystem.read_command_output ~ignore_stderr:true) cmd_args)

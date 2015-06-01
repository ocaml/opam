(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "CURL" fmt

let curl_args = [
  CString "--write-out", None;
  CString "%%{http_code}\\n", None;
  CString "--insecure", None;
  CString "--retry", None; CIdent "retry", None;
  CString "--retry-delay", None; CString "2", None;
  CString "--compressed",
  Some (FIdent (OpamFilter.ident_of_string "compressed"));
  CString "-L", None;
  CString "-o", None; CIdent "out", None;
  CIdent "url", None;
]

let wget_args = [
  CString "--content-disposition", None;
  CString "--no-check-certificate", None;
  CString "-t", None; CIdent "retry", None;
  CString "-O", None; CIdent "out", None;
  CIdent "url", None;
]

let download_args ~url ~out ~retry ?checksum ~compress =
  let cmd, _ = Lazy.force OpamRepositoryConfig.(!r.download_tool) in
  let cmd =
    match cmd with
    | [(CIdent "wget"|CString "wget"), _] -> cmd @ wget_args
    | [_] -> cmd @ curl_args (* Assume curl if the command is a single arg *)
    | _ -> cmd
  in
  OpamFilter.single_command (fun v ->
      if not (OpamVariable.Full.is_global v) then None else
      match OpamVariable.to_string (OpamVariable.Full.variable v) with
      | "curl" -> Some (S "curl")
      | "wget" -> Some (S "wget")
      | "url" -> Some (S url)
      | "out" -> Some (S out)
      | "retry" -> Some (S (string_of_int retry))
      | "compress" -> Some (B compress)
      | "checksum" -> OpamStd.Option.map (fun c -> S c) checksum
      | _ -> None)
    cmd

let tool_return src ret =
  match Lazy.force OpamRepositoryConfig.(!r.download_tool) with
  | _, `Default -> Done (OpamSystem.raise_on_process_error ret)
  | _, `Curl ->
    OpamSystem.raise_on_process_error ret;
    match ret.OpamProcess.r_stdout with
    | [] ->
      OpamSystem.internal_error "curl: empty response while downloading %s" src
    | l  ->
      let code = List.hd (List.rev l) in
      let num = try int_of_string code with Failure _ -> 999 in
      if num >= 400 then
        OpamSystem.internal_error "curl: code %s while downloading %s" code src
      else Done ()

let download_command ~compress ?checksum ~src ~dst =
  let cmd, args =
    match
      download_args
        ~url:src
        ~out:dst
        ~retry:OpamRepositoryConfig.(!r.retries)
        ?checksum
        ~compress
    with
    | cmd::args -> cmd, args
    | [] -> OpamConsole.error_and_exit "Empty custom download command"
  in
  OpamSystem.make_command cmd args @@> tool_return src

let really_download ~overwrite ?(compress=false) ?checksum ~src ~dst =
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
        log "Could not download file at %s." src;
        raise e)
    (download_command ~compress ?checksum ~src ~dst:tmp_dst
     @@+ fun () ->
     if not (Sys.file_exists tmp_dst) then
       OpamSystem.internal_error "Downloaded file not found"
     else if Sys.file_exists dst && not overwrite then
       OpamSystem.internal_error "The downloaded file will overwrite %s." dst;
     OpamSystem.mv tmp_dst dst;
     Done ())

let download_as ~overwrite ?compress ?checksum src dst =
  if dst = src then Done () else
  if OpamFilename.exists src then (
    if OpamFilename.exists dst then
      if overwrite then OpamFilename.remove dst else
        OpamSystem.internal_error "The downloaded file will overwrite %s."
          (OpamFilename.to_string dst);
    OpamFilename.copy ~src ~dst;
    Done ()
  ) else (
    OpamFilename.(mkdir (dirname dst));
    really_download ~overwrite ?compress ?checksum
      ~src:(OpamFilename.to_string src)
      ~dst:(OpamFilename.to_string dst)
  )

let download ~overwrite ?compress ?checksum src dstdir =
  let dst = OpamFilename.(create dstdir (basename src)) in
  download_as ~overwrite ?compress ?checksum src dst @@| fun () -> dst

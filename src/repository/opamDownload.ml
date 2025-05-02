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

let curl_args =
  let main_args = [
    CString "--retry", None; CIdent "retry", None; (* 7.12.3 20-Dec-2004 *)
    CString "--retry-delay", None; CString "2", None; (* 7.12.3 20-Dec-2004 *)
    CString "--compressed",
    Some (FIdent (OpamFilter.ident_of_string "compress")); (* 7.10 1-Oct-2002 *)
    CString "--user-agent", None; user_agent, None; (* 4.5.1 12-Jun-1998 *)
    CString "-L", None; (* 4.9 7-Oct-1998 *)
    CString "-o", None; CIdent "out", None; (* 2.3 21-Aug-1997 *)
    CString "--", None; (* End list of options; 5.0 1-Dec-1998 *)
    CIdent "url", None;
  ] in
  fun ~with_mitigation ->
    if with_mitigation then
      (* --fail is as old as curl; though the assumption that it leads to exit
         code 22 when there's an error is probably 5.3 21-Dec-1998 (prior to
         that it led to exit code 21) *)
      (CString "--fail", None) :: main_args
    else
      (CString "--write-out", None) ::
      (CString "%%{http_code}\\n", None) :: (* 6.5 13-Mar-2000 *)
      main_args

let wget_args = [
  (* wget2 by default has
     Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
     which makes some servers return an html page instead of the expected
     content. This is the case for the Software Heritage REST API.
     Using "Accept: */*" makes it on par with curl
  *)
  CString "--header=Accept: */*", None;
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

let download_args ~url ~out ~retry ?(with_curl_mitigation=false)
                  ?checksum ~compress () =
  let cmd, _ = Lazy.force OpamRepositoryConfig.(!r.download_tool) in
  let cmd =
    match cmd with
    | [(CIdent "wget"), _] -> cmd @ wget_args
    | [(CIdent "fetch"), _] -> cmd @ fetch_args
    | [(CIdent "ftp"), _] -> cmd @ ftp_args
      (* Assume curl if the command is a single arg *)
    | [_] -> cmd @ curl_args ~with_mitigation:with_curl_mitigation
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

let download_command_t ~with_curl_mitigation ~compress ?checksum ~url ~dst c =
  let cmd, args =
    match
      download_args
        ~url
        ~out:dst
        ~retry:OpamRepositoryConfig.(!r.retries)
        ~with_curl_mitigation
        ?checksum
        ~compress
        ()
    with
    | cmd::args -> cmd, args
    | [] ->
      OpamConsole.error_and_exit `Configuration_error
        "Empty custom download command"
  in
  let stdout = OpamSystem.temp_file ~auto_clean:false "dl" in
  OpamProcess.Job.finally (fun () -> OpamSystem.remove_file stdout) @@ fun () ->
  OpamSystem.make_command ~allow_stdin:false ~stdout cmd args @@> c

let tool_return redownload_command url ret =
  match Lazy.force OpamRepositoryConfig.(!r.download_tool) with
  | _, `Default ->
    if OpamProcess.is_failure ret then
      Done (`fail (Some "Download command failed",
                   Printf.sprintf "Download command failed: %s"
                     (OpamProcess.result_summary ret)))
    else Done `ok
  | _, `Curl ->
    match ret with
    | { r_code = 0 ; r_stdout = []; _ } ->
      Done (`fail (Some "curl empty response",
                   Printf.sprintf "curl: empty response while downloading %s"
                     (OpamUrl.to_string url)))
    | { r_code = 0 ; r_stdout = (_::_ as l); _ } ->
      let code = List.hd (List.rev l) in
      (try
         let num = int_of_string code in
         if num >= 400 then
           Done (`http_error num)
         else Done `ok
       with Failure _ ->
         Done (`fail (Some ("curl error " ^ code),
                      Printf.sprintf "curl: error %s while downloading %s"
                        code (OpamUrl.to_string url))))
    | { r_code = 43; _ } ->
      (* Code 43 is CURLE_BAD_FUNCTION_ARGUMENT (7.1 7-Aug-2000). This should
         never be encountered using the curl binary, so we assume that it's
         a manifestation of curl/curl#13845 (see also #6120). *)
      log "Attempting to mitigate curl/curl#13845";
      (redownload_command ~with_curl_mitigation:true @@ function ret ->
           if OpamProcess.is_failure ret then
             if ret.r_code = 22 then
               (* If this broken version of curl persists for some time, it is
                  relatively straightforward to parse the http response code from
                  the message, as it hasn't changed. *)
               Done (`fail (Some "curl failed owing to a server-side issue",
                            Printf.sprintf "curl failed with server-side error: %s"
                              (OpamProcess.result_summary ret)))
             else
               Done (`fail (Some "curl failed",
                            Printf.sprintf "curl failed: %s"
                              (OpamProcess.result_summary ret)))
           else Done `ok)
    | _ -> (* code <> 0 / 43 *)
      Done (`fail (Some "curl failed",
                   Printf.sprintf "curl failed: %s"
                     (OpamProcess.result_summary ret)))

let download_command_http_error ~compress ?checksum ~url ~dst () =
  let download_command = download_command_t ~compress ?checksum ~url ~dst in
  download_command ~with_curl_mitigation:false
  @@ tool_return download_command url

let download_command ~compress ?checksum ~url ~dst () =
  download_command_http_error ~compress ?checksum ~url ~dst ()
  @@| function
  | `ok -> ()
  | `http_error code ->
    fail (Some ("HTTP error code " ^ string_of_int code),
          Printf.sprintf "code %d while downloading %s"
            code (OpamUrl.to_string url))
  | `fail (s,l) -> fail (s,l)

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
  let base =
    let base = OpamUrl.basename url in
    if Sys.win32 then
      let f c =
        if OpamStd.Sys.is_valid_basename_char c then c else '_'
      in
      String.map f base
    else
      base
  in
  let dst =
    OpamFilename.(create dstdir (Base.of_string base))
  in
  download_as ?quiet ?validate ~overwrite ?compress ?checksum url dst @@|
  fun () -> dst


(** Stdout output retrieval and post requests management *)

let get_output ~post ?(args=[]) url =
  let cmd_args =
    download_args ~url ~out:"-" ~retry:OpamRepositoryConfig.(!r.retries)
      ~compress:false ()
    @ args
  in
  let cmd_args =
    if post then
      match cmd_args with
      | ("curl" as cmd)::args -> Some (cmd :: ["--request"; "POST"] @ args)
      | ("wget" as cmd)::args -> Some (cmd :: ["--method"; "POST"] @ args)
      | _ -> None
    else Some cmd_args
  in
  Done (OpamStd.Option.map
          (OpamSystem.read_command_output ~ignore_stderr:true) cmd_args)

module SWHID = struct

  (** SWHID retrieval functions *)

  let log fmt = OpamConsole.log "CURL(SWHID)" fmt

  let instance = OpamUrl.of_string "https://archive.softwareheritage.org"
  (* we keep api 1 hardcoded for the moment *)
  let full_url middle hash =
    OpamUrl.Op.(instance / "api" / "1" / middle / hash / "")
  let vault_url kind hash =
    full_url ("vault/" ^ kind) ("swh:1:dir:" ^ hash)

  let fallback_err fmt = Printf.sprintf ("SWH fallback: "^^fmt)

  let get_output ?(post=false) url =
    get_output ~post url @@| function
    | Some out -> out
    | None ->
      (* Shouldn't happen, we already checked that a post tool is used *)
      assert false

  let get_value key s =
    match OpamJson.of_string s with
    | Some (`O elems) ->
      (match OpamStd.List.assoc_opt String.equal key elems with
       | Some (`String v) -> Some v
       | _ -> None)
    | _ -> None

  let check_liveness () =
    OpamProcess.Job.catch (fun _ -> Done false)
    @@ fun () ->
    get_output ~post:false OpamUrl.Op.(instance / "api" / "1" / "ping" / "")
    @@| function
    | pong::_ ->
      (* curl output after answering the http code *)
      (* https://archive.softwareheritage.org/api/1/ping/ *)
      OpamStd.String.starts_with ~prefix:"\"pong\"" pong
    | _ -> false

  (*
    Returned error JSONs
    {
     "error":"Resource not found",
     "reason":"The resource /api/1/vault/flat/swh:1:dir:6b700f4b287aee509adbc723d030309188684f4/ could not be found on the server."
    }
    {
     "exception":"NotFoundExc",
     "reason":"Cooking of swh:1:dir:6b700f4b287aee509adbc723d030309188684f04 was never requested."
    }
    {
     "exception":"NotFoundExc",
     "reason":"swh:1:dir:0000000000000000000000000000000000000000 not found."
    }
  *)
  let parse_err json =
    match get_value "exception" json with
    | Some "NotFoundExc" ->
      (match get_value "reason" json with
       | Some reason ->
         if OpamStd.String.ends_with ~suffix:"was never requested." reason then
           `Uncooked
         else if OpamStd.String.ends_with ~suffix:"not found." reason then
           `Not_found
         else `Error
       | None -> `Error)
    | Some "Resource not found" -> `Not_found
    | Some _ | None -> `Error

  let is_it_cooked url =
    OpamSystem.with_tmp_file @@ fun dst ->
    let download_cmd ~with_curl_mitigation return =
      let cmd, args =
        match
          download_args ~url ~out:dst
            ~with_curl_mitigation
            ~retry:OpamRepositoryConfig.(!r.retries)
            ~compress:false ()
        with
        | cmd::args -> cmd, args
        | _ -> assert false
      in
      let stdout = OpamSystem.temp_file ~auto_clean:false "dl" in
      OpamProcess.Job.finally (fun () -> OpamSystem.remove_file stdout)
      @@ fun () ->
      OpamSystem.make_command ~allow_stdin:false ~stdout cmd args
      @@> return
    in
    (download_cmd ~with_curl_mitigation:false
     @@ tool_return download_cmd url)
    @@| fun status ->
    let read_last_line file =
      let out = String.trim (OpamSystem.read file) in
      match String.rindex_opt out '\n' with
      | Some b ->
        String.sub out (b + 1) (String.length out - b - 1)
      | None -> out
    in
    let status =
      match status with
      | `ok ->
        let json = read_last_line dst in
        if String.equal json "" then `Error else `Cooked json
      | `http_error 404 ->
        let json = read_last_line dst in
        parse_err json
      | `http_error _ | `fail _ -> `Error
    in
    status

  (* SWH request output example
     directory: retrieve "status" & "fetch_url"
     $ curl https://archive.softwareheritage.org/api/1/vault/directory/4453cfbdab1a996658cd1a815711664ee7742380/
     {
      "fetch_url": "https://archive.softwareheritage.org/api/1/vault/flat/swh:1:dir:4453cfbdab1a996658cd1a815711664ee7742380/raw/",
      "progress_message": null,
      "id": 398307347,
      "status": "done",
      "swhid": "swh:1:dir:4453cfbdab1a996658cd1a815711664ee7742380",
      "obj_type": "directory",
      "obj_id": "4453cfbdab1a996658cd1a815711664ee7742380"
     }
  *)

  let read_flat_out json =
    let status = get_value "status" json in
    let fetch_url = get_value "fetch_url" json in
    match status, fetch_url with
    | None, _ | _, None ->
      (match parse_err json with
       | `Not_found -> `Not_found
       | `Error | `Uncooked -> `Malformed)
    | Some status, Some fetch_url ->
      match status with
      | "done" -> `Done (OpamUrl.of_string fetch_url)
      | "pending" -> `Pending
      | "new" -> `New
      | "failed" -> `Failed
      | _ -> `Unknown

  let get_url ?(max_tries=6) swhid =
    let request_cooking ?(post=false) url =
      get_output ~post url @@| fun out -> String.concat "" out
    in
    let hash = OpamSWHID.hash swhid in
    (* https://archive.softwareheritage.org/api/1/vault/flat/doc/ *)
    let url = vault_url "flat" hash in
    let rec loop attempt json =
      match read_flat_out json with
      | `Done fetch_url -> Done (Result fetch_url)
      | `Pending | `New ->
        log "%s is cooking (%d/%d)..."
          (OpamSWHID.to_string swhid) attempt max_tries;
        if (attempt : int) >= (max_tries : int) then
          Done (Not_available
                  (Some (fallback_err "attempt"),
                   fallback_err "%d attempts tried; aborting" max_tries))
        else
          (Unix.sleep 10;
           request_cooking ~post:false url
           @@+ loop (attempt + 1))
      | `Malformed ->
        Done (Not_available (None, fallback_err "Malformed request answer"))
      | `Failed | `Unknown | `Not_found ->
        Done (Not_available (None, fallback_err "Unknown swhid"))
    in
    let retrieve_url json = loop 1 json in
    is_it_cooked url
    @@+ function
    | `Error -> Done (Not_available (None, fallback_err "Request error"))
    | `Not_found -> Done (Not_available (None, fallback_err "Unknown swhid"))
    | `Cooked json ->
      log "%s is cooked or cooking, requesting url" (OpamSWHID.to_string swhid);
      retrieve_url json
    | `Uncooked ->
      log "%s is uncooked, request cooking" (OpamSWHID.to_string swhid);
      request_cooking ~post:true url
      @@+ retrieve_url

  (* for the moment only used in sources, not extra sources or files *)
  let archive_fallback ?max_tries urlf dirnames =
    match OpamFile.URL.swhid urlf with
    | None -> Done (Result None)
    | Some swhid ->
      match Lazy.force OpamRepositoryConfig.(!r.download_tool) with
      | _, `Curl ->
        check_liveness () @@+ fun alive ->
        if alive then
          (log "API is working";
           (* Add a global modifier and/or command for default answering *)
           if OpamConsole.confirm ~default:false
               "Source %s is not available. Do you want to try to retrieve it \
                from Software Heritage cache (https://www.softwareheritage.org)? \
                It may take few minutes."
               (OpamConsole.colorise `underline
                  (OpamUrl.to_string (OpamFile.URL.url urlf))) then
             (log "SWH fallback for %s with %s"
                (OpamStd.Format.pretty_list
                   (List.map (fun (nv,_,_) -> nv) dirnames))
                (OpamSWHID.to_string swhid);
              get_url ?max_tries swhid @@+ function
              | Not_available _ as error -> Done error
              | Up_to_date _ -> assert false
              | Result url ->
                log "Downloading %s for %s" (OpamSWHID.to_string swhid)
                  (OpamStd.Format.pretty_list
                     (List.map (fun (nv,_,_) -> nv) dirnames));
                let hash = OpamSWHID.hash swhid in
                OpamFilename.with_tmp_dir_job @@ fun dir ->
                let archive =  OpamFilename.Op.(dir // hash) in
                download_as ~overwrite:true url archive @@+ fun () ->
                let sources = OpamFilename.Op.(dir / "src") in
                OpamFilename.extract_job archive sources @@| function
                | Some e ->
                  Not_available (
                    Some (fallback_err "archive extraction failure"),
                    fallback_err "archive extraction failure %s"
                      (match e with
                       | Failure s -> s
                       | OpamSystem.Process_error pe ->
                         OpamProcess.string_of_result pe
                       | e -> Printexc.to_string e))
                | None ->
                  (match OpamSWHID.compute sources with
                   | None ->
                     Not_available (
                       Some (fallback_err "can't check archive validity"),
                       fallback_err
                         "error on swhid computation, can't check its validity")
                   | Some computed ->
                     if String.equal computed hash then
                       (List.iter (fun (_nv, dst, _sp) ->
                            match dst with
                            | None -> ()
                            | Some dst ->
                              (* add a text *)
                              OpamFilename.copy_dir ~src:sources ~dst)
                           dirnames;
                        Result (Some "SWH fallback"))
                     else
                       Not_available (
                         Some (fallback_err "archive not valid"),
                         fallback_err
                           "archive corrupted, opam file swhid %S vs computed %S"
                           hash computed)))
           else
             Done (Not_available
                     (Some (fallback_err "skip retrieval"),
                      fallback_err "retrieval refused by user")))
        else
          Done (Not_available
                  (Some (fallback_err "unreachable"),
                   fallback_err "network failure or API down"))
      | _ ->
        Done (Not_available
                (Some (fallback_err "no retrieval"),
                 fallback_err "curl is required for Software Heritage fallback"))
end

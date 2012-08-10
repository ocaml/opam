(* Init scrip for curl-ed repositories *)

open Types
open Repo_helpers
open Curl

let () =
  let state = Repo_helpers.make_state () in
  let curl_state = Curl.make_state state in

  (* Download index.tar.gz *)
  let warning () =
    Globals.msg "Cannot find index.tar.gz on the OPAM repository.\nInitialisation might take some time ...\n" in

  try match Filename.download curl_state.remote_index_archive state.local_path with
    | None   -> warning ()
    | Some _ ->
        (* Untar the files *)
        Filename.extract_in curl_state.local_index_archive state.local_path
  with _ -> warning ()

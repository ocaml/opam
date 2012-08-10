(* Download scrip for curl-ed repositories *)

open Types
open Repositories

let () =
  let d = Repositories.read_download_info () in
  match Filename.download d.remote_filename d.local_path with
  | None   -> exit 1
  | Some f -> Printf.printf "%s" (Filename.to_string f)

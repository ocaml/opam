(* Download scrip for curl-ed repositories *)

open Types
open Repositories

let () =
  let d = Repositories.read_download_info () in
  match Filename.download (Filename.create d.remote_dir d.basename) d.local_dir with
  | None   -> exit 1
  | Some f -> Printf.printf "%s" (Filename.to_string f)

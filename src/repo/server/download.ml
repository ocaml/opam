(* Download script for OPAM server repositories *)

module F = Filename

open Types
open Repositories
open Protocol
open Unix

let () =
  let d = Repositories.read_download_info () in
  let remote_address =
    let server = Dirname.to_string d.remote_dir in
    try inet_addr_of_string server
    with _ ->
      (gethostbyname server).h_addr_list.(0) in
  let archive = Client.get_archive remote_address d.nv in
  let local_file = d.local_dir // F.basename (Basename.to_string d.basename) in
  Filename.write local_file archive;
  Globals.msg "%s\n%!" (Filename.to_string local_file)

(* Init scrip for curl-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

let remote_address = Sys.argv.(1)

open Types
open Misc

let () =
  Run.mkdir "opam";
  Run.mkdir "descr";
  Run.mkdir "archives";
  Run.mkdir "compilers";
  Run.mkdir "url";
  Run.mkdir "files";

  (* Download index.tar.gz *)
  let err = Dirname.exec local_path [wget remote_index_archive] in
  if err <> 0 then
    Globals.msg "Cannot find index.tar.gz on the OPAM repository.\nInitialisation might take some time ...\n"
  else
    (* Untar the files *)
    let err = Run.command [ "tar"; "xfz"; Filename.to_string local_index_archive ] in
    if err <> 0 then begin
      Globals.error "Cannot untar %s" (Filename.to_string local_index_archive);
      Globals.exit err
    end

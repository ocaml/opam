(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open Types

let log fmt = Globals.log "REPO" fmt

let run cmd root repo args =
  log "opam-%s: %s %s" cmd (Repository.to_string repo) (String.concat " " args);
  let path = Path.R.root root in
  let cmd =  Printf.sprintf "opam-%s-%s" (Repository.kind repo) cmd in
  let i = Run.in_dir (Dirname.to_string path) (fun () ->
    Run.command "%s %s %s" cmd (Repository.address repo) (String.concat " " args);
  ) in
  if i <> 0 then
    Globals.error_and_exit "%s failed" cmd

let init root r =
  run "init" root r [];
  File.Repo_config.write (Path.R.config root) r;
  Dirname.mkdir (Path.R.opam_dir root);
  Dirname.mkdir (Path.R.descr_dir root);
  Dirname.mkdir (Path.R.archive_dir root);
  (* XXX *)
  ()

(* Generic repository pluggins *)
let update root r =
  run "update" root r [];
  (* XXX *)
  ()

let download root r nv =
  run "download" root r [NV.to_string nv];
  (* XXX *)
  ()

let upload root r =
  run "upload" root r [];
  (* XXX *)
  ()

module Raw = struct

  let rsync l src dst = 
    match l with
      | [`A ; `R] | [`R ; `A] ->
          let dst = Filename.to_string dst in
          let err =
            Run.command "rsync -ar %s %s" src dst in
          if err <> 0 then
            Globals.error_and_exit "rsync (%S, %S) command failed (%d)" src dst err
      | _ -> failwith "TODO"

end

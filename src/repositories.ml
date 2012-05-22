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

let run cmd repo args =
  log "opam-%s: %s %s" cmd (Repository.to_string repo) (String.concat " " args);
  let path = Path.R.root (Path.R.create repo) in
  let cmd =  Printf.sprintf "opam-%s-%s" (Repository.kind repo) cmd in
  let i = Run.in_dir (Dirname.to_string path) (fun () ->
    Run.command "%s %s %s" cmd (Repository.address repo) (String.concat " " args);
  ) in
  if i <> 0 then
    Globals.error_and_exit "%s failed" cmd

let init r =
  let root = Path.R.create r in
  Dirname.mkdir (Path.R.root root);
  run "init" r [];
  File.Repo_config.write (Path.R.config root) r;
  Dirname.mkdir (Path.R.opam_dir root);
  Dirname.mkdir (Path.R.descr_dir root);
  Dirname.mkdir (Path.R.archive_dir root)

let update r =
  run "update" r []

let download r nv =
  run "download" r [NV.to_string nv]

let upload r =
  run "upload" r []

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

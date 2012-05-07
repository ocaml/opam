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

let run cmd root repo =
  log "opam-%s: %s" cmd (Repository.to_string repo);
  let path = Path.R.root root in
  let cmd =  Printf.sprintf "opam-%s-%s" (Repository.kind repo) cmd in
  let i = Run.in_dir (Dirname.to_string path) (fun () ->
    Run.command "%s %s" cmd (Repository.address repo);
  ) in
  if i <> 0 then
    Globals.error_and_exit "%s failed" cmd

let opam_init root r =
  run "init" root r;
  File.Repo_config.write (Path.R.config root) r;
  Dirname.mkdir (Path.R.opam_dir root);
  Dirname.mkdir (Path.R.descr_dir root);
  Dirname.mkdir (Path.R.archive_dir root);
  (* XXX *)
  ()

(* Generic repository pluggins *)
let opam_update root r =
  run "update" root r;
  (* XXX *)
  ()

let opam_download root r =
  run "download" root r;
  (* XXX *)
  ()

let opam_upload root r =
  run "upload" root r;
  (* XXX *)
  ()

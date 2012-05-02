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
open Path

let log fmt = Globals.log "REPO" fmt

module Script = struct
  let opam_init     r = Printf.sprintf "opam-%s-init"     r.repo_kind
  let opam_update   r = Printf.sprintf "opam-%s-update"   r.repo_kind
  let opam_download r = Printf.sprintf "opam-%s-download" r.repo_kind
  let opam_upload   r = Printf.sprintf "opam-%s-upload"   r.repo_kind
end

let run fn root repo =
  let path = Repository.root (Repository.create root repo) in
  let i = Run.in_dir (Dirname.to_string path) (fun () ->
    Run.command "%s" (fn repo);
  ) () in
  if i <> 0 then
    Globals.error_and_exit "%s failed" (fn repo)

let opam_init root r =  
  log "opam-init: %s" (string_of_repository r);
  run Script.opam_init root r;
  (* XXX *)
  ()

(* Generic repository pluggins *)
let opam_update root r =
  log "opam-update: %s" (string_of_repository r);
  run Script.opam_update root r;
  (* XXX *)
  ()

let opam_download root r =
  log "opam-download: %s" (string_of_repository r);
  run Script.opam_download root r;
  (* XXX *)
  ()

let opam_upload root r =
  log "opam-upload: %s" (string_of_repository r);
  run Script.opam_upload root r;
  (* XXX *)
  ()
  

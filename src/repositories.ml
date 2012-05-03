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

module Script = struct
  let opam_init     r = Printf.sprintf "opam-%s-init"     (Repository.kind r)
  let opam_update   r = Printf.sprintf "opam-%s-update"   (Repository.kind r)
  let opam_download r = Printf.sprintf "opam-%s-download" (Repository.kind r)
  let opam_upload   r = Printf.sprintf "opam-%s-upload"   (Repository.kind r)
end

let run fn root repo =
  let path = Path.R.root root in
  let i = Run.in_dir (Dirname.to_string path) (fun () ->
    Run.command "%s" (fn repo);
  ) in
  if i <> 0 then
    Globals.error_and_exit "%s failed" (fn repo)

let opam_init root r =
  log "opam-init: %s" (Repository.to_string r);
  run Script.opam_init root r;
  (* XXX *)
  ()

(* Generic repository pluggins *)
let opam_update root r =
  log "opam-update: %s" (Repository.to_string r);
  run Script.opam_update root r;
  (* XXX *)
  ()

let opam_download root r =
  log "opam-download: %s" (Repository.to_string r);
  run Script.opam_download root r;
  (* XXX *)
  ()

let opam_upload root r =
  log "opam-upload: %s" (Repository.to_string r);
  run Script.opam_upload root r;
  (* XXX *)
  ()

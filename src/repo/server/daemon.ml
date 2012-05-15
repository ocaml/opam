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

open Protocol
open Types

let log fmt = Globals.log "DAEMON" fmt

type t = {
  (* ~/.opam-server/ *)
  global: Path.G.t;

  (* ~/.opam-server/opam/ files *)
  available: NV.Set.t
}

let load_state () =
  let global = Path.G.create (Dirname.of_string !Globals.root_path) in
  let available = Path.G.available global in
  { global; available }

let get_file n v fn =
  let t = load_state () in
  let nv = NV.create (N.of_string n) (V.of_string v) in
  Run.read (Filename.to_string (fn t.global nv))

let write_files n v o d a =
  let t = load_state () in
  let nv = NV.create (N.of_string n) (V.of_string v) in
  let write fn c =
    Run.write (Filename.to_string (fn t.global nv)) c in
  write Path.G.opam o;
  write Path.G.descr d;
  write Path.G.archive a

let process_request id = function
  | ClientVersion v ->
      log "ClientVersion %s" v;
      if v = Globals.opam_version then
        ServerVersion v
      else
        Error "Wrong API version"
  | GetList ->
      log "GetList";
      let t = load_state () in
      let l = NV.Set.fold (fun nv l ->
        (N.to_string (NV.name nv), V.to_string (NV.version nv)) :: l
      ) t.available [] in
      PackageList (List.rev l)
  | GetOPAM (n,v) ->
      log "GetOPAM (%s,%s)" n v;
      OPAM (get_file n v Path.G.opam)
  | GetDescr (n,v) ->
      log "GetDescr (%s,%s)" n v;
      Descr (get_file n v Path.G.descr)
  | GetArchive (n,v) ->
      log "GetArchive (%s,%s)" n v;
      Archive (get_file n v Path.G.archive)
  | NewPackage (n,v,o,d,a) ->
      log "NewPackage (%s,%s,%s,%s,_)" n v o d;
      write_files n v o d a;
      let key = Key.create () in
      Key.write (N.of_string n) key;
      Key (Key.to_string key)
  | NewVersion (n,v,o,d,a,k) ->
      log "NewVersion (%s,%s,%s,%s,_,%s)" n v o d k;
      let key = Key.read (N.of_string n) in
      if key = (Digest.string k) then (
        write_files n v o d a;
        OK
      ) else
        Error "Wrong key"

let process (stdin, stdout) fn =
 process_server (stdin, stdout) fn

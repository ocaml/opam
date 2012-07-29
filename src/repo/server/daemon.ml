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
open Utils

let log fmt = Globals.log "DAEMON" fmt

type t = {
  (* ~/.opam-server/ *)
  root: Path.R.t;

  (* ~/.opam-server/opam/ files *)
  available: NV.Set.t
}

let load_root () =
  Path.R.of_dirname (Dirname.of_string !Globals.root_path)

let init () =
  let t = load_root () in
  log "init server state (%s)" (Dirname.to_string (Path.R.root t));
  Dirname.mkdir (Path.R.packages_dir t);
  Dirname.mkdir (Path.R.compilers_dir t);
  Dirname.mkdir (Path.R.archives_dir t);
  Dirname.mkdir (Key.hashes_dir t)

let load_state () =
  let root = load_root () in
  let available = Path.R.available_packages root in
  { root; available }

let get_file n v fn =
  let t = load_state () in
  let nv = NV.create (N.of_string n) (V.of_string v) in
  Run.read (Filename.to_string (fn t.root nv))

let global_mutex = Mutex.create ()

let write_files n v o d a =
  let t = load_state () in
  let nv = NV.create (N.of_string n) (V.of_string v) in
  let write fn c =
    Run.write (Filename.to_string (fn t.root nv)) c in
  Mutex.lock global_mutex;
  write Path.R.opam o;
  write Path.R.descr d;
  write Path.R.archive a;
  Mutex.unlock global_mutex

let process_request id = function
  | ClientVersion v ->
      log "ClientVersion %s" v;
      if v = Globals.opam_version then
        ServerVersion v
      else
        Error "Wrong API version"
  | GetList ->
      log "GetList %s" !Globals.root_path;
      let t = load_state () in
      let l = NV.Set.fold (fun nv l ->
        (N.to_string (NV.name nv), V.to_string (NV.version nv)) :: l
      ) t.available [] in
      PackageList (List.rev l)
  | GetOPAM (n,v) ->
      log "GetOPAM (%s,%s)" n v;
      OPAM (get_file n v Path.R.opam)
  | GetDescr (n,v) ->
      log "GetDescr (%s,%s)" n v;
      Descr (get_file n v Path.R.descr)
  | GetArchive (n,v) ->
      log "GetArchive (%s,%s)" n v;
      Archive (get_file n v Path.R.archive)
  | NewPackage (n,v,o,d,a) ->
      log "NewPackage (%s,%s,%s,%s,_)" n v o d;
      write_files n v o d a;
      let root = load_root () in
      let key = Key.create () in
      Key.write_hash root (N.of_string n) (Key.hash key);
      Key (Key.to_string key)
  | NewVersion (n,v,o,d,a,k) ->
      log "NewVersion (%s,%s,%s,%s,_,_)" n v o d;
      let root = load_root () in
      let key = Key.of_string k in
      let name = N.of_string n in
      if Key.exists_hash root name then
        let hash = Key.read_hash root name in
        if hash =  Key.hash key then (
          write_files n v o d a;
          OK
        ) else
          Error (n ^ ": wrong key")
      else
        Error (n ^ ": unknown package")
  | GetCompilers ->
      log "getCompilers";
      let root = load_root () in
      let set = Path.R.available_compilers root in
      let list = OCaml_V.Set.elements set in
      let files = List.map (Path.R.compiler root) list in
      let contents =
        List.map (File.Comp.read |> File.Comp.to_raw |> Raw.to_string) files in
      Compilers contents

let process (stdin, stdout) fn =
 process_server (stdin, stdout) fn

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

open Unix
open Protocol
open Types

let log fmt = Globals.log "CLIENT" fmt

let rpc host =
   let addr = ADDR_INET (host, default_port) in
   process_client (open_connection addr)

let protocol_error x msg = match x with
  | Error r -> Globals.error_and_exit "Protocol error: %s (%s)" msg r
  | _       -> Globals.error_and_exit "Protocol error: %s" msg

let client_version host =
  let req = ClientVersion Globals.opam_version in
  match rpc host req with
  | ServerVersion v ->
      if v <> Globals.opam_version then
        Globals.error_and_exit "API versions differ!"
  | x -> protocol_error x "client_version"

let get_list host =
  client_version host;
  match rpc host GetList with
  | PackageList l ->
      List.fold_left (fun accu (n,v) ->
        NV.Set.add (NV.create (N.of_string n) (V.of_string v)) accu
      ) NV.Set.empty l
  | x -> protocol_error x "get_list"

let get_opam host nv =
  client_version host;
  let req = GetOPAM (N.to_string (NV.name nv), V.to_string (NV.version nv)) in
  match rpc host req with
  | OPAM s -> File.OPAM.of_raw (Raw.of_string s)
  | x -> protocol_error x "get_opam"

let get_descr host nv =
  client_version host;
  let req = GetDescr (N.to_string (NV.name nv), V.to_string (NV.version nv)) in
  match rpc host req with
  | Descr s -> File.Descr.of_raw (Raw.of_string s)
  | x -> protocol_error x "get_descr"

let get_archive host nv =
  client_version host;
  let req = GetArchive (N.to_string (NV.name nv), V.to_string (NV.version nv)) in
  match rpc host req with
  | Archive s -> Raw.of_string s
  | x -> protocol_error x "get_archive"

let new_package host opam descr archive =
  client_version host;
  let n = File.OPAM.name opam in
  let v = File.OPAM.version opam in
  let req = NewPackage (N.to_string n, V.to_string v,
                        Raw.to_string (File.OPAM.to_raw opam),
                        Raw.to_string (File.Descr.to_raw descr),
                        Raw.to_string archive) in
  match rpc host req with
  | Key s -> Key.of_string s
  | x -> protocol_error x "new_package"

let new_version host opam descr archive key =
  client_version host;
  let n = File.OPAM.name opam in
  let v = File.OPAM.version opam in
  let req = NewVersion (N.to_string n, V.to_string v,
                        Raw.to_string (File.OPAM.to_raw opam),
                        Raw.to_string (File.Descr.to_raw descr),
                        Raw.to_string archive,
                        Key.to_string key) in
  match rpc host req with
  | OK -> ()
  | x  -> protocol_error x "new_package"

let get_compilers host =
  client_version host;
  match rpc host GetCompilers with
  | Compilers l -> List.map (fun s -> File.Comp.of_raw (Raw.of_string s)) l
  | x -> protocol_error x "get_compilers"

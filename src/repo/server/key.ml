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

let log fmt = Globals.log "KEY"

type t = string

type hash = string

let hash = Digest.string

let make n =
  let s = String.create n in
  for i = 0 to n-1 do
    s.[i] <- char_of_int (Random.int 255)
  done;
  s

let len = 128

let create () =
  make len

let to_string x = x

let of_string x = x

let (+/+) = Filename.concat

open Types

let key_dir t = Path.R.root t / "keys"

let key_path t n = key_dir t // N.to_string n

let exists t n =
  Filename.exists (key_path t n)

let read t n =
  Raw.to_string (Filename.read (key_path t n))

let write t n c =
  Filename.write (key_path t n) (Raw.of_string c)

(* Key-hashes *)

let hash_dir () =
  !Globals.root_path +/+ "hashes"

let hash_path n =
  hash_dir () +/+ N.to_string n

let exists_hash n =
  Sys.file_exists (hash_path n)

let read_hash n =
  Run.read (hash_path n)

let write_hash n c =
  Run.mkdir (hash_dir ());
  Run.write (hash_path n) c

let hashes_dir () =
  Dirname.of_string (hash_dir ())

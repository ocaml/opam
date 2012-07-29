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

type root = Path.R.t

let key_dir t = Path.R.root t / "keys"

let key_path t n = key_dir t // N.to_string n

let exists t n =
  Filename.exists (key_path t n)

let read t n =
  Raw.to_string (Filename.read (key_path t n))

let write t n c =
  Filename.write (key_path t n) (Raw.of_string c)

(* Key-hashes *)

let hash_dir d =
  Dirname.to_string (Path.R.root d) +/+ "hashes"

let hash_path d n =
  hash_dir d +/+ N.to_string n

let exists_hash d n =
  Sys.file_exists (hash_path d n)

let read_hash d n =
  Run.read (hash_path d n)

let write_hash d n c =
  Run.mkdir (hash_dir d);
  Run.write (hash_path d n) c

let hashes_dir d =
  Dirname.of_string (hash_dir d)

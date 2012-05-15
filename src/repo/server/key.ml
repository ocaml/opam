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

type t = string

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

let (/) = Filename.concat

let hash_dir () =
  !Globals.root_path / "hashes"

let hash_path n =
  hash_dir () / "hashes" / Types.N.to_string n

let read n =
  Run.read (hash_path n)

let write n c =
  Run.mkdir (hash_dir ());
  Run.write (hash_path n) (Digest.string c)

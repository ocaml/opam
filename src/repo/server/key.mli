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

(** Security keys *)

(** Type for keys *)
type t

(** Create a new key *)
val create: unit -> t

(** Convert the key to string *)
val to_string: t -> string

(** Convert a string to a key *)
val of_string: string -> t

(** Is a key is associated to the given package *)
val exists: Path.R.t -> name -> bool

(** Read key in {i $opam/repo/$repo/keys/$name} *)
val read: Path.R.t -> name -> t

(** Write key in {i $opam/repo/$repo/keys/$name} *)
val write: Path.R.t -> name -> t -> unit

(** {2 Server functions} *)

(** For security reasons, the server never store the keys directly. It
    receives complete keys from the clients, and then compare their hash
    with what is stored on its filesystem. *)

(** Type for key hashes *)
type hash

(** Directory where key hashes are stored *)
val hashes_dir: unit -> dirname

(** Hash a key *)
val hash: t -> hash

(** Is a key hash is associated to the given package *)
val exists_hash: name -> bool

(** Read key hash in {i $opam-server/hashes/$name} *)
val read_hash: name -> hash

(** Write key hash in {i $opam-server/keys/$name} *)
val write_hash: name -> hash -> unit

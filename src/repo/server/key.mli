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

(** Read key hash in {i $opam-server/hashes/$name} *)
val read: name -> string

(** Write key hash in {i $opam-server/keys/$name} *)
val write: name -> t -> unit

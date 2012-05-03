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

(** [copy src dst] copies [src] to [dst] *)
val copy: string -> string -> unit

(** [link src dst] links [src] to [dst] *)
val link: string -> string -> unit

(** [real_path p] returns the real path associated to [p]: [..] are
    expanded and relative paths become absolute. *)
val real_path: string -> string

(** [read filename] returns the contents of [filename] *)
val read: string -> string

(** [write filename contents] write [contents] to [filename] *)
val write: string -> string -> unit

(** [remove filename] removes [filename]. Works whether [filename] is
    a file or a directory *)
val remove: string -> unit

(** [remove_file filename] removes [filename]. Works only for normal
    files *)
val remove_file: string -> unit

(** [remove_dir filename] removes [filename]. Works only for
    directory. *)
val remove_dir: string -> unit

(** [in_dir dir fn] evaluates [fn] in the directory [dir] *)
val in_dir: string -> (unit -> 'a) -> 'a

(** [files dir] returns the files in the directory [dir] *)
val files: string -> string list

(** [command fmt] executes the command [fmt] *)
val command: ('a, unit, string, int) format4 -> 'a

(** [commands cmds] executes the commands [cmds]. It stops whenever
    one command fails. *)
val commands: string list -> int

(** [extract filename dirname] untar the archive [filename] to
    [dirname] *)
val extract: string -> string -> unit

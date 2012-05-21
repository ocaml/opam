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

(** Low-level untyped system operations. *)

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

(** Change the current working directory *)
val chdir: string -> unit

(** [in_dir dir fn] evaluates [fn] in the directory [dir] *)
val in_dir: string -> (unit -> 'a) -> 'a

(** [files dir] returns the files in the directory [dir] *)
val files: string -> string list

(** [files dir] returns the directories in the directory [dir] *)
val directories: string -> string list

(** [command fmt] executes the command [fmt] *)
val command: ('a, unit, string, int) format4 -> 'a

(** [commands cmds] executes the commands [cmds]. It stops whenever
    one command fails. *)
val commands: string list -> int

(** [read_command_output fmt] executes the command [fmt] and return
    the lines from stdout *)
val read_command_output: ('a, unit, string, string list) format4 -> 'a

(** [extract filename dirname] untar the archive [filename] to
    [dirname] *)
val extract: string -> string -> unit

(** Return the current working directory *)
val cwd: unit -> string

(** Create a directory. Do not fail if the directory already
    exist. *)
val mkdir: string -> unit

(** {2 File locking function} *)

(** [flock ()] takes the global file lock. If the lock is already
    taken, sleep for 1s and then retry. Abort after 5 tentatives. *)
val flock: unit -> unit

(** [funlock ()] unlocks the global file lock. Work only if the
    current processus is the same as the one who took the lock at the
    first place. *)
val funlock: unit -> unit

(** Functional version of [flock / funlock] *)
val with_flock: ('a -> 'b) -> 'a -> 'b

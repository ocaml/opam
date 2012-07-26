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

(** [with_tmp_dir fn] executes [fn] in a tempory directory *)
val with_tmp_dir: (string -> 'a) -> 'a

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
    files (or also at least for symlinks) *)
val remove_file: string -> unit

(** [remove_dir filename] removes [filename]. Works only for
    directory (not for symlinks or other files). *)
val remove_dir: string -> unit

(** Change the current working directory *)
val chdir: string -> unit

(** [in_dir dir fn] evaluates [fn] in the directory [dir] *)
val in_dir: string -> (unit -> 'a) -> 'a

(** [files_with_links dir] returns the files in the directory [dir].
    Links simulating directory are ignored, others links are returned. *)
val files_with_links: string -> string list

(** [rec_files dir] returns the list of all files in [dir], 
    recursively.
    Links behaving like directory are crossed. *)
val rec_files: string -> string list

(** Return the version of the current OCaml compiler. If no OCaml
    compiler is present in the path, then it returns [None]. *)
val ocaml_version: unit -> string option

(** Return the path where ocamlc library is installed *)
val ocamlc_where: unit -> string option

(** [directories_with_links dir] returns the directories in the directory [dir].
    Links pointing to directory are also returned. *)
val directories_with_links: string -> string list

(** a command is a list of words *)
type command = string list

(** [command cmd] executes the command [cmd]. Return the exit code. *)
val command:
  ?add_to_env:(string*string) list ->
  ?add_to_path:string list -> command -> int

(** [commands ~add_to_path cmds] executes the commands [cmds] 
    in a context where $PATH contains [add_to_path] at the beginning. 
    It stops whenever one command fails. *)
val commands:
  ?add_to_env:(string*string) list ->
  ?add_to_path:string list -> command list -> int

(** [read_command_output cmd] executes the command [cmd] and return
    the lines from stdout *)
val read_command_output:
  ?add_to_env:(string*string) list ->
  ?add_to_path:string list -> command -> string list option

(** [extract filename dirname] extracts the archive [filename] into
    [dirname]. [dirname] should not exists and [filename] should
    contain only one top-level directory.*)
val extract: string -> string -> unit

(** [extract_in filename dirname] extracts the archive [filename] into
    [dirname]. [dirname] should already exists. *)
val extract_in: string -> string -> unit

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
val with_flock: (unit -> unit) -> unit

(** {2 Function used only by the switch commnand} *)

(** download compiler sources *)
val download: filename:string -> dirname:string -> string option

(** Apply a patch file in the current directory. Return whether the
    patch has been applied succesfully. *)
val patch: string -> bool

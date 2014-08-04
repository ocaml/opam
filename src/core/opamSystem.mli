(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(** Low-level untyped system operations *)

(** Exception raised when subprocess fails *)
exception Process_error of OpamProcess.result

(** raise [Process_error] *)
val process_error: OpamProcess.result -> 'a

(** Exception raised when a computation in the current process
    fails. *)
exception Internal_error of string

(** Raise [Internal_error] *)
val internal_error: ('a, unit, string, 'b) format4 -> 'a


(** [with_tmp_dir fn] executes [fn] in a tempory directory *)
val with_tmp_dir: (string -> 'a) -> 'a

(** [copy src dst] copies [src] to [dst]. Remove [dst] before the copy
    if it is a link. *)
val copy: string -> string -> unit

(** [install ?exec src dst] copies file [src] as file [dst] using [install].
    If [exec], make the resulting file executable (otherwise, look at the
    permissions of the original file to decide). *)
val install: ?exec:bool -> string -> string -> unit

(** Checks if a file is an executable (regular file with execution
    permission) *)
val is_exec: string -> bool

(** [link src dst] links [src] to [dst]. Remove [dst] if it is a file,
    not a directory. *)
val link: string -> string -> unit

(** [real_path p] returns the real path associated to [p]: [..] are
    expanded and relative paths become absolute. *)
val real_path: string -> string

(** Return the contents of a channel. *)
val string_of_channel: in_channel -> string

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

(** Return the list of files in the current directory. *)
val files: string -> string list

(** [rec_dirs dir] return the list list of all directories recursively
    (going through symbolink links). *)
val rec_dirs: string -> string list

(** Return the list of directories in the current directory. *)
val dirs: string -> string list

val dir_is_empty: string -> bool

(** Return the version of the current OCaml compiler. If no OCaml
    compiler is present in the path, then it returns [None]. *)
val ocaml_version: string option Lazy.t

(** Returns true if the "ocamlopt" is available in the current switch *)
val ocaml_native_available: bool Lazy.t

(** Returns true if the ".opt" version of the current OCaml compiler is
    available*)
val ocaml_opt_available: bool Lazy.t

(** Checks if native dynlink is available in a given ocaml lib prefix
    (looks for libdir/ocaml/dynlink.cmxa) *)
val ocaml_natdynlink_available: string -> bool

(** Return the path where the system ocamlc library is installed *)
val system_ocamlc_where: string option Lazy.t

(** Return the version of the system compiler *)
val system_ocamlc_version: string option Lazy.t

(** [directories_with_links dir] returns the directories in the directory [dir].
    Links pointing to directory are also returned. *)
val directories_with_links: string -> string list

(** a command is a list of words *)
type command = string list

(** Test whether a command exists in the environment. *)
val command_exists: ?env:string array -> string -> bool

(** Find directory containing a file by the name [name] in the PATH *)
val find_in_path: string -> string option

(** [command cmd] executes the command [cmd] in the correct OPAM
    environment. *)
val command: ?verbose:bool -> ?env:string array -> ?name:string ->
  ?metadata:(string * string) list -> command -> unit

(** [commands cmds] executes the commands [cmds] in the correct OPAM
    environment. It stops whenever one command fails unless [keep_going] is set
    to [true]. In this case, the first error is re-raised at the end. *)
val commands: ?verbose:bool -> ?env:string array -> ?name:string ->
  ?metadata:(string * string) list -> ?keep_going:bool -> command list -> unit

(** [read_command_output cmd] executes the command [cmd] in the
    correct OPAM environment and return the lines from stdout if the command
    exists normally. If the command does not exist or if the command exited
    with a non-empty exit-code, throw an error. *)
val read_command_output: ?verbose:bool -> ?env:string array ->
  ?metadata:(string * string) list -> command -> string list

(** Test whether the file is an archive, by looking as its extension *)
val is_tar_archive: string -> bool

(** [extract filename dirname] extracts the archive [filename] into
    [dirname]. [dirname] should not exists and [filename] should
    contain only one top-level directory.*)
val extract: string -> string -> unit

(** [extract_in filename dirname] extracts the archive [filename] into
    [dirname]. [dirname] should already exists. *)
val extract_in: string -> string -> unit

(** Create a directory. Do not fail if the directory already
    exist. *)
val mkdir: string -> unit

(** {2 File locking function} *)

type lock

(** Acquires a lock on the given file. Retries 5 times max.
    By default, this is a write lock. *)
val flock: ?read:bool -> string -> lock

(** Releases an acquired locl *)
val funlock: lock -> unit

(** {2 Misc} *)

(** download compiler sources *)
val download: overwrite:bool -> ?compress:bool ->
  filename:string -> dst:string -> string

(** Apply a patch file in the current directory. *)
val patch: string -> unit

(** Create a tempory file in {i ~/.opam/logs/<name>XXX} *)
val temp_file: ?dir:string -> string -> string

(** Print stats *)
val print_stats: unit -> unit

(** The separator character for the PATH variable *)
val path_sep: char

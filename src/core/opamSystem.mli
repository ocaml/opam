(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Bindings of lots of filesystem and system operations *)

(** Exception raised when subprocess fails *)
exception Process_error of OpamProcess.result

exception Command_not_found of string

(** raise [Process_error] *)
val process_error: OpamProcess.result -> 'a

(** raise [Process_error] if the process didn't return 0 *)
val raise_on_process_error: OpamProcess.result -> unit

(** Exception raised when a computation in the current process
    fails. *)
exception Internal_error of string

(** Raise [Internal_error] *)
val internal_error: ('a, unit, string, 'b) format4 -> 'a

(** [with_tmp_dir fn] executes [fn] in a tempory directory *)
val with_tmp_dir: (string -> 'a) -> 'a

(** Runs a job with a temp dir that is cleaned up afterwards *)
val with_tmp_dir_job: (string -> 'a OpamProcess.job) -> 'a OpamProcess.job

(** Returns true if the default verbose level for base commands (cp, mv, etc.)
    is reached *)
val verbose_for_base_commands: unit -> bool

(** [copy_file src dst] copies [src] to [dst]. Remove [dst] before the copy
    if it is a link. *)
val copy_file: string -> string -> unit

(** [copy_dir src dst] copies the contents of directory [src] into directory
    [dst], creating it if necessary, merging directory contents and ovewriting
    files otherwise *)
val copy_dir: string -> string -> unit

val mv: string -> string -> unit

(** [install ?exec src dst] copies file [src] as file [dst] using [install].
    If [exec], make the resulting file executable (otherwise, look at the
    permissions of the original file to decide). *)
val install: ?exec:bool -> string -> string -> unit

(** Checks if a file is an executable (regular file with execution
    permission) *)
val is_exec: string -> bool

val file_is_empty: string -> bool

(** [link src dst] links [src] to [dst]. Remove [dst] if it is a file,
    not a directory. *)
val link: string -> string -> unit

(** [real_path p] returns the real path associated to [p]: [..] are
    expanded and relative paths become absolute. *)
val real_path: string -> string

(** Return the contents of a channel. *)
val string_of_channel: in_channel -> string

(** Raised when a file or directory can't be accessed (doesn't exist,
    bad permissions, etc.) *)
exception File_not_found of string

(** [read filename] returns the contents of [filename] (while taking an advisory
    read lock to prevent concurrent writes) *)
val read: string -> string

(** [write filename contents] write [contents] to [filename] (while taking an
    advisory write lock to prevent concurrent reads or writes) *)
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

(** Returns the list of files and directories in the given directory (full
    names) *)
val ls: string -> string list

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

(** [directories_with_links dir] returns the directories in the directory [dir].
    Links pointing to directory are also returned. *)
val directories_with_links: string -> string list

(** Make a comman suitable for OpamProcess.Job. if [verbose], is set,
    command and output will be displayed (at command end for the
    latter, if concurrent commands are running). [name] is used for
    naming log files. [text] is what is displayed in the status line
    for this command. May raise Command_not_found, unless
    [check_existence] is set to false (in which case you can end up
    with a process error instead) *)
val make_command:
  ?verbose:bool -> ?env:string array -> ?name:string -> ?text:string ->
  ?metadata:(string * string) list -> ?allow_stdin:bool -> ?stdout:string ->
  ?dir:string -> ?check_existence:bool ->
  string -> string list -> OpamProcess.command

(** OLD COMMAND API, DEPRECATED *)

(** a command is a list of words *)
type command = string list

(** Test whether a command exists in the environment. *)
val command_exists: ?env:string array -> ?dir:string -> string -> bool

(** [command cmd] executes the command [cmd] in the correct OPAM
    environment. *)
val command: ?verbose:bool -> ?env:string array -> ?name:string ->
  ?metadata:(string * string) list -> ?allow_stdin:bool ->
  command -> unit

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
  ?metadata:(string * string) list ->  ?allow_stdin:bool ->
  command -> string list

(** END *)

(** Test whether the file is an archive, by looking as its extension *)
val is_tar_archive: string -> bool

(** [extract ~dir:dirname filename] extracts the archive [filename] into
    [dirname]. [dirname] should not exists and [filename] should
    contain only one top-level directory.*)
val extract: dir:string -> string -> unit

(** Same as [extract], but as an OpamProcess.job *)
val extract_job: dir:string -> string -> exn option OpamProcess.job

(** [extract_in ~dir:dirname filename] extracts the archive [filename] into
    [dirname]. *)
val extract_in: dir:string -> string -> unit

(** [extract_in_job] is similar to [extract_in], but as a job *)
val extract_in_job: dir:string -> string -> exn option OpamProcess.job

(** Create a directory. Do not fail if the directory already
    exist. *)
val mkdir: string -> unit

(** Get the number of active processors on the system *)
val cpu_count: unit -> int

(** {2 File locking function} *)

(** Unix file locks (mutable structure, to follow actual semantics) *)
type lock

(** The different kinds of unix advisory locks available (`Lock_none doesn't
    actually lock anything, or even create the lock file) *)
type lock_flag = [ `Lock_none | `Lock_read | `Lock_write ]

(** Dummy lock *)
val lock_none: lock

(** Raised when locks can't be acquired and [dontblock] was specified) *)
exception Locked

(** Acquires a lock on the given file.
    Raises [Locked] if the lock can't be acquired and [dontblock] is set. Raises
    [OpamStd.Sys.Exit] if [safe_mode] is set and a write lock is required. Also
    raises Unix errors if the lock file can't be opened. *)
val flock: [< lock_flag ] -> ?dontblock:bool -> string -> lock

(** Updates an existing lock to the given level. Raises the same exceptions as
    [flock]. *)
val flock_update: [< lock_flag ] -> ?dontblock:bool -> lock -> unit

(** Releases an acquired lock (equivalent to [flock_update `Lock_none]) *)
val funlock: lock -> unit

(** Returns the highest of the two lock flags (with the order no lock < read
    lock < write lock) *)
val lock_max: lock_flag -> lock_flag -> lock_flag

(** Returns true if the lock already has the lock_flag rights or more *)
val lock_isatleast: [< lock_flag ] -> lock -> bool

(** Returns the current kind of the lock *)
val get_lock_flag: lock -> lock_flag

(** {2 Misc} *)

(** Apply a patch file in the current directory. Returns the error if the patch
    didn't apply. *)
val patch: dir:string -> string -> exn option OpamProcess.job

(** Create a tempory file in {i ~/.opam/logs/<name>XXX}. ?auto_clean controls
    whether the file is automatically deleted when opam terminates
    (default: [true]). *)
val temp_file: ?auto_clean:bool -> ?dir:string -> string -> string

(** Print stats *)
val print_stats: unit -> unit

(** Registers an exception printer that adds some OPAM version info, and details
    on process and Unix errors *)
val register_printer: unit -> unit

(** Initialises signal handlers, catch_break and some exception printers. The
    lib may not perform properly without this if [Sys.catch_break] isn't set
    and SIGPIPE isn't handled (with a no-op) *)
val init: unit -> unit

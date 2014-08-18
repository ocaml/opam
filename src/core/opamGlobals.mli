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

(** Convention:
    all the global OPAM variables can be set using environment variables
    using OPAM<variable> *)

val debug : bool ref
val debug_level : int ref
val verbose : bool ref
val color_tri_state : [> `Always | `Auto | `Never ]
val color : bool ref
val keep_build_dir : bool ref
val no_base_packages : bool ref
val no_checksums : bool ref
val req_checksums : bool ref
val yes : bool ref
val strict : bool ref
val build_test : bool ref
val build_doc : bool ref
val show : bool ref
val dryrun : bool ref
val fake : bool ref
val print_stats : bool ref
val utf8_msgs : bool ref
(* val autoremove : bool ref *)
val do_not_copy_files : bool ref
val sync_archives : bool ref
val compat_mode_1_0 : bool ref
val use_external_solver : bool ref
val no_self_upgrade : bool ref
val skip_version_checks : bool ref

(* Value set when opam calls itself *)
val self_upgrade_bootstrapping_value : string
val is_self_upgrade : bool

val curl_command : string option
val jobs : int option ref
val dl_jobs : int option ref
val download_retry : int
val cudf_file : string option ref
val solver_timeout : float

type solver_criteria = [ `Default | `Upgrade | `Fixup ]
val default_preferences : solver_criteria -> string 
val compat_preferences : solver_criteria -> string

(** Solver preference bindings. Used with List.assoc: first one wins *)
val solver_preferences : (solver_criteria * string) list ref

(** Get the currently configured solver criteria as a string *)
val get_solver_criteria : [ `Default | `Upgrade | `Fixup ] -> string

val default_external_solver : string
val external_solver : string option ref
val get_external_solver : unit -> string

val default_repository_name : string
val default_repository_address : string
(* val default_build_command : string list list *)
val global_config : string
val system : string
val switch : [ `Command_line of string | `Env of string | `Not_set ] ref
val external_tags : string list ref
val home : string
val default_opam_dir : string
val root_dir : string ref
val timer : unit -> unit -> float

(* For forked process, we want to get the time since the beginning of
   the parent process. *)
val global_start_time : float

type text_style =
    [ `black
    | `blue
    | `bold
    | `cyan
    | `green
    | `magenta
    | `red
    | `underline
    | `white
    | `yellow ]

(** not nestable *)
val colorise : text_style -> string -> string

val indent_left : string -> int -> string
val acolor : text_style -> out_channel -> string -> unit
val acolor_w : int -> text_style -> out_channel -> string -> unit
val timestamp : unit -> string

(** [log section ~level fmt args]. Used for debug messages, default
    level is 1 *)
val log : string -> ?level:int -> ('a, out_channel, unit) format -> 'a

(** Helper to pass stringifiers to log (use [log "%a" (slog to_string) x]
    rather than [log "%s" (to_string x)] to avoid costly unneeded
    stringifications *)
val slog : ('a -> string) -> out_channel -> 'a -> unit

val error : ('a, unit, string, unit) format4 -> 'a
val warning : ('a, unit, string, unit) format4 -> 'a
val note : ('a, unit, string, unit) format4 -> 'a

(** Raised to exit the program in a clean way. Parameter is the exit code. *)
exception Exit of int

(** Raised to [exec()] another binary, after making sure finalisations have been
    made properly. Parameters as per [Unix.execvpe] *)
exception Exec of string * string array * string array

exception Package_error of string

val error_and_exit : ('a, unit, string, 'b) format4 -> 'a
val msg : ('a, out_channel, unit, unit) format4 -> 'a
val header_msg : ('a, unit, string, unit) format4 -> 'a
val header_error :
  ('a, unit, string, ('b, unit, string, unit) format4 -> 'b) format4 -> 'a
val editor : string lazy_t

type os =
    Darwin
  | Linux
  | FreeBSD
  | OpenBSD
  | NetBSD
  | DragonFly
  | Cygwin
  | Win32
  | Unix
  | Other of string
val os : unit -> os
val os_string : unit -> string

val arch : unit -> string

val makecmd : (unit -> string) ref

val log_limit : int
val log_line_limit : int

val default_jobs : int
val default_dl_jobs : int

val exit : int -> 'a

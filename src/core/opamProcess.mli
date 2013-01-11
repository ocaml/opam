(***********************************************************************)
(*                                                                     *)
(*    Copyright 2011-2012 OCamlPro                                     *)
(*    Copyright 2011-2012 INRIA                                        *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Process handling *)

(** The type for processes *)
type t = {
  p_name   : string;        (** Command name *)
  p_args   : string list;   (** Command args *)
  p_pid    : int;           (** Process PID *)
  p_cwd    : string;        (** Process initial working directory *)
  p_time   : float;         (** Process start time *)
  p_stdout : string option; (** stdout dump file *)
  p_stderr : string option; (** stderr dump file *)
  p_env    : string option; (** dump environement variables *)
  p_info   : string option; (** dump process info *)
}

(** [create cmd args] create a new process to execute the command
    [cmd] with arguments [args]. If [stdout_file] or [stderr_file] are
    set, the channels are redirected to the corresponding files.  The
    outputs are discarded is [verbose] is set to false. The current
    environment can also be overriden if [env] is set. The environment
    which is used to run the process is recorded into [env_file] (if
    set). *)
val create :
  ?info_file:string -> ?env_file:string -> ?stdout_file:string -> ?stderr_file:string ->
  ?env:string array ->
  verbose:bool -> string -> string list -> t

(** Process results *)
type result = {
  r_code     : int;         (** Process exit code *)
  r_duration : float;       (** Process duration *)
  r_info     : string;      (** Process info *)
  r_stdout   : string list; (** Content of stdout dump file *)
  r_stderr   : string list; (** Content of stderr dump file *)
  r_cleanup  : string list; (** List of files to clean-up *)
}

(** [wait p] waits for the processus [p] to end and returns its results. *)
val wait: t -> result

(** [run ~name cmd args] synchronously call the command [cmd] with
    arguments [args]. It waits until the process is finished. The file
    [name.info], [name.env], [name.out] and [name.err] and are
    created, and contains the process main description, the environment
    variables, the standard output and the standard error. *)
val run : ?env:string array -> verbose:bool -> name:string -> string -> string list -> result

(** Is the process result a success ? *)
val is_success : result -> bool

(** Is the process result a failure ? *)
val is_failure : result -> bool

(** Clean-up process result files *)
val clean_files : result -> unit

(** {2 Misc} *)
val read_lines: string -> string list

(** Pretty printing of process. *)
val string_of_result: result -> string

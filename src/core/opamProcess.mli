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

(** Process handling *)

(** The type of shell commands *)
type command

val command:
  ?env:string array ->              (** env for the comman d*)
  ?verbose:bool ->                  (** force verbosity *)
  ?name:string ->                   (** title, used to name log files, etc. *)
  ?metadata:(string*string) list -> (** additional info to log *)
  ?dir:string ->                    (** CWD for the command *)
  ?allow_stdin:bool ->              (** whether to forward stdin *)
  ?text:string ->                   (** Short text that may be displayed in
                                        status *)
  string ->                         (** The command itself *)
  string list ->                    (** Command-line arguments *)
  command

val string_of_command: command -> string
val text_of_command: command -> string option
val is_verbose_command: command -> bool

(** Returns a label suitable for printing the summary of running commands. First
    string is the topic (e.g. package), second the action (e.g. command name).
    Optional command arguments may be used for details (e.g. make action). *)
val make_command_text: ?color:OpamGlobals.text_style -> string -> ?args:string list -> string -> string

(** The type for processes *)
type t = {
  p_name   : string;        (** Command name *)
  p_args   : string list;   (** Command args *)
  p_pid    : int;           (** Process PID *)
  p_cwd    : string;        (** Process initial working directory *)
  p_time   : float;         (** Process start time *)
  p_stdout : string option; (** stdout dump file *)
  p_stderr : string option; (** stderr dump file *)
  p_env    : string option; (** dump environment variables *)
  p_info   : string option; (** dump process info *)
  p_metadata: (string * string) list; (** Metadata associated to the process *)
  p_verbose: bool;           (** whether output of the process should be displayed *)
}

(** Process results *)
type result = {
  r_code     : int;         (** Process exit code, or 256 on error *)
  r_signal   : int option;  (** Signal received if the processed was killed *)
  r_duration : float;       (** Process duration *)
  r_info     : (string * string) list; (** Process info *)
  r_stdout   : string list; (** Content of stdout dump file *)
  r_stderr   : string list; (** Content of stderr dump file *)
  r_cleanup  : string list; (** List of files to clean-up *)
}

(** [run command] synchronously call the command [command.cmd] with
    arguments [command.args]. It waits until the process is finished. The files
    [name.info], [name.env], [name.out] and [name.err], with
    [name = command.cmd_name] are
    created, and contain the process main description, the environment
    variables, the standard output and the standard error.
    Don't forget to call [cleanup result] afterwards *)
val run : command -> result

(** Same as [run], but doesn't wait. Use wait_one to wait and collect
    results;
    Don't forget to call [cleanup result] afterwards *)
val run_background: command -> t

(** Similar to [run_background], except that no process is created, and a dummy
    process (suitable for dry_wait_one) is returned. *)
val dry_run_background: command -> t

(** [wait p] waits for the processus [p] to end and returns its results. Be
    careful to handle Sys.Break *)
val wait: t -> result

(** Like [wait], but returns None immediately if the process hasn't ended *)
val dontwait: t -> result option

(** Wait for the first of the listed processes to terminate, and return its
    termination status *)
val wait_one: t list -> t * result

(** Similar to [wait_one] for simulations, to be used with
    [dry_run_background] *)
val dry_wait_one: t list -> t * result

(** Send SIGINT to a process (or SIGKILL on Windows) *)
val interrupt: t -> unit

(** Is the process result a success ? *)
val is_success : result -> bool

(** Is the process result a failure ? *)
val is_failure : result -> bool

(** Should be called after process termination, to cleanup temporary files.
    Leaves artefacts in case OpamGlobals.debug is on and on failure, unless
    force has been set. *)
val cleanup : ?force:bool -> result -> unit

(** {2 Misc} *)
val read_lines: string -> string list

(** Pretty printing of process. *)
val string_of_result: ?color:OpamGlobals.text_style -> result -> string

(** Higher-level interface to allow parallelism *)
module Job: sig

  (** Open type and add combinators. Meant to be opened *)
  module Op: sig
    type 'a job =
      | Done of 'a
      | Run of command * (result -> 'a job)

    (** Stage a shell command with its continuation, eg:
        {[
          command "ls" ["-a"] @@> fun result ->
          if OpamProcess.is_success result then Done result.r_stdout
          else failwith "ls"
        ]}
    *)
    val (@@>): command -> (result -> 'a job) -> 'a job

    (** [job1 @@+ fun r -> job2] appends the computation of tasks in [job2] after
        [job1] *)
    val (@@+): 'a job -> ('a -> 'b job) -> 'b job

    (** [job @@| f] maps [f] on the results of [job].
        Equivalent to [job @@+ fun r -> Done (f r)] *)
    val (@@|): 'a job -> ('a -> 'b) -> 'b job
  end

  (** Sequential run of a job *)
  val run: 'a Op.job -> 'a

  (** Same as [run] but doesn't actually run any shell command,
      and feed a dummy result to the cont. *)
  val dry_run: 'a Op.job -> 'a

  (** Catch exceptions raised within a job *)
  val catch: (exn -> 'a Op.job) -> 'a Op.job -> 'a Op.job

  (** Ignore all non-fatal exceptions raised by job and return default *)
  val ignore_errors: default:'a -> ?message:string -> 'a Op.job -> 'a Op.job

  (** Register an exception-safe finaliser in a job.
      [finally job fin] is equivalent to
      [catch job (fun e -> fin (); raise e) @@+ fun r -> fin (); Done r] *)
  val finally: (unit -> unit) -> 'a Op.job -> 'a Op.job

  (** Converts a list of commands into a job that returns None on success, or
      the first failed command and its result.
      Unless [keep_going] is true, stops on first error. *)
  val of_list: ?keep_going:bool -> command list ->
    (command * result) option Op.job

  (** Sets and overrides text of the underlying commands *)
  val with_text: string -> 'a Op.job -> 'a Op.job
end

type 'a job = 'a Job.Op.job

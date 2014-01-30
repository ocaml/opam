(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

(** The type for processes *)
type t = {
  p_name   : string;        (** Command name *)
  p_args   : string list;   (** Command args *)
  p_pid    : int;           (** Process PID *)
  p_time   : float;         (** Process start time *)
  p_stdout : string option; (** stdout dump file *)
  p_stderr : string option; (** stderr dump file *)
  p_info   : string option; (** dump info file *)
}

(** [create cmd args] create a new process to execute the command
    [cmd] with arguments [args].  If stdout/stderr are set, the
    channels are redirected to files. The current environment can also
    be overriden if [env] is set. *)
val create :
  ?info:string -> ?stdout:string -> ?stderr:string -> ?env:string array
  -> string -> string list -> t

(** The type for result processes *)
type result = {
  r_proc     : t;           (** Process *)
  r_code     : int;         (** Process exit code *)
  r_duration : float;       (** Process duration *)
  r_stdout   : string list; (** Content of stdout dump file *)
  r_stderr   : string list; (** Content of stderr dump file *)
}

(** [wait p] waits for the processus [p] to end and returns its results *)
val wait : t -> result

(** [run ~name cmd args] synchronously call the command [cmd] with
    arguments [args]. It waits until the process is finished. The file
    [name.out], [name.err] and [name.info] are created, which contains
    the standard output, the standart error and some process info
    respectively *)
val run : ?env:string array -> name:string -> string -> string list -> result

(** Is the process result a success ? *)
val is_success : result -> bool

(** Is the process result a failure ? *)
val is_failure : result -> bool

(** Clean-up process result files *)
val clean_files : result -> unit

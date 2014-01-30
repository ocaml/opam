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


(* open BuildBase *)
(* open Stdlib2 *)

(* open OcpLang *)

let verbose = DebugVerbosity.verbose [ "BE" ] "BuildEngineTypes"

type delete_orphans =
    KeepOrphans
  | DeleteOrphanFiles
  | DeleteOrphanFilesAndDirectories

type rule_state =
    RULE_INACTIVE (* A rule is RULE_INACTIVE if it is not needed for the current compilation process. *)
  | RULE_ACTIVE   (* A rule is RULE_ACTIVE if it is needed for the current compilation process. However,
      it will move to RULE_WAITING if it should be evaluated because its chain of dependencies requires it. *)
  | RULE_WAITING  (* A rule is RULE_WAITING if it needs to be evaluated *)
  | RULE_EXECUTING (* A rule is RULE_EXECUTING if it has been extracted from the queues by 'next_rule' and
       not yet finished evaluating. *)
  | RULE_EXECUTED  (* A rule is RULE_EXECUTED if it has been executed. *)

type file_kind =
  | FILE_REAL
  | FILE_VIRTUAL
  | FILE_TEMPORARY


module DigestMap = Map.Make(struct
  type t = Digest.t let compare = compare
end)
type loc = string * int * string

type build_rule = {
  rule_id : int;
  rule_main_target : build_file;
  mutable rule_forced : bool;
  mutable rule_commands :  build_action list;
  rule_loc : loc; (* project_info *)
  mutable rule_sources :  build_file IntMap.t;

  (* rule_time_dependencies: dependencies that are not required, but if the rules that generate them
     are active, they should be executed before. *)
  mutable rule_time_dependencies :  build_file IntMap.t;
  mutable rule_temporaries : build_file list;
  mutable rule_targets :  build_file IntMap.t;
  mutable rule_state : rule_state;
  mutable rule_missing_sources : int;

  rule_context : build_context;
}

and dependency_loader =  string -> (string * string list list) list

and  build_action =
    Execute of build_command
  | LoadDeps of dependency_loader *  build_file *  build_rule
  | Copy of command_argument * command_argument
  | Move of command_argument * command_argument
  | MoveIfExists of command_argument * command_argument
      * command_argument option (* create a link ? *)
  | DynamicAction of string * (build_action list Lazy.t)
  | NeedTempDir
  | Function of string  (* name, for debug *)
      * (Buffer.t -> unit)  (* a printer for the command to compute digests *)
      * (unit -> unit)  (* the function itself *)

and build_command = {
  cmd_command : string list;
  mutable cmd_args : command_argument list;
  mutable cmd_stdout_pipe : string option;
  mutable cmd_stdin_pipe : string option;
  mutable cmd_stderr_pipe : string option;
  mutable cmd_move_to_dir : string option;
}

and command_argument =
    S of string (* string *)
  | T of string  (* temporary file in rule temporary directory *)
  | F of File.t (* File.t *)
  | BF of build_file (* build_file type *)
  | BD of build_directory (* build_file type *)

(* TODO: we should support the fact that directories could also be created by build rules ! *)
and  build_file = {
  file_id : int;
  mutable file_kind : file_kind; (* mutable because we sometimes discover that
                                    a file is virtual afterwards. *)
  file_dir :  build_directory;
  file_file : File.t;
  file_basename : string;
  mutable file_exists : bool;
  mutable file_mtime : BuildEngineMtime.t;
  mutable file_target_of :  build_rule list;
  mutable file_source_for :  build_rule list;
}

and  build_directory = {
  dir_key : int * int64; (* (st_dev, st_ino) *)
  dir_id : int;
  dir_basename : string;
  mutable dir_file : File.t;
  dir_parent :  build_directory;
  mutable dir_files :  build_file StringMap.t;
  mutable dir_dirs :  build_directory StringMap.t;
  mutable dir_fullname : string;
}

and build_context = {
  mutable build_should_restart : bool;

  mutable build_rules : (int, build_rule) Hashtbl.t;
  mutable build_files : (int, build_file) Hashtbl.t;

  mutable build_stats_to_execute : int;
  mutable build_stats_executed : int;
  mutable build_stats_running_rules : (int*float) list; (* rule id, tstart *)
  mutable build_stats_lastpoint : int;

  mutable build_directories : (int * int64, build_directory) Hashtbl.t;
  mutable build_next_dir_id : int;
  mutable build_next_file_id : int;
  mutable build_next_rule_id : int;
  mutable build_next_process_id : int;

  mutable cross_arg : string option;
  mutable stop_on_error_arg : bool;

  mutable build_dir_basename : string;
  mutable build_dir_filename : string;
  mutable build_dir : File.t;

  mutable build_log : out_channel;

  mutable build_cache_input : Digest.t DigestMap.t;
  mutable build_cache_entries : (Digest.t * Digest.t) IntMap.t;
  mutable build_cache_filename : string;
  mutable build_cache_log : out_channel;

  mutable queue_inactive : build_rule list;
  mutable queue_ready : build_rule IntMap.t;
  mutable queue_waiting : build_rule IntMap.t;
  mutable queue_not_waiting : build_rule IntMap.t;
  mutable temp_files : ( build_rule * build_rule list ref ) IntMap.t;
  mutable unmanaged_dependencies : string list;
  (* TODO: What's the difference between those ? *)
  mutable fatal_errors : string list list;
  mutable errors : string list list;

  mutable stats_command_executed : int;
  mutable stats_files_generated : int;
  mutable stats_total_time : float;
}

type build_process = {
  mutable proc_step : int;
  proc_rule : build_rule;
  mutable proc_commands : build_action list;
  mutable proc_last : build_command option;
}


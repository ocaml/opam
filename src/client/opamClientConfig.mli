(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Configuration options for the client lib (record, global reference, setter,
    initialisation), plus helper for global setup *)

type t = private {
  print_stats: bool;
  pin_kind_auto: bool;
  autoremove: bool;
  editor: string;
}

type 'a options_fun =
  ?print_stats:bool ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->
  'a
  (* constraint 'a = 'b -> 'c *)

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun

(** Extra files included in [opam search] *)
val search_files: string list

(** Load the global configuration file (opamroot/config) and initialise all opam
    sub-libraries, overriding the given arguments *)

val opam_init:
  ?root_dir:OpamTypes.dirname ->
  ?strict:bool ->
  ?skip_version_checks:bool ->
  ?all_parens:bool ->
  ?log_dir:OpamTypes.dirname ->
  ?print_stats:bool ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->
  ?current_switch:OpamSwitch.t ->
  ?switch_from:[ `Command_line | `Default | `Env ] ->
  ?jobs:int Lazy.t ->
  ?dl_jobs:int ->
  ?external_tags:string list ->
  ?keep_build_dir:bool ->
  ?reuse_build_dir:bool ->
  ?inplace_build:bool ->
  ?no_base_packages:bool ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?show:bool ->
  ?dryrun:bool ->
  ?fake:bool ->
  ?makecmd:string Lazy.t ->
  ?json_out:string option ->
  ?cudf_file:string option ->
  ?solver_timeout:float ->
  ?external_solver:OpamTypes.arg list option Lazy.t ->
  ?solver_preferences_default:string Lazy.t option ->
  ?solver_preferences_upgrade:string Lazy.t option ->
  ?solver_preferences_fixup:string Lazy.t option ->
  ?download_tool:(OpamTypes.arg list * OpamRepositoryConfig.dl_tool_kind) Lazy.t ->
  ?retries:int ->
  ?force_checksums:bool option ->
  ?debug_level:int ->
  ?verbose_level:int ->
  ?color:[ `Always | `Auto | `Never ] ->
  ?utf8:[ `Always | `Auto | `Extended | `Never ] ->
  ?disp_status_line:[ `Always | `Auto | `Never ] ->
  ?answer:bool option ->
  ?safe_mode:bool ->
  ?keep_log_dir:bool ->
  ?errlog_length:int ->
  ?merged_output:bool ->
  unit -> unit

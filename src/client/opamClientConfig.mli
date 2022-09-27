(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Configuration options for the client lib (record, global reference, setter,
    initialisation), plus helper for global setup *)

module E: sig
  type OpamStd.Config.E.t +=
    | ASSUMEDEPEXTS of bool option
    | AUTOREMOVE of bool option
    | CLI of string option
    | DROPWORKINGDIR of bool option
    | EDITOR of string option
    | FAKE of bool option
    | IGNOREPINDEPENDS of bool option
    | INPLACEBUILD of bool option
    | JSON of string option
    | KEEPBUILDDIR of bool option
    | NOAGGREGATE of bool option
    | NOAUTOUPGRADE of bool option
    | NOSELFUPGRADE of string option
    | PINKINDAUTO of bool option
    | REUSEBUILDDIR of bool option
    | ROOTISOK of bool option
    | SHOW of bool option
    | SKIPUPDATE of bool option
    | STATS of bool option
    | WORKINGDIR of bool option
    val cli: unit -> string option
    val rootisok: unit -> bool option
    val noaggregate: unit -> bool option
    val noselfupgrade: unit -> string option
end

type t = private {
  print_stats: bool;
  pin_kind_auto: bool;
  autoremove: bool;
  editor: string;
  keep_build_dir: bool;
  reuse_build_dir: bool;
  inplace_build: bool;
  working_dir: bool;
  drop_working_dir: bool;
  ignore_pin_depends: bool;
  show: bool;
  fake: bool;
  skip_dev_update: bool;
  json_out: string option;
  root_is_ok: bool;
  no_auto_upgrade: bool;
  assume_depexts: bool;
  cli: OpamCLIVersion.t;
  scrubbed_environment_variables: string list;
}

type 'a options_fun =
  ?print_stats:bool ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->

  ?keep_build_dir:bool ->
  ?reuse_build_dir:bool ->
  ?inplace_build:bool ->
  ?working_dir:bool ->
  ?drop_working_dir:bool ->
  ?ignore_pin_depends:bool ->
  ?show:bool ->
  ?fake:bool ->
  ?skip_dev_update:bool ->
  ?json_out:string option ->
  ?root_is_ok:bool ->
  ?no_auto_upgrade:bool ->
  ?assume_depexts:bool ->
  ?cli:OpamCLIVersion.t ->
  ?scrubbed_environment_variables:string list ->
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
  ?solver:(module OpamCudfSolver.S) Lazy.t ->
  ?skip_version_checks:bool ->
  ?all_parens:bool ->
  ?log_dir:OpamTypes.dirname ->
  ?print_stats:bool ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->
  ?keep_build_dir:bool ->
  ?reuse_build_dir:bool ->
  ?inplace_build:bool ->
  ?working_dir:bool ->
  ?drop_working_dir:bool ->
  ?ignore_pin_depends:bool ->
  ?show:bool ->
  ?fake:bool ->
  ?skip_dev_update:bool ->
  ?json_out:string option ->
  ?root_is_ok:bool ->
  ?no_auto_upgrade:bool ->
  ?assume_depexts:bool ->
  ?cli:OpamCLIVersion.t ->
  ?scrubbed_environment_variables:string list ->
  ?current_switch:OpamSwitch.t ->
  ?switch_from:OpamStateTypes.provenance ->
  ?jobs:int Lazy.t ->
  ?dl_jobs:int ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?dev_setup:bool ->
  ?dryrun:bool ->
  ?makecmd:string Lazy.t ->
  ?ignore_constraints_on:OpamPackage.Name.Set.t ->
  ?unlock_base:bool ->
  ?no_env_notice:bool ->
  ?locked:string option ->
  ?no_depexts:bool ->
  ?cudf_file:string option ->
  ?best_effort:bool ->
  ?solver_preferences_default:string option Lazy.t ->
  ?solver_preferences_upgrade:string option Lazy.t ->
  ?solver_preferences_fixup:string option Lazy.t ->
  ?solver_preferences_best_effort_prefix: string option Lazy.t ->
  ?solver_timeout:float option ->
  ?solver_allow_suboptimal:bool ->
  ?cudf_trim:string option ->
  ?dig_depth:int ->
  ?preprocess:bool ->
  ?version_lag_power:int ->
  ?download_tool:(OpamTypes.arg list * OpamRepositoryConfig.dl_tool_kind) Lazy.t ->
  ?validation_hook:OpamTypes.arg list option ->
  ?retries:int ->
  ?force_checksums:bool option ->
  ?repo_tarring:bool ->
  ?debug_level:int ->
  ?debug_sections:OpamStd.Config.sections ->
  ?verbose_level:OpamStd.Config.level ->
  ?color:OpamStd.Config.when_ ->
  ?utf8:OpamStd.Config.when_ext ->
  ?disp_status_line:OpamStd.Config.when_ ->
  ?confirm_level:OpamStd.Config.answer ->
  ?yes:bool option ->
  ?safe_mode:bool ->
  ?keep_log_dir:bool ->
  ?errlog_length:int ->
  ?merged_output:bool ->
  ?precise_tracking:bool ->
  unit -> unit

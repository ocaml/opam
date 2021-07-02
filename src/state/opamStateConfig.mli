(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Configuration options for the state lib (record, global reference, setter,
    initialisation) *)

open OpamTypes
open OpamStateTypes

type t = private {
  root_dir: OpamFilename.Dir.t;
  current_switch: OpamSwitch.t option;
  switch_from: [ `Env | `Command_line | `Default ];
  jobs: int Lazy.t;
  dl_jobs: int;
  build_test: bool;
  build_doc: bool;
  dryrun: bool;
  makecmd: string Lazy.t;
  ignore_constraints_on: name_set;
  unlock_base: bool;
  no_env_notice: bool;
  locked: string option;
}

type 'a options_fun =
  ?root_dir:OpamFilename.Dir.t ->
  ?current_switch:OpamSwitch.t ->
  ?switch_from:[ `Env | `Command_line | `Default ] ->
  ?jobs:(int Lazy.t) ->
  ?dl_jobs:int ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?dryrun:bool ->
  ?makecmd:string Lazy.t ->
  ?ignore_constraints_on:name_set ->
  ?unlock_base:bool ->
  ?no_env_notice:bool ->
  ?locked:string option ->
  'a

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun

(** Get the initial opam root value (from default, env or optional argument).
    This allows one to get it before doing the init, which is useful to get the
    configuration file used to fill some options to init() *)
val opamroot: ?root_dir:dirname -> unit -> dirname

(** Loads the global configuration file, protecting against concurrent writes *)
val load: ?lock_kind: 'a lock -> dirname -> OpamFile.Config.t option
val safe_load: ?lock_kind: 'a lock -> dirname -> OpamFile.Config.t

(** Loads the config file from the OPAM root and updates default values for all
    related OpamXxxConfig modules. Doesn't read the env yet, the [init]
    functions should still be called afterwards. OpamFormat should be
    initialised beforehand, as it may impact the config file loading.

    Returns the config file that was found, if any *)
val load_defaults:
  ?lock_kind:'a lock -> OpamFilename.Dir.t -> OpamFile.Config.t option

(** Returns the current switch, failing with an error message is none is set. *)
val get_switch: unit -> switch

(** Returns the current switch, if any is set. *)
val get_switch_opt: unit -> switch option

(** The function used to locate an external switch from parents of the current
    directory. Takes the opam root as parameter, and rejects any external switch
    configured with a different root *)
val get_current_switch_from_cwd: OpamFilename.Dir.t -> switch option

(** Checks if a local switch exists and is configurade for the given root *)
val local_switch_exists: OpamFilename.Dir.t -> switch -> bool

(** Resolves the switch if it is a link to a global switch in the given root
    (return unchanged otherwise) *)
val resolve_local_switch: OpamFilename.Dir.t -> switch -> switch

(** Given the required lock, returns [true] if the opam root is newer than the
    binary, so that it can only be loaded read-only by the current binary. *)
val is_newer_than_self: ?lock_kind:'a lock -> 'b global_state -> bool

(** Check config root version regarding self-defined one *)
val is_newer: OpamFile.Config.t ->  bool

val load_config_root:
  ?lock_kind:'a lock ->
  ((OpamFile.Config.t OpamFile.t -> 'b) * (OpamFile.Config.t OpamFile.t -> 'b)) ->
  dirname -> 'b

module Switch : sig
  val safe_load_t:
    ?lock_kind: 'a lock -> dirname -> switch -> OpamFile.Switch_config.t
  val safe_load:
    ?lock_kind: 'a lock -> 'b global_state -> switch -> OpamFile.Switch_config.t
  val safe_read_selections:
    ?lock_kind: 'a lock -> 'b global_state -> switch -> switch_selections
  val safe_read_selections_t:
    ?lock_kind: 'a lock -> dirname -> switch -> switch_selections
  val read_opt:
    ?lock_kind: 'a lock -> 'b global_state -> switch ->
    OpamFile.Switch_config.t option
end

module Repos : sig
  val safe_read:
    ?lock_kind: 'a lock -> 'b global_state -> OpamFile.Repos_config.t
end

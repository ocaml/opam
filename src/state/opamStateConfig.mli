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
  'a

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun

(** Get the initial opam root value (from default, env or optional argument).
    This allows to get it before doing the init, which is useful to get the
    configuration file used to fill some options to init() *)
val opamroot: ?root_dir:dirname -> unit -> dirname

(** Loads the global configuration file, protecting against concurrent writes *)
val load: dirname -> OpamFile.Config.t option

(** Loads the config file from the OPAM root and updates default values for all
    related OpamXxxConfig modules. Doesn't read the env yet, the [init]
    functions should still be called afterwards. OpamFormat should be
    initialised beforehand, as it may impact the config file loading.

    Returns the config file that was found, if any *)
val load_defaults: OpamFilename.Dir.t -> OpamFile.Config.t option

(** Returns the current switch, failing with an error message is none is set. *)
val get_switch: unit -> switch

(** Returns the current switch, if any is set. *)
val get_switch_opt: unit -> switch option

(** The function used to locate an external switch from parents of the current
    directory. Takes the opam root as parameter, and rejects any external switch
    configured with a different root *)
val get_current_switch_from_cwd: OpamFilename.Dir.t -> switch option

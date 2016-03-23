(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
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

open OpamTypes

type t = private {
  root_dir: OpamFilename.Dir.t;
  current_switch: OpamSwitch.t option;
  switch_from: [ `Env | `Command_line | `Default ];
  jobs: int Lazy.t;
  dl_jobs: int;
  external_tags: string list;
  keep_build_dir: bool;
  no_base_packages: bool;
  build_test: bool;
  build_doc: bool;
  show: bool;
  dryrun: bool;
  fake: bool;
  makecmd: string Lazy.t;
  json_out: string option;
}

type 'a options_fun =
  ?root_dir:OpamFilename.Dir.t ->
  ?current_switch:OpamSwitch.t ->
  ?switch_from:[ `Env | `Command_line | `Default ] ->
  ?jobs:(int Lazy.t) ->
  ?dl_jobs:int ->
  ?external_tags:string list ->
  ?keep_build_dir:bool ->
  ?no_base_packages:bool ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?show:bool ->
  ?dryrun:bool ->
  ?fake:bool ->
  ?makecmd:string Lazy.t ->
  ?json_out:string option ->
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

(** Writes the global configuration file, protecting against concurrent reads *)
val write: dirname -> OpamFile.Config.t -> unit

(** Filters flagged dependencies in an ext_formula using the currently set
    options (doc, test). Build dependencies are included *)
val filter_deps: ?dev:bool -> ext_formula -> formula

(** Loads the config file from the OPAM root and updates default values for all
    related OpamXxxConfig modules. Doesn't read the env yet, the [init]
    functions should still be called afterwards. OpamFormat should be
    initialised beforehand, as it may impact the config file loading.

    Returns the config file that was found, if any *)
val load_defaults: OpamFilename.Dir.t -> OpamFile.Config.t option

(** Returns the current switch, failing with an error message is none is set. *)
val get_switch: unit -> switch

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
  current_switch: OpamSwitch.t;
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
  unit -> 'a

val default : t

val set : t -> t options_fun

val setk : (t -> 'a) -> t -> 'a options_fun

val r : t ref

val update : ?noop:_ -> unit options_fun

(** Sets the options, reading the environment to get default
    values when unspecified *)
val init: ?noop:_ -> unit options_fun

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
val filter_deps: ext_formula -> formula

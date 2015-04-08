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

type t = private {
  root_dir: OpamFilename.Dir.t;
  current_switch: OpamSwitch.t;
  switch_from: [ `Env | `Command_line | `Default ];
  jobs: int;
  dl_jobs: int;
  external_tags: string list;
  keep_build_dir: bool;
  no_base_packages: bool;
  build_test: bool;
  build_doc: bool;
  show: bool;
  dryrun: bool;
  fake: bool;
  print_stats: bool;
  sync_archives: bool;
  self_upgrade: [ `Disable | `Running | `None ];
  pin_kind_auto: bool;
  autoremove: bool;
  editor: string;
  makecmd: string Lazy.t;
}

type 'a options_fun =
  ?root_dir:OpamFilename.Dir.t ->
  ?current_switch:OpamSwitch.t ->
  ?switch_from:[ `Env | `Command_line | `Default ] ->
  ?jobs: int ->
  ?dl_jobs: int ->
  ?external_tags:string list ->
  ?keep_build_dir:bool ->
  ?no_base_packages:bool ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?show:bool ->
  ?dryrun:bool ->
  ?fake:bool ->
  ?print_stats:bool ->
  ?sync_archives:bool ->
  ?self_upgrade:[ `Disable | `Running | `None ] ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->
  ?makecmd:string Lazy.t ->
  unit -> 'a

val default : t

val set : t -> t options_fun

val setk : (t -> 'a) -> (unit -> t) -> 'a options_fun

val r : t ref

val update : unit options_fun

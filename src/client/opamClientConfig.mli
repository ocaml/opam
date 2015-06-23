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
  print_stats: bool;
  sync_archives: bool;
  self_upgrade: [ `Disable | `Running | `None ];
  pin_kind_auto: bool;
  autoremove: bool;
  editor: string;
}

type 'a options_fun =
  ?print_stats:bool ->
  ?sync_archives:bool ->
  ?self_upgrade:[ `Disable | `Running | `None ] ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->
  unit -> 'a

val default : t

val set : t -> t options_fun

val setk : (t -> 'a) -> t -> 'a options_fun

val r : t ref

val update : ?noop:_ -> unit options_fun

(** Sets the options, reading the environment to get default
    values when unspecified *)
val init: ?noop:_ -> unit options_fun

(** OPAMNOSELFUPGRADE is set to this value when the current opam process has
    been called by an older opam process using the self-upgrade mechanism *)
val self_upgrade_bootstrapping_value: string

(** Extra files included in [opam search] *)
val search_files: string list

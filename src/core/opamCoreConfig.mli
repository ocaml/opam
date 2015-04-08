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
  debug_level : int;
  verbose_level : int;
  color : [ `Always | `Never | `Auto ];
  utf8 : [ `Extended | `Always | `Never | `Auto ];
  disp_status_line: [ `Always | `Never | `Auto ];
  answer : bool option;
  strict : bool;
  skip_version_checks : bool;
  safe_mode : bool;
  lock_retries : int;
  all_parens : bool;
  log_dir : string;
  keep_log_dir : bool;
}

type 'a options_fun =
  ?debug_level:int ->
  ?verbose_level:int ->
  ?color:[ `Always | `Never | `Auto ] ->
  ?utf8:[ `Extended | `Always | `Never | `Auto ] ->
  ?disp_status_line:[ `Always | `Never | `Auto ] ->
  ?answer:bool option ->
  ?strict:bool ->
  ?skip_version_checks:bool ->
  ?safe_mode:bool ->
  ?lock_retries:int ->
  ?all_parens:bool ->
  ?log_dir:string ->
  ?keep_log_dir:bool ->
  unit -> 'a

val default : t

val set : t -> t options_fun

val setk : (t -> 'a) -> (unit -> t) -> 'a options_fun

val r : t ref

val update : unit options_fun

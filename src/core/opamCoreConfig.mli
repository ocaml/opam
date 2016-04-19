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

(** Configuration options for the core lib (record, global reference and
    setter) *)

type t = private {
  debug_level : int;
  (** Controls debug messages, 0 to disable *)
  verbose_level : int;
  (** Controls printing of external commands and output, 0 to disable, more
      means print more low-level commands *)
  color : [ `Always | `Never | `Auto ];
  (** Console ANSI color control *)
  utf8 : [ `Extended | `Always | `Never | `Auto ];
  (** Controls usage of UTF8 in OPAM-generated messages. Extended adds camel
      emojis *)
  disp_status_line: [ `Always | `Never | `Auto ];
  (** Controls on-line display of parallel commands being run, using ANSI
      escapes *)
  answer : bool option;
  (** Affects interactive questions in OpamConsole: auto-answer with the given
      bool if Some *)
  safe_mode : bool;
  (** Fail on writes or delays, don't ask questions (for quick queries, e.g.
      for shell completion) *)
  log_dir : string;
  (** Where to store log and temporary files (output from commands...) *)
  keep_log_dir : bool;
  (** Whether to cleanup temporary and log files on exit *)
  errlog_length : int;
  (** The number of log lines displayed on process error. 0 for all *)
}

type 'a options_fun =
  ?debug_level:int ->
  ?verbose_level:int ->
  ?color:[ `Always | `Never | `Auto ] ->
  ?utf8:[ `Extended | `Always | `Never | `Auto ] ->
  ?disp_status_line:[ `Always | `Never | `Auto ] ->
  ?answer:bool option ->
  ?safe_mode:bool ->
  ?log_dir:string ->
  ?keep_log_dir:bool ->
  ?errlog_length:int ->
  'a

val default : t

val set : t -> (unit -> t) options_fun

val setk : (t -> 'a) -> t -> 'a options_fun

val r : t ref

val update : ?noop:_ -> (unit -> unit) options_fun

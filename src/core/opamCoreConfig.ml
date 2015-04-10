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

type t = {
  debug_level: int;
  verbose_level: int;
  color: [ `Always | `Never | `Auto ];
  utf8: [ `Extended | `Always | `Never | `Auto ];
  disp_status_line: [ `Always | `Never | `Auto ];
  answer: bool option;
  strict: bool;
  skip_version_checks: bool;
  safe_mode: bool;
  lock_retries: int;
  all_parens: bool;
  log_dir: string;
  keep_log_dir: bool;
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

let default = {
  debug_level = 0;
  verbose_level = 0;
  color = `Auto;
  utf8 = `Auto;
  disp_status_line = `Auto;
  answer = None;
  strict = false;
  skip_version_checks = false;
  safe_mode = false;
  lock_retries = 5;
  all_parens = false;
  log_dir =
    (let base = Printf.sprintf "opam-%s-%d" (Unix.getlogin()) (Unix.getpid()) in
     Filename.(concat (get_temp_dir_name ()) base));
  keep_log_dir = false;
}

let setk k t
    ?debug_level
    ?verbose_level
    ?color
    ?utf8
    ?disp_status_line
    ?answer
    ?strict
    ?skip_version_checks
    ?safe_mode
    ?lock_retries
    ?all_parens
    ?log_dir
    ?keep_log_dir
    ()
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    debug_level = t.debug_level + debug_level;
    verbose_level = t.verbose_level + verbose_level;
    color = t.color + color;
    utf8 = t.utf8 + utf8;
    disp_status_line = t.disp_status_line + disp_status_line;
    answer = t.answer + answer;
    strict = t.strict + strict;
    skip_version_checks = t.skip_version_checks + skip_version_checks;
    safe_mode = t.safe_mode + safe_mode;
    lock_retries = t.lock_retries + lock_retries;
    all_parens = t.all_parens + all_parens;
    log_dir = t.log_dir + log_dir;
    keep_log_dir = t.keep_log_dir + keep_log_dir;
  }

let set t = setk (fun x -> x) t

(* Global configuration reference *)

let r = ref default

let update ?noop:_ = setk (fun cfg -> r := cfg) !r

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCompat

module StringMap = OpamStd.String.Map

type t = {
  debug_level: int;
  debug_sections: int option StringMap.t;
  verbose_level: int;
  color: OpamStd.Config.when_;
  utf8: OpamStd.Config.when_ext;
  disp_status_line: OpamStd.Config.when_;
  answer: bool option;
  safe_mode: bool;
  log_dir: string;
  keep_log_dir: bool;
  errlog_length: int;
  merged_output: bool;
  use_openssl: bool;
  precise_tracking: bool;
  set: bool;
}

type 'a options_fun =
  ?debug_level:int ->
  ?debug_sections:int option StringMap.t ->
  ?verbose_level:int ->
  ?color:OpamStd.Config.when_ ->
  ?utf8:OpamStd.Config.when_ext ->
  ?disp_status_line:OpamStd.Config.when_ ->
  ?answer:bool option ->
  ?safe_mode:bool ->
  ?log_dir:string ->
  ?keep_log_dir:bool ->
  ?errlog_length:int ->
  ?merged_output:bool ->
  ?use_openssl:bool ->
  ?precise_tracking:bool ->
  'a

let default = {
  debug_level = 0;
  debug_sections = StringMap.empty;
  verbose_level = 0;
  color = `Auto;
  utf8 = `Auto;
  disp_status_line = `Auto;
  answer = None;
  safe_mode = false;
  log_dir =
    (let user = try Unix.getlogin() with Unix.Unix_error _ -> "xxx" in
     let base = Printf.sprintf "opam-%s-%d" user (OpamStubs.getpid()) in
     Filename.(concat (get_temp_dir_name ()) base));
  keep_log_dir = false;
  errlog_length = 12;
  merged_output = true;
  use_openssl = true;
  precise_tracking = false;
  set = false;
}

let setk k t
    ?debug_level
    ?debug_sections
    ?verbose_level
    ?color
    ?utf8
    ?disp_status_line
    ?answer
    ?safe_mode
    ?log_dir
    ?keep_log_dir
    ?errlog_length
    ?merged_output
    ?use_openssl
    ?precise_tracking
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    debug_level = t.debug_level + debug_level;
    debug_sections = t.debug_sections + debug_sections;
    verbose_level = t.verbose_level + verbose_level;
    color = t.color + color;
    utf8 = t.utf8 + utf8;
    disp_status_line = t.disp_status_line + disp_status_line;
    answer = t.answer + answer;
    safe_mode = t.safe_mode + safe_mode;
    log_dir = t.log_dir + log_dir;
    keep_log_dir = t.keep_log_dir + keep_log_dir;
    errlog_length = t.errlog_length + errlog_length;
    merged_output = t.merged_output + merged_output;
    use_openssl = t.use_openssl + use_openssl;
    precise_tracking = t.precise_tracking + precise_tracking;
    set = true;
  }

let set t = setk (fun x () -> x) t

(* Global configuration reference *)

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let initk k =
  let open OpamStd in
  let open OpamStd.Config in
  let utf8 = Option.Op.(
      env_when_ext "UTF8" ++
      (env_bool "UTF8MSGS" >>= function
        | true -> Some `Extended
        | false -> None)
    ) in
  let answer = match env_bool "YES", env_bool "NO" with
    | Some true, _ -> Some (Some true)
    | _, Some true -> Some (Some false)
    | None, None -> None
    | _ -> Some None
  in
  (setk (setk (fun c -> r := c; k)) !r)
    ?debug_level:(env_level "DEBUG")
    ?debug_sections:(env_sections "DEBUGSECTIONS")
    ?verbose_level:(env_level "VERBOSE")
    ?color:(env_when "COLOR")
    ?utf8
    ?disp_status_line:(env_when "STATUSLINE")
    ?answer
    ?safe_mode:(env_bool "SAFE")
    ?log_dir:(env_string "LOGS")
    ?keep_log_dir:(env_bool "KEEPLOGS")
    ?errlog_length:(env_int "ERRLOGLEN")
    ?merged_output:(env_bool "MERGEOUT")
    ?use_openssl:(env_bool "USEOPENSSL")
    ?precise_tracking:(env_bool "PRECISETRACKING")

let init ?noop:_ = initk (fun () -> ())

#ifdef DEVELOPER
let developer = true
#else
let developer = false
#endif

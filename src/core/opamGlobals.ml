(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
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

module Config = struct

  let env conv var =
    try OpamMisc.Option.map conv (OpamMisc.Env.getopt ("OPAM"^var))
    with Failure _ ->
      OpamConsole.warning
        "Invalid value for environment variable OPAM%s, ignored." var;
      None

  let env_bool var =
    env (fun s -> match String.lowercase s with
        | "" | "0" | "no" | "false" -> false
        | "1" | "yes" | "true" -> true
        | _ -> failwith "env_bool")
      var

  let env_int var = env int_of_string var

  let env_level var =
    env (fun s -> match String.lowercase s with
        | "" | "no" | "false" -> 0
        | "yes" | "true" -> 1
        | s -> int_of_string s)
      var

  let env_string var =
    env (fun s -> s) var

  let env_float var =
    env float_of_string var

  let when_ext s =
    match String.lowercase s with
    | "extended" -> `Extended
    | "always" -> `Always
    | "never" -> `Never
    | "auto" -> `Auto
    | _ -> failwith "env_when"

  let env_when_ext var = env when_ext var

  let env_when var =
    env (fun v -> match when_ext v with
        | (`Always | `Never | `Auto) as w -> w
        | `Extended -> failwith "env_when")
      var

  let resolve_when ~auto = function
    | `Always -> true
    | `Never -> false
    | `Auto -> Lazy.force auto

  let command_of_string s =
    List.map (fun s -> OpamTypes.CString s, None) (OpamMisc.String.split s ' ')
end


let init_config () =
  let open Config in
  let utf8 = OpamMisc.Option.Op.(
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
  OpamCoreConfig.(
    setk
      (fun conf -> setk (fun c -> r := c) (fun () -> conf))
      (fun () -> !r)
  )
    ?debug_level:(env_level "DEBUG")
    ?verbose_level:(env_level "VERBOSE")
    ?color:(env_when "COLOR")
    ?utf8
    ?disp_status_line:(env_when "STATUSLINE")
    ?answer
    ?strict:(env_bool "STRICT")
    ?skip_version_checks:(env_bool "SKIPVERSIONCHECKS")
    ?safe_mode:(env_bool "SAFE")
    ?lock_retries:(env_int "LOCKRETRIES")
    ?all_parens:(env_bool "ALLPARENS")
    ?log_dir:(env_string "LOGS")
    ?keep_log_dir:(env_bool "KEEPLOGS")
    ()

let system = "system"
let default_repository_name    = "default"
let default_repository_address = "https://opam.ocaml.org"

(*
type repositories_config = {
  req_checksums: bool;
  download_retry: int;
  download_tool: [
    | `Wget
    | `Curl
    | `Custom of
        url:string -> out:string -> retry:int -> compress:bool -> string list
  ];
}

type config_origin = [ `Unset | `File of string | `Env of string | `Command_line ]

type solver_config = {
  use_external_solver: bool;
  cudf_file: string option;
  solver_timeout: float;
  solver_preferences_default: string;
  solver_preferences_upgrade: string;
  solver_preferences_fixup: string;
  external_solver:
    (input:string -> output:string -> criteria:string -> string list);
  external_solver_from: config_origin;
}
*)
(*
let default_config = {
  debug = false;
  debug_level = 0;
  verbose = false;
  verbose_level = 0;
  color = false;
  utf8 = false;
  utf8_msgs = false;
  keep_build_dir = false;
  no_base_packages = false;
  no_checksums = false;
  req_checksums = false;
  yes = false;
  strict = false;
  build_test = false;
  build_doc = false;
  show = false;
  dryrun = false;
  fake = false;
  print_stats = false;
  do_not_copy_files = false;
  sync_archives = false;
  use_external_solver = false;
  no_self_upgrade = false;
  skip_version_checks = false;
  safe_mode = false;
  lock_retries: int;
  pin_kind_auto = false;
  all_parens = false;
  jobs = 1;
  dl_jobs = 3;
  download_retry = 3;
  cudf_file = None;
  solver_timeout = 5.;
  solver_preferences_default = "-removed,-notuptodate,-changed";
  solver_preferences_upgrade = "-removed,-notuptodate,-changed";
  solver_preferences_fixup = "-changed,-notuptodate";
  external_solver = (fun ~input:_ ~output:_ ~criteria:_ -> failwith "Uninitialised"):
  external_solver_from = `Default;
  switch = string;
  switch_from = config_origin;
  external_tags = string list;
  root_dir =
    Filename.concat (Filename.get_temp_dir_name ())
      ("opam-" ^ string_of_int (Unix.getpid ()));
}
*)


(*
let check ?(warn=true) var = ref (
    try
      match String.lowercase (OpamMisc.Env.get ("OPAM"^var)) with
      | "" | "0" | "no" | "false" -> false
      | "1" | "yes" | "true" -> true
      | v ->
        if warn then
          prerr_endline
            (OpamMisc.Format.reformat ~indent:10
               (Printf.sprintf
                  "[WARNING] Invalid value %S for env variable OPAM%s, assumed \
                   true." v var));
        true
    with Not_found -> false
  )

let when_var v =
    try (match OpamMisc.Env.get ("OPAM"^v) with
      | "always" -> `Always
      | "never" -> `Never
      | _ -> `Auto
      )
    with
      | Not_found -> `Auto

let dumb_term =
  try OpamMisc.Env.get "TERM" = "dumb" with Not_found -> true

let debug            = check ~warn:false "DEBUG"
let debug_level      =
  try ref (int_of_string (OpamMisc.Env.get ("OPAMDEBUG")))
  with Not_found | Failure _ -> ref 1
let _ = if !debug_level > 1 then debug := true
let verbose          = check ~warn:false "VERBOSE"
let verbose_level    =
  try ref (int_of_string (OpamMisc.Env.get ("OPAMVERBOSE"))) with
  | Not_found -> ref 0
  | Failure _ -> ref 1
let color_when       = when_var "COLOR"
let color            =
  ref (color_when = `Always ||
       color_when = `Auto && OpamMisc.Sys.tty_out && not dumb_term)
let disp_status_line_when = when_var "STATUSLINE"
let disp_status_line () =
  disp_status_line_when = `Always ||
  disp_status_line_when = `Auto &&
  OpamMisc.Sys.tty_out && (!color || not dumb_term)
let keep_build_dir   = check "KEEPBUILDDIR"
let no_base_packages = check "NOBASEPACKAGES"
let no_checksums     = check "NOCHECKSUMS"
let req_checksums    = check "REQUIRECHECKSUMS"
let yes              = check "YES"
let no               = check "NO"
let strict           = check "STRICT"
let build_test       = check "BUILDTEST"
let build_doc        = check "BUILDDOC"
let show             = check "SHOW"
let dryrun           = check "DRYRUN"
let fake             = check "FAKE"
let print_stats      = check "STATS"
let utf8_msgs        = check "UTF8MSGS"
let utf8_when        = when_var "UTF8"
let utf8             =
  ref (utf8_when = `Always || utf8_when = `Auto && locale_utf8 () || !utf8_msgs)
let autoremove       = check "AUTOREMOVE"
let do_not_copy_files = check "DONOTCOPYFILES"
let sync_archives    = check "SYNCARCHIVES"
let no_self_upgrade  = check ~warn:false "NOSELFUPGRADE"
let skip_version_checks = check "SKIPVERSIONCHECKS"
let safe_mode        = check "SAFE"
let lock_retries     =
  try ref (int_of_string (OpamMisc.Env.get ("OPAMLOCKRETRIES")))
  with Not_found | Failure _ -> ref 5
let pin_kind_auto    = check "PINKINDAUTO"
let all_parens       = ref false
*)

(*
let jobs = ref (
    try Some (int_of_string (OpamMisc.Env.get "OPAMJOBS"))
    with Not_found | Failure _ -> None
  )

let dl_jobs = ref (
    try Some (int_of_string (OpamMisc.Env.get "OPAMDOWNLOADJOBS"))
    with Not_found | Failure _ -> None
  )

let download_retry =
  try max 1 (int_of_string (OpamMisc.Env.get "OPAMRETRY"))
  with Not_found | Failure _ -> 3

let cudf_file = ref (
    try Some (OpamMisc.Env.get "OPAMCUDFFILE")
    with Not_found -> None
  )

let solver_timeout =
  try float_of_string (OpamMisc.Env.get "OPAMSOLVERTIMEOUT")
  with Not_found | Failure _ -> 5.
*)

(*
let default_repository_name    = "default"
let default_repository_address = "https://opam.ocaml.org"

let download_tool_env =
  try ref (Some (OpamMisc.Env.get "OPAMFETCH"))
  with Not_found -> ref None

let curl_command = try OpamMisc.Env.get "OPAMCURL" with Not_found -> "curl"

type download_tool = [
  | `Wget
  | `Curl
  | `Custom of
      url:string -> out:string -> retry:int -> compress:bool -> string list
]
let download_tool = ref None

let search_files = ref ["findlib"]

let default_build_command = [ [ "./build.sh" ] ]

let global_config = "global-config"

let system = "system"

let switch: [`Env of string
            | `Command_line of string
            | `Not_set ] ref
  = ref (
    try `Env (OpamMisc.Env.get "OPAMSWITCH")
    with Not_found -> `Not_set
  )

let external_tags = ref ([] : string list)

let home =
  try OpamMisc.Env.get "HOME"
  with Not_found -> Sys.getcwd ()

let default_opam_dir =
  try OpamMisc.Env.get "OPAMROOT"
  with Not_found -> Filename.concat home ".opam"

let root_dir_tmp =
  Filename.concat (Filename.get_temp_dir_name ())
    ("opam-" ^ string_of_int (Unix.getpid ()))

let root_dir = ref root_dir_tmp

let editor = lazy (
  try OpamMisc.Env.get "OPAM_EDITOR" with Not_found ->
  try OpamMisc.Env.get "VISUAL" with Not_found ->
  try OpamMisc.Env.get "EDITOR" with Not_found ->
    "nano"
)

let makecmd = ref OpamMisc.Sys.(fun () ->
    match os () with
    | FreeBSD
    | OpenBSD
    | NetBSD
    | DragonFly -> "gmake"
    | _ -> "make"
  )
*)

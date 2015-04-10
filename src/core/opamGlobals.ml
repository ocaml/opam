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
    try OpamStd.Option.map conv (OpamStd.Env.getopt ("OPAM"^var))
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
    List.map (fun s -> OpamTypes.CString s, None) (OpamStd.String.split s ' ')
end


let init_config () =
  let open Config in
  let utf8 = OpamStd.Option.Op.(
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

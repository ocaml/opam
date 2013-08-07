(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

(* Convention:
   all the global OPAM variables can be set using environment variables
   using OPAM<variable> *)

let check var = ref (
    try OpamMisc.getenv ("OPAM"^var) <> ""
    with Not_found -> false
  )

let debug            = check "DEBUG"
let verbose          = check "VERBOSE"
let keep_build_dir   = check "KEEPBUILDDIR"
let no_base_packages = check "NOBASEPACKAGES"
let no_checksums     = check "NOCHECKSUMS"
let yes              = check "YES"
let build_test       = check "BUILDTEST"
let build_doc        = check "BUILDDOC"
let dryrun           = check "DRYRUN"
let fake             = check "FAKE"
let print_stats      = check "STATS"
let utf8_msgs        = check "UTF8MSGS"
let autoremove       = check "AUTOREMOVE"
let do_not_copy_files = check "DONOTCOPYFILES"
let sync_archives    = check "SYNCARCHIVES"

let jobs = ref (
    try Some (int_of_string (OpamMisc.getenv "OPAMJOBS"))
    with _ -> None
  )

let download_retry =
  try max 1 (int_of_string (OpamMisc.getenv "OPAMRETRY"))
  with _ -> 10

let cudf_file = ref (
    try Some (OpamMisc.getenv "OPAMCUDFFILE")
    with _ -> None
  )

let solver_timeout =
  try float_of_string (OpamMisc.getenv "OPAMSOLVERTIMEOUT")
  with _ -> 5.

let aspcud_criteria =
  try OpamMisc.strip (OpamMisc.getenv "OPAMCRITERIA")
  with _ -> "-removed,-notuptodate,-new"

let default_repository_name    = "default"
let default_repository_address = "http://opam.ocamlpro.com"

let default_build_command = [ [ "./build.sh" ] ]

let global_config = "global-config"

let system = "system"

let switch: [`Env of string
            | `Command_line of string
            | `Not_set ] ref
  = ref (
    try `Env (OpamMisc.getenv "OPAMSWITCH")
    with _ -> `Not_set
  )

let opam_version = "1"

let external_tags = ref ([] : string list)

let home =
  try OpamMisc.getenv "HOME"
  with _ -> Sys.getcwd ()

let default_opam_dir =
  Filename.concat home ".opam"

let root_dir = ref (
    try OpamMisc.getenv "OPAMROOT"
    with _ -> default_opam_dir
  )

let init_time = Unix.gettimeofday ()

let timestamp () =
  let time = Unix.gettimeofday () -. init_time in
  let tm = Unix.gmtime time in
  let msec = time -. (floor time) in
  Printf.sprintf "%.2d:%.2d.%.3d"
    (tm.Unix.tm_hour * 60 + tm.Unix.tm_min)
    tm.Unix.tm_sec
    (int_of_float (1000.0 *. msec))

let log section fmt =
  Printf.ksprintf (fun str ->
    if !debug then
      Printf.eprintf "%s  %06d  %-25s  %s\n%!"
        (timestamp ()) (Unix.getpid ()) section str
  ) fmt

let error fmt =
  Printf.ksprintf (fun str ->
    Printf.eprintf "%s\n%!" str
  ) fmt

let warning fmt =
  Printf.ksprintf (fun str ->
    Printf.eprintf "[WARNING] %s\n%!" str
  ) fmt

exception Exit of int

let error_and_exit fmt =
  Printf.ksprintf (fun str ->
    error "%s" str;
    raise (Exit 66)
  ) fmt

let display_messages = ref true

let msg fmt =
  Printf.ksprintf (fun str ->
    flush stderr;
    if !display_messages then (
      Printf.printf "%s%!" str
    )
  ) fmt

type os =
  | Darwin
  | Linux
  | FreeBSD
  | OpenBSD
  | NetBSD
  | DragonFly
  | Cygwin
  | Win32
  | Unix
  | Other of string

let osref = ref None

let os () =
  match !osref with
  | None ->
    let os = match Sys.os_type with
      | "Unix" -> begin
          match OpamMisc.uname_s () with
          | Some "Darwin"    -> Darwin
          | Some "Linux"     -> Linux
          | Some "FreeBSD"   -> FreeBSD
          | Some "OpenBSD"   -> OpenBSD
          | Some "NetBSD"    -> NetBSD
          | Some "DragonFly" -> DragonFly
          | _                -> Unix
        end
      | "Win32"  -> Win32
      | "Cygwin" -> Cygwin
      | s        -> Other s in
    osref := Some os;
    os
  | Some os -> os

let string_of_os = function
  | Darwin    -> "darwin"
  | Linux     -> "linux"
  | FreeBSD   -> "freebsd"
  | OpenBSD   -> "openbsd"
  | NetBSD    -> "netbsd"
  | DragonFly -> "dragonfly"
  | Cygwin    -> "cygwin"
  | Win32     -> "win32"
  | Unix      -> "unix"
  | Other x   -> x

let os_string () =
  string_of_os (os ())

let makecmd = ref (fun () ->
    match os () with
    | FreeBSD
    | OpenBSD
    | NetBSD
    | DragonFly -> "gmake"
    | _ -> "make"
  )

let log_limit = 10
let log_line_limit = 5 * 80

let default_jobs = 1

let exit i =
  raise (Exit i)

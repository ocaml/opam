(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(* Convention:
   all the global OPAM variables can be set using environment variables
   using OPAM<variable> *)

let check var = ref (
  try OpamMisc.getenv ("OPAM"^var) <> ""
  with Not_found -> false
)

let debug = ref (
  try int_of_string (OpamMisc.getenv "OPAMDEBUG") >= 2
  with _ -> false
)

let verbose =
  try ref (int_of_string (OpamMisc.getenv "OPAMDEBUG") >= 1)
  with _ -> check "VERBOSE"

let keep_build_dir   = check "KEEPBUILDDIR"
let no_base_packages = check "NOBASEPACKAGES"
let no_checksums     = check "NOCHECKSUMS"
let yes              = check "YES"
let build_test       = check "BUILDTEST"
let build_doc        = check "BUILDDOC"
let dryrun           = check "DRYRUN"
let fake             = check "FAKE"
let print_stats      = check "STATS"

let cudf_file = ref (None: string option)
let aspcud_criteria =
  try OpamMisc.strip (OpamMisc.getenv "OPAMCRITERIA")
  with _ -> "paranoid"

let default_repository_name    = "default"
let default_repository_address = "http://opam.ocamlpro.com"

let default_build_command = [ [ "./build.sh" ] ]

let default_package = "conf-ocaml"

let default_switch = "system"

let switch : string option ref = ref None

let opam_version = "1"

let home =
  try OpamMisc.getenv "HOME"
  with _ -> Sys.getcwd ()

let default_opam_dir =
  Filename.concat home ".opam"

let root_dir = ref (
  try OpamMisc.getenv "OPAMROOT"
  with _ -> default_opam_dir
)

let log section fmt =
  Printf.ksprintf (fun str ->
    if !debug then
      Printf.eprintf "[%d] %-20s %s\n%!" (Unix.getpid ()) section str
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

let msg fmt =
  Printf.ksprintf (fun str ->
    Printf.printf "%s%!" str
  ) fmt

type os =
  | Darwin
  | Linux
  | FreeBSD
  | OpenBSD
  | Cygwin
  | Win32
  | Unix
  | Other of string

let uname_s =
  ref (fun () -> assert false)

let os = lazy (
  match Sys.os_type with
  | "Unix" -> begin
    match !uname_s () with
    | "Darwin"  -> Darwin
    | "Linux"   -> Linux
    | "FreeBSD" -> FreeBSD
    | "OpenBSD" -> OpenBSD
    | _         -> Unix
  end
  | "Win32"  -> Win32
  | "Cygwin" -> Cygwin
  | s        -> Other s
)

let string_of_os = function
  | Darwin  -> "darwin"
  | Linux   -> "linux"
  | FreeBSD
  | OpenBSD -> "bsd"
  | Cygwin  -> "cygwin"
  | Win32   -> "win32"
  | Unix    -> "unix"
  | Other x -> x

let os_string =
  lazy (string_of_os (Lazy.force os))

let makecmd = ref (lazy (
  match Lazy.force os with
  | FreeBSD
  | OpenBSD -> "gmake"
  | _ -> "make"
)
)

let ulimit_pipe = 65536

let log_limit = 10
let log_line_limit = 5 * 80

let default_jobs = 1

let exit i =
  raise (Exit i)

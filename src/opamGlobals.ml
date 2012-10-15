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

let debug = ref (
  try int_of_string (Sys.getenv "OPAMDEBUG") >= 2
  with _ -> false
)

let verbose = ref (
  try int_of_string (Sys.getenv "OPAMDEBUG") >= 1
  with _ -> false
)

let base_packages = ref true
let verify_checksums = ref true
let yes = ref false

let default_repository_name    = "default"
let default_repository_address = "http://opam.ocamlpro.com"
let default_repository_kind    = "curl"

let default_build_command = [ [ "./build.sh" ] ]

let default_package = "conf-ocaml"

let default_alias = "system"

let opam_version = "1"

let home = Unix.getenv "HOME"
let default_opam_dir = Filename.concat home ".opam"

let root_dir = ref default_opam_dir

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

let uname_s () =
  let stdout_in, stdout = Unix.pipe () in
  let _, stderr = Unix.pipe () in
  let stdin, _ = Unix.pipe () in
  let _ = Unix.create_process "uname" [|"-s"|] stdin stdout stderr in
  let buf = String.create 128 in
  let r = Unix.read stdout_in buf 0 128 in
  (* Trim a newline if one is present *)
  try
    let idx = String.index buf '\n' in
    String.sub buf 0 (if idx < r then idx else r)
  with _ ->
    String.sub buf 0 r

let os = match Sys.os_type with
  | "Unix" -> begin
    match uname_s () with
    | "Darwin"  -> Darwin
    | "Linux"   -> Linux
    | "FreeBSD" -> FreeBSD
    | "OpenBSD" -> OpenBSD
    | _         -> Unix
  end
  | "Win32"  -> Win32
  | "Cygwin" -> Cygwin
  | s        -> Other s

let string_of_os = function
  | Darwin  -> "darwin"
  | Linux   -> "linux"
  | FreeBSD
  | OpenBSD -> "bsd"
  | Cygwin  -> "cygwin"
  | Win32   -> "win32"
  | Unix    -> "unix"
  | Other x -> x

let os_string = string_of_os os

let makecmd = ref (match os with FreeBSD | OpenBSD -> "gmake" | _ -> "make") (* WARNING check the result of "which gmake", "which make", ... *)

let default_cores = 1

let exit i =
  raise (Exit i)

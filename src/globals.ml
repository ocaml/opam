(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

let debug = ref (
  try let (_:string) = Sys.getenv "OCPGETDEBUG" in true
  with _ -> false
)

let version = "0.1+dev"

let default_hostname = "opam.ocamlpro.com"
let default_port = 9999

let ocaml_version = Sys.ocaml_version
let api_version = 1

let home = Unix.getenv "HOME"
let default_opam_server_path = Filename.concat home ".opam-server"
let default_opam_path = Filename.concat home ".opam"

let root_path = ref default_opam_path

let log section fmt =
  Printf.kprintf (fun str ->
    if !debug then
      Printf.eprintf " %-20s %s\n%!" section str
  ) fmt

let error fmt =
  Printf.kprintf (fun str ->
    Printf.eprintf "ERROR: %s\n%!" str
  ) fmt

let warning fmt =
  Printf.kprintf (fun str ->
    Printf.eprintf "WARNING: %s\n%!" str
  ) fmt
  
let error_and_exit fmt =
  Printf.kprintf (fun str ->
    error "%s" str;
    exit 1
  ) fmt

let msg fmt =
  Printf.kprintf (fun str ->
    Printf.printf "%s%!" str
  ) fmt

type os = 
  | Darwin
  | Linux
  | FreeBSD
  | Cygwin
  | Win32
  | Unix

let os =
  match Sys.os_type with
  | "Unix" -> begin
    match input_line (Unix.open_process_in "uname -s") with
    | "Darwin"  -> Darwin
    | "Linux"   -> Linux
    | "FreeBSD" -> FreeBSD
    | _         -> Unix
  end
  | "Win32"  -> Win32
  | "Cygwin" -> Cygwin
  | _        -> assert false












(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)


(* open OcpLang *)
(* open OcpSystem *)


open BuildMisc

open BuildEngineTypes
open BuildEngineGlobals
open BuildEngineContext
open BuildEngineRules
open BuildEngineRules

let byte_exe =
  match MinUnix.os_type with
     MinUnix.WINDOWS
   | MinUnix.CYGWIN -> ".byte.exe"
   | MinUnix.UNIX -> ".byte"

let asm_exe =
  match MinUnix.os_type with
     MinUnix.WINDOWS
   | MinUnix.CYGWIN -> ".asm.exe"
   | MinUnix.UNIX -> ".asm"

let add_dst_file b dst_dir filename =
  add_file b dst_dir (Filename.basename filename)

exception NoSuchFileInDir of string * string

let find_dst_file dst_dir filename =
  try
    find_file dst_dir (Filename.basename filename)
  with Not_found ->
    raise (NoSuchFileInDir (filename, dst_dir.dir_fullname))

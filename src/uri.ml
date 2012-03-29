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

open ExtString

(* XXX: config:// andn install:// are weird URI *)
type uri = 
  | Http
  | Git
  | Local

let uri_of_url s =
  let git = Filename.check_suffix s ".git" in
  try let s1, s2 = String.split s "://" in
    match s1 with
    | _ when git -> Some Git    , s
    | "git"      -> Some Git    , s2
    | "local"    -> Some Local  , s2
    | "http"     -> Some Http   , s2
    | _          -> None        , s2
  with _->
    None, s

let string_of_uri = function
  | Local   -> "local://"
  | Http    -> "http://"
  | Git     -> "git://"

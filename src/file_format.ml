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

type content =
  | String of string
  | List of content list

type statement = {
  kind: string;
  name: string;
  contents: (string * content) list
}

type file = {
  version: int;
  statements: statement list;
}

let parse_string = function
  | String s -> s
  | _        -> Globals.error_and_exit "Bad format: expecting a string, got a list"

let parse_string_list = function
  | List l -> List.map parse_string l
  | _      -> Globals.error_and_exit "Bad format: expecting a list, got s string"

let parse_pair = function
  | List[String k; String v] -> (k, v)
  | _                        -> Globals.error_and_exit "Bad format: expecting a pair"

let parse_pair_list = function
  | List l -> List.map parse_pair l
  | _      -> Globals.error_and_exit "Bad format: expecting a list, got a string"

let string_list n s =
  try parse_string_list (List.assoc n s.contents)
  with Not_found -> []

let pair_list n s =
  try parse_pair_list (List.assoc n s.contents)
  with Not_found -> []

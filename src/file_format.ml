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
open Uri

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

let is_valid s fields =
  List.for_all (fun f -> List.mem f fields) (List.map fst s.contents)

let parse_string = function
  | String s -> s
  | _        -> Globals.error_and_exit "Bad format: expecting a string, got a list"

let parse_string_list = function
  | List l -> List.map parse_string l
  | _      -> Globals.error_and_exit "Bad format: expecting a list, got a string"

let parse_pair_opt = function
  | List[String k; String v] -> k, v
  | List[String k]           -> k, Filename.basename k
  | _                        -> Globals.error_and_exit "Bad format: expecting a pair"

let parse_pair = function
  | List[String k; String v] -> (k, v)
  | _                        -> Globals.error_and_exit "Bad format: expecting a pair"

let parse_pair_list_ parse_pair = function
  | List l -> List.map parse_pair l
  | _      -> Globals.error_and_exit "Bad format: expecting a list, got a string"

let parse_pair_list = parse_pair_list_ parse_pair
let parse_pair_opt_list = parse_pair_list_ parse_pair_opt

let string_list n s =
  try parse_string_list (List.assoc n s.contents)
  with Not_found -> []

let pair_list_ parse_pair_list n s =
  try parse_pair_list (List.assoc n s.contents)
  with Not_found -> []

let pair_list = pair_list_ parse_pair_list
let pair_opt_list = pair_list_ parse_pair_opt_list

let string n s =
  try parse_string (List.assoc n s.contents)
  with Not_found -> Globals.error_and_exit "Bad format: field'%S is missing" n

let string_option n s =
  try Some (parse_string (List.assoc n s.contents))
  with Not_found -> None

let rec string_of_content = function
  | String s -> Printf.sprintf "%S" s
  | List l   ->
      Printf.sprintf "[%s]"
        (String.concat "; "  (List.map string_of_content l))

let parse_l_url =
  List.map (fun s ->
    match uri_of_url s with
    | None      , s2
    | Some Local, s2 -> Path.Internal (Run.real_path s2)
    | Some uri  , s2 -> Path.External (uri, s2)
  )

let command n s = 
  List.map 
    (function 
      | List l -> Run.Sh (parse_string_list (List l))
      | s -> Run.OCaml (parse_string s))
    (match try List.assoc n s.contents with Not_found -> List [] with
      | List l -> l
      | _ -> Globals.error_and_exit "Bad format: expecting a list, got a string")

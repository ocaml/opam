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
    try
      let s1, s2 = String.split s "://" in
      match s1 with
        | "http" -> Path.External ((match Globals.os with Globals.Darwin -> Run.Http_ftp | _ -> Run.Http_wget), s2)
        | "local" -> Path.Internal s2
        | "git" -> Path.External (Run.Git, s2)
        | "config" -> Path.External (Run.Config, s2)
        | "install" -> Path.External (Run.Install, s2)
        | _ -> failwith "to complete !"
    with Invalid_string -> Printf.kprintf failwith "to complete : %S" s)

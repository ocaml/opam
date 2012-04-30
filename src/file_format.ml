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

type value =
  | Bool of bool
  | String of string
  | Symbol of string
  | Ident of string
  | List of value list
  | Option of value * value list

type section = {
  kind : string;
  name : string;
  items: item list
}

and item =
  | Section of section
  | Variable of string * value

let variables items =
  let l = List.fold_left (fun accu -> function
    | Variable (k,v) -> (k,v) :: accu
    | _              -> accu
  ) [] items in
  List.rev l

let sections items =
  let l = List.fold_left (fun accu -> function
    | Section s -> (s.kind, s) :: accu
    | _          -> accu
  ) [] items in
  List.rev l

type file = item list

let bad_format fmt =
  Printf.kprintf (Globals.error_and_exit "Bad format: %s") fmt

let rec is_valid items fields =
  List.for_all (function
    | Variable (f, _) ->  List.mem f fields
    | Section s       -> is_valid s.items fields
  ) items

let kind = function
  | Bool _   -> "bool"
  | Ident _  -> "ident"
  | Symbol _ -> "symbol"
  | String _ -> "string"
  | List _   -> "list"
  | Option _ -> "option"

let kinds l =
  String.concat "," (List.map kind l)

(* Base parsing functions *)
let parse_bool = function
  | Bool b -> b
  | x      -> bad_format "expecting a bool, got %s" (kind x)

let parse_ident = function
  | Ident i -> i
  | x       -> bad_format "expecting an ident, got %s" (kind x)

let parse_ident = function
  | Symbol s -> s
  | x       -> bad_format "expecting a symbol, got %s" (kind x)

let parse_string = function
  | String s -> s
  | x        -> bad_format "expecting a string, got %s" (kind x)

let parse_list fn = function
  | List s -> List.map fn s
  | x      -> bad_format "expecting a list, got %s" (kind x)

let parse_singleton fn = function
  | List [s] -> fn s
  | List l   -> bad_format "expecting a singleton, gat list of size %d" (List.length l)
  | x        -> bad_format "expecting a singleton, got %s" (kind x)

let parse_string_list = parse_list parse_string

let parse_option fnk fnv = function
  | Option (k,v) -> fnk, fnv v
  | x            -> bad_format "expecting an option, got %s" (kind x)

let (|>) f g x = g (f x) 

let make_string str = String str

let make_ident str = Ident str

let make_symbol str = Symbol str

let make_bool b = Bool b

let make_list fn l = List (List.map fn l)

let make_option fk fv k v = Option (fk k, fv v)

(* Printing *)

let rec string_of_value = function
  | Symbol s
  | Ident s     -> Printf.sprintf "%s" s
  | Bool b      -> Printf.sprintf "%b" b
  | String s    -> Printf.sprintf "%S" s
  | List l      -> Printf.sprintf "[%s]" (string_of_values l)
  | Option(v,l) -> Printf.sprintf "%s ( %s )" (string_of_value v) (string_of_values l)

and string_of_values l =
  String.concat " "  (List.map string_of_value l)

let rec string_of_item = function
  | Variable (i, v) -> Printf.sprintf "%s: %s\n" i (string_of_value v)
  | Section s       ->
      Printf.sprintf "%s %S {\n%s\n}\n" s.kind s.name (string_of_items s.items)

and string_of_items is =
  String.concat "\n" (List.map string_of_item is)

let string_of_file = string_of_items

(* Reading section contents *)

let assoc s n parse =
  try parse (List.assoc n (variables s.items))
  with Not_found -> bad_format "field  %S is missing" n

let assoc_section s n =
  try List.find_all (fun (k,_) -> k=n) (sections s.items)
  with Not_found -> bad_format "section %S is missing" n

let assoc_option s n parse =
  try parse (List.assoc n (variables s.items))
  with Not_found -> None

let assoc_list s n parse =
  try parse (List.assoc n (variables s.items))
  with Not_found -> []

let assoc_string_list s n =
  assoc_list s n (parse_list parse_string)

(* Parsing of dependency formulas *)
let rec parse_constraints name = function
  | [] -> []
  | [Symbol r; String v] ->
      [ (name, Some (r, v)) ]
  | Symbol r :: String v :: Symbol "&" :: t ->
      (name, Some (r, v)) :: parse_constraints name t
  | x -> bad_format "expecting a constraint, got %s" (kinds x)

let rec parse_and_formula = function 
  | String name             -> [ (name, None) ]
  | Option (String name, l) -> parse_constraints name l
  | List l                  -> List.flatten (List.map parse_and_formula l)
  | x                       -> bad_format "expecting an AND formala, got %s" (kind x)

let parse_or_formula = function
  | List l -> List.map parse_and_formula l
  | x      -> [ parse_and_formula x ]

let make_constraint = function
  | name, None       -> String name
  | name, Some (r,v) -> Option (String name, [Symbol r; String v])

let make_and_formula l =
  List (List.map make_constraint l)

let make_or_formula l =
  List (List.map make_and_formula l)

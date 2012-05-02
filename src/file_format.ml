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
  | List of value list           (* [ .. ] *)
  | Group of value list          (* ( .. ) *)
  | Option of value * value list (* ... { ... } *)

type section = {
  kind : string;
  name : string;
  items: item list
}

and item =
  | Section of section
  | Variable of string * value

type file = {
  contents: item list;
  filename: string;
}

let empty = {
  contents = [];
  filename = "<none>";
}

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

exception Bad_format of string

let bad_format fmt =
  Printf.kprintf (fun str -> raise (Bad_format str)) fmt

let rec is_valid items fields =
  List.for_all (function
    | Variable (f, _) -> List.mem f fields
    | Section s       -> is_valid s.items fields
  ) items

let kind = function
  | Bool _   -> "bool"
  | Ident _  -> "ident"
  | Symbol _ -> "symbol"
  | String _ -> "string"
  | List _   -> "list"
  | Group _  -> "group"
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

let parse_symbol = function
  | Symbol s -> s
  | x        -> bad_format "expecting a symbol, got %s" (kind x)

let parse_ident = function
  | Symbol s -> s
  | x       -> bad_format "expecting a symbol, got %s" (kind x)

let parse_string = function
  | String s -> s
  | x        -> bad_format "expecting a string, got %s" (kind x)

let parse_list fn = function
  | List s -> List.map fn s
  | x      -> bad_format "expecting a list, got %s" (kind x)

let parse_group fn = function
  | Group g -> List.map fn g
  | x        -> bad_format "expecting a group, got %s" (kind x)

let parse_option f g = function
  | Option (k,l) -> f k, List.map g l
  | k            -> f k, []

let parse_string_option = function
  | Option (String k, [String v]) -> k, Some v
  | String k                      -> k, None
  | x -> bad_format "expecting a string option, got %s" (kind x)

let parse_string_list = parse_list parse_string

let rec parse_or fns v =
  match fns with
  | []   -> bad_format "cannot parse %s" (kind v)
  | h::t ->
      try h v
      with _ -> parse_or t v

let (|>) f g x = g (f x) 

let make_string str = String str

let make_ident str = Ident str

let make_symbol str = Symbol str

let make_bool b = Bool b

let make_list fn l = List (List.map fn l)

let make_group fn g = Group (List.map fn g)

let make_option f g (v,l) = Option (f v, List.map g l)

(* Printing *)

let rec string_of_value = function
  | Symbol s
  | Ident s     -> Printf.sprintf "%s" s
  | Bool b      -> Printf.sprintf "%b" b
  | String s    -> Printf.sprintf "%S" s
  | List l      -> Printf.sprintf "[%s]" (string_of_values l)
  | Group g     -> Printf.sprintf "(%s)" (string_of_values g)
  | Option(v,l) -> Printf.sprintf "%s {%s}" (string_of_value v) (string_of_values l)

and string_of_values l =
  String.concat " "  (List.map string_of_value l)

let rec string_of_item = function
  | Variable (i, v) -> Printf.sprintf "%s: %s\n" i (string_of_value v)
  | Section s       ->
      Printf.sprintf "%s %S {\n%s\n}\n" s.kind s.name (string_of_items s.items)

and string_of_items is =
  String.concat "\n" (List.map string_of_item is)

let string_of_file f = string_of_items f.contents

(* Reading section contents *)

let assoc items n parse =
  try parse (List.assoc n (variables items))
  with Not_found -> bad_format "field  %S is missing" n

let get_all_section_by_kind items kind =
  try List.map snd (List.find_all (fun (k,_) -> k=kind) (sections items))
  with Not_found -> bad_format "section kind=%S is missing" kind

let get_section_by_kind items kind =
  try snd (List.find (fun (k,_) -> k=kind) (sections items))
  with Not_found -> bad_format "section kind=%S is missing" kind

let assoc_sections items kind parse =
  List.map parse (get_all_section_by_kind items kind)

let assoc_option items n parse =
  try Some (parse (List.assoc n (variables items)))
  with Not_found -> None

let assoc_list items n parse =
  try parse (List.assoc n (variables items))
  with Not_found -> []

let assoc_string_list s n =
  assoc_list s n (parse_list parse_string)

(* transform: "foo" (< "1", > "2") => "foo" (< "1"), "foo" (>"2") *)
let rec parse_constraints name = function
  | [] -> []
  | (Symbol r) :: (String v) :: [] ->
      [ (name, Some (r, v)) ]
  | (Symbol r) :: (String v) :: (Symbol ",") :: t ->
      (name, Some (r, v)) :: parse_constraints name t
  | x -> bad_format "expecting a constraint, got %s" (kinds x)

(* contains only "," *)
let rec parse_and_formula_aux = function
  | []                     -> []
  | [String name]          -> [ (name, None) ]
  | [String name; Group g] -> parse_constraints name g
  | [Group g]              -> parse_and_formula_aux g
  | e1 :: Symbol "," :: e2 -> parse_and_formula_aux [e1] @ parse_and_formula_aux e2
  | x -> bad_format "expecting an AND formula, got %s" (kinds x)

let parse_and_formula = function
  | List l -> parse_and_formula_aux l
  | x      -> bad_format "expecting list, got %s" (kind x)

(* contains only toplevel "|" *)
let rec parse_or_formula_aux = function
  | [f]                    -> [parse_and_formula_aux [f]]
  | e1 :: Symbol "|" :: e2 -> parse_and_formula_aux [e1] :: parse_or_formula_aux e2 
  | x -> bad_format "expecting an OR formula, got %s" (kinds x)

let parse_or_formula = function
  | List l -> parse_or_formula_aux l
  | x      -> bad_format "expecting list, got %s" (kind x)

let make_constraint = function
  | name, None       -> [String name]
  | name, Some (r,v) -> [String name; Group [Symbol r; String v]]

let make_and_formula_aux l =
  let l = List.map make_constraint l in
  List.fold_right (fun elt -> function
    | []   -> elt
    | accu -> accu @ [Symbol ","] @ elt
  ) l []

let make_and_formula l =
  List (make_and_formula_aux l)

let make_or_formula l =
  let l = List.map make_and_formula_aux l in
  let l = List.fold_right (fun elt -> function
    | []   -> elt
    | accu -> [ Group accu ; Symbol "|" ; Group elt ]
  ) l [] in
  List l

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamMisc.OP

type indent_variable = string -> bool

let empty = {
  file_contents = [];
  file_name     = "<none>";
  file_format   = OpamVersion.current;
}

let map fn f =
  let file_contents = List.fold_left (fun accu -> function
      | Section _ as s -> s :: accu
      | Variable(k,v)  ->
        match fn k v with
        | Some (k,v) -> Variable(k,v) :: accu
        | None       -> accu
    ) [] f.file_contents in
  let file_contents = List.rev file_contents in
  { f with file_contents }

let variables items =
  let l = List.fold_left (fun accu -> function
      | Variable (k,v) -> (k,v) :: accu
      | _              -> accu
    ) [] items in
  List.rev l

let sections items =
  let l = List.fold_left (fun accu -> function
      | Section s -> (s.section_kind, s) :: accu
      | _          -> accu
    ) [] items in
  List.rev l

exception Bad_format of string

let bad_format fmt =
  Printf.ksprintf
    (fun str -> raise (Bad_format (Printf.sprintf "Bad format! %s" str)))
    fmt

let names items =
  let tbl = ref OpamMisc.StringMap.empty in
  let add f =
    if OpamMisc.StringMap.mem f !tbl then
      let i = OpamMisc.StringMap.find f !tbl in
      tbl := OpamMisc.StringMap.add f (i+1) (OpamMisc.StringMap.remove f !tbl)
    else
      tbl := OpamMisc.StringMap.add f 1 !tbl in
  let rec aux items =
    List.iter (function
      | Variable (f, _) -> add f
      | Section s       -> aux s.section_items
    ) items in
  aux items;
  !tbl

let invalid_fields items fields =
  let tbl = names items in
  OpamMisc.StringMap.fold (fun f i accu ->
    if List.mem f fields && i = 1 then accu else f :: accu
  ) tbl []

let is_valid items fields =
  invalid_fields items fields = []

let rec kind = function
  | Bool b   -> Printf.sprintf "bool(%b)" b
  | Int i    -> Printf.sprintf "int(%d)" i
  | Ident i  -> Printf.sprintf "ident(%s)" i
  | Symbol s -> Printf.sprintf "symbol(%s)" s
  | String s -> Printf.sprintf "string(%S)" s
  | List l   -> Printf.sprintf "list(%s)" (kinds l)
  | Group g  -> Printf.sprintf "group(%s)" (kinds g)
  | Option(o,l) -> Printf.sprintf "option(%s,%s)" (kind o) (kinds l)

and kinds l =
  Printf.sprintf "{%s}" (String.concat " " (List.map kind l))

let rec string_of_value = function
  | Symbol s
  | Ident s     -> Printf.sprintf "%s" s
  | Int i       -> Printf.sprintf "%d" i
  | Bool b      -> Printf.sprintf "%b" b
  | String s    -> Printf.sprintf "%S" s
  | List l      -> Printf.sprintf "[%s]" (string_of_values l)
  | Group g     -> Printf.sprintf "(%s)" (string_of_values g)
  | Option(v,l) -> Printf.sprintf "%s {%s}" (string_of_value v) (string_of_values l)

and string_of_values l =
  String.concat " " (List.rev (List.rev_map string_of_value l))

let is_list = function
  | List _ -> true
  | _      -> false

(* Base parsing functions *)
let parse_bool = function
  | Bool b -> b
  | x      -> bad_format "Expecting a bool, got %s" (kind x)

let parse_int = function
  | Int i -> i
  | x     -> bad_format "Expecting an int, got %s" (kind x)

let parse_ident = function
  | Ident i -> i
  | x       -> bad_format "Expecting an ident, got %s" (kind x)

let parse_symbol = function
  | Symbol s -> s
  | x        -> bad_format "Expecting a symbol, got %s" (kind x)

let parse_string = function
  | String s -> s
  | x        -> bad_format "Expecting a string, got %s" (kind x)

let parse_list fn = function
  | List s -> List.rev (List.rev_map fn s)
  | x      -> [fn x]

let parse_list_list fn ll = match ll with
  | List l ->
    if List.for_all is_list l then List.rev (List.rev_map fn l)
    else [fn ll]
  | _      -> [fn ll]

let parse_group fn = function
  | Group g -> List.rev (List.rev_map fn g)
  | x       -> bad_format "Expecting a group, got %s" (kind x)

let parse_option f g = function
  | Option (k,l) -> f k, Some (g l)
  | k            -> f k, None

let parse_single_option f g = function
  | Option (k,[v]) -> f k, Some (g v)
  | k              -> f k, None

let parse_string_option f = function
  | Option (k,l) -> parse_string k, Some (f l)
  | k            -> parse_string k, None

let parse_string_list =
  parse_list parse_string

let parse_string_pair_of_list = function
  | [String x; String y] -> (x,y)
  | x                    -> bad_format "Expecting a pair of strings, got %s" (kinds x)

let parse_string_pair = function
  | List [String x; String y] -> (x,y)
  | x                         -> bad_format "Expecting a pair of strings, got %s"(kind x)

let parse_single_string = function
  | [String x] -> x
  | x          -> bad_format "Expecting a single string, got %s" (kinds x)

let parse_pair fa fb = function
  | List [a; b] -> (fa a, fb b)
  | x           -> bad_format "Expecting a pair, got %s" (kind x)

let parse_or fns v =
  let dbg = List.map fst fns in
  let rec aux = function
    | []   ->
      bad_format "Expecting %s, got %s" (String.concat " or " dbg) (kind v)
    | (_,h)::t ->
      try h v
      with Bad_format _ -> aux t in
  aux fns

let parse_sequence fns v =
  let rec aux = function
    | (_,f) :: fns, h :: t -> f h :: aux (fns, t)
    | []          , []     -> []
    | lfns        , l      ->
      bad_format
        "Expecting %s (%s) got %s"
        (String.concat ", " (List.map fst lfns))
        (String.concat ", " (List.map fst fns))
        (string_of_values l) in
  match v with
  | List l -> aux (fns, l)
  | x      -> aux (fns, [x])

let make_string str = String str

let make_ident str = Ident str

let make_symbol str = Symbol str

let make_bool b = Bool b

let make_int i = Int i

let make_list fn l = List (List.rev (List.rev_map fn l))

let make_string_list = make_list make_string

let make_group fn g = Group (List.rev (List.rev_map fn g))

let make_option f g = function
  | (v, None)   -> f v
  | (v, Some o) -> Option (f v, g o)

let make_pair fa fb (k,v) = List [fa k; fb v]

let make_string_pair = make_pair make_string make_string

(* Printing *)

let compute_indent = function
  | []          -> false, []
  | b :: indent -> b    , indent

let pop_indent indent =
  let _, indent = compute_indent indent in
  indent

let can_simplify = function
  | List [ List [_] ]
  | List [ _ ] -> true
  | _ -> false

let rec pretty_string_of_value ~simplify ~depth ~indent =
  let depth = depth + 1 in
  function
  | Symbol s    -> s
  | Ident s     ->
    if !OpamGlobals.compat_mode_1_0 && OpamMisc.contains s ':'
    then Printf.sprintf "\"%%{%s}%%\"" s
    else s
  | Int i       -> Printf.sprintf "%d" i
  | Bool b      -> Printf.sprintf "%b" b
  | String s    ->
    if not !OpamGlobals.compat_mode_1_0
    && OpamMisc.starts_with ~prefix:"%{" s && OpamMisc.ends_with ~suffix:"}%" s then
      String.sub s 2 (String.length s - 4)
    else
      Printf.sprintf "%S" s
  | List[List[]]-> Printf.sprintf "[]"
  | List l      -> pretty_string_of_list ~simplify ~depth ~indent l
  | Group g     -> Printf.sprintf "(%s)"
                     (pretty_string_of_values ~simplify ~depth ~indent " " g)
  | Option(v,l) ->
    Printf.sprintf "%s {%s}"
      (pretty_string_of_value ~simplify ~depth ~indent v)
      (pretty_string_of_values ~simplify ~depth ~indent " " l)

and pretty_string_of_list ~simplify ~depth ~indent =
  let depth = depth + 1 in
  function
  | []                -> "[]"
  | [v] when simplify ->
    pretty_string_of_value ~simplify:false ~depth ~indent:(pop_indent indent) v
  | l                 ->
    let force, indent = compute_indent indent in
    let simplify = false in
    let depth = succ depth in
    if depth < 6 && ((List.length l > 1 && force) || List.for_all is_list l) then
      Printf.sprintf "[\n  %s\n]" (pretty_string_of_values ~simplify ~depth ~indent "\n  " l)
    else
      Printf.sprintf "[%s]" (pretty_string_of_values ~simplify ~depth ~indent " " l)

and pretty_string_of_values ~simplify ~depth ~indent sep l =
  String.concat sep
    (List.rev (List.rev_map (pretty_string_of_value ~simplify ~depth ~indent) l))

let incr tab = "  " ^ tab

let rec string_of_item_aux tab ~simplify ?(indent_variable = fun _ -> false) = function
  | Variable (_, List []) -> None
  | Variable (_, List[List[]]) -> None
  | Variable (i, v) ->
    Some (Printf.sprintf "%s%s: %s" tab i
            (pretty_string_of_value ~simplify ~depth:0 ~indent:[indent_variable i] v))
  | Section s ->
    Some (Printf.sprintf "%s%s %S {\n%s\n}"
        tab s.section_kind s.section_name
        (string_of_items_aux (incr tab) ~simplify ~indent_variable s.section_items))

and string_of_items_aux tab ~simplify ?(indent_variable = fun _ -> false) is =
  String.concat "\n"
    (OpamMisc.filter_map (string_of_item_aux tab ~simplify ~indent_variable) is)

let string_of_item = string_of_item_aux ""
let string_of_items = string_of_items_aux ""

let string_of_file ~simplify ?(indent_variable = fun _ -> false) f =
  let simplify = if !OpamGlobals.compat_mode_1_0 then false else simplify in
  string_of_items f.file_contents ~simplify ~indent_variable ^ "\n"

(* Reading section contents *)

let assoc items n parse =
  try parse (List.assoc n (variables items))
  with Not_found -> bad_format "Field %S is missing" n

let get_all_section_by_kind items kind =
  try List.rev_map snd (List.find_all (fun (k,_) -> k=kind) (sections items))
  with Not_found -> bad_format "Section kind %S is missing" kind

let get_section_by_kind items kind =
  try snd (List.find (fun (k,_) -> k=kind) (sections items))
  with Not_found -> bad_format "Section kind %S is missing" kind

let assoc_sections items kind parse =
  List.rev_map parse (get_all_section_by_kind items kind)

let assoc_option items n parse =
  try Some (parse (List.assoc n (variables items)))
  with Not_found -> None

let assoc_default d items n parse =
  try parse (List.assoc n (variables items))
  with Not_found -> d

let assoc_list items n parse =
  try parse (List.assoc n (variables items))
  with Not_found -> []

let assoc_string_list s n =
  assoc_list s n (parse_list parse_string)

(* Parse any constraint list *)
let rec parse_constraints t =
  let version = OpamPackage.Version.of_string in
  let relop = OpamFormula.relop_of_string in
  match t with
  | []                                            -> Empty
  | (Symbol r) :: (String v) :: []                -> Atom (relop r, version v)
  | (Symbol r) :: (String v) :: (Symbol "&") :: t -> And (Atom (relop r, version v),
                                                          parse_constraints t)
  | (Symbol r) :: (String v) :: (Symbol "|") :: t -> Or (Atom (relop r, version v),
                                                         parse_constraints t)
  | [Group g]                                     -> Block (parse_constraints g)
  | x -> bad_format "Expecting a list of constraints, got %s" (kinds x)

let rec make_constraints t =
  match t with
  | Empty       -> []
  | Atom (r, v) -> [Symbol (OpamFormula.string_of_relop r);
                    String (OpamPackage.Version.to_string v)]
  | And (x, y)  -> make_constraints x @ [Symbol "&"] @ make_constraints y
  | Or (x, y)   -> make_constraints x @ [Symbol "|"] @ make_constraints y
  | Block g     -> [Group (make_constraints g)]

(* parse a list of formulas *)
let parse_formulas opt t =
  let name = OpamPackage.Name.of_string in
  let rec aux = function
    | []                     -> Empty
    | [String n]             -> Atom (name n, Empty)
    | [Option(String n, g)]  -> Atom (name n, parse_constraints g)
    | [Group g]              -> Block (aux g)
    | [x]                    -> bad_format "Expected a formula list of the \
                                            form [ \"item\" {condition}... ], \
                                            got %s" (kind x)
    | e1 :: Symbol "|" :: e2 -> let left = aux [e1] in Or (left, aux e2)
    | e1 :: Symbol "&" :: e2 -> let left = aux [e1] in And (left, aux e2)
    | e1 :: e2 when opt      -> let left = aux [e1] in Or (left, aux e2)
    | e1 :: e2               -> let left = aux [e1] in And (left, aux e2) in
  match t with
  | List l -> aux l
  | x      -> aux [x]

let make_formulas opt t =
  let name = OpamPackage.Name.to_string in
  let rec aux = function
    | Empty             -> []
    | Atom (n, Empty)   -> [String (name n)]
    | Atom (n, cs)      -> [Option(String (name n), make_constraints cs)]
    | Block f           -> [Group (aux f)]
    | And(e,f) when opt -> aux e @ [Symbol "&"] @ aux f
    | And(e,f)          -> aux e @ aux f
    | Or(e,f) when opt  -> aux e @ aux f
    | Or(e,f)           -> aux e @ [Symbol "|"] @ aux f in
  List (aux t)

let make_formula =
  make_formulas false

let parse_formula =
  parse_formulas false

let parse_opt_formula =
  parse_formulas true

let make_opt_formula =
  make_formulas true

let parse_relop = function
  | "="  -> `Eq
  | ">=" -> `Geq
  | ">"  -> `Gt
  | "<=" -> `Leq
  | "<"  -> `Lt
  | "!"  -> `Neq
  | _    -> invalid_arg "parse_relop"

let string_of_relop = function
  | `Eq  -> "="
  | `Geq -> ">="
  | `Gt  -> ">"
  | `Leq -> "<="
  | `Lt  -> "<"
  | `Neq -> "!"
  | _    -> invalid_arg "parse_relop"

let parse_compiler_constraint t =
  let rec aux = function
    | []                         -> Empty
    | [Symbol r; Ident v] when
        v = OpamCompiler.to_string OpamCompiler.system ->
      let version = OpamCompiler.Version.of_string v in
      Atom (parse_relop r, version)
    | [Symbol r; String v]       ->
      (try Atom (parse_relop r, OpamCompiler.Version.of_string v)
       with _ -> bad_format "Expecting a relop, got %s" r)
    | [Group g]                  -> Block (aux g)
    | e1::e2 :: Symbol "|" :: e3 -> Or (aux [e1;e2], aux e3)
    | e1::e2 :: Symbol "&" :: e3 -> And (aux [e1;e2], aux e3)
    | x -> bad_format "Expecting a compiler constraint, got %s" (kinds x) in
  match t with
  | List l -> aux l
  | x      -> aux [x]

let make_compiler_constraint t =
  let rec aux = function
    | Empty       -> []
    | Atom (r, v) ->
      let mk version = [ Symbol (string_of_relop r); version ] in
      let system = OpamCompiler.to_string OpamCompiler.system in
      if OpamCompiler.Version.to_string v = system then
        mk (Ident system)
      else
        mk (String (OpamCompiler.Version.to_string v))
    | Block f     -> [Group (aux f)]
    | And(e,f)    -> aux e @ [Symbol "&"] @ aux f
    | Or(e,f)     -> aux e @ [Symbol "|"] @ aux f in
  List (aux t)

let parse_os_constraint l =
  let pos s = Atom (true, s) in
  let neg s = Atom (false, s) in
  let rec aux = function
    | []                                         -> Empty
    | [Group g]                                  -> Block (aux g)
    | [String os]                                -> pos os
    | [Symbol "!"; String os]                    -> neg os
    | String os :: Symbol "&" :: l               -> And (pos os, aux l)
    | Symbol "!" :: String os :: Symbol "&" :: l -> And (neg os, aux l)
    | String os :: Symbol "|" :: l               -> Or (pos os, aux l)
    | Symbol "!" :: String os :: Symbol "|" :: l -> Or (neg os, aux l)
    | l -> bad_format "Expecting an OS constraint, got %s" (kinds l) in
  match l with
  | List l -> aux l
  | x      -> aux [x]

let make_os_constraint l =
  let rec aux = function
    | Empty            -> []
    | Atom (true , os) -> [String os]
    | Atom (false, os) -> [Symbol "!"; String os]
    | Block g          -> [Group (aux g)]
    | And(e,f)         -> aux e @ [Symbol "&"] @ aux f
    | Or(e,f)          -> aux e @ [Symbol "|"] @ aux f in
  List (aux l)

let parse_env_variable v =
  let l = parse_sequence [
      ("ident" , parse_ident);
      ("symbol", parse_symbol);
      ("string", parse_string);
    ] v in
  match l with
  | [ident; symbol; string] -> (ident, symbol, string)
  | _ -> assert false

let make_env_variable (ident, symbol, string) =
  List [make_ident ident; make_symbol symbol; make_string string]

(* Filters *)

let rec parse_filter = function
  | [Bool b]         -> FBool b
  | [String s]       -> FString s
  | [Ident s]        -> FIdent s
  | [Group g]        -> parse_filter g
  | [Symbol "!"; f]  -> FNot (parse_filter [f])
  | [e; Symbol s; f] ->
    let e = parse_filter [e] in
    let f = parse_filter [f] in
    begin match s with
      | "="  -> FOp(e,Eq,f)
      | "!=" -> FOp(e,Neq,f)
      | ">=" -> FOp(e,Ge,f)
      | "<=" -> FOp(e,Le,f)
      | ">"  -> FOp(e,Gt,f)
      | "<"  -> FOp(e,Lt,f)
      | "||"
      | "|"  -> FOr(e,f)
      | "&&"
      | "&"  -> FAnd(e,f)
      | _    -> bad_format "Got %s, expecting a valid symbol" s
    end;
  | x    -> bad_format "Got %s, expecting a filter expression" (kinds x)

let lift = function
  | [x] -> x
  | l   -> Group l

let rec make_filter = function
  | FString s  -> [String s]
  | FIdent s   -> [Ident s]
  | FBool b    -> [Bool b]
  | FOp(e,s,f) ->
    let s = Symbol (string_of_symbol s) in
    let e = lift (make_filter e) in
    let f = lift (make_filter f) in
    [ e; s; f]
  | FOr(e,f) ->
    let e = lift (make_filter e) in
    let f = lift (make_filter f) in
    [ e; Symbol "|"; f]
  | FAnd(e,f) ->
    let e = lift (make_filter e) in
    let f = lift (make_filter f) in
    [ e; Symbol "&"; f ]
  | FNot f -> [Symbol "!"; lift (make_filter f)]

let make_simple_arg = function
  | CString s -> make_string s
  | CIdent s  -> make_ident s

let make_arg =
  make_option make_simple_arg make_filter

let make_command =
  make_option (make_list make_arg) make_filter

let make_commands =
  make_list make_command

let parse_simple_arg =
  parse_or [
    "ident" , (parse_ident  ++ fun x -> CIdent x);
    "string", (parse_string ++ fun x -> CString x);
  ]

let parse_arg =
  parse_option parse_simple_arg parse_filter

let parse_command =
  parse_option (parse_list parse_arg) parse_filter

let parse_commands =
  parse_or [
    "command"     , (fun x -> [parse_command x]);
    "command-list", parse_list parse_command;
  ]

let parse_message =
  parse_option parse_string parse_filter

let parse_messages =
  parse_list parse_message

(* TAGS *)

let parse_string_set =
  parse_or [
    "string"     , (parse_string ++ OpamMisc.StringSet.singleton);
    "string-list", (parse_string_list ++ OpamMisc.StringSet.of_list);
  ]

let make_string_set s =
  make_list make_string (OpamMisc.StringSet.elements s)

let parse_tag_line =
  let fn = parse_string_set in
  parse_pair fn fn

let make_tag_line =
  let fn = make_string_set in
  make_pair fn fn

let parse_tags v =
  let l =
    parse_or [
      "tagline"     , (fun x -> [parse_tag_line x]);
      "tagline-list", (parse_list parse_tag_line);
    ] v in
  OpamMisc.StringSetMap.of_list l

let make_tags t =
  let l = OpamMisc.StringSetMap.bindings t in
  make_list make_tag_line l

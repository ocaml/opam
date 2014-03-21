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
open OpamTypesBase
open OpamMisc.OP

let empty = {
  file_contents = [];
  file_name     = "<none>";
  file_format   = OpamVersion.current;
}

exception Bad_format of pos option * string

let bad_format ?pos fmt =
  Printf.ksprintf
    (fun str ->
       raise (Bad_format (pos,str)))
    fmt

let add_pos pos = function
  | Bad_format (None,msg) -> Bad_format (Some pos, msg)
  | e -> e

let item_pos = function
  | Section (pos,_) | Variable (pos,_,_) -> pos

let protect_item f item =
  try f item with e ->
    raise (add_pos (item_pos item) e)

let map fn f =
  let file_contents =
    List.fold_left (fun accu ->
        protect_item (function
            | Section _ as s -> s :: accu
            | Variable(pos,k,v)  ->
              match fn k v with
              | Some (k,v) -> Variable(pos,k,v) :: accu
              | None       -> accu
          )) [] f.file_contents in
  let file_contents = List.rev file_contents in
  { f with file_contents }

let variables items =
  let l = List.fold_left (fun accu -> function
      | Variable (_,k,v) -> (k,v) :: accu
      | _              -> accu
    ) [] items in
  List.rev l

let sections items =
  let l = List.fold_left (fun accu -> function
      | Section (_,s) -> (s.section_kind, s) :: accu
      | _          -> accu
    ) [] items in
  List.rev l

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
      | Variable (_, f, _) -> add f
      | Section (_, s)     -> aux s.section_items
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
  | Bool (_,b)   -> Printf.sprintf "bool(%b)" b
  | Int (_,i)    -> Printf.sprintf "int(%d)" i
  | Ident (_,i)  -> Printf.sprintf "ident(%s)" i
  | Symbol (_,s) -> Printf.sprintf "symbol(%s)" s
  | String (_,s) -> Printf.sprintf "string(%S)" s
  | List (_,l)   -> Printf.sprintf "list(%s)" (kinds l)
  | Group (_,g)  -> Printf.sprintf "group(%s)" (kinds g)
  | Option(_,o,l)-> Printf.sprintf "option(%s,%s)" (kind o) (kinds l)

and kinds l =
  Printf.sprintf "{%s}" (String.concat " " (List.map kind l))

let rec string_of_value = function
  | Symbol (_,s)
  | Ident (_,s)     -> Printf.sprintf "%s" s
  | Int (_,i)       -> Printf.sprintf "%d" i
  | Bool (_,b)      -> Printf.sprintf "%b" b
  | String (_,s)    -> Printf.sprintf "%S" s
  | List (_,l)      -> Printf.sprintf "[%s]" (string_of_values l)
  | Group (_,g)     -> Printf.sprintf "(%s)" (string_of_values g)
  | Option(_,v,l)   -> Printf.sprintf "%s {%s}" (string_of_value v) (string_of_values l)

and string_of_values l =
  String.concat " " (List.rev (List.rev_map string_of_value l))

let is_list = function
  | List _ -> true
  | _      -> false

let value_pos = function
  | Bool (pos, _) | Int (pos, _) | String (pos, _) | Symbol (pos, _)
  | Ident (pos, _) | List (pos, _) | Group (pos, _) | Option (pos, _, _)
    -> pos

let protect_value f value =
  try f value with e ->
    raise (add_pos (value_pos value) e)

let values_pos = function
  | [] -> None
  | x::_ -> Some (value_pos x)

let protect_values f values =
  try f values with e ->
    match values_pos values with
    | None -> raise e
    | Some pos -> raise (add_pos pos e)

(* Base parsing functions *)
let parse_bool = function
  | Bool (_,b) -> b
  | x      -> bad_format ~pos:(value_pos x) "Expected a bool"

let parse_int = function
  | Int (_,i) -> i
  | x     -> bad_format ~pos:(value_pos x) "Expected an int"

let parse_ident = function
  | Ident (_,i) -> i
  | x       -> bad_format ~pos:(value_pos x) "Expected an ident"

let parse_symbol = function
  | Symbol (_,s) -> s
  | x        -> bad_format ~pos:(value_pos x) "Expected a symbol"

let parse_string = function
  | String (_,s) -> s
  | x        -> bad_format ~pos:(value_pos x) "Expected a string"

let parse_list fn =
  let fn = protect_value fn in
  function
  | List (_,s) -> List.rev (List.rev_map fn s)
  | x      -> [fn x]

let parse_list_list fn ll =
  let fn = protect_value fn in
  match ll with
  | List (_,l) ->
    if List.for_all is_list l then List.rev (List.rev_map fn l)
    else [fn ll]
  | _      -> [fn ll]

let parse_group fn =
  let fn = protect_value fn in
  function
  | Group (_,g) -> List.rev (List.rev_map fn g)
  | x       -> bad_format ~pos:(value_pos x) "Expected a group"

let parse_option f g =
  let f = protect_value f in
  let g = protect_values g in
  function
  | Option (_,k,l) -> f k, Some (g l)
  | k            -> f k, None

let parse_single_option f g =
  let f = protect_value f in
  let g = protect_value g in
  function
  | Option (_,k,[v]) -> f k, Some (g v)
  | k              -> f k, None

let parse_string_option f =
  let f = protect_values f in
  function
  | Option (_,k,l) -> parse_string k, Some (f l)
  | k            -> parse_string k, None

let parse_string_list =
  parse_list parse_string

let parse_string_pair_of_list = function
  | [String (_,x); String (_,y)] -> (x,y)
  | x                    ->
    bad_format ?pos:(values_pos x) "Expected a pair of strings"

let parse_string_pair = function
  | List (_,[String (_,x); String (_,y)]) -> (x,y)
  | x                         ->
    bad_format ~pos:(value_pos x) "Expected a pair of strings"

let parse_single_string = function
  | [String (_,x)] -> x
  | x          ->
    bad_format ?pos:(values_pos x) "Expected a single string"

let parse_pair fa fb =
  let fa = protect_value fa in
  let fb = protect_value fb in
  function
  | List (_,[a; b]) -> (fa a, fb b)
  | x           ->
    bad_format ~pos:(value_pos x) "Expected a pair"

let parse_or fns v =
  let dbg = List.map fst fns in
  let rec aux = function
    | []   ->
      bad_format ~pos:(value_pos v)
        "Expected %s" (String.concat " or " dbg)
    | (_,h)::t ->
      try h v
      with Bad_format _ -> aux t in
  aux fns

let parse_sequence fns v =
  let fns = List.map (fun (a,f) -> a, protect_value f) fns in
  let rec aux = function
    | (_,f) :: fns, h :: t -> f h :: aux (fns, t)
    | []          , []     -> []
    | lfns        , l      ->
      bad_format ?pos:(values_pos l)
        "Expected %s (%s)"
        (String.concat ", " (List.map fst lfns))
        (String.concat ", " (List.map fst fns)) in
  match v with
  | List (_,l) -> aux (fns, l)
  | x      -> aux (fns, [x])

let make_string str = String (pos_null,str)

let make_ident str = Ident (pos_null,str)

let make_symbol str = Symbol (pos_null,str)

let make_bool b = Bool (pos_null,b)

let make_int i = Int (pos_null,i)

let make_list fn l = List (pos_null, List.rev (List.rev_map fn l))

let make_string_list = make_list make_string

let make_group fn g = Group (pos_null, List.rev (List.rev_map fn g))

let make_option f g = function
  | (v, None)   -> f v
  | (v, Some o) -> Option (pos_null, f v, g o)

let make_pair fa fb (k,v) = List (pos_null, [fa k; fb v])

let make_string_pair = make_pair make_string make_string

(* Printing *)

let rec pretty_string_of_value depth ~simplify ~indent = function
  | Symbol (_,s)    -> s
  | Ident (_,s)     ->
    if !OpamGlobals.compat_mode_1_0 && OpamMisc.contains s ':'
    then Printf.sprintf "\"%%{%s}%%\"" s
    else s
  | Int (_,i)       -> Printf.sprintf "%d" i
  | Bool (_,b)      -> Printf.sprintf "%b" b
  | String (_,s)    ->
    if not !OpamGlobals.compat_mode_1_0
    && OpamMisc.starts_with ~prefix:"%{" s && OpamMisc.ends_with ~suffix:"}%" s then
      String.sub s 2 (String.length s - 4)
    else
      Printf.sprintf "%S" s
  | List (_,[List(_,[])]) -> Printf.sprintf "[]"
  | List (_,l)      -> pretty_string_of_list depth ~simplify ~indent l
  | Group (_,g)     -> Printf.sprintf "(%s)"
                     (pretty_string_of_values (depth+1) ~simplify ~indent g)
  | Option(_,v,l) ->
    Printf.sprintf "%s {%s}"
      (pretty_string_of_value depth ~simplify ~indent v)
      (pretty_string_of_values depth ~simplify ~indent l)

and pretty_string_of_list depth ~simplify ~indent = function
  | []                             -> "[]"
  | [v] when depth = 0 && simplify -> pretty_string_of_value (depth+1) ~simplify ~indent v
  | l                              ->
    if depth = 0 && indent && List.length l > 1 then
      Printf.sprintf "[\n  %s\n]" (pretty_string_of_values depth ~simplify ~indent l)
    else
      Printf.sprintf "[%s]" (pretty_string_of_values depth ~simplify ~indent l)

and pretty_string_of_values depth ~simplify ~indent l =
  let sep = if depth = 0 && indent then "\n  " else " " in
  String.concat sep
    (List.rev (List.rev_map (pretty_string_of_value (depth+1) ~simplify ~indent) l))

let incr tab = "  " ^ tab

let rec string_of_item_aux tab ~simplify ~indent ~ignore = function
  | Variable (_, _, List (_,[]))      -> None
  | Variable (_, _, List (_,[List(_,[])])) -> None
  | Variable (_, i, v)            ->
    let return ~simplify ~indent =
      Some (Printf.sprintf "%s%s: %s" tab i (pretty_string_of_value 0 ~simplify ~indent v)) in
    if List.mem i ignore then
      return ~simplify:false ~indent:false
    else
      return ~simplify ~indent
  | Section (_,s)                 ->
    Some (Printf.sprintf "%s%s %S {\n%s\n}"
            tab s.section_kind s.section_name
            (string_of_items_aux (incr tab) ~simplify ~indent ~ignore s.section_items))

and string_of_items_aux tab ~simplify ~indent ~ignore is =
  String.concat "\n"
    (OpamMisc.filter_map (string_of_item_aux tab ~simplify ~indent ~ignore) is)

let string_of_item = string_of_item_aux ""
let string_of_items = string_of_items_aux ""

let string_of_file ~simplify ~indent ?(ignore=[]) f =
  let simplify = not !OpamGlobals.compat_mode_1_0 && simplify in
  string_of_items f.file_contents ~simplify ~indent ~ignore ^ "\n"

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

let make_section s = Section (pos_null, s)

let make_variable (k, v) = Variable (pos_null, k, v)

(* Parse any constraint list *)
let rec parse_constraints t =
  let version = OpamPackage.Version.of_string in
  let relop = OpamFormula.relop_of_string in
  match t with
  | []                                            -> Empty
  | (Symbol (_,r)) :: (String (_,v)) :: []                -> Atom (relop r, version v)
  | (Symbol (_,r)) :: (String (_,v)) :: (Symbol (_,"&")) :: t ->
    And (Atom (relop r, version v), parse_constraints t)
  | (Symbol (_,r)) :: (String (_,v)) :: (Symbol (_,"|")) :: t ->
    Or (Atom (relop r, version v),  parse_constraints t)
  | (Group (_,g)) :: (Symbol (_,"&")) :: t                ->
    And (Block (parse_constraints g), parse_constraints t)
  | (Group (_,g)) :: (Symbol (_,"|")) :: t                ->
    Or (Block (parse_constraints g), parse_constraints t)
  | [Group (_,g)]                                     -> Block (parse_constraints g)
  | x -> bad_format ?pos:(values_pos x)
           "Expected a list of constraints"

let rec make_constraints t =
  match t with
  | Empty       -> []
  | Atom (r, v) -> [Symbol (pos_null,OpamFormula.string_of_relop r);
                    String (pos_null,OpamPackage.Version.to_string v)]
  | And (x, y)  -> make_constraints x @ [make_symbol "&"] @ make_constraints y
  | Or (x, y)   -> make_constraints x @ [make_symbol "|"] @ make_constraints y
  | Block g     -> [Group (pos_null,make_constraints g)]

(* parse a list of formulas *)
let parse_formulas opt t =
  let name = OpamPackage.Name.of_string in
  let rec aux = function
    | []                     -> Empty
    | [String (_,n)]             -> Atom (name n, Empty)
    | [Option(_, String (_,n), g)]  -> Atom (name n, parse_constraints g)
    | [Group (_,g)]              -> Block (aux g)
    | [x]                    ->
      bad_format ~pos:(value_pos x)
        "Expected a formula list of the \
         form [ \"item\" {condition}... ]"
    | e1 :: Symbol (_,"|") :: e2 -> let left = aux [e1] in Or (left, aux e2)
    | e1 :: Symbol (_,"&") :: e2 -> let left = aux [e1] in And (left, aux e2)
    | e1 :: e2 when opt      -> let left = aux [e1] in Or (left, aux e2)
    | e1 :: e2               -> let left = aux [e1] in And (left, aux e2) in
  match t with
  | List (_,l) -> aux l
  | x      -> aux [x]

let make_formulas opt t =
  let name = OpamPackage.Name.to_string in
  let rec aux = function
    | Empty             -> []
    | Atom (n, Empty)   -> [make_string (name n)]
    | Atom (n, cs)      -> [Option (pos_null, make_string (name n), make_constraints cs)]
    | Block f           -> [Group (pos_null,aux f)]
    | And(e,f) when opt -> aux e @ [make_symbol "&"] @ aux f
    | And(e,f)          -> aux e @ aux f
    | Or(e,f) when opt  -> aux e @ aux f
    | Or(e,f)           -> aux e @ [make_symbol "|"] @ aux f in
  List (pos_null, aux t)

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
    | [Symbol (_,r); Ident (_,v)] when
        v = OpamCompiler.to_string OpamCompiler.system ->
      let version = OpamCompiler.Version.of_string v in
      Atom (parse_relop r, version)
    | [Symbol (pos,r); String (_,v)]       ->
      (try Atom (parse_relop r, OpamCompiler.Version.of_string v)
       with Invalid_argument _ -> bad_format ~pos "Expected a relop")
    | [Group (_,g)]                  -> Block (aux g)
    | e1::e2 :: Symbol (_,"|") :: e3 -> Or (aux [e1;e2], aux e3)
    | e1::e2 :: Symbol (_,"&") :: e3 -> And (aux [e1;e2], aux e3)
    | x -> bad_format ?pos:(values_pos x)
             "Expected a compiler constraint" in
  match t with
  | List (_,l) -> aux l
  | x      -> aux [x]

let make_compiler_constraint t =
  let rec aux = function
    | Empty       -> []
    | Atom (r, v) ->
      let mk version = [ make_symbol (string_of_relop r); version ] in
      let system = OpamCompiler.to_string OpamCompiler.system in
      if OpamCompiler.Version.to_string v = system then
        mk (make_ident system)
      else
        mk (make_string (OpamCompiler.Version.to_string v))
    | Block f     -> [Group (pos_null,aux f)]
    | And(e,f)    -> aux e @ [make_symbol "&"] @ aux f
    | Or(e,f)     -> aux e @ [make_symbol "|"] @ aux f in
  List (pos_null, aux t)

let parse_os_constraint l =
  let pos s = Atom (true, s) in
  let neg s = Atom (false, s) in
  let rec aux = function
    | []                                         -> Empty
    | [Group (_,g)]                                  -> Block (aux g)
    | [String (_,os)]                                -> pos os
    | [Symbol (_,"!"); String (_,os)]                    -> neg os
    | String (_,os) :: Symbol (_,"&") :: l               -> And (pos os, aux l)
    | Symbol (_,"!") :: String (_,os) :: Symbol (_,"&") :: l -> And (neg os, aux l)
    | String (_,os) :: Symbol (_,"|") :: l               -> Or (pos os, aux l)
    | Symbol (_,"!") :: String (_,os) :: Symbol (_,"|") :: l -> Or (neg os, aux l)
    | l -> bad_format ?pos:(values_pos l)
             "Expected an OS constraint" in
  match l with
  | List (_,l) -> aux l
  | x      -> aux [x]

let make_os_constraint l =
  let rec aux = function
    | Empty            -> []
    | Atom (true , os) -> [make_string os]
    | Atom (false, os) -> [make_symbol "!"; make_string os]
    | Block g          -> [Group (pos_null,aux g)]
    | And(e,f)         -> aux e @ [make_symbol "&"] @ aux f
    | Or(e,f)          -> aux e @ [make_symbol "|"] @ aux f in
  List (pos_null, aux l)

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
  List (pos_null,[make_ident ident; make_symbol symbol; make_string string])

(* Filters *)

let rec parse_filter = function
  | []                   -> FBool true
  | [Bool (_,b)]         -> FBool b
  | [String (_,s)]       -> FString s
  | [Ident (_,s)]        -> FIdent s
  | [Group (_,g)]        -> parse_filter g
  | [Symbol (_,"!"); f]  -> FNot (parse_filter [f])
  | [e; Symbol (pos,s); f] ->
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
      | _    -> bad_format ~pos "Expected a valid symbol" s
    end;
  | x -> bad_format ?pos:(values_pos x)
           "Expected a filter expression"

let lift = function
  | [x] -> x
  | l   ->
    let pos = match values_pos l with Some p -> p | None -> pos_null in
    Group (pos, l)

let rec make_filter = function
  | FString s  -> [make_string s]
  | FIdent s   -> [make_ident s]
  | FBool b    -> [make_bool b]
  | FOp(e,s,f) ->
    let s = make_symbol (string_of_symbol s) in
    let e = lift (make_filter e) in
    let f = lift (make_filter f) in
    [ e; s; f]
  | FOr(e,f) ->
    let e = lift (make_filter e) in
    let f = lift (make_filter f) in
    [ e; make_symbol "|"; f]
  | FAnd(e,f) ->
    let e = lift (make_filter e) in
    let f = lift (make_filter f) in
    [ e; make_symbol "&"; f ]
  | FNot f -> [make_symbol "!"; lift (make_filter f)]

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
    "ident" , (parse_ident  @> fun x -> CIdent x);
    "string", (parse_string @> fun x -> CString x);
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

let make_flag = function
  | LightUninstall -> make_symbol "light-uninstall"
  | BuildDep -> make_symbol "build-dep"

let parse_flag = function
  | Ident (_,"light-uninstall") -> LightUninstall
  | Ident (_,"build-dep") -> BuildDep
  | x ->
    bad_format ~pos:(value_pos x)
      "Expected a package flag"

(* TAGS *)

let parse_string_set =
  parse_string_list @> OpamMisc.StringSet.of_list

let make_string_set =
  OpamMisc.StringSet.elements @> make_string_list

let parse_tag_line =
  let fn = parse_string_set in
  parse_pair fn fn

let make_tag_line =
  let fn = make_string_set in
  make_pair fn fn

let parse_tags v =
  let l = parse_or [
      "tag" , (fun x -> [parse_tag_line x]);
      "tags", (parse_list parse_tag_line);
    ] v in
  OpamMisc.StringSetMap.of_list l

let make_tags t =
  let l = OpamMisc.StringSetMap.bindings t in
  make_list make_tag_line l

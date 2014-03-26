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

exception Bad_format of pos option * string list * string

let bad_format ?pos fmt =
  Printf.ksprintf
    (fun str ->
       raise (Bad_format (pos,[],str)))
    fmt

let add_pos pos = function
  | Bad_format (None,btl,msg) ->
    let backtrace = Printexc.get_backtrace () in
    Bad_format (Some pos, backtrace::btl, msg)
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

let rec string_of_value = function
  | Relop (_,op,l,r) ->
    Printf.sprintf "%s %s %s"
      (string_of_value l) (string_of_relop op) (string_of_value r)
  | Logop (_,op,l,r) ->
    Printf.sprintf "%s %s %s"
      (string_of_value l) (string_of_logop op) (string_of_value r)
  | Pfxop (_,op,r) ->
    Printf.sprintf "%s %s" (string_of_pfxop op) (string_of_value r)
  | Prefix_relop (_,op,r) ->
    Printf.sprintf "%s %s"
      (string_of_relop op) (string_of_value r)
  | Ident (_,s)     -> Printf.sprintf "%s" s
  | Int (_,i)       -> Printf.sprintf "%d" i
  | Bool (_,b)      -> Printf.sprintf "%b" b
  | String (_,s)    -> Printf.sprintf "%S" s
  | List (_,l)      -> Printf.sprintf "[%s]" (string_of_values l)
  | Group (_,g)     -> Printf.sprintf "(%s)" (string_of_values g)
  | Option(_,v,l)   -> Printf.sprintf "%s {%s}" (string_of_value v) (string_of_values l)
  | Env_binding (_,op,id,v) ->
    Printf.sprintf "[ %s %s %s ]" (string_of_value id) op (string_of_value v)

and string_of_values l =
  String.concat " " (List.rev (List.rev_map string_of_value l))

let is_list = function
  | List _ -> true
  | _      -> false

let value_pos = function
  | Bool (pos, _) | Int (pos, _) | String (pos, _)
  | Logop (pos, _, _, _) | Pfxop (pos, _, _)
  | Relop (pos, _, _, _) | Prefix_relop (pos, _, _)
  | Ident (pos, _) | List (pos, _) | Group (pos, _) | Option (pos, _, _)
  | Env_binding (pos, _, _, _)
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

let make_string str = String (pos_null,str)

let make_ident str = Ident (pos_null,str)

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

let rec pretty_string_of_value depth ~simplify ~indent value =
  let psov =
    pretty_string_of_value (depth + 1) ~simplify ~indent in
  match value with
  | Logop (_,op,l,r) ->
    Printf.sprintf "%s %s %s" (psov l) (string_of_logop op) (psov r)
  | Pfxop (_,op,r) ->
    Printf.sprintf "%s %s" (string_of_pfxop op) (psov r)
  | Relop (_,op,l,r) ->
    Printf.sprintf "%s %s %s" (psov l) (string_of_relop op) (psov r)
  | Prefix_relop (_,op,r) ->
    Printf.sprintf "%s %s" (string_of_relop op) (psov r)
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
  | Env_binding (_,op,l,r) ->
    Printf.sprintf "[ %s %s %s ]" (psov l) op (psov r)

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
  let rec aux = function
    | Prefix_relop (_, op, (String (_,v))) ->
      Atom (op, OpamPackage.Version.of_string v)
    | Logop (_, `And, l, r) ->
      And (aux l, aux r)
    | Logop (_, `Or, l, r) ->
      Or (aux l, aux r)
    | Pfxop (_,`Not,v) ->
      OpamFormula.neg (fun (op, s) -> (OpamFormula.neg_relop op, s)) (aux v)
    | Group (_, g) ->
      Block (parse_constraints g)
    | x -> bad_format ~pos:(value_pos x) "Expected a list of constraints"
  in
  OpamFormula.ands (List.map aux t)

let lift_list = function
  | List (_, l) -> l
  | x -> [x]

let rec make_constraints t =
  let rec aux = function
    | Empty       -> assert false
    | Atom (r, v) -> Prefix_relop (pos_null, r,
                                   make_string (OpamPackage.Version.to_string v))
    | And (x, y)  -> Logop (pos_null, `And, aux x, aux y)
    | Or (x, y)   -> Logop (pos_null, `Or, aux x, aux y)
    | Block g     -> Group (pos_null, make_constraints g)
  in
  match t with
  | Empty -> []
  | t -> [aux t]

(* parse a list of formulas *)
let rec parse_formulas opt t =
  let name = OpamPackage.Name.of_string in
  let rec aux = function
    | String (_,n) -> Atom (name n, Empty)
    | Option (_, String (_,n), g) -> Atom (name n, parse_constraints g)
    | Group (_,g) -> Block (parse_formulas opt (List (pos_null, g)))
    | Logop (_, `Or, e1, e2) -> let left = aux e1 in Or (left, aux e2)
    | Logop (_, `And, e1, e2) -> let left = aux e1 in And (left, aux e2)
    | x ->
      bad_format ~pos:(value_pos x)
        "Expected a formula list of the form [ \"item\" {condition}... ]"
  in
  OpamFormula.(if opt then ors else ands)
    (List.map aux (lift_list t))

let rec make_formulas opt t =
  let name = OpamPackage.Name.to_string in
  let rec aux = function
    | Empty             -> assert false
    | Atom (n, Empty)   -> make_string (name n)
    | Atom (n, cs)      -> Option (pos_null, make_string (name n), make_constraints cs)
    | Block f           -> Group (pos_null, lift_list (make_formulas opt f))
    | And(e,f)          -> Logop (pos_null, `And, aux e, aux f)
    | Or(e,f)           -> Logop (pos_null, `Or, aux e, aux f)
  in
  let to_list = OpamFormula.(if opt then ors_to_list else ands_to_list) in
  List (pos_null, List.map aux (to_list t))

let make_formula =
  make_formulas false

let parse_formula =
  parse_formulas false

let parse_opt_formula =
  parse_formulas true

let make_opt_formula =
  make_formulas true

let rec parse_compiler_constraint t =
  let rec aux = function
    | Prefix_relop (_, op, Ident (_,v))
      when v = OpamCompiler.to_string OpamCompiler.system ->
      Atom (op, OpamCompiler.Version.of_string v)
    | Prefix_relop (_, op, String (_,v)) ->
      Atom (op, OpamCompiler.Version.of_string v)
    | Group (_, g) -> Block (parse_compiler_constraint (List (pos_null,g)))
    | Logop (_, `Or, e1, e2) -> Or (aux e1, aux e2)
    | Logop (_, `And, e1, e2) -> And (aux e1, aux e2)
    | Pfxop (_,`Not,v) ->
      OpamFormula.neg (fun (op, s) -> (OpamFormula.neg_relop op, s)) (aux v)
    | x -> bad_format ~pos:(value_pos x)
             "Expected a compiler constraint" in
  OpamFormula.ors (List.map aux (lift_list t))

let rec make_compiler_constraint t =
  let rec aux = function
    | Empty -> assert false
    | Atom (op, v) ->
      let system = OpamCompiler.to_string OpamCompiler.system in
      let v =
        if OpamCompiler.Version.to_string v = system then make_ident system
        else make_string (OpamCompiler.Version.to_string v) in
      Prefix_relop (pos_null,op,v)
    | Block f     -> Group (pos_null, lift_list (make_compiler_constraint f))
    | And(e,f)    -> Logop (pos_null, `And, aux e,  aux f)
    | Or(e,f)     -> Logop (pos_null, `Or, aux e,  aux f) in
  match t with
  | Empty -> List (pos_null, [])
  | t -> List (pos_null, List.map aux (OpamFormula.ors_to_list t))

let rec parse_os_constraint l =
  let rec aux = function
    | Group (_,g) -> Block (parse_os_constraint (List (pos_null,g)))
    | String (_,os) -> Atom (true, os)
    | Logop (_,`And,l,r) -> And (aux l, aux r)
    | Logop (_,`Or,l,r) -> Or (aux l, aux r)
    | Pfxop (_,`Not,v) ->
      OpamFormula.neg (fun (b, s) -> (not b, s)) (aux v)
    | x -> bad_format ~pos:(value_pos x) "Expected an OS constraint" in
  OpamFormula.ors (List.map aux (lift_list l))

let rec make_os_constraint l =
  let rec aux = function
    | Empty            -> assert false
    | Atom (true , os) -> make_string os
    | Atom (false, os) -> Pfxop (pos_null, `Not, make_string os)
    | Block g          -> Group (pos_null, lift_list (make_os_constraint g))
    | And(e,f)         -> Logop (pos_null, `And, aux e, aux f)
    | Or(e,f)          -> Logop (pos_null, `Or, aux e, aux f) in
  match l with
  | Empty -> List (pos_null, [])
  | l -> List (pos_null, List.map aux (OpamFormula.ors_to_list l))

let parse_env_variable l =
  let aux = function
    | Relop (_, `Eq, Ident (_,i), String (_,s)) -> i, "=", s
    | Env_binding (_, op, Ident (_,i), String (_,s)) -> i, op, s
    | x -> bad_format ~pos:(value_pos x) "Expected an \"ident = string\" binding"
  in
  match l with List (_,[x]) | x -> aux x

let make_env_variable (ident, op, string) =
  Env_binding (pos_null, op, make_ident ident, make_string string)

(* Filters *)

let rec parse_filter l =
  let rec aux = function
    | Bool (_,b)         -> FBool b
    | String (_,s)       -> FString s
    | Ident (_,s)        -> FIdent s
    | Group (_,g)        -> parse_filter g
    | Relop (_,op,e,f) -> FOp (aux e, op, aux f)
    | Pfxop (_,`Not,e) -> FNot (aux e)
    | Logop(_,`And,e,f)-> FAnd (aux e, aux f)
    | Logop(_,`Or, e,f)-> FOr (aux e, aux f)
    | x -> bad_format ~pos:(value_pos x) "Expected a filter expression"
  in
  match l with
  | [] -> FBool true
  | [f] -> aux f
  | x::_ ->
    bad_format ~pos:(value_pos x) "Expected a single filter expression"

let lift = function
  | [x] -> x
  | l   ->
    let pos = match values_pos l with Some p -> p | None -> pos_null in
    Group (pos, l)

let make_filter f =
  let rec aux = function
    | FString s  -> make_string s
    | FIdent s   -> make_ident s
    | FBool b    -> make_bool b
    | FOp(e,s,f) -> Relop (pos_null, s, aux e, aux f)
    | FOr(e,f) -> Logop (pos_null, `Or, aux e, aux f)
    | FAnd(e,f) -> Logop (pos_null, `And, aux e, aux f)
    | FNot f -> Pfxop (pos_null, `Not, aux f)
  in
  [aux f]

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
  | LightUninstall -> make_ident "light-uninstall"
  | BuildDep -> make_ident "build-dep"

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

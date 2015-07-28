(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
open OpamStd.Op

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

let string_of_backtrace_list = function
  | [] | _ when not (Printexc.backtrace_status ()) -> ""
  | btl -> List.fold_left (fun s bts ->
      let bt_lines = OpamStd.String.split bts '\n' in
      "\n  Backtrace:\n    "^(String.concat "\n    " bt_lines)^s
    ) "" btl

let string_of_bad_format ?file e =
  match e, file with
  | Bad_format (Some pos, btl, msg), _ ->
    Printf.sprintf "At %s:\n  %s%s"
      (string_of_pos pos) msg (string_of_backtrace_list btl)
  | Bad_format (None, btl, msg), Some f ->
    Printf.sprintf "In %s:\n  %s%s"
      (OpamFilename.to_string f) msg (string_of_backtrace_list btl)
  | Bad_format (None, btl, msg), None ->
    Printf.sprintf "Input error:\n  %s%s"
      msg (string_of_backtrace_list btl)
  | _ -> ""

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
  let tbl = ref OpamStd.String.Map.empty in
  let add f =
    if OpamStd.String.Map.mem f !tbl then
      let i = OpamStd.String.Map.find f !tbl in
      tbl := OpamStd.String.Map.add f (i+1) (OpamStd.String.Map.remove f !tbl)
    else
      tbl := OpamStd.String.Map.add f 1 !tbl in
  let rec aux items =
    List.iter (function
      | Variable (_, f, _) -> add f
      | Section (_, s)     -> aux s.section_items
    ) items in
  aux items;
  !tbl

let invalid_fields ?(allow_extensions=false) items fields =
  let tbl = names items in
  OpamStd.String.Map.fold (fun f i accu ->
    if (List.mem f fields ||
        allow_extensions && OpamStd.String.starts_with ~prefix:"x-" f)
       && i = 1
    then accu
    else f :: accu
  ) tbl []

let is_valid ?allow_extensions items fields =
  invalid_fields ?allow_extensions items fields = []

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
  let rec aux = function
    | []   ->
      bad_format ~pos:(value_pos v)
        "Expected %s" (OpamStd.List.concat_map " or " fst fns)
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

let escape_string s =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len -1 do
    match s.[i] with
    | '\\' | '"' as c -> Buffer.add_char buf '\\'; Buffer.add_char buf c
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let rec format_value fmt = function
  | Relop (_,op,l,r) ->
    Format.fprintf fmt "@[<h>%a %s@ %a@]"
      format_value l (string_of_relop op) format_value r
  | Logop (_,op,l,r) ->
    Format.fprintf fmt "@[<hv>%a %s@ %a@]"
      format_value l (string_of_logop op) format_value r
  | Pfxop (_,op,r) ->
    Format.fprintf fmt "@[<h>%s%a@]" (string_of_pfxop op) format_value r
  | Prefix_relop (_,op,r) ->
    Format.fprintf fmt "@[<h>%s@ %a@]"
      (string_of_relop op) format_value r
  | Ident (_,s)     -> Format.fprintf fmt "%s" s
  | Int (_,i)       -> Format.fprintf fmt "%d" i
  | Bool (_,b)      -> Format.fprintf fmt "%b" b
  | String (_,s)    ->
    if String.contains s '\n'
    then Format.fprintf fmt "@[<h>\"\n%s@\n\"@]" (escape_string s)
    else Format.fprintf fmt "\"%s\"" (escape_string s)
  | List (_, l) ->
    Format.fprintf fmt "@[<hv>[@;<0 2>@[<hv>%a@]@,]@]" format_values l
  | Group (_,g)     -> Format.fprintf fmt "@[<hv>(%a)@]" format_values g
  | Option(_,v,l)   -> Format.fprintf fmt "@[<hv 2>%a@ {@[<hv>%a@]}@]"
                         format_value v format_values l
  | Env_binding (_,op,id,v) ->
    Format.fprintf fmt "@[<h>[ %a %s@ %a ]@]" format_value id op format_value v

and format_values fmt = function
  | [] -> ()
  | [v] -> format_value fmt v
  | v::r ->
    format_value fmt v;
    Format.pp_print_space fmt ();
    format_values fmt r

let string_of_value v =
  format_value Format.str_formatter v; Format.flush_str_formatter ()
let string_of_values vs =
  format_values Format.str_formatter vs; Format.flush_str_formatter ()

let rec format_item fmt = function
  | Variable (_, _, List (_,[])) -> ()
  | Variable (_, _, List (_,[List(_,[])])) -> ()
  | Variable (_, i, List (_,l)) ->
    if List.exists (function List _ | Option (_,_,_::_) -> true | _ -> false) l
    then Format.fprintf fmt "@[<v>%s: [@;<0 2>@[<v>%a@]@,]@]" i format_values l
    else Format.fprintf fmt "@[<hv>%s: [@;<0 2>@[<hv>%a@]@,]@]" i format_values l
  | Variable (_, i, v) ->
    Format.fprintf fmt "@[<hov 2>%s:@ %a@]" i format_value v
  | Section (_,s) ->
    Format.fprintf fmt "%s \"%s\" {@[<v 2>%a@]}"
      s.section_kind
      (escape_string s.section_name)
      format_items s.section_items
and format_items fmt is =
  Format.pp_open_vbox fmt 0;
  List.iter (fun i -> format_item fmt i; Format.pp_print_cut fmt ()) is;
  Format.pp_close_box fmt ()

let string_of_items l =
  format_items Format.str_formatter l; Format.flush_str_formatter ()

let rec simplify_items items =
  List.map (function
      | Variable
          (pos, name, List
             (_, [(String _ | List _) as v])) -> Variable (pos, name, v)
      | Section (pos, s) ->
        Section (pos, {s with section_items = simplify_items s.section_items})
      | i -> i)
    items

let string_of_file ~simplify f =
  let items =
    if simplify then simplify_items f.file_contents
    else f.file_contents
  in
  string_of_items items

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

let parse_dep_flag = function
  | Ident (_, "build") -> Depflag_Build
  | Ident (_, "test") -> Depflag_Test
  | Ident (_, "doc") -> Depflag_Doc
  | Ident (_, "dev") -> Depflag_Dev
  | Ident (_, s) -> Depflag_Unknown s
  | x ->
    bad_format ~pos:(value_pos x)
      "Invalid dependency flag %s, must be an ident"
      (string_of_value x)

let make_dep_flag = function
  | Depflag_Build -> make_ident "build"
  | Depflag_Test -> make_ident "test"
  | Depflag_Doc -> make_ident "doc"
  | Depflag_Dev -> make_ident "dev"
  | Depflag_Unknown s -> make_ident s

(* Version constraints with additional leading keywords ("build","test"...) *)
let rec parse_ext_constraints = function
  | Ident (_, _) as kw :: r ->
    let kws, f = parse_ext_constraints r in
    parse_dep_flag kw :: kws, f
  | Logop (_, `And, t1, t2) :: r -> parse_ext_constraints (t1::t2::r)
  | t ->
    [], parse_constraints t

let make_ext_constraints (kws, t) =
  (* The kws must be aggregated with an '&' to the first constraint, if any *)
  match make_constraints t, kws with
  | [], [] -> []
  | [], kw::kws ->
    [List.fold_left (fun acc kw ->
         Logop (pos_null, `And, make_dep_flag kw, acc))
        (make_dep_flag kw) kws]
  | c::cs, kws ->
    List.fold_left (fun acc kw ->
        Logop (pos_null, `And, make_dep_flag kw, acc))
      c kws
    :: cs

let parse_package_name ?expected = function
  | String (pos,n) ->
    let name =
      (try OpamPackage.Name.of_string n with
       | Failure _ -> bad_format ~pos "Invalid package name %s" n)
    in
    (match expected with
     | Some exp when name <> exp ->
       bad_format ~pos "Unexpected name %s"
         (OpamPackage.Name.to_string name)
     | _ -> name)
  | x -> bad_format ~pos:(value_pos x) "Expected a package name"

let parse_package_version ?expected = function
  | String (pos,n) ->
    let version = OpamPackage.Version.of_string n in
    (match expected with
     | Some exp when version <> exp ->
       bad_format ~pos "Unexpected version %s"
         (OpamPackage.Version.to_string version)
     | _ -> version)
  | x -> bad_format ~pos:(value_pos x) "Expected a package version"

(* parse a list of formulas *)
let rec parse_formulas join ~constraints t =
  let rec aux = function
    | String _ as t -> Atom (parse_package_name t, constraints [])
    | Option (_, t, g) -> Atom (parse_package_name t, constraints g)
    | Group (_,g) -> Block (parse_formulas join ~constraints (List (pos_null, g)))
    | Logop (_, `Or, e1, e2) -> let left = aux e1 in Or (left, aux e2)
    | Logop (_, `And, e1, e2) -> let left = aux e1 in And (left, aux e2)
    | x ->
      bad_format ~pos:(value_pos x)
        "Expected a formula list of the form [ \"item\" {condition}... ]"
  in
  join (List.map aux (lift_list t))

let rec make_formulas split ~constraints t =
  let name = OpamPackage.Name.to_string in
  let rec aux = function
    | Empty             -> assert false
    | Block f           -> Group (pos_null, lift_list (make_formulas split ~constraints f))
    | And(e,f)          -> Logop (pos_null, `And, aux e, aux f)
    | Or(e,f)           -> Logop (pos_null, `Or, aux e, aux f)
    | Atom (n, cs)      ->
      match constraints cs with
      | [] -> make_string (name n)
      | cs -> Option (pos_null, make_string (name n), cs)
  in
  List (pos_null, List.map aux (split t))


let make_formula kind constraints t =
  let split = match kind with
    | `Conj -> OpamFormula.ands_to_list
    | `Disj -> OpamFormula.ors_to_list
  in
  make_formulas split ~constraints t

let parse_formula kind constraints t =
  let join = match kind with
    | `Conj -> OpamFormula.ands
    | `Disj -> OpamFormula.ors
  in
  parse_formulas join ~constraints t

let parse_compiler_version = function
  | Ident (_,v)
    when v = OpamCompiler.to_string OpamCompiler.system ->
    OpamCompiler.Version.of_string v
  | String (pos,v) ->
    (try OpamCompiler.Version.of_string v
     with Invalid_argument msg -> bad_format ~pos "%s" msg)
  | x -> bad_format ~pos:(value_pos x)
           "Expected a compiler version"


let rec parse_compiler_constraint t =
  let rec aux = function
    | Prefix_relop (_, op, v) ->
      Atom (op, parse_compiler_version v)
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
  | l ->
    let l =
      if OpamFormatConfig.(!r.all_parens) then [l]
      else OpamFormula.ors_to_list l in
    List (pos_null, List.map aux l)

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

let parse_filter_ident = function
  | Ident (pos, s) ->
    (try FIdent (filter_ident_of_string s)
     with Failure msg -> bad_format ~pos "%s" msg)
  | x ->
    bad_format ~pos:(value_pos x)
      "Expected a filter ident: \
       [pkg[+pkg...]:]varname[?str_if_true:str_if_false_or_undef]"

let rec parse_filter l =
  let rec aux = function
    | Bool (_,b)         -> FBool b
    | String (_,s)       -> FString s
    | Ident _ as id      -> parse_filter_ident id
    | Group (_,g)        -> parse_filter g
    | Relop (_,op,e,f) -> FOp (aux e, op, aux f)
    | Pfxop (_,`Not,e) -> FNot (aux e)
    | Logop(_,`And,e,f)-> FAnd (aux e, aux f)
    | Logop(_,`Or, e,f)-> FOr (aux e, aux f)
    | x -> bad_format ~pos:(value_pos x) "Expected a filter expression"
  in
  match l with
  | [] -> FBool true
  | [Group (_, ([] | _::_::_))] | _::_::_ as x ->
    bad_format ?pos:(values_pos x) "Expected a single filter expression"
  | [Group(_,[f])] | [f] -> aux f

let make_filter f =
  let rec aux ?paren f =
    match f with
    | FString s  -> make_string s
    | FIdent (pkgs,var,converter) ->
      let s =
        OpamStd.List.concat_map ~nil:"" "+" ~right:":"
          OpamPackage.Name.to_string pkgs ^
        OpamVariable.to_string var ^
        (match converter with
         | Some (it,ifu) -> "?"^it^":"^ifu
         | None -> "")
      in
      make_ident s
    | FBool b    -> make_bool b
    | FOp(e,s,f) ->
      let f = Relop (pos_null, s, aux e, aux f) in
      if OpamFormatConfig.(!r.all_parens) then Group (pos_null, [f]) else f
    | FOr(e,f) -> (* And, Or have the same priority, left-associative *)
      let f = Logop (pos_null, `Or, aux e, aux ~paren:`Or f) in
      if OpamFormatConfig.(!r.all_parens) then Group (pos_null, [f]) else
        (match paren with None | Some `Or -> f | _ -> Group (pos_null, [f]))
    | FAnd(e,f) ->
      let f = Logop (pos_null, `And, aux e, aux ~paren:`And f) in
      if OpamFormatConfig.(!r.all_parens) then Group (pos_null, [f]) else
        (match paren with None | Some `And -> f | _ -> Group (pos_null, [f]))
    | FNot f ->
      let f = Pfxop (pos_null, `Not, aux ~paren:`Not f) in
      if OpamFormatConfig.(!r.all_parens) then Group (pos_null, [f]) else f
    | FUndef -> make_ident "#undefined"
  in
  [aux f]

let make_simple_arg = function
  | CString s -> make_string s
  | CIdent s  -> make_ident s

let make_arg =
  make_option make_simple_arg make_filter

let make_single_command = make_list make_arg

let make_command =
  make_option (make_list make_arg) make_filter

let make_commands =
  make_list make_command

let make_libraries =
  make_list (make_option make_string make_filter)

let parse_libraries =
  parse_list (parse_option parse_string parse_filter)

let parse_simple_arg =
  parse_or [
    "ident" , (parse_ident  @> fun x -> CIdent x);
    "string", (parse_string @> fun x -> CString x);
  ]

let parse_arg =
  parse_option parse_simple_arg parse_filter

let parse_single_command = parse_list parse_arg

let parse_command =
  parse_option (parse_list parse_arg) parse_filter

let parse_commands =
  parse_or [
    "command"     , (fun x -> [parse_command x]);
    "command-list", parse_list parse_command;
  ]

let parse_message =
  parse_option (parse_string @> OpamStd.String.strip) parse_filter

let parse_messages =
  parse_list parse_message

let make_flag = function
  | Pkgflag_LightUninstall -> make_ident "light-uninstall"
  | Pkgflag_AllSwitches -> make_ident "all-switches"
  | Pkgflag_Verbose -> make_ident "verbose"
  | Pkgflag_Plugin -> make_ident "plugin"
  | Pkgflag_Compiler -> make_ident "compiler"
  | Pkgflag_Unknown s -> make_ident s

let parse_flag = function
  | Ident (_,"light-uninstall") -> Pkgflag_LightUninstall
  | Ident (_,"all-switches") -> Pkgflag_AllSwitches
  | Ident (_,"verbose") -> Pkgflag_Verbose
  | Ident (_,"plugin") -> Pkgflag_Plugin
  | Ident (_,"compiler") -> Pkgflag_Compiler
  | Ident (_,s) -> Pkgflag_Unknown s
  | x ->
    bad_format ~pos:(value_pos x)
      "Invalid package flag %s, must be an ident"
      (string_of_value x)

(* TAGS *)

let parse_string_set =
  parse_string_list @> OpamStd.String.Set.of_list

let make_string_set =
  OpamStd.String.Set.elements @> make_string_list

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
  OpamStd.String.SetMap.of_list l

let make_tags t =
  let l = OpamStd.String.SetMap.bindings t in
  make_list make_tag_line l

(* FEATURES *)

let parse_features t =
  let rec aux = function
    | [] -> []
    | id :: opt :: r ->
      let id = OpamVariable.of_string (parse_ident id) in
      (match parse_option parse_string parse_filter opt with
       | doc, Some fil -> (id, doc, fil) :: aux r
       | _, None ->
         bad_format ~pos:(value_pos opt) "Expecting a filter definition, e.g. \
                                          `var \"Enable var\" { <condition> }'")
    | t ->
      bad_format ?pos:(values_pos t) "Bad feature definition, expected \
                                      `var \"Enable var\" { <condition> }'"
  in
  match t with
  | List (_, l) -> aux l
  | _ -> bad_format ~pos:(value_pos t) "Expected a list of feature definitions"

let make_features feat =
  let rec aux = function
    | [] -> []
    | (var,doc,fil) :: r ->
      make_ident (OpamVariable.to_string var) ::
      make_option make_string make_filter (doc, Some fil) ::
      aux r
  in
  List (pos_null, aux feat)

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

type bad_format = pos option * string list * string

exception Bad_format of bad_format
exception Bad_format_list of bad_format list

let bad_format ?pos fmt =
  Printf.ksprintf
    (fun str ->
       raise (Bad_format (pos,[],str)))
    fmt

let add_pos pos = function
  | Bad_format (pos_opt,btl,msg) as e ->
    if pos_opt = None || pos_opt = Some pos_null then
      let backtrace = Printexc.get_backtrace () in
      Bad_format (Some pos, backtrace::btl, msg)
    else e
  | e -> e

let string_of_backtrace_list = function
  | [] | _ when not (Printexc.backtrace_status ()) -> ""
  | btl -> List.fold_left (fun s bts ->
      let bt_lines = OpamStd.String.split bts '\n' in
      "\n  Backtrace:\n    "^(String.concat "\n    " bt_lines)^s
    ) "" btl

let rec string_of_bad_format ?file e =
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
  | Bad_format_list bfl, _ ->
    OpamStd.List.concat_map "\n"
      (fun bf -> string_of_bad_format ?file (Bad_format bf)) bfl
  | _ -> Printexc.to_string e

let item_pos = function
  | Section (pos,_) | Variable (pos,_,_) -> pos

(*
let items_pos = function
  | i::_ -> Some (item_pos i)
  | [] -> None
*)
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
  | x      -> bad_format ~pos:(value_pos x) "expected a bool"

let parse_int = function
  | Int (_,i) -> i
  | x     -> bad_format ~pos:(value_pos x) "expected an int"

let parse_ident = function
  | Ident (_,i) -> i
  | x       -> bad_format ~pos:(value_pos x) "expected an ident"

let parse_string = function
  | String (_,s) -> s
  | x        -> bad_format ~pos:(value_pos x) "expected a string"

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
  | x       -> bad_format ~pos:(value_pos x) "expected a group"

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
    bad_format ?pos:(values_pos x) "expected a single string"

let parse_pair fa fb =
  let fa = protect_value fa in
  let fb = protect_value fb in
  function
  | List (_,[a; b]) -> (fa a, fb b)
  | x           ->
    bad_format ~pos:(value_pos x) "expected a pair"

let parse_or fns v =
  let rec aux = function
    | []   ->
      bad_format ~pos:(value_pos v)
        "expected %s" (OpamStd.List.concat_map " or " fst fns)
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
    | x -> bad_format ~pos:(value_pos x) "expected a list of constraints"
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
  | x -> bad_format ~pos:(value_pos x) "expected a package name"

let parse_package_version ?expected = function
  | String (pos,n) ->
    let version = OpamPackage.Version.of_string n in
    (match expected with
     | Some exp when version <> exp ->
       bad_format ~pos "Unexpected version %s"
         (OpamPackage.Version.to_string version)
     | _ -> version)
  | x -> bad_format ~pos:(value_pos x) "expected a package version"

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
        "expected a formula list of the form [ \"item\" {condition}... ]"
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
           "expected a compiler version"


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
             "expected a compiler constraint" in
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
    | x -> bad_format ~pos:(value_pos x) "expected an OS constraint" in
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
    | x -> bad_format ~pos:(value_pos x) "expected an \"ident = string\" binding"
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
      "expected a filter ident: \
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
    | x -> bad_format ~pos:(value_pos x) "expected a filter expression"
  in
  match l with
  | [] -> FBool true
  | [Group (_, ([] | _::_::_))] | _::_::_ as x ->
    bad_format ?pos:(values_pos x) "expected a single filter expression"
  | [Group(_,[f])] | [f] -> aux f

let make_filter f =
  let rec aux ?paren f =
    match f with
    | FString s  -> make_string s
    | FIdent fid -> make_ident (string_of_filter_ident fid)
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
  | _ -> bad_format ~pos:(value_pos t) "expected a list of feature definitions"

let make_features feat =
  let rec aux = function
    | [] -> []
    | (var,doc,fil) :: r ->
      make_ident (OpamVariable.to_string var) ::
      make_option make_string make_filter (doc, Some fil) ::
      aux r
  in
  List (pos_null, aux feat)


(* pp stuff *)


let log f = OpamConsole.log "FORMAT" f

module Normalise = struct
  (** OPAM normalised file format, for signatures:
      - each top-level field on a single line
      - file ends with a newline
      - spaces only after [fieldname:], between elements in lists, before braced
        options, between operators and their operands
      - fields are sorted lexicographically by field name (using [String.compare])
      - newlines in strings turned to ['\n'], backslashes and double quotes
        escaped
      - no comments (they don't appear in the internal file format anyway)
      - fields containing an empty list, or a singleton list containing an empty
        list, are not printed at all
  *)

  let escape_string s =
    let len = String.length s in
    let buf = Buffer.create (len * 2) in
    Buffer.add_char buf '"';
    for i = 0 to len -1 do
      match s.[i] with
      | '\\' | '"' as c -> Buffer.add_char buf '\\'; Buffer.add_char buf c
      | '\n' -> Buffer.add_string buf "\\n"
      | c -> Buffer.add_char buf c
    done;
    Buffer.add_char buf '"';
    Buffer.contents buf

  let rec value = function
    | Relop (_,op,l,r) ->
      String.concat " " [value l; OpamFormula.string_of_relop op; value r]
    | Logop (_,op,l,r) ->
      String.concat " " [value l; string_of_logop op; value r]
    | Pfxop (_,op,r) ->
      String.concat " " [string_of_pfxop op; value r]
    | Prefix_relop (_,op,r) ->
      String.concat " " [string_of_relop op; value r]
    | Ident (_,s) -> s
    | Int (_,i) -> string_of_int i
    | Bool (_,b) -> string_of_bool b
    | String (_,s) -> escape_string s
    | List (_, l) -> OpamStd.List.concat_map ~left:"[" ~right:"]" " " value l
    | Group (_,g) -> OpamStd.List.concat_map ~left:"(" ~right:")" " " value g
    | Option(_,v,l) ->
      OpamStd.List.concat_map ~left:(value v ^ " {") ~right: "}"
        " " value l
    | Env_binding (_,op,id,v) ->
      String.concat " " ["["; value id; op; value v; "]"]

  let rec item = function
    | Variable (_, _, List (_,([]|[List(_,[])]))) -> ""
    | Variable (_, i, List (_,l)) ->
      OpamStd.List.concat_map ~left:(i ^ ": [") ~right:"]" " "
        value l
    | Variable (_, i, v) -> String.concat ": " [i; value v]
    | Section (_,s) ->
      Printf.sprintf "%s %s {\n%s\n}"
        s.section_kind
        (escape_string s.section_name)
        (OpamStd.List.concat_map "\n" item s.section_items)

  let item_order a b = match a,b with
    | Section _, Variable _ -> 1
    | Variable _, Section _ -> -1
    | Variable (_,i,_), Variable (_,j,_) -> String.compare i j
    | Section (_,s), Section (_,t) ->
      String.compare s.section_name t.section_name

  let items its =
    let its = List.sort item_order its in
    OpamStd.List.concat_map ~right:"\n" "\n" item its
end

module Pp = struct

  type ('a,'b) t = {
    parse: pos:pos -> 'a -> 'b;
    print: 'b -> 'a;
    name: string;
    name_constr: string -> string;
  }

  let pp ?(name="") ?(name_constr=fun x -> x) parse print =
    {
      parse; print; name; name_constr;
    }

  let of_pair name (simple_parse, print) =
    pp ~name (fun ~pos:_ -> simple_parse) print

  (** Utility functions *)

  exception Unexpected of pos option
  let unexpected ?pos () = raise (Unexpected pos)

  let raise_with_pos pos_opt e = match pos_opt with
    | Some p -> raise (add_pos p e)
    | None -> raise e

  let try_with_pos ~pos ?msg f x =
    try f x with
    | Bad_format _ | Bad_format_list _ as e -> add_pos pos e
    | e ->
      OpamStd.Exn.fatal e;
      bad_format ~pos "%s%s"
        (match msg with Some m -> m^": " | None -> "")
        (match e with
         | Failure m -> m
         | e -> Printexc.to_string e)

  let warn ?pos ?(strict=OpamFormatConfig.(!r.strict)) ?exn fmt =
    if strict then
      match exn with
      | Some e -> raise e
      | None -> bad_format ?pos fmt
    else
      Printf.ksprintf
        (fun s -> log "%s" (string_of_bad_format (Bad_format (pos, [], s))))
        fmt

  (** Basic pp usage *)

  let parse pp ~pos x = try pp.parse ~pos x with
    | Bad_format _ | Bad_format_list _ as e -> raise (add_pos pos e)
    | Unexpected (Some pos) -> bad_format ~pos "expected %s" pp.name
    | Unexpected None -> bad_format ~pos "expected %s" pp.name
    | Failure msg ->
      bad_format ~pos "while expecting %s: %s" pp.name msg
    | e ->
      OpamStd.Exn.fatal e;
      bad_format ~pos "while expecting %s: %s" pp.name (Printexc.to_string e)

  let print pp x = pp.print x


  (** Pp combination and transformation *)

  (** Piping *)
  let (-|) pp1 pp2 = {
    parse = (fun ~pos x ->
        let y = pp1.parse ~pos x in
        parse pp2 ~pos y
      );
    print = pp1.print @* pp2.print;
    name = (match pp2.name with "" -> pp1.name | name -> pp1.name_constr name);
    name_constr = pp1.name_constr @* pp2.name_constr;
  }

  let identity = {
    parse = (fun ~pos:_ x -> x);
    print = (fun x -> x);
    name = "";
    name_constr = (fun x -> x);
  }

  let ignore = {
    parse = (fun ~pos:_ -> OpamStd.Option.none);
    print = (fun _ -> assert false);
    name = "ignored";
    name_constr = (fun _ -> "<ignored>");
  }

  let check ?name ?errmsg f =
    pp
      ?name
      (fun ~pos x ->
         if not (f x) then
           match errmsg with
           | Some m -> bad_format ~pos "%s" m
           | None -> unexpected ()
         else x)
      (fun x -> assert (f x); x)

  let map_pair ?name ?posf1 ?posf2 pp1 pp2 =
    let name = match name with
      | None -> Printf.sprintf "(%s, %s)" pp1.name pp2.name
      | Some n -> n
    in
    pp ~name
      (fun ~pos (a,b) ->
         let posf1 = OpamStd.Option.default (fun _ -> pos) posf1 in
         parse pp1 ~pos:(posf1 a) a,
         let posf2 = OpamStd.Option.default (fun _ -> pos) posf2 in
         parse pp2 ~pos:(posf2 b) b)
      (fun (a,b) -> print pp1 a, print pp2 b)

  let map_list ?name ?posf pp1 =
    let name = match name with
      | None -> pp1.name ^ "*"
      | Some n -> n
    in
    pp ~name
      (fun ~pos l ->
         let posf = OpamStd.Option.default (fun _ -> pos) posf in
         List.rev (List.rev_map (fun x -> parse pp1 ~pos:(posf x) x) l))
      (List.rev @* List.rev_map (print pp1))

  let map_option ?name pp1 =
    let name = match name with
      | None -> pp1.name ^ "?"
      | Some n -> n
    in
    pp ~name
      (fun ~pos -> OpamStd.Option.map (parse pp1 ~pos))
      (OpamStd.Option.map (print pp1))

  let singleton = {
    parse = (fun ~pos:_ -> function [x] -> x | _ -> unexpected ());
    print = (fun x -> [x]);
    name = "";
    name_constr = (fun x -> x);
  }

  (** Pps from strings *)

  module type STRINGABLE = sig
    type t
    val of_string: string -> t
    val to_string: t -> string
  end

  let of_module :
    type a. string -> (module STRINGABLE with type t = a) -> (string, a) t
    =
    fun name (module X: STRINGABLE with type t = a) ->
    pp ~name
      (fun ~pos:_ -> X.of_string)
      X.to_string

  (** low-level Pps for the Lines parser ([string list list]) *)

  let lines_set empty add fold pp1 =
    pp
      ~name:(Printf.sprintf "(%s) lines" pp1.name)
      (fun ~pos:(file,_,_) lines ->
         List.fold_left (fun (i,acc) -> function
             | [] -> i + 1, acc
             | line -> i + 1, add (parse pp1 ~pos:(file,i,0) line) acc)
           (1, empty) lines
         |> snd)
      (fun x ->
         List.rev (fold (fun v acc -> print pp1 v::acc) x []))

  let lines_map empty add fold pp1 =
    pp
      ~name:(Printf.sprintf "(%s) lines" pp1.name)
      (fun ~pos:(file,_,_) lines ->
         List.fold_left (fun (i,acc) -> function
             | [] -> i + 1, acc
             | line ->
               let k,v = parse pp1 ~pos:(file,i,0) line in
               i + 1, add k v acc)
           (1, empty) lines
         |> snd)
      (fun x ->
         List.rev (fold (fun k v acc -> print pp1 (k,v)::acc) x []))

  (** Build tuples from lists *)
  let (^+) pp1 pp2 =
    pp
      ~name:(Printf.sprintf "%s %s" pp1.name pp2.name)
      (fun ~pos -> function
         | x::r -> parse pp1 ~pos x, parse pp2 ~pos r
         | [] -> unexpected ())
      (fun (x,y) -> print pp1 x :: print pp2 y)

  let last = singleton

  let opt pp1 =
    pp
      ~name:("?"^pp1.name)
      (fun ~pos -> function [] -> None | l -> Some (pp1.parse ~pos l))
      (function Some x -> pp1.print x | None -> [])

  let default d =
    pp
      (fun ~pos:_ -> function None -> d | Some x -> x)
      (fun x -> Some x)

  module Op = struct
    let ( -| ) = ( -| )
    let ( ^+ ) = ( ^+ )
  end

(*
  let list2 pp1 pp2 =
    pp ~name:(Printf.sprintf "%s %s" pp1.name pp2.name)
      (function [a; b] -> parse pp1 a, parse pp2 b
              | _ -> unexpected ())
      (fun (x,y) -> [print pp1 x; print pp2 y])
*)

  (** All Pps dealing with the [value] type *)
  module V = struct

    (** Low-level Pps *)

    let bool =
      pp ~name:"bool"
        (fun ~pos:_ -> function Bool (_,b) -> b | _ -> unexpected ())
        (fun b -> Bool (pos_null,b))

    let int =
      pp ~name:"int"
        (fun ~pos:_ -> function Int (_,i) -> i | _ -> unexpected ())
        (fun i -> Int (pos_null,i))

    let pos_int = int -| check ~name:"positive-int" (fun i -> i >= 0)

    let ident =
      pp ~name:"ident"
        (fun ~pos:_ -> function Ident (_,i) -> i | _ -> unexpected ())
        (fun str -> Ident (pos_null,str))

    let string =
      pp ~name:"string"
        (fun ~pos:_ -> function String (_,s) -> s | _ -> unexpected ())
        (fun str -> String (pos_null,str))

    let string_tr = string -| pp (fun ~pos:_ -> String.trim) (fun x -> x)

    let simple_arg =
      pp ~name:"ident-or-string"
        (fun ~pos:_ -> function
           | Ident (_,i) -> CIdent i
           | String (_,s) -> CString s
           | _ -> unexpected ())
        (function
          | CIdent i -> Ident (pos_null, i)
          | CString s -> String (pos_null, s))

    let variable_contents =
      pp ~name:"string-or-bool"
        (fun ~pos:_ -> function
           | String (_,s) -> S s
           | Bool (_,b) -> B b
           | _ -> unexpected ())
        (function
          | S s -> String (pos_null, s)
          | B b -> Bool (pos_null, b))

    let list =
      pp ~name:"list" ~name_constr:(Printf.sprintf "[%s]")
        (fun ~pos:_ -> function
          | List (_,l) -> l
          | x -> [x])
        (fun l -> List (pos_null, l))

    let group =
      pp ~name:"group" ~name_constr:(Printf.sprintf "(%s)")
        (fun ~pos:_ -> function
          | Group (_,l) -> l
          | x -> [x])
        (fun l -> Group (pos_null, l))

    let option =
      pp ~name:"option"
        (fun ~pos:_ -> function
          | Option (_,k,l) -> k, l
          | k -> k, [])
        (function
          | (v, []) -> v
          | (v, l) -> Option (pos_null, v, l))

    let map_group pp1 = group -| map_list ~posf:value_pos pp1

    let map_list pp1 =
      pp ~name:(Printf.sprintf "[%s]" pp1.name)
        (fun ~pos v ->
         try [pp1.parse ~pos v] with
         | Bad_format _ | Bad_format_list _ | Unexpected _ as err ->
           match v with
           | List (_, l) ->
             List.rev @@
             List.rev_map (fun v -> parse pp1 ~pos:(value_pos v) v) l
           | _ -> raise err)
        (function
         | [x] -> pp1.print x
         | l -> List (pos_null, List.rev @@ List.rev_map (print pp1) l))

    let map_option pp1 pp2 =
      option -|
      map_pair ~name:(Printf.sprintf "%s ?{%s}" pp1.name pp2.name)
        ~posf1:value_pos
        ~posf2:(fun v -> OpamStd.Option.default pos_null (values_pos v))
        pp1 pp2

    let map_pair pp1 pp2 =
      pp ~name:(Printf.sprintf "[%s %s]" pp1.name pp2.name)
        (fun ~pos:_ -> function
          | List (_,[a; b]) ->
            parse pp1 ~pos:(value_pos a) a, parse pp2 ~pos:(value_pos b) b
          | _ -> unexpected ())
        (fun (a, b) -> List (pos_null, [pp1.print a; pp2.print b]))
(*
  let triplet pp1 pp2 pp3 =
    pp
      ~name:(Printf.sprintf "[%s %s %s]" pp1.name pp2.name pp3.name)
      ~pos:value_pos_opt
      (function
        | List (_,[a; b; c]) -> (parse pp1 a, parse pp2 b, parse pp3 c)
        | _ -> unexpected ())
      (fun (a,b,c) -> List (pos_null, [pp1.print a; pp2.print b; pp3.print c]))
*)
(*
    let single_option pp1 pp2 =
      map_option pp1 (singleton -| pp2)
*)

    (** Pps for the [value] type to higher level types *)

    let address =
      string -|
      pp ~name:"url"
        (fun ~pos:_ -> address_of_string)
        (fun a -> string_of_address a)

    let url =
      string -|
      pp ~name:"url"
        (fun ~pos:_ -> OpamTypesBase.(address_of_string @> parse_url))
        (fun (addr,kind) -> OpamTypesBase.string_of_address ~kind addr)

    (* a hack to allow "system" compiler as ident rather than string. For
       backwards-compat. *)
    let compiler_version =
      let str_system = OpamCompiler.(to_string system) in
      let comp = of_module "compiler-version" (module OpamCompiler.Version) in
      let parse ~pos = function
        | Ident (_, v) when v = str_system -> parse comp ~pos v
        | String (_, v) -> parse comp ~pos v
        | _ -> unexpected ()
      in
      let print v =
        let v = print comp v in
        if v = str_system then print ident v
        else print string v
      in
      pp ~name:"compiler-version" parse print

    let filter_ident =
      ident -|
      pp ~name:"filter-ident"
        (fun ~pos:_ -> filter_ident_of_string)
        string_of_filter_ident

    let filter =
      let rec parse_filter ~pos l =
        let rec aux = function
          | Bool (_,b) -> FBool b
          | String (_,s) -> FString s
          | Ident (pos,_) as id -> FIdent (parse ~pos filter_ident id)
          | Group (pos,g) -> parse_filter ~pos g
          | Relop (_,op,e,f) -> FOp (aux e, op, aux f)
          | Pfxop (_,`Not,e) -> FNot (aux e)
          | Logop(_,`And,e,f)-> FAnd (aux e, aux f)
          | Logop(_,`Or, e,f)-> FOr (aux e, aux f)
          | _ -> unexpected ()
        in
        match l with
        | [] -> FBool true
        | [Group (_, ([] | _::_::_))] | _::_::_ ->
          bad_format ~pos "expected a single filter expression"
        | [Group(_,[f])] | [f] -> aux f
      in
      let print_filter f =
        let rec aux ?paren f =
          let group ?kind f =
            if OpamFormatConfig.(!r.all_parens) ||
               (paren <> None && paren <> kind)
            then Group (pos_null, [f]) else f
          in
          match f with
          | FString s  -> print string s
          | FIdent fid -> print filter_ident fid
          | FBool b    -> print bool b
          | FOp(e,s,f) -> group (Relop (pos_null, s, aux e, aux f))
          | FOr(e,f) -> (* And, Or have the same priority, left-associative *)
            group ~kind:`Or (Logop (pos_null, `Or, aux e, aux ~paren:`Or f))
          | FAnd(e,f) ->
            group ~kind:`And (Logop (pos_null, `And, aux e, aux ~paren:`And f))
          | FNot f -> group (Pfxop (pos_null, `Not, aux ~paren:`Not f))
          | FUndef -> assert false
        in
        [aux f]
      in
      pp ~name:"filter-expression" parse_filter print_filter

    let arg = map_option simple_arg (opt filter)

    let command = map_option (map_list arg) (opt filter)

    let constraints version =
      let rec parse_constraints ~pos:_ l =
        let rec aux = function
          | Prefix_relop (pos, op, v) ->
            Atom (op, parse version ~pos v)
          | Logop (_, `And, l, r) ->
            And (aux l, aux r)
          | Logop (_, `Or, l, r) ->
            Or (aux l, aux r)
          | Pfxop (_,`Not,v) ->
            OpamFormula.neg (fun (op, s) -> (OpamFormula.neg_relop op, s)) (aux v)
          | Group (pos, g) ->
            Block (parse_constraints ~pos g)
          | v -> unexpected ~pos:(value_pos v) ()
        in
        OpamFormula.ands (List.map aux l)
      in
      let rec print_constraints cs =
        let rec aux = function
          | Empty       -> assert false
          | Atom (r, v) -> Prefix_relop (pos_null, r, print version v)
          | And (x, y)  -> Logop (pos_null, `And, aux x, aux y)
          | Or (x, y)   -> Logop (pos_null, `Or, aux x, aux y)
          | Block g     -> Group (pos_null, print_constraints g)
        in
        match cs with
        | Empty -> []
        | cs -> [aux cs]
      in
      pp ~name:(version.name ^ "-constraints")
        parse_constraints print_constraints

    let dep_flag =
      ident -| pp ~name:"dependency-flag"
        (fun ~pos:_ -> dep_flag_of_string)
        string_of_dep_flag

    (* Version constraints with additional leading keywords
       ("build","test"...), used for dependency flags *)
    let ext_constraints version =
      let rec parse_ext_constraints ~pos = function
        | Ident _ as kw :: r ->
          let rpos = OpamStd.Option.default pos (values_pos r) in
          let kws, f = parse_ext_constraints ~pos:rpos r in
          parse dep_flag ~pos kw :: kws, f
        | Logop (pos, `And, t1, t2) :: r -> parse_ext_constraints ~pos (t1::t2::r)
        | t -> [], parse (constraints version) ~pos t
      in
      let print (kws, cs) =
        (* The kws must be aggregated with an '&' to the first constraint, if
           any *)
        match print (constraints version) cs, kws with
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
      in
      pp ~name:"ext-constraints" parse_ext_constraints print

    let package_atom constraints =
      map_option
        (string -| of_module "pkg-name" (module OpamPackage.Name))
        (constraints
           (string -| of_module "pkg-version" (module OpamPackage.Version)))

    let package_formula kind constraints =
      let split, join = match kind with
        | `Conj -> OpamFormula.(ands_to_list, ands)
        | `Disj -> OpamFormula.(ors_to_list, ors)
      in
      let rec parse_formula ~pos:_ l =
        let rec aux = function
          | String (pos,_) | Option (pos,_,_) as at ->
            Atom (parse (package_atom constraints) ~pos at)
          | Group (pos,g) -> Block (parse_formula ~pos g)
          | Logop (_, `Or, e1, e2) -> let left = aux e1 in Or (left, aux e2)
          | Logop (_, `And, e1, e2) -> let left = aux e1 in And (left, aux e2)
          | v -> unexpected ~pos:(value_pos v) ()
        in
        join (List.map aux l)
      in
      let rec print_formula f =
        let rec aux = function
          | Empty -> assert false
          | Block f -> Group (pos_null, print_formula f)
          | And (e,f) -> Logop (pos_null, `And, aux e, aux f)
          | Or (e,f) -> Logop (pos_null, `Or, aux e, aux f)
          | Atom at -> print (package_atom constraints) at
        in
        List.map aux (split f)
      in
      list -| pp ~name:"pkg-formula" parse_formula print_formula

    let env_binding =
      let parse ~pos:_ = function
        | Relop (_, `Eq, Ident (_,i), String (_,s)) -> i, "=", s
        | Env_binding (_, op, Ident (_,i), String (_,s)) -> i, op, s
        | _ -> unexpected ()
      in
      let print (id, op, str) =
        Env_binding (pos_null, op, print ident id, print string str)
      in
      list -| singleton -| pp ~name:"env-binding" parse print

    let features =
      let var = ident -| of_module "variable" (module OpamVariable) in
      let doc_filt = map_option string filter in
      let rec parse_features ~pos = function
        | [] -> []
        | [_] -> unexpected ()
        | id :: opt :: r ->
          let doc, filt = parse doc_filt ~pos:(value_pos opt) opt in
          (parse var ~pos id, doc, filt) ::
          parse_features ~pos:(OpamStd.Option.default pos (values_pos r)) r
      in
      let print ft =
        List.fold_right (fun (id, doc, filt) acc ->
            print var id :: print doc_filt (doc, filt) :: acc)
          ft []
      in
      list -| pp ~name:"(variable \"doc\" {filter})*" parse_features print

    (* Only used by the deprecated "os" field *)
    let os_constraint =
      let rec parse_osc ~pos:_ l =
        let rec aux = function
          | Group (pos,g) -> Block (parse_osc ~pos g)
          | String (_,os) -> Atom (true, os)
          | Logop (_,`And,l,r) -> And (aux l, aux r)
          | Logop (_,`Or,l,r) -> Or (aux l, aux r)
          | Pfxop (_,`Not,v) ->
            OpamFormula.neg (fun (b, s) -> (not b, s)) (aux v)
          | v -> unexpected ~pos:(value_pos v) ()
        in
        OpamFormula.ors (List.map aux l)
      in
      let print_osc f =
        let rec aux = function
          | Empty -> assert false
          | Atom (true , os) -> print string os
          | Atom (false, os) -> Pfxop (pos_null, `Not, print string os)
          | Block g -> Group (pos_null, [aux g])
          | And(e,f) -> Logop (pos_null, `And, aux e, aux f)
          | Or(e,f) -> Logop (pos_null, `Or, aux e, aux f)
        in
        match f with
        | Empty -> []
        | f -> [aux f]
      in
      list -| pp ~name:"os-constraint" parse_osc print_osc

  end

  (** Pps for file contents (item lists), mostly list of [Variable(...)]
      fields *)

  type 'a field_parser = ('a * value option, 'a) t

  (** add setter/getter and an accumulator to a pp; useful to use
      to get/set field records *)
  let ppacc_opt
    (* : ('a -> 'b -> 'a) -> ('a -> 'b option) -> ('value, 'b) t -> 'a field_parser *)
    = fun ?(cleanup = fun ~pos:_ _acc x -> x) set get pp1 ->
      let parse ~pos = function
        | acc, Some s ->
          set acc (cleanup ~pos acc (pp1.parse ~pos s))
        | acc, None -> acc
      in
      let print s = s, OpamStd.Option.map pp1.print (get s) in
      {
        parse; print;
        name = pp1.name;
        name_constr = (fun x -> x);
      }

  let ppacc ?cleanup set get pp =
    ppacc_opt set (fun x -> Some (get x)) ?cleanup pp

  let ppacc_ignore = {
    parse = (fun ~pos:_ (acc,_) -> acc);
    print = (fun s -> s, None);
    name = "<ignored>";
    name_constr = (fun _ -> "<ignored>");
  }

  (** Parsers for item lists (standard opam file contents: list of field
      bindings). *)
  module I = struct

    let item =
      pp ~name:"field-binding"
        (fun ~pos:_ -> function
           | Section (pos,sec) ->
             bad_format ~pos "Unexpected section %s" sec.section_name
           | Variable (_,k,v) -> k,v)
        (fun (k,v) -> Variable (pos_null, k, v))

    let items = map_list ~posf:item_pos item

    let good_fields ?name ?(allow_extensions=false) fields =
      let parse ~pos:_ items =
        let rem_fields, ok_items, extra_items =
          List.fold_left (fun (fields,ok,extra) -> function
              | Section _ as s -> fields, ok, s::extra
              | Variable (_,k,_) as v ->
                if List.mem_assoc k fields
                then List.remove_assoc k fields, v::ok, extra
                else if
                  allow_extensions &&
                  OpamStd.String.starts_with ~prefix:"x-" k &&
                  not @@ List.exists
                    (function Variable (_,k1,_) -> k1 = k | _ -> false)
                    ok
                then fields, v::ok, extra
                else fields, ok, v :: extra)
            (fields,[],[]) items
        in
        rem_fields, List.rev ok_items, List.rev extra_items
      in
      let print (_, valid_items, _invalid) = valid_items in
      pp ?name parse print

    let check_fields ?name ?(allow_extensions=false) ?strict fields =
      let in_name = OpamStd.Option.Op.((name >>| fun n -> " in "^n) +! "") in
      let parse ~pos:_ items =
        let _, valid_fields, extra_fields =
          List.fold_left (fun (fields,ok,extra) -> function
              | Section (pos,sec) ->
                bad_format ~pos "Unexpected section %s%s"
                  sec.section_name in_name
              | Variable (pos,k,_) as v ->
                if List.mem_assoc k fields
                then List.remove_assoc k fields, v::ok, extra
                else if
                  allow_extensions &&
                  OpamStd.String.starts_with ~prefix:"x-" k &&
                  not @@ List.exists
                    (function Variable (_,k1,_) -> k1 = k | _ -> false)
                    ok
                then fields, v::ok, extra
                else fields, ok, (pos,k) :: extra)
            (fields,[],[]) items
        in
        match extra_fields with
        | [] -> items
        | (pos,_) :: _  ->
          warn ~pos ?strict "Unexpected or duplicate fields%s:%s" in_name
            (OpamStd.Format.itemize
               (fun (pos,k) ->
                  Printf.sprintf "'%s:' at %s" k (string_of_pos pos))
               extra_fields);
          valid_fields
      in
      let print items =
        assert (List.for_all (function
            | Variable (_,k,_) -> List.mem_assoc k fields
            | _ -> false)
            items);
        items
      in
      pp ?name parse print

    let fields ?name ?strict ~empty ppas =
      let in_name =
        OpamStd.Option.Op.((name >>| Printf.sprintf "In %s, ") +! "")
      in
      let parse ~pos items =
        (* For consistency, always read fields in ppa order, ignoring file
           order. Some parsers may depend on it. *)
        let field_map =
          List.fold_left
            (fun map -> function
               | Section _ -> map
               | Variable (pos, k, v) -> OpamStd.String.Map.add k (pos,v) map)
            OpamStd.String.Map.empty items
        in
        let errs, r =
          List.fold_left
            (fun (errs,acc) (field,ppa) ->
               try
                 let pos, v = OpamStd.String.Map.find field field_map in
                 try errs, parse ppa ~pos (acc, Some v) with
                 | Bad_format (pos,btl,msg) ->
                   let msg =
                     Printf.sprintf "%sfield '%s:' %s" in_name field msg
                   in
                   (field,(pos, Printexc.get_backtrace()::btl, msg)) :: errs,
                   acc
               with
               | Not_found -> errs, acc)
            ([],empty) ppas
        in
        if errs <> [] then
          warn ~pos ?strict ~exn:(Bad_format_list (List.map snd errs))
            "Errors in fields: %s" (OpamStd.List.concat_map ", " fst errs);
        r
      in
      let print acc =
        OpamStd.List.filter_map
          (fun (field,ppa) ->
             match snd (ppa.print acc) with
             | None | Some (List (_,[]) | Group (_,[])) -> None
             | Some value -> Some (Variable (pos_null, field, value)))
          ppas
      in
      pp ?name parse print

    let partition_fields filter =
      pp
        (fun ~pos:_ -> List.partition (function
             | Variable (_,k,_) -> filter k
             | _ -> false))
        (fun (a,b) -> a @ b)

    let field name parse =
      pp
        (fun ~pos items ->
           match
             OpamStd.List.filter_map (function
                 | Variable (_,k,v) when k = name -> Some v
                 | _ -> None)
               items
           with
           | [] -> bad_format ~pos "Missing '%s:' field" name
           | _::_::_ -> bad_format ~pos "Duplicate '%s:' field" name
           | [v] -> parse ~pos v, items)
        (fun (_,x) -> x)


    let extract_field name =
      partition_fields ((=) name) -|
      map_pair
        (opt (singleton -| item -|
              pp ~name:(Printf.sprintf "'%s:' field" name)
                (fun ~pos:_ (_,v) -> v)
                (fun v -> name,v)))
        identity

    let check_opam_version
        ?(v=OpamVersion.current_nopatch)
        ?(f=fun v -> OpamVersion.(compare current_nopatch (nopatch v) >= 0))
        ()
      =
      let name = "opam-version" in
      let opam_v = V.string -| of_module "opam-version" (module OpamVersion) in
      let f v = OpamFormatConfig.(!r.skip_version_checks) || f v in
      field name (parse opam_v) -|
      map_pair (check ~name ~errmsg:"Unsupported opam version" f) identity -|
      pp (fun ~pos:_ (_,x) -> x) (fun x -> v,x)

    let signature =
      V.list -| (V.string ^+ V.string ^+ last -| V.string)

    exception Invalid_signature of pos * (string*string*string) option
(*
    let signed check =
      extract_field "signature" -|
      pp ~name:"signed-file"
        (function
          | Some (pos,sgs), items ->
            let sgs = parse (V.map_list signature) sgs in
            let str = Normalise.items items in
            if not (check sgs str) then
              raise (Invalid_signature (pos, Some sgs))
            else items
          | None, _ ->
            raise (Invalid_signature
                     (OpamStd.Option.default pos_null (items_pos items),
                      None)))
        (fun _ -> assert false)
*)
  end


end

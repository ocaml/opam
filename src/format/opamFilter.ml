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

let log ?level fmt =
  OpamConsole.log "FILTER" ?level fmt

type env = full_variable -> variable_contents option

type fident = name list * variable * (string * string) option

let to_string t =
  let rec aux ?(context=`Or) t =
    let paren ?(cond=false) f =
      if cond || OpamFormatConfig.(!r.all_parens)
      then Printf.sprintf "(%s)" f else f
    in
    match t with
    | FBool b    -> string_of_bool b
    | FString s  -> Printf.sprintf "%S" s
    | FIdent (pkgs,var,converter) ->
      OpamStd.List.concat_map "+" OpamPackage.Name.to_string pkgs ^
      (if pkgs <> [] then ":" else "") ^
      OpamVariable.to_string var ^
      (match converter with
       | Some (it,ifu) -> "?"^it^":"^ifu
       | None -> "")
    | FOp(e,s,f) ->
      paren ~cond:(context <> `Or && context <> `And)
        (Printf.sprintf "%s %s %s"
           (aux ~context:`Relop e) (string_of_relop s) (aux ~context:`Relop f))
    | FAnd (e,f) ->
      paren ~cond:(context <> `Or && context <> `And)
        (Printf.sprintf "%s & %s" (aux ~context:`And e) (aux ~context:`And f))
    | FOr (e,f)  ->
      paren ~cond:(context <> `Or)
        (Printf.sprintf "%s | %s" (aux e) (aux f))
    | FNot e     ->
      paren ~cond:(context = `Relop)
        (Printf.sprintf "!%s" (aux ~context:`Not e))
    | FUndef f -> Printf.sprintf "#undefined(%s)" (aux f)
  in
  aux t

let rec fold_down_left f acc filter = match filter with
  | FOp(l,_,r) | FAnd(l,r) | FOr(l,r) ->
    fold_down_left f (fold_down_left f (f acc filter) l) r
  | FNot(x) -> fold_down_left f (f acc filter) x
  | x -> f acc x

(* ["%%"], ["%{xxx}%"], or ["%{xxx"] if unclosed *)
let string_interp_regex =
  let open Re in
  let notclose =
    rep (alt [
        diff notnl (set "}");
        seq [char '}'; alt [diff notnl (set "%"); stop] ]
      ])
  in
  compile (alt [
      str "%%";
      seq [str "%{"; group (greedy notclose); opt (group (str "}%"))];
    ])

let fident_variables = function
  | [], var, _ -> [OpamVariable.Full.global var]
  | pkgs, var, _ -> List.map (fun n -> OpamVariable.Full.create n var) pkgs

(* extracts variables appearing in interpolations in a string*)
let string_variables s =
  let matches =
    let rec aux acc pos =
      try
        let ss = Re.exec ~pos string_interp_regex s in
        if Re.test ss 2 then
          aux (Re.get ss 1 :: acc)
            (fst (Re.get_ofs ss 0) + String.length (Re.get ss 0))
        else
          aux acc (pos+1)
      with Not_found -> acc
    in
    aux [] 0
  in
  List.fold_left (fun acc s ->
      try fident_variables (filter_ident_of_string s) @ acc
      with Failure _ -> acc)
    [] matches

let variables filter =
  fold_down_left (fun acc -> function
      | FString s -> string_variables s @ acc
      | FIdent f -> fident_variables f @ acc
      | _ -> acc)
    [] filter

(* Some cast functions on values *)

let value ?default = function
  | FBool b -> B b
  | FString s -> S s
  | FUndef f ->
    (match default with
     | Some d -> d
     | None -> failwith ("Undefined filter value: "^to_string f))
  | e -> raise (Invalid_argument ("filter value: "^to_string e))

let value_string ?default = function
  | FBool b -> string_of_bool b
  | FString s -> s
  | FUndef f ->
    (match default with
     | Some d -> d
     | None -> failwith ("Undefined string filter value: "^to_string f))
  | e -> raise (Invalid_argument ("value_string: "^to_string e))

let value_bool ?default = function
  | FBool b -> b
  | FString "true" -> true
  | FString "false" -> false
  | FUndef f ->
    (match default with
     | Some d -> d
     | None -> failwith ("Undefined boolean filter value: "^to_string f))
  | e ->
    (match default with
     | Some d -> d
     | None -> raise (Invalid_argument ("value_bool: "^to_string e)))

(* Desugars the "enable" pseudo-variable *)
let desugar_fident ((packages,var,converter) as fident) =
  let enable = OpamVariable.of_string "enable" in
  if packages <> [] && var = enable && converter = None then
    packages, OpamVariable.of_string "installed", Some ("enable","disable")
  else fident

(* Resolves [FIdent] to string or bool, using its package and converter
   specification *)
let resolve_ident env fident =
  let open OpamStd.Option.Op in
  let packages,var,converter = desugar_fident fident in
  let bool_of_value = function
    | B b -> Some b
    | S s -> try Some (bool_of_string s) with Invalid_argument _ -> None
  in
  let resolve name = env (OpamVariable.Full.create name var) in
  let value_opt : variable_contents option = match packages with
  | [] -> env (OpamVariable.Full.global var)
  | [name] -> resolve name
  | names ->
    List.fold_left (fun acc name ->
        if acc = Some false then acc else
        match resolve name with
        | Some (B true) -> acc
        | v -> v >>= bool_of_value)
      (Some true) names
    >>| fun b -> B b
  in
  match converter with
  | None ->
    (match value_opt with
     | Some (B b) -> FBool b
     | Some (S s) -> FString s
     | None -> FUndef (FIdent fident))
  | Some (iftrue, iffalse) ->
    match value_opt >>= bool_of_value with
    | Some true -> FString iftrue
    | Some false -> FString iffalse
    | None -> FString iffalse

(* Resolves ["%{x}%"] string interpolations *)
let expand_string ?default env text =
  let subst str =
    if str = "%%" then "%"
    else if not (OpamStd.String.ends_with ~suffix:"}%" str) then
      (log "ERR: Unclosed variable replacement in %S\n" str;
       str)
    else
    let fident = String.sub str 2 (String.length str - 4) in
    resolve_ident env (filter_ident_of_string fident)
    |> value_string ~default:(OpamStd.Option.default str default)
  in
  Re_pcre.substitute ~rex:string_interp_regex ~subst text

let unclosed_expansions text =
  let re =
    Re.(
      compile (alt [
          str "%%";
          seq [str "%{";
               group (greedy (rep (diff notnl (char '}'))));
               opt (group (str "}%"))];
        ])
    )
  in
  Re.all re text |> OpamStd.List.filter_map @@ fun gr ->
  if Re.Group.test gr 1 && not (Re.Group.test gr 2) then
    Some (Re.Group.offset gr 0, Re.Group.get gr 0)
  else None

let logop1 cstr op = function
  | FUndef f -> FUndef (cstr f)
  | e ->
    try FBool (op (value_bool e))
    with Invalid_argument s -> log "ERR: %s" s; FUndef (cstr e)

let logop2 cstr op absorb e f = match e, f with
  | _, FBool x | FBool x, _ when x = absorb -> FBool x
  | FUndef x, FUndef y | FUndef x, y | x, FUndef y -> FUndef (cstr x y)
  | f, g ->
    try FBool (op (value_bool f) (value_bool g))
    with Invalid_argument s -> log "ERR: %s" s; FUndef (cstr f g)

(* Reduce expressions to values *)

let rec reduce_aux ~default_str env = function
  | FUndef x -> FUndef x
  | FBool b -> FBool b
  | FString s -> FString s
  | FIdent i -> resolve_ident env i
  | FOp (e,relop,f) ->
    (match reduce ~default_str env e, reduce ~default_str env f with
     | FUndef x, FUndef y | FUndef x, y | x, FUndef y ->
       FUndef (FOp (x, relop, y))
     | e,f ->
       FBool (OpamFormula.check_relop relop
                (OpamVersionCompare.compare (value_string e) (value_string f))))
  | FAnd (e,f) ->
    logop2 (fun e f -> FAnd (e,f)) (&&) false
      (reduce ~default_str env e) (reduce ~default_str env f)
  | FOr (e,f) ->
    logop2 (fun e f -> FOr (e,f)) (||) true
      (reduce ~default_str env e) (reduce ~default_str env f)
  | FNot e ->
    logop1 (fun e -> FNot e) not
      (reduce ~default_str env e)

and reduce ?(default_str=Some "") env e =
  match reduce_aux ~default_str env e with
  | FString s -> FString (expand_string ?default:default_str env s)
  | e -> e

let eval ?default env e = value ?default (reduce env e)

let eval_to_bool ?default env e = value_bool ?default (reduce env e)

let opt_eval_to_bool env opt =
  match opt with
  | None -> true
  | Some e -> value_bool ~default:false (reduce env e)

let eval_to_string ?default env e = value_string ?default (reduce env e)

let ident_of_var v =
  (match OpamVariable.Full.package v with
   | Some p -> [p]
   | None -> []),
  OpamVariable.Full.variable v, None

let ident_of_string s =
  ident_of_var (OpamVariable.Full.of_string s)

let ident_value ?default env id = value ?default (resolve_ident env id)

let ident_string ?default env id = value_string ?default (resolve_ident env id)

let ident_bool ?default env id = value_bool ?default (resolve_ident env id)

(* Substitute the file contents *)
let expand_interpolations_in_file env file =
  let f = OpamFilename.of_basename file in
  let src = OpamFilename.add_extension f "in" in
  let ic = OpamFilename.open_in src in
  let oc = OpamFilename.open_out f in
  let rec aux () =
    match try Some (input_line ic) with End_of_file -> None with
    | Some s ->
      output_string oc (expand_string ~default:"" env s);
      output_char oc '\n';
      aux ()
    | None -> ()
  in
  aux ();
  close_in ic;
  close_out oc

(* Apply filters and interpolations to package commands *)

let arguments env (a,f) =
  if opt_eval_to_bool env f then
    match a with
    | CString s -> Some (expand_string ~default:"" env s)
    | CIdent i  ->
      try
        let fident = filter_ident_of_string i in
        Some (value_string (resolve_ident env fident))
      with Failure msg -> log "ERR in replacement: %s" msg; None
  else
    None

let command env (l, f) =
  if opt_eval_to_bool env f then
    match OpamStd.List.filter_map (arguments env) l with
    | [] -> None
    | l  -> Some l
  else
    None

let commands env l = OpamStd.List.filter_map (command env) l

let single_command env l = OpamStd.List.filter_map (arguments env) l

let simple_arg_variables = function
  | CString s -> string_variables s
  | CIdent i ->
    try fident_variables (filter_ident_of_string i)
    with Failure _ -> []

let filter_opt_variables = function
  | None -> []
  | Some f -> variables f
let argument_variables (a,f) =
  simple_arg_variables a @ filter_opt_variables f
let command_variables (l,f) =
  List.fold_left (fun acc a -> argument_variables a @ acc)
    (filter_opt_variables f) l
let commands_variables l =
  List.fold_left (fun acc c -> command_variables c @ acc) [] l

let rec of_formula atom_f = function
  | Empty -> FBool true
  | Atom at -> atom_f at
  | Block f -> of_formula atom_f f
  | And (a, b) -> FAnd (of_formula atom_f a, of_formula atom_f b)
  | Or (a, b) -> FOr (of_formula atom_f a, of_formula atom_f b)

let filter_constraints env filtered_constraint =
  OpamFormula.partial_eval
    (function
      | Filter flt ->
        if eval_to_bool ~default:false env flt then `True else `False
      | Constraint c -> `Formula (Atom c))
    filtered_constraint

let partial_filter_constraints env filtered_constraint =
  OpamFormula.partial_eval
    (function
      | Filter flt ->
        (match reduce ~default_str:None env flt with
         | FUndef f -> `Formula (Atom (Filter f))
         | FBool true -> `True
         | FBool false -> `False
         | f -> `Formula (Atom (Filter f)))
      | Constraint c -> `Formula (Atom (Constraint c)))
    filtered_constraint

let gen_filter_formula constraints filtered_formula =
  OpamFormula.map
    (fun (name, fc) -> match constraints fc with
       | `True -> Atom (name, Empty)
       | `False -> Empty
       | `Formula c -> Atom (name, c))
    filtered_formula

let filter_formula env ff =
  gen_filter_formula (filter_constraints env) ff

let partial_filter_formula env ff =
  gen_filter_formula (partial_filter_constraints env) ff

let string_of_filtered_formula =
  let string_of_constraint =
    OpamFormula.string_of_formula (function
        | Constraint (op,v) ->
          Printf.sprintf "%s %s"
            (string_of_relop op) (OpamPackage.Version.to_string v)
        | Filter f -> to_string f)
  in
  OpamFormula.string_of_formula (function
      | n, Empty -> OpamPackage.Name.to_string n
      | n, c ->
        let paren = match c with Atom _ -> true | _ -> false in
        Printf.sprintf "%s %s%s%s"
          (OpamPackage.Name.to_string n)
          (if paren then "(" else "")
          (string_of_constraint c)
          (if paren then ")" else ""))

let variables_of_filtered_formula ff =
  OpamFormula.fold_left
    (fun acc (_, f) ->
       OpamFormula.fold_left (fun acc -> function
           | Constraint _ -> acc
           | Filter f -> variables f @ acc)
         acc f)
    [] ff

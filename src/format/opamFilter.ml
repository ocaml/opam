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

let rec to_string = function
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
    Printf.sprintf "%s %s %s"
      (to_string e) (string_of_relop s) (to_string f)
  | FAnd (e,f) -> Printf.sprintf "%s & %s" (to_string e) (to_string f)
  | FOr (e,f)  -> Printf.sprintf "%s | %s" (to_string e) (to_string f)
  | FNot (FBool _ | FString _ | FIdent _ as e) ->
    Printf.sprintf "!%s" (to_string e)
  | FNot e     ->
    Printf.sprintf "!(%s)" (to_string e)
  | FUndef -> "#undefined"

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
  | FUndef ->
    (match default with
     | Some d -> d
     | None -> failwith "Undefined filter value")
  | e -> raise (Invalid_argument ("filter value: "^to_string e))

let value_string ?default = function
  | FBool b -> string_of_bool b
  | FString s -> s
  | FUndef ->
    (match default with
     | Some d -> d
     | None -> failwith "Undefined string filter value")
  | e -> raise (Invalid_argument ("value_string: "^to_string e))

let value_bool ?default = function
  | FBool b -> b
  | FString "true" -> true
  | FString "false" -> false
  | FUndef ->
    (match default with
     | Some d -> d
     | None -> failwith "Undefined boolean filter value")
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
     | None -> FUndef)
  | Some (iftrue, iffalse) ->
    match value_opt >>= bool_of_value with
    | Some true -> FString iftrue
    | Some false -> FString iffalse
    | None -> FString iffalse

(* Resolves ["%{x}%"] string interpolations *)
let expand_string env text =
  let subst str =
    if str = "%%" then "%"
    else if not (OpamStd.String.ends_with ~suffix:"}%" str) then
      (log "ERR: Unclosed variable replacement in %S\n" str;
       str)
    else
    let str = String.sub str 2 (String.length str - 4) in
    resolve_ident env (filter_ident_of_string str)
    |> value_string ~default:""
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

let logop1 op = function
  | FUndef -> FUndef
  | e ->
    try FBool (op (value_bool e))
    with Invalid_argument s -> log "ERR: %s" s; FUndef

let logop2 op absorb e f = match e, f with
  | FUndef, x | x, FUndef -> if x = FBool absorb then x else FUndef
  | f, g ->
    try FBool (op (value_bool f) (value_bool g))
    with Invalid_argument s -> log "ERR: %s" s; FUndef

(* Reduce expressions to values *)

let rec reduce_aux env = function
  | FUndef -> FUndef
  | FBool b -> FBool b
  | FString s -> FString s
  | FIdent i -> resolve_ident env i
  | FOp (e,relop,f) ->
    (match reduce env e, reduce env f with
     | FUndef, _ | _, FUndef -> FUndef
     | e,f ->
       FBool (OpamFormula.check_relop relop
                (OpamVersionCompare.compare (value_string e) (value_string f))))
  | FAnd (e,f) -> logop2 (&&) false (reduce env e) (reduce env f)
  | FOr (e,f) -> logop2 (||) true (reduce env e) (reduce env f)
  | FNot e -> logop1 not (reduce env e)

and reduce env e = match reduce_aux env e with
  | FString s -> FString (expand_string env s)
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
      output_string oc (expand_string env s);
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
    | CString s -> Some (expand_string env s)
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

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

let log ?level fmt =
  OpamGlobals.log "FILTER" ?level fmt
let slog = OpamGlobals.slog

type env = full_variable -> variable_contents option

type fident = name list * variable * (string * string) option

let rec to_string = function
  | FBool b    -> string_of_bool b
  | FString s  -> Printf.sprintf "%S" s
  | FIdent (pkgs,var,converter) ->
    String.concat "+" (List.map OpamPackage.Name.to_string pkgs) ^
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

let variables filter =
  fold_down_left (fun acc -> function
      | FIdent ([],var,_) -> OpamVariable.Full.global var :: acc
      | FIdent (pkgs,var,_) ->
        List.fold_left (fun acc n -> OpamVariable.Full.create n var :: acc)
          acc pkgs
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
  let open OpamMisc.Option.Op in
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
    if not (OpamMisc.ends_with ~suffix:"}%" str) then
      (log "ERR: Unclosed variable replacement in %S\n" str;
       str)
    else
    let str = String.sub str 2 (String.length str - 4) in
    resolve_ident env (filter_ident_of_string str)
    |> value_string ~default:""
  in
  let rex =
    let open Re in
    let notclose =
      rep (seq [opt (char '%'); opt (char '}'); diff notnl (set "}%")])
    in
    compile (seq [str "%{"; notclose; opt (str "}%")])
  in
  Re_pcre.substitute ~rex ~subst text

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
                (Debian.Version.compare (value_string e) (value_string f))))
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
  let p = OpamVariable.Full.package v in
  let var = OpamVariable.Full.variable v in
  if p = OpamPackage.Name.global_config then [], var, None
  else [p], var, None

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
    match OpamMisc.filter_map (arguments env) l with
    | [] -> None
    | l  -> Some l
  else
    None

let commands env l = OpamMisc.filter_map (command env) l

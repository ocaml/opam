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

open OpamStd.Op

open OpamTypes

include OpamCompat

exception Lexer_error of string

let download_map fn = function
  | Up_to_date f    -> Up_to_date (fn f)
  | Result  f       -> Result (fn f)
  | Not_available d -> Not_available d

let download_dir = download_map (fun d -> D d)
let download_file = download_map (fun f -> F f)
let string_of_download = function
  | Up_to_date _ -> "already up-to-date"
  | Result _ -> "synchronized"
  | Not_available _ -> OpamConsole.colorise `red "unavailable"

let string_of_generic_file = function
  | D d -> OpamFilename.Dir.to_string d
  | F f -> OpamFilename.to_string f

let string_of_shell = function
  | `fish -> "fish"
  | `csh  -> "csh"
  | `zsh  -> "zsh"
  | `sh   -> "sh"
  | `bash -> "bash"

let file_null = OpamFilename.of_string ""
let pos_file filename = filename, -1, -1
let pos_null = pos_file file_null

let pos_best (f1,_li1,col1 as pos1) (f2,_li2,_col2 as pos2) =
  if f1 = file_null then pos2
  else if f2 = file_null then pos1
  else if col1 = -1 then pos2
  else pos1

let string_of_pos (file,line,col) =
  OpamFilename.prettify file ^
  if line >= 0 then
    ":" ^ string_of_int line ^
    if col >= 0 then ":" ^ string_of_int col
    else ""
  else ""

(* Command line arguments *)

let url_backend_of_pin_kind = function
  | `version -> None
  | #OpamUrl.backend as k -> Some k

let looks_like_version_re =
  Re.(compile @@
      seq [bos; digit; rep @@ diff any (set "/\\"); eos])

let pin_option_of_string ?kind ?(guess=false) s =
  match kind with
  | Some `version ->
    Version (OpamPackage.Version.of_string s)
  | None when Re.execp looks_like_version_re s ->
    Version (OpamPackage.Version.of_string s)
  | Some (#OpamUrl.backend as backend) ->
    Source (OpamUrl.parse ~backend s)
  | None ->
    let backend =
      if guess then OpamUrl.guess_version_control s
      else None
    in
    Source (OpamUrl.parse ?backend ~handle_suffix:guess s)

let string_of_pin_kind = function
  | `version -> "version"
  | `rsync   -> "path"
  | #OpamUrl.backend as ub -> OpamUrl.string_of_backend ub

let pin_kind_of_string = function
  | "version" -> `version
  | "path"    -> `rsync
  | s -> OpamUrl.backend_of_string s

let string_of_pin_option = function
  | Version v -> OpamPackage.Version.to_string v
  | Source url -> OpamUrl.to_string url

let kind_of_pin_option = function
  | Version _ -> `version
  | Source url -> (url.OpamUrl.backend :> pin_kind)

let string_of_relop = OpamFormula.string_of_relop
let relop_of_string = OpamFormula.relop_of_string

let string_of_logop = function
  | `And -> "&"
  | `Or -> "|"

let logop_of_string = function
  | "&" -> `And
  | "|" -> `Or
  | _ -> raise (Invalid_argument "logop_of_string")

let string_of_pfxop = function
  | `Not -> "!"

let pfxop_of_string = function
  | "!" -> `Not
  | _ -> raise (Invalid_argument "pfxop_of_string")

let string_of_env_update_op = function
  | Eq -> "="
  | PlusEq -> "+="
  | EqPlus -> "=+"
  | EqPlusEq -> "=+="
  | ColonEq -> ":="
  | EqColon -> "=:"

let env_update_op_of_string = function
  | "=" -> Eq
  | "+=" -> PlusEq
  | "=+" -> EqPlus
  | "=+=" -> EqPlusEq
  | ":=" -> ColonEq
  | "=:" -> EqColon
  | _ -> raise (Invalid_argument "env_update_op_of_string")

let env_array l =
  Array.of_list (List.rev_map (fun (k,v, _) -> k^"="^v) l)


let string_of_filter_ident (pkgs,var,converter) =
  OpamStd.List.concat_map ~nil:"" "+" ~right:":"
    OpamPackage.Name.to_string pkgs ^
  OpamVariable.to_string var ^
  (match converter with
   | Some (it,ifu) -> "?"^it^":"^ifu
   | None -> "")

let filter_ident_of_string s =
  match OpamStd.String.rcut_at s ':' with
  | None -> [], OpamVariable.of_string s, None
  | Some (p,last) ->
    let get_names s =
      List.map (fun n ->
          try OpamPackage.Name.of_string n
          with Failure _ -> failwith ("Invalid package name "^n))
        (OpamStd.String.split s '+')
    in
    match OpamStd.String.rcut_at p '?' with
    | None ->
      get_names p, OpamVariable.of_string last, None
    | Some (p,val_if_true) ->
      let converter = Some (val_if_true, last) in
      match OpamStd.String.rcut_at p ':' with
      | None ->
        [], OpamVariable.of_string p, converter
      | Some (packages,var) ->
        get_names packages, OpamVariable.of_string var, converter

let dep_flag_of_string = function
  | "build" -> Depflag_Build
  | "test" -> Depflag_Test
  | "doc" -> Depflag_Doc
  | "dev" -> Depflag_Dev
  | s -> Depflag_Unknown s

let string_of_dep_flag = function
  | Depflag_Build -> "build"
  | Depflag_Test -> "test"
  | Depflag_Doc -> "doc"
  | Depflag_Dev -> "dev"
  | Depflag_Unknown s -> s

let filter_deps ~build ~test ~doc ~dev =
  let filter =
    List.for_all (function
        | Depflag_Build -> build
        | Depflag_Test -> test
        | Depflag_Doc -> doc
        | Depflag_Dev -> dev
        | Depflag_Unknown _ -> true (* ignored *))
  in
  OpamFormula.formula_of_extended ~filter

let string_of_pkg_flag = function
  | Pkgflag_LightUninstall -> "light-uninstall"
  | Pkgflag_AllSwitches -> "all-switches"
  | Pkgflag_Verbose -> "verbose"
  | Pkgflag_Plugin -> "plugin"
  | Pkgflag_Compiler -> "compiler"
  | Pkgflag_Virtual -> "virtual"
  | Pkgflag_Unknown s -> s

let pkg_flag_of_string = function
  | "light-uninstall" -> Pkgflag_LightUninstall
  | "all-switches" -> Pkgflag_AllSwitches
  | "verbose" -> Pkgflag_Verbose
  | "plugin" -> Pkgflag_Plugin
  | "compiler" -> Pkgflag_Compiler
  | s -> Pkgflag_Unknown s

let action_contents = function
  | `Remove p | `Install p | `Reinstall p | `Build p -> p
  | `Change (_,_,p) -> p

let full_action_contents = function
  | `Remove p | `Install p | `Reinstall p | `Build p -> [p]
  | `Change (_,p1,p2) -> [p1; p2]

let map_atomic_action f = function
  | `Remove p -> `Remove (f p)
  | `Install p -> `Install (f p)

let map_highlevel_action f = function
  | #atomic_action as a -> map_atomic_action f a
  | `Change (direction, p1, p2) -> `Change (direction, f p1, f p2)
  | `Reinstall p -> `Reinstall (f p)

let map_concrete_action f = function
  | #atomic_action as a -> map_atomic_action f a
  | `Build p -> `Build (f p)

let map_action f = function
  | #highlevel_action as a -> map_highlevel_action f a
  | #concrete_action as a -> map_concrete_action f a

let string_of_cause to_string =
  let list_to_string l = match List.map to_string l with
    | a::b::c::_::_::_ -> Printf.sprintf "%s, %s, %s, etc." a b c
    | l -> String.concat ", " l in
  function
  | Upstream_changes -> "upstream changes"
  | Use pkgs         -> Printf.sprintf "uses %s" (list_to_string pkgs)
  | Required_by pkgs ->
    Printf.sprintf "required by %s" (list_to_string pkgs)
  | Conflicts_with pkgs ->
    Printf.sprintf "conflicts with %s" (list_to_string pkgs)
  | Requested        -> ""
  | Unknown          -> ""

let map_success f = function
  | Success x -> Success (f x)
  | Conflicts c -> Conflicts c

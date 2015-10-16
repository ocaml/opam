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



(*
let url_kind_of_string = function
  | "http" | "https" | "ftp" | "wget" | "curl" -> `http
  | "file" -> `rsync
  | "git" -> `git
  | "darcs" -> `darcs
  | "hg" -> `hg
  | "local" | "rsync" | "ssh" | "scp" | "sftp" -> `rsync
  | p -> failwith (Printf.sprintf "Unsupported protocol %s" p)

let string_of_url_kind = function
  | `http  -> "http"
  | `rsync -> "file"
  | `git   -> "git"
  | `darcs -> "darcs"
  | `hg    -> "hg"

let unsplit_url (vc, transport, path, _suffix) =
  String.concat "" [
    (match vc with
     | Some vc -> vc ^ "+"
     | None -> "");
    (match transport with
     | Some t -> t ^ "://"
     | _ -> "");
    path
  ]

let string_of_address ?kind (url,hash) =
  let url =
    match kind with
    | None -> url
    | Some k ->
      match split_url url with
      | Some v, _, _, _ | None, Some v, _, _ when url_kind_of_string v = k ->
        url
      | _, Some t, path, _ ->
        unsplit_url (Some (string_of_url_kind k), Some t, path, None)
      | _, None, _, _ ->
        unsplit_url (None, Some (string_of_url_kind k), url, None)
  in
  match hash with
  | None -> url
  | Some c -> Printf.sprintf "%s#%s" url c

let path_of_address (url,_) =
  let _vc, transport, path, suffix = split_url url in
  (* let transport = *)
  (*   match transport with *)
  (*   | Some ("https"|"http"|"ftp" as t) -> Some t *)
  (*   | _ -> None *)
  (* in *)
  unsplit_url (None, transport, path, suffix)

let address_of_string str =
  let addr, hash =
    match OpamStd.String.cut_at str '#' with
    | None -> str, None
    | Some (addr,hash) -> addr, Some hash
  in
  let vc, transport, path, suffix = split_url addr in
  let path =
    match transport with
    | Some tr when url_kind_of_string tr = `rsync -> OpamSystem.real_path path
    | Some _ -> path
    | None -> OpamSystem.real_path path
  in
  unsplit_url (vc, transport, path, suffix), hash

let parse_url (s,c) =
  match split_url s with
  | Some vc, Some transport, path, _ ->
    (Printf.sprintf "%s://%s" transport path, c), url_kind_of_string vc
  | None, Some ("git"|"hg"|"darcs" as t), path, _ ->
    (* VC without specified transport *)
    let kind = url_kind_of_string t in
    (if Re.(execp (compile @@ seq [rep1 any; str "://"])) path then
       (path, c), kind
     else if
       Re.(execp (compile @@ seq [repn (diff any (char '/')) 2 None; char ':']))
         path
     then
       (* Assume ssh for "host:..." (like git does) *)
       ("ssh://" ^ path, c), kind
     else
       ("file://" ^ OpamSystem.real_path path, c), `rsync)
  | None, _, _, Some ("git"|"hg"|"darcs" as suffix) ->
    (s,c), url_kind_of_string suffix
  | None, Some("file"|"rsync"|"ssh"|"scp"|"sftp"), path, _ ->
    (path, c), `rsync (* strip the leading xx:// *)
  | None, Some t, path, _ ->
    let kind = url_kind_of_string t in
    (match split_url path with
     | _, Some _, _, _ -> (path,c), kind
     | _, None, _, _ -> ("http://" ^ path, c), kind) (* assume http transport *)
  | None, None, _, _ ->
    (s, c), `rsync
  | Some _, None, _, _ -> assert false

let string_of_repository_kind = string_of_url_kind

let repository_kind_of_string = url_kind_of_string
*)

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
      seq [digit; rep @@ diff any (set "/\\"); eos])

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
    Source (OpamUrl.parse ?backend s)

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

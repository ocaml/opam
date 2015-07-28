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

let guess_version_control dir =
  let open OpamFilename in
  let open Op in
  if exists_dir (dir / ".git") then Some`git else
  if exists_dir (dir / ".hg") then Some `hg else
  if exists_dir (dir / "_darcs") then Some `darcs else
    None

let url_kind_of_string = function
  | "http" | "https" | "ftp" | "wget" | "curl" -> `http
  | "file" -> `local
  | "git" -> `git
  | "darcs" -> `darcs
  | "hg" -> `hg
  | "local" | "rsync" | "ssh" | "scp" | "sftp" -> `local
  | p -> failwith (Printf.sprintf "Unsupported protocol %s" p)

let string_of_url_kind = function
  | `http  -> "http"
  | `local -> "file"
  | `git   -> "git"
  | `darcs -> "darcs"
  | `hg    -> "hg"

let split_url =
  let re =
    let (@@) f x = f x in
    Re.(compile @@ seq [
        opt @@ seq [
          opt @@ seq [group @@ rep1 @@ diff any (set "+:"); char '+'];
          group @@ rep1 @@ diff any (char ':');
          str "://"
        ];
        group @@ seq [
          non_greedy @@ rep1 any;
          opt @@ seq [ char '.'; group @@ rep1 @@ diff any (char '.')]
        ];
        eos;
      ])
  in
  fun u ->
    match Re.get_all (Re.exec re u) with
    | [|_;vc;transport;path;suffix|] ->
      let opt = function "" -> None | s -> Some s in
      opt vc, opt transport, path, opt suffix
    | _ -> assert false

let string_of_address ?kind (url,hash) =
  let url =
    match kind with
    | None -> url
    | Some k ->
      match split_url url with
      | Some v, _, _, _ | None, Some v, _, _ when url_kind_of_string v = k ->
        url
      | _, Some t, path, _ ->
        Printf.sprintf "%s+%s://%s" (string_of_url_kind k) t path
      | _, None, _, _ ->
        Printf.sprintf "%s://%s" (string_of_url_kind k) url
  in
  match hash with
  | None -> url
  | Some c -> Printf.sprintf "%s#%s" url c

let address_of_string str =
  let addr, hash =
    match OpamStd.String.cut_at str '#' with
    | None -> str, None
    | Some (addr,hash) -> addr, Some hash
  in
  match split_url addr with
  | _, Some vc, _, _ when url_kind_of_string vc <> `local -> addr, hash
  | _, Some local, path, _ ->
    Printf.sprintf "%s://%s" local (OpamSystem.real_path path), hash
  | _, None, path, _ -> OpamSystem.real_path path, hash

let parse_url (s,c) =
  match split_url s with
  | Some vc, Some transport, path, _ ->
    (Printf.sprintf "%s://%s" transport path, c), url_kind_of_string vc
  | None, Some "git", _, _ ->
    (s,c), `git
  | None, _, _, Some ("git"|"hg"|"darcs" as suffix) ->
    (s,c), url_kind_of_string suffix
  | None, Some("file"|"rsync"|"ssh"|"scp"|"sftp"), path, _ ->
    (path, c), `local (* strip the leading xx:// *)
  | None, Some t, path, _ ->
    let kind = url_kind_of_string t in
    (match split_url path with
     | _, Some _, _, _ -> (path,c), kind
     | _, None, _, _ -> ("http://" ^ path, c), kind) (* assume http transport *)
  | None, None, _, _ ->
    (s, c), `local
  | Some _, None, _, _ -> assert false

let string_of_repository_kind = string_of_url_kind

let repository_kind_of_string = url_kind_of_string

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

let string_of_upload u =
  Printf.sprintf "opam=%s descr=%s archive=%s"
    (OpamFilename.to_string u.upl_opam)
    (OpamFilename.to_string u.upl_descr)
    (OpamFilename.to_string u.upl_archive)

let repository_kind_of_pin_kind = function
  | `version -> None
  | (`http|`git|`darcs|`hg|`local as k) -> Some k

let pin_of_url (url,kind) = match kind with
  | `http -> Http url
  | `git -> Git url
  | `darcs -> Darcs url
  | `hg -> Hg url
  | `local -> Local (OpamFilename.Dir.of_string (fst url))

let url_of_pin = function
  | Http u -> u, `http
  | Git u -> u, `git
  | Darcs u -> u, `darcs
  | Hg u -> u, `hg
  | Local d -> (OpamFilename.Dir.to_string d, None), `local
  | Version _ -> failwith "Not a source pin"

let pin_option_of_string ?kind ?(guess=false) s =
  match kind with
  | Some `version -> Version (OpamPackage.Version.of_string s)
  | None | Some (`http|`git|`hg|`darcs|`local) ->
    if kind = None &&
       not (String.contains s (Filename.dir_sep.[0])) &&
       String.length s > 0 && '0' <= s.[0] && s.[0] <= '9' then
      Version (OpamPackage.Version.of_string s)
    else
    let s, k = parse_url (address_of_string s) in
    match kind, k with
    | Some `version, _ | None, `version -> assert false
    | Some `http, _ | None, `http -> Http s
    | Some `git, _ | None, `git -> Git s
    | Some `hg, _ | None, `hg   -> Hg s
    | Some `darcs, _ | None, `darcs -> Darcs s
    | Some `local, _ ->
      Local (OpamFilename.Dir.of_string (fst s))
    | None, `local ->
      let dir = OpamFilename.Dir.of_string (fst s) in
      if guess then match guess_version_control dir with
        | Some vc -> pin_of_url (s, vc)
        | None -> Local dir
      else Local dir

let string_of_pin_kind = function
  | `version -> "version"
  | `git     -> "git"
  | `darcs   -> "darcs"
  | `hg      -> "hg"
  | `http    -> "http"
  | `local   -> "path"

let pin_kind_of_string = function
  | "version" -> `version
  | "http"    -> `http
  | "git"     -> `git
  | "darcs"   -> `darcs
  | "hg"      -> `hg
  | "rsync"
  | "local"
  | "path"    -> `local
  | s -> OpamConsole.error_and_exit "%s is not a valid kind of pinning." s

let string_of_pin_option = function
  | Version v -> OpamPackage.Version.to_string v
  | Http p
  | Git p
  | Darcs p
  | Hg p      -> string_of_address p
  | Local p   -> OpamFilename.Dir.to_string p

let kind_of_pin_option = function
  | Version _ -> `version
  | Http _    -> `http
  | Git _     -> `git
  | Darcs _   -> `darcs
  | Hg _      -> `hg
  | Local _   -> `local

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

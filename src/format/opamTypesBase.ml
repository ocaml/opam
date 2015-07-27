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

let string_of_address = function
  | url, None   -> url
  | url, Some c -> Printf.sprintf "%s#%s" url c

let address_of_string str =
  match OpamStd.String.cut_at str '#' with
  | None       -> OpamSystem.real_path str, None
  | Some (a,c) -> OpamSystem.real_path a, Some c

let guess_version_control dir =
  let open OpamFilename in
  let open Op in
  if exists_dir (dir / ".git") then Some`git else
  if exists_dir (dir / ".hg") then Some `hg else
  if exists_dir (dir / "_darcs") then Some `darcs else
    None

let parse_url (s,c) =
  let url_kind_of_string = function
    | "http" | "https" | "ftp" -> `http
    | "file" -> `local
    | "git" -> `git
    | "darcs" -> `darcs
    | "hg" -> `hg
    | "rsync" | "ssh" | "scp" | "sftp" -> `local
    | p -> raise (Invalid_argument (Printf.sprintf "Unsupported protocol %s" p))
  in
  let suffix =
    try let n = String.rindex s '.' in String.sub s (n+1) (String.length s-n-1)
    with Not_found -> ""
  in
  match suffix with
  | "git" -> (s,c), `git
  | "hg" -> (s,c), `hg
  | _ ->
    let urlsplit = "://" in
    match Re_str.bounded_split (Re_str.regexp_string urlsplit) s 2 with
    | ["file"|"rsync"|"ssh"|"scp"|"sftp"; address] ->
      (* strip the leading xx:// *)
      (address,c), `local
    | [proto; address] ->
      (match OpamStd.String.cut_at proto '+' with
       | Some (proto1,proto2) ->
         (proto2^urlsplit^address, c), url_kind_of_string proto1
       | None ->
         let addr = match proto with
           | "git" -> s (* git:// urls legit *)
           | _ ->
             if Re_str.string_match (Re_str.regexp (".*"^urlsplit)) address 0
             then address
             else "http://" ^ address (* assume http transport by default *)
         in
         (addr,c), url_kind_of_string proto)
    | [address] -> (address,c), `local
    | _ -> raise (Invalid_argument (Printf.sprintf "Bad url format %S" s))

let string_of_repository_kind = function
  | `http  -> "http"
  | `local -> "local"
  | `git   -> "git"
  | `darcs -> "darcs"
  | `hg    -> "hg"

let repository_kind_of_string = function
  | "wget"
  | "curl"
  | "http"  -> `http
  | "rsync"
  | "local" -> `local
  | "git"   -> `git
  | "darcs" -> `darcs
  | "hg"    -> `hg
  | s -> OpamConsole.error_and_exit "%s is not a valid repository kind." s

let string_of_shell = function
  | `fish -> "fish"
  | `csh  -> "csh"
  | `zsh  -> "zsh"
  | `sh   -> "sh"
  | `bash -> "bash"

let pos_null = OpamFilename.of_string "",-1,-1

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
  | `local | `version -> failwith "Not a recognised version-control URL"

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

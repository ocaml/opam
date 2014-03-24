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

let download_map fn = function
  | Up_to_date f    -> Up_to_date (fn f)
  | Result  f       -> Result (fn f)
  | Not_available d -> Not_available d

let download_dir = download_map (fun d -> D d)
let download_file = download_map (fun f -> F f)

let string_of_address = function
  | url, None   -> url
  | url, Some c -> Printf.sprintf "%s#%s" url c

let address_of_string str =
  match OpamMisc.cut_at str '#' with
  | None       -> OpamSystem.real_path str, None
  | Some (a,c) -> OpamSystem.real_path a, Some c

let guess_repository_kind kind (address, ext) =
  match kind with
  | Some k -> k
  | None   ->
    if ext = None && Sys.file_exists address then
      `local
    else
      if OpamMisc.starts_with ~prefix:"git" address
      || OpamMisc.ends_with ~suffix:"git" address then
        `git
      else if OpamMisc.starts_with ~prefix:"hg" address then
        `hg
      else
        `http

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
  | s -> OpamGlobals.error_and_exit "%s is not a valid repository kind." s

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
  | (`git|`darcs|`hg|`local as k) -> Some k

let pin_option_of_string ?kind s =
  match kind with
  | Some `version -> Version (OpamPackage.Version.of_string s)
  | Some `git     -> Git (address_of_string s)
  | Some `hg      -> Hg (address_of_string s)
  | Some `darcs   -> Darcs (address_of_string s)
  | Some `local   -> Local (OpamFilename.Dir.of_string s)
  | None          ->
    if Sys.file_exists s then
      Local (OpamFilename.Dir.of_string s)
    else if OpamMisc.contains s ('/') then
      Git (address_of_string s)
    else
      Version (OpamPackage.Version.of_string s)

let string_of_pin_kind = function
  | `version -> "version"
  | `git     -> "git"
  | `darcs   -> "darcs"
  | `hg      -> "hg"
  | `local   -> "local"

let pin_kind_of_string = function
  | "version" -> `version
  | "git"     -> `git
  | "darcs"   -> `darcs
  | "hg"      -> `hg
  | "rsync"
  | "local"   -> `local
  | s -> OpamGlobals.error_and_exit "%s is not a valid kind of pinning." s

let string_of_pin_option = function
  | Version v -> OpamPackage.Version.to_string v
  | Git p
  | Darcs p
  | Hg p      -> string_of_address p
  | Local p   -> OpamFilename.Dir.to_string p

let kind_of_pin_option = function
  | Version _ -> `version
  | Git _     -> `git
  | Darcs _   -> `darcs
  | Hg _      -> `hg
  | Local _   -> `local

let option fn = function
  | None   -> ""
  | Some k -> fn k

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

let rec string_of_filter = function
  | FBool b    -> string_of_bool b
  | FString s  -> Printf.sprintf "%S" s
  | FIdent i   -> i
  | FOp(e,s,f) ->
    Printf.sprintf "%s %s %s"
      (string_of_filter e) (string_of_relop s) (string_of_filter f)
  | FAnd (e,f) -> Printf.sprintf "%s & %s" (string_of_filter e) (string_of_filter f)
  | FOr (e,f)  -> Printf.sprintf "%s | %s" (string_of_filter e) (string_of_filter f)
  | FNot e     -> Printf.sprintf "!%s" (string_of_filter e)

let action_contents = function
  | To_change (_, p)
  | To_recompile p
  | To_delete p -> p

let full_action_contents = function
  | To_change (Some p1, p2) -> [p1; p2]
  | To_change (None, p) | To_recompile p | To_delete p -> [p]

let string_of_cause to_string =
  let list_to_string l = String.concat ", " (List.map to_string l) in
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

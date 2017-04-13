(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

include OpamCompat

let std_path_of_string = function
  | "prefix" -> Prefix
  | "lib" -> Lib
  | "bin" -> Bin
  | "sbin" -> Sbin
  | "share" -> Share
  | "doc" -> Doc
  | "etc" -> Etc
  | "man" -> Man
  | "toplevel" -> Toplevel
  | "stublibs" -> Stublibs
  | _ -> failwith "Wrong standard path"

let string_of_std_path = function
  | Prefix -> "prefix"
  | Lib -> "lib"
  | Bin -> "bin"
  | Sbin -> "sbin"
  | Share -> "share"
  | Doc -> "doc"
  | Etc -> "etc"
  | Man -> "man"
  | Toplevel -> "toplevel"
  | Stublibs -> "stublibs"

let all_std_paths =
  [ Prefix; Lib; Bin; Sbin; Share; Doc; Etc; Man; Toplevel; Stublibs ]

let string_of_shell = function
  | `fish -> "fish"
  | `csh  -> "csh"
  | `zsh  -> "zsh"
  | `sh   -> "sh"
  | `bash -> "bash"

let file_null = ""
let pos_file filename = OpamFilename.to_string filename, -1, -1
let pos_null = file_null, -1, -1

let pos_best (f1,_li1,col1 as pos1) (f2,_li2,_col2 as pos2) =
  if f1 = file_null then pos2
  else if f2 = file_null then pos1
  else if col1 = -1 then pos2
  else pos1

let string_of_pos (file,line,col) =
  file ^
  if line >= 0 then
    ":" ^ string_of_int line ^
    if col >= 0 then ":" ^ string_of_int col
    else ""
  else ""

let string_of_user_action = function
  | Query -> "query"
  | Install -> "install"
  | Upgrade -> "upgrade"
  | Reinstall -> "reinstall"
  | Remove -> "remove"
  | Switch -> "switch"
  | Import -> "import"

(* Command line arguments *)

let env_array l =
  (* The env list may contain successive bindings of the same variable, make
     sure to keep only the last *)
  let bindings =
    List.fold_left (fun acc (k,v,_) -> OpamStd.String.Map.add k v acc)
      OpamStd.String.Map.empty l
  in
  let a = Array.make (OpamStd.String.Map.cardinal bindings) "" in
  OpamStd.String.Map.fold
    (fun k v i -> a.(i) <- String.concat "=" [k;v]; succ i)
    bindings 0
  |> ignore;
  a


let string_of_filter_ident (pkgs,var,converter) =
  OpamStd.List.concat_map ~nil:"" "+" ~right:":"
    (function None -> "_" | Some n -> OpamPackage.Name.to_string n) pkgs ^
  OpamVariable.to_string var ^
  (match converter with
   | Some (it,ifu) -> "?"^it^":"^ifu
   | None -> "")

let filter_ident_of_string s =
  match OpamStd.String.rcut_at s ':' with
  | None -> [], OpamVariable.of_string s, None
  | Some (p,last) ->
    let get_names s =
      List.map
        (function "_" -> None | s -> Some (OpamPackage.Name.of_string s))
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

let all_package_flags = [
  Pkgflag_LightUninstall;
  (* Pkgflag_AllSwitches; This has no "official" existence yet and does
     nothing *)
  Pkgflag_Verbose;
  Pkgflag_Plugin;
  Pkgflag_Compiler;
  Pkgflag_Conf;
]

let string_of_pkg_flag = function
  | Pkgflag_LightUninstall -> "light-uninstall"
  | Pkgflag_Verbose -> "verbose"
  | Pkgflag_Plugin -> "plugin"
  | Pkgflag_Compiler -> "compiler"
  | Pkgflag_Conf -> "conf"
  | Pkgflag_Unknown s -> s

let pkg_flag_of_string = function
  | "light-uninstall" -> Pkgflag_LightUninstall
  | "verbose" -> Pkgflag_Verbose
  | "plugin" -> Pkgflag_Plugin
  | "compiler" -> Pkgflag_Compiler
  | "conf" -> Pkgflag_Conf
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

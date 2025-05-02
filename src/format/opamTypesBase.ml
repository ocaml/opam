(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamParserTypes.FullPos
open OpamTypes

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
  | SH_fish -> "fish"
  | SH_csh  -> "csh"
  | SH_zsh  -> "zsh"
  | SH_sh   -> "sh"
  | SH_bash -> "bash"
  | SH_pwsh Powershell_pwsh -> "pwsh"
  | SH_pwsh Powershell -> "powershell"
  | SH_cmd -> "cmd"

let file_null = ""
let pos_file filename =
  { filename = OpamFilename.to_string filename;
    start = -1, -1;
    stop = -1, -1;
  }
let pos_null =
  { filename = file_null;
    start = -1, -1;
    stop = -1, -1;
  }
let nullify_pos pelem = {pelem; pos = pos_null}
let nullify_pos_map f {pelem; pos = _} = nullify_pos (f pelem)

let rec nullify_pos_value {pelem; pos = _} =
  nullify_pos @@
  match pelem with
  | Bool b -> Bool (b : bool)
  | Int i -> Int (i : int)
  | String s -> String (s : string)
  | Relop (relop, v1, v2) ->
    Relop
      (nullify_pos_map
         (fun (x : OpamParserTypes.FullPos.relop_kind) -> x)
         relop,
       nullify_pos_value v1,
       nullify_pos_value v2)
  | Prefix_relop (relop, v) ->
    Prefix_relop
      (nullify_pos_map
         (fun (x : OpamParserTypes.FullPos.relop_kind) -> x)
         relop,
       nullify_pos_value v)
  | Logop (logop, v1, v2) ->
    Logop
      (nullify_pos_map
         (fun (x : OpamParserTypes.FullPos.logop_kind) -> x)
         logop,
       nullify_pos_value v1,
       nullify_pos_value v2)
  | Pfxop (pfxop, v) ->
    Pfxop
      (nullify_pos_map
         (fun (x : OpamParserTypes.FullPos.pfxop_kind) -> x)
         pfxop,
       nullify_pos_value v)
  | Ident s -> Ident (s : string)
  | List {pelem = l; pos = _} ->
    List (nullify_pos (List.map nullify_pos_value l))
  | Group {pelem = l; pos = _} ->
    Group (nullify_pos (List.map nullify_pos_value l))
  | Option (v, {pelem = filter; pos = _}) ->
    Option
      (nullify_pos_value v,
       nullify_pos (List.map nullify_pos_value filter))
  | Env_binding (v1, env_update_op, v2) ->
    Env_binding
      (nullify_pos_value v1,
       nullify_pos_map
         (fun (x : OpamParserTypes.FullPos.env_update_op_kind) -> x)
         env_update_op,
       nullify_pos_value v2)

(* XXX update *)
let pos_best pos1 pos2 =
  match pos1, pos2 with
  | { filename = ""; _ }, _ -> pos2
  | _, { filename = ""; _ } -> pos1
  | { start = (-1,_) ; _ }, _ -> pos2
  | _, { start = (-1,_) ; _ } -> pos1
  | _, _ -> pos1

(*
  if f1 = file_null then pos2
  else if f2 = file_null then pos1
  else if col1 = -1 then pos2
  else pos1
*)

let string_of_pos pos =
  let check x = if x >= 0 then string_of_int x else "-" in
  Printf.sprintf "%s:%s:%s-%s:%s:"
    pos.filename
    (check (fst pos.start))
    (check (snd pos.start))
    (check (fst pos.stop))
    (check (snd pos.stop))

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
    List.fold_left (fun acc (k,v,_) -> OpamStd.Env.Name.Map.add k v acc)
      OpamStd.Env.Name.Map.empty l
  in
  let a = Array.make (OpamStd.Env.Name.Map.cardinal bindings) "" in
  OpamStd.Env.Name.Map.fold
    (fun k v i -> a.(i) <- (k :> string) ^ "=" ^ v; succ i)
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

exception Parse_variable of string * string

let filter_ident_of_string_t ~interpolation ?(accept=true) s =
  match OpamStd.String.rcut_at s ':' with
  | None -> [], OpamVariable.of_string s, None
  | Some (p,last) ->
    let get_names s =
      List.filter_map
        (function
          | "_" -> Some None
          | "" -> if accept then None else raise (Parse_variable (s, last))
          | s -> Some ( Some (OpamPackage.Name.of_string s)))
        (OpamStd.String.split_delim s '+')
    in
    match OpamStd.String.rcut_at p '?' with
    | None ->
      get_names p, OpamVariable.of_string last, None
    | Some ("",val_if_true) when interpolation ->
      (* TODO: Remove in opam 3.0.
         Hack added in opam 2.2. This is a compatible syntax with opam 2.0 and
         2.1 but supports + in the package name.  See
         https://github.com/ocaml/opam-file-format/issues/59 *)
      (match OpamStd.String.rcut_at val_if_true ':' with
       | None ->
         (* behaviour from opam 2.0 and 2.1 *)
         [], OpamVariable.of_string last, None
       | Some (p, var) ->
         [Some (OpamPackage.Name.of_string p)],
         OpamVariable.of_string var, None)
    | Some (p,val_if_true) ->
      let converter = Some (val_if_true, last) in
      match OpamStd.String.rcut_at p ':' with
      | None ->
        [], OpamVariable.of_string p, converter
      | Some (packages,var) ->
        get_names packages, OpamVariable.of_string var, converter

let filter_ident_of_string s =
  filter_ident_of_string_t ~interpolation:false ~accept:true s

let filter_ident_of_string_interp ?accept s =
  filter_ident_of_string_t ~interpolation:true ?accept s

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
  | Pkgflag_AvoidVersion -> "avoid-version"
  | Pkgflag_Deprecated -> "deprecated"
  | Pkgflag_Explicit -> "explicit"
  | Pkgflag_Unknown s -> s

let pkg_flag_of_string = function
  | "light-uninstall" -> Pkgflag_LightUninstall
  | "verbose" -> Pkgflag_Verbose
  | "plugin" -> Pkgflag_Plugin
  | "compiler" -> Pkgflag_Compiler
  | "conf" -> Pkgflag_Conf
  | "avoid-version" -> Pkgflag_AvoidVersion
  | "deprecated" -> Pkgflag_Deprecated
  | "explicit" -> Pkgflag_Explicit
  | s -> Pkgflag_Unknown s

let action_contents = function
  | `Remove p | `Install p | `Reinstall p | `Build p
  | `Change (_,_,p) -> [p]
  | `Fetch pl -> pl

let full_action_contents = function
  | `Change (_,p1,p2) -> [p1; p2]
  | a -> action_contents a

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
  | `Fetch pl -> `Fetch (List.map f pl)

let map_action f = function
  | #highlevel_action as a -> map_highlevel_action f a
  | #concrete_action as a -> map_concrete_action f a

let string_of_cause to_string =
  let list_to_string l = match List.map to_string l with
    | a::b::c::_::_::_ -> Printf.sprintf "%s, %s, %s, etc." a b c
    | l -> String.concat ", " l in
  function
  | Upstream_changes -> OpamConsole.colorise `bold "upstream or system changes"
  | Use pkgs         -> Printf.sprintf "uses %s" (list_to_string pkgs)
  | Required_by pkgs ->
    Printf.sprintf "required by %s" (list_to_string pkgs)
  | Conflicts_with pkgs ->
    Printf.sprintf "conflicts with %s" (list_to_string pkgs)
  | Requested        -> ""
  | Unavailable      -> OpamConsole.colorise `bold "no longer available"
  | Unknown          -> ""

let map_success f = function
  | Success x -> Success (f x)
  | Conflicts c -> Conflicts c

let iter_success f = function
  | Success x -> f x
  | Conflicts _ -> ()

(** Environment update helpers *)
let env_update ?comment:envu_comment ~rewrite:envu_rewrite
    envu_var envu_op envu_value =
  { envu_var; envu_op; envu_value; envu_comment; envu_rewrite }

let env_update_resolved ?comment:envu_comment ?rewrite
    envu_var envu_op envu_value =
  { envu_var; envu_op; envu_value; envu_comment;
    envu_rewrite = OpamStd.Option.default (Some (SPF_Resolved None)) rewrite;
  }

let env_update_unresolved ?comment:envu_comment ?rewrite
    envu_var envu_op envu_value =
  { envu_var; envu_op; envu_value; envu_comment;
    envu_rewrite =
      OpamStd.Option.default (Some (SPF_Unresolved (Empty, Empty)))
        rewrite;
  }

external op_of_raw:
  OpamParserTypes.FullPos.env_update_op_kind
    -> euok_writeable env_update_op_kind = "%identity"
external raw_of_op:
  euok_writeable env_update_op_kind
    -> OpamParserTypes.FullPos.env_update_op_kind = "%identity"
(* cf. tests/lib/typeGymnastics.ml *)

(** Environment update path transformers functions *)
let string_of_path_format = function
  | Host -> "host"
  | Target -> "target"
  | Target_quoted -> "target-quoted"
  | Host_quoted -> "host-quoted"

let char_of_separator = function
  | SSemiColon -> ';'
  | SColon -> ':'

let switch_selections_compare x
    {sel_installed; sel_roots; sel_compiler; sel_pinned} =
  let cmp = OpamPackage.Set.compare x.sel_installed sel_installed in
  if cmp <> 0 then cmp else
    let cmp = OpamPackage.Set.compare x.sel_roots sel_roots in
    if cmp <> 0 then cmp else
      let cmp = OpamPackage.Set.compare x.sel_compiler sel_compiler in
      if cmp <> 0 then cmp else
        OpamPackage.Set.compare x.sel_pinned sel_pinned

let switch_selections_equal x y =
  switch_selections_compare x y = 0

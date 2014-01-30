(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)


(* open BuildBase *)
(* open Stdlib2 *)
open StringSubst

let global_subst = empty_subst ()

let add_to_subst env var vv =
(*  Printf.eprintf "BuildSubst.add %S -> %S\n%!" v vv; *)
  add_to_subst env  (Printf.sprintf "%%{%s}%%" var) vv

let add_to_global_subst var var_value =
  add_to_subst global_subst var var_value

let _ =
  Array.iter (fun s ->
    let var, var_value = OcpString.cut_at s '=' in
    add_to_global_subst var var_value;
  ) (MinUnix.environment ())

let putenv var var_value =
  MinUnix.putenv var var_value;
  add_to_global_subst var var_value

let subst env_subst s =
  let ss = snd (iter_subst env_subst s) in
(*  Printf.eprintf "BuildSubst.subst %S -> %S\n%!" s ss; *)
  ss

let subst_global = subst global_subst

let add_to_local_subst env var vv =
  add_to_copy env  (Printf.sprintf "%%{%s}%%" var) vv

let create_substituter list =
  let subst = M.empty_subst () in
  List.iter (fun (name, f) ->
    M.add_to_subst subst ("%{" ^ name ^ "}%") f
  ) list;
  subst

let apply_substituter subst s info =
  let _, s1 = M.iter_subst subst s info in
(*  Printf.eprintf "apply_substituter: %S -> %S\n%!" s s1; *)
  s1


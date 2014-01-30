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
open MetaTypes

let empty () = {
  meta_version = None;
  meta_description = None;
  meta_exists_if = [];
  meta_directory = None;
  meta_preprocessor = None;
  meta_name = None;
  meta_linkopts = None;
  meta_license = None;
(*  meta_browse_interfaces = []; *)

  meta_error = StringMap.empty;
  meta_requires = StringMap.empty;
  meta_archive = StringMap.empty;

  meta_package = [];
}

let key_of_preds preds =
  let preds = List.map (fun (s, bool) ->
    if bool then s else "-" ^ s) preds in
  String.concat ", " preds

let add_requires meta preds values =
  let key = key_of_preds preds in
  try
    let var = StringMap.find key meta.meta_requires in
    var.metavar_value <- var.metavar_value @ values
  with Not_found ->
    let var = {
      metavar_key = key;
      metavar_preds = preds;
      metavar_value = values;
    } in
    meta.meta_requires <- StringMap.add key var meta.meta_requires

let add_archive meta preds values =
  let key = key_of_preds preds in
  try
    let var = StringMap.find key meta.meta_archive in
    var.metavar_value <- var.metavar_value @ values
  with Not_found ->
    let var = {
      metavar_key = key;
      metavar_preds = preds;
      metavar_value = values;
    } in
    meta.meta_archive <- StringMap.add key var meta.meta_archive

let fprintf_option_field oc indent name field =
    match field with
      None -> ()
    | Some s ->
      Printf.fprintf oc "%s%s = %S\n" indent name s

let fprintf_list_field oc indent name field =
    match field with
      [] -> ()
    |  s ->
      Printf.fprintf oc "%s%s = %S\n" indent name (String.concat ", " s)

let fprintf_entries oc indent name entries =
  StringMap.iter (fun _ var ->
    Printf.fprintf oc "%s%s%s = %S\n"
      indent name
      (if var.metavar_key = "" then "" else
          Printf.sprintf "(%s)" var.metavar_key)
      (String.concat " " var.metavar_value)
  ) entries

let create_meta_file filename meta =
  let oc = open_out filename in
  let rec fprintf_meta oc indent meta =
    fprintf_option_field oc indent "version" meta.meta_version;
    fprintf_option_field oc indent "description" meta.meta_description;
    fprintf_option_field oc indent "name" meta.meta_name;
    fprintf_option_field oc indent "directory" meta.meta_directory;
    fprintf_option_field oc indent "license" meta.meta_license;
    fprintf_option_field oc indent "preprocessor" meta.meta_preprocessor;
    fprintf_option_field oc indent "linkopts" meta.meta_linkopts;
    fprintf_entries oc indent "requires" meta.meta_requires;
    fprintf_entries oc indent "archive" meta.meta_archive;
    fprintf_list_field oc indent "exists_if" meta.meta_exists_if;
    List.iter (fun (name, meta) ->
      Printf.fprintf oc "%spackage %S (\n" indent name;
      fprintf_meta oc (indent ^ "  ") meta;
      Printf.fprintf oc "%s)\n" indent;
    ) meta.meta_package
  in
  fprintf_meta oc "" meta;
  close_out oc

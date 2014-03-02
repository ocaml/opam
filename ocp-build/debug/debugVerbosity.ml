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


module StringMap = Map.Make(String)
module StringSet = Set.Make(String)


type verbosity = {
  mutable verbosity : int;
  mutable submodules : StringSet.t;
}

let split s c =
  let len = String.length s in
  let rec iter pos =
    if pos >= len then [] else
    try
      let pos2 = String.index_from s pos c in
      (String.sub s pos (pos2-pos)) :: iter (pos2+1)
    with Not_found -> [String.sub s pos (len-pos)]
  in
  iter 0

let modules = ref StringMap.empty

let get_verbosity name =
  try StringMap.find name !modules with
      Not_found ->
        let verbosity = {
          verbosity = 0;
          submodules = StringSet.empty;
        } in
        modules := StringMap.add name verbosity !modules;
        verbosity

let debug_verbose =
  let v = get_verbosity "DebugVerbosity" in
  (function n -> v.verbosity >= n)

let rec increase_verbosity modname n =
  if debug_verbose 1 then
    Printf.eprintf "increase_verbosity %s to %d\n%!" modname n;
  try
    let v = StringMap.find modname !modules in
    if v.verbosity < n then begin
      v.verbosity <- n;
      StringSet.iter (fun modname ->
        increase_verbosity modname n) v.submodules
    end
  with Not_found ->
    let v = {
      verbosity = n;
      submodules = StringSet.empty;
    } in
    modules := StringMap.add modname v !modules

let string_before s pos = String.sub s 0 pos
let string_after s pos =
  let len = String.length s in
  String.sub s pos (len-pos)

let string_cut_at s c =
  try
    let pos = String.index s c in
    string_before s pos, string_after s (pos+1)
  with _ -> s, ""

let _ =
  let args =
    try
      split (Sys.getenv "OCP_DEBUG_MODULES") ':'
    with Not_found -> []
  in
  List.iter (fun s ->
    let modname,n = string_cut_at s '=' in
    let n = try
              int_of_string n
      with _ ->
        (-1)
    in
    if n < 0 then
      let v = get_verbosity modname in
      increase_verbosity modname (v.verbosity + 1)
    else
      increase_verbosity modname n
  ) args

let verbosity modname = get_verbosity modname
let increase_verbosities modnames n =
  List.iter (fun modname -> increase_verbosity modname n) modnames

let add_submodules modname modnames =
  let v = get_verbosity modname in
  List.iter (fun modname ->
    if not (StringSet.mem modname v.submodules) then begin
      v.submodules <- StringSet.add modname v.submodules;
      if v.verbosity > 0 then
        increase_verbosity modname v.verbosity
    end
  ) modnames

let verbose modnames modname =
  List.iter (fun m ->
    add_submodules m [modname]
  ) modnames;
  let v = get_verbosity modname in
  if debug_verbose 1 then
    Printf.eprintf "DebugVerbosity.of_module %S = %d\n%!" modname v.verbosity;
  (function n -> v.verbosity >= n)




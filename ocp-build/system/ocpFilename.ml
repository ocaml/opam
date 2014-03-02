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

include Filename

open OcpLang

let (/) = concat

let get_extension filename =
  let filename = basename filename in
  try
    let n = String.rindex filename '.' in
    Some (String.after filename n)
  with Not_found ->
    None

let suffix = get_extension

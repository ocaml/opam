(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open OpamMisc.OP

let log fmt = OpamGlobals.log "COMPILER" fmt

module Version = struct

  include OpamMisc.Base

  type constr = (OpamFormula.relop * t) OpamFormula.formula

  let current () =
    match Lazy.force OpamSystem.ocaml_version with
    | None   -> None
    | Some o -> Some (of_string o)

  let system () =
    match Lazy.force OpamSystem.system_ocamlc_version with
    | None   -> None
    | Some v -> Some (of_string v)

  let compare v1 r v2 =
    let v1 = to_string v1 in
    let v2 = to_string v2 in
    match r with
    | `Eq  -> Debian.Version.equal v1 v2
    | `Neq -> not (Debian.Version.equal v1 v2)
    | `Geq -> Debian.Version.compare v1 v2 >= 0
    | `Gt  -> Debian.Version.compare v1 v2 > 0
    | `Leq -> Debian.Version.compare v1 v2 <= 0
    | `Lt  -> Debian.Version.compare v1 v2 < 0

end

include OpamMisc.Base

let of_filename =
  OpamFilename.chop_extension
  |> OpamFilename.basename
  |> OpamFilename.Base.to_string
  |> of_string

let list t =
  log "list dir=%s" (OpamFilename.Dir.to_string t);
  if OpamFilename.exists_dir t then (
    let files = OpamFilename.list_files t in
    let files = List.filter (fun f -> OpamFilename.check_suffix f ".comp") files in
    let l = List.map of_filename files in
    Set.of_list l
  ) else
    Set.empty

let default = of_string OpamGlobals.default_switch

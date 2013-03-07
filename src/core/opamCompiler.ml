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
    let files = OpamFilename.rec_files t in
    let comps  = List.filter (fun f -> OpamFilename.check_suffix f ".comp") files in
    let descrs = List.filter (fun f -> OpamFilename.check_suffix f ".descr") files in
    let same =
      let comp  = OpamFilename.Base.of_string ".comp"  in
      let descr = OpamFilename.Base.of_string ".descr" in
      fun c d ->
        OpamFilename.remove_suffix comp c = OpamFilename.remove_suffix descr d in
    (* XXX: dummy pairing *)
    let pairs = List.map (fun c ->
        if List.exists (same c) descrs then
          (c, Some (List.find (same c) descrs))
        else
          (c, None)
      ) comps in
    let l = List.map (fun (c,d) -> of_filename c, (c, d)) pairs in
    Map.of_list l
  ) else
    Map.empty

let system = of_string OpamGlobals.system

let unknown compiler =
  if compiler = system then (
    let root =
      if !OpamGlobals.root_dir = OpamGlobals.default_opam_dir then
        ""
      else
        Printf.sprintf " --root=%s" !OpamGlobals.root_dir in
    OpamGlobals.error_and_exit
      "No OCaml compiler found in path. You should use:\n\
       \n\
      \    opam init%s --comp=VERSION\n"
      root
  ) else
    OpamGlobals.error_and_exit
      "%S is not a valid compiler."
      (to_string compiler)

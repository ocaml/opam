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

open OpamMisc.OP

let log fmt = OpamGlobals.log "COMPILER" fmt

module Version = struct

  include OpamMisc.Base

  let of_string str =
    match OpamMisc.cut_at str '+' with
    | None       -> of_string str
    | Some (s,_) -> of_string s

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

module O = struct

  type t = {
    version: Version.t;
    name   : string;
  }

  let compare t1 t2 =
    String.compare t1.name t2.name

  let to_string t = t.name

  let of_string str =
    let version = Version.of_string str in
    { version; name = str }

  let to_json t = `String (to_string t)

end

include O
module Set = OpamMisc.Set.Make(O)
module Map = OpamMisc.Map.Make(O)

let version t = t.version

(* DIR/$NAME.comp *)
let of_filename f =
  if OpamFilename.check_suffix f ".comp" then
    (OpamFilename.chop_extension
     |> OpamFilename.basename
     |> OpamFilename.Base.to_string
     |> of_string
     |> fun x -> Some x) f
  else
    None

let list dir =
  log "list %s" (OpamFilename.Dir.to_string dir);
  if OpamFilename.exists_dir dir then (
    let files = OpamFilename.rec_files dir in
    List.fold_left (fun set f ->
        match of_filename f with
        | None   -> set
        | Some c -> Set.add c set
      ) Set.empty files
  ) else
    Set.empty

let prefixes dir =
  log "prefixes %s" (OpamFilename.Dir.to_string dir);
  if OpamFilename.exists_dir dir then (
    let files = OpamFilename.rec_files dir in
    List.fold_left (fun map f ->
        match of_filename f with
        | None   -> map
        | Some c ->
          let suffix = OpamFilename.Dir.to_string (OpamFilename.dirname f) in
          let prefix =
            match
              OpamMisc.remove_prefix ~prefix:(OpamFilename.Dir.to_string dir) suffix
            with
            | "" -> None
            | p  -> Some p in
          Map.add c prefix map
      ) Map.empty files
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

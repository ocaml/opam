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
let slog = OpamGlobals.slog

module Version = struct

  include OpamMisc.Base

  let of_string str =
    if OpamMisc.contains str '+' then
      raise (Invalid_argument
               "'+' is not allowed in compiler version constraints");
    of_string str

  type constr = (OpamFormula.relop * t) OpamFormula.formula

  let cut_version_number str =
    match OpamMisc.cut_at str '+' with
    | None -> str
    | Some (s, _) -> s

  let current () =
    match Lazy.force OpamSystem.ocaml_version with
    | None   -> None
    | Some o -> Some (of_string (cut_version_number o))

  let system () =
    match Lazy.force OpamSystem.system_ocamlc_version with
    | None   -> None
    | Some v -> Some (of_string (cut_version_number v))

  let compare v1 v2 = Debian.Version.compare (to_string v1) (to_string v2)

  let eval_relop relop v1 v2 = OpamFormula.check_relop relop (compare v1 v2)

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
    match OpamMisc.cut_at str '+' with
    | Some (v,_) -> { name = str; version = Version.of_string v }
    | None -> { name = str; version = Version.of_string str }

  let to_json t = `String (to_string t)

end

include O
module Set = OpamMisc.Set.Make(O)
module Map = OpamMisc.Map.Make(O)

let version t = t.version

(* DIR/$NAME.comp *)
let of_filename f =
  if OpamFilename.check_suffix f ".comp" then
    f
    |> OpamFilename.chop_extension
    |> OpamFilename.basename
    |> OpamFilename.Base.to_string
    |> of_string
    |> fun x -> Some x
  else
    None

let list dir =
  log "list %a" (slog OpamFilename.Dir.to_string) dir;
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
  log "prefixes %a" (slog OpamFilename.Dir.to_string) dir;
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

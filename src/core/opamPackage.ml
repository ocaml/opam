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

let log fmt = OpamGlobals.log "PACKAGE" fmt

module Version = struct

  type version =
    | Version of string
    | Pinned

  type t = version

  let to_string = function
    | Version x -> x
    | Pinned    -> "pinned"

  let of_string = function
    | "pinned"  -> Pinned
    | v         -> Version v

  let pinned = Pinned

  let compare x y = match (x,y) with
    | Version v1, Version v2 -> Debian.Version.compare v1 v2
    | Pinned    , Pinned     -> 0
    | Pinned    , _          -> 1
    | _         , Pinned     -> -1

  let to_json x =
    `String (to_string x)

  module O = struct
    type t = version
    let to_string = to_string
    let compare = compare
    let to_json = to_json
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

module Name = struct

  type t = string

  let to_string x = x

  let of_string x = x

  let global_config = OpamGlobals.global_config

  let compare n1 n2 =
    match compare (String.lowercase n1) (String.lowercase n2) with
    | 0 -> compare n1 n2
    | i -> i

  let to_json x = `String x

  module O = struct
    type t = string
    let to_string = to_string
    let compare = compare
    let to_json = to_json
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

type t = {
  name   : Name.t;
  version: Version.t;
}

let create name version = { name; version }

let pinned name = { name; version = Version.pinned }

let name t = t.name

let version t = t.version

let is_pinned t = t.version = Version.pinned

let sep = '.'

let of_string_opt s =
  if OpamMisc.contains s ' ' || OpamMisc.contains s '\n' then
    None
  else match OpamMisc.cut_at s sep with
    | None        -> None
    | Some (n, v) -> Some { name = Name.of_string n; version = Version.of_string v }

let of_string s = match of_string_opt s with
  | Some x -> x
  | None   -> OpamGlobals.error_and_exit "%s is not a valid versioned package name" s

let to_string t =
  Printf.sprintf "%s%c%s" (Name.to_string t.name) sep (Version.to_string t.version)

let compare nv1 nv2 =
  match Name.compare nv1.name nv2.name with
  | 0 -> Version.compare nv1.version nv2.version
  | i -> i

let hash nv = Hashtbl.hash nv

let equal nv1 nv2 =
  compare nv1 nv2 = 0

let to_json nv =
  `O [ ("name", Name.to_json (name nv));
       ("version", Version.to_json (version nv));
     ]

module O = struct
  type tmp = t
  type t = tmp
  let compare = compare
  let to_string = to_string
  let to_json = to_json
end

module Set = OpamMisc.Set.Make (O)

module Map = OpamMisc.Map.Make (O)

let to_map nv =
  Set.fold (fun nv map ->
    let name = name nv in
    let version = version nv in
    let versions =
      if Name.Map.mem name map then
        Name.Map.find name map
      else
        Version.Set.empty in
    Name.Map.add name (Version.Set.add version versions) (Name.Map.remove name map)
  ) nv Name.Map.empty

(* $DIR/$NAME.$VERSION/ *)
let of_dirname =
  OpamFilename.basename_dir
  |> OpamFilename.Base.to_string
  |> of_string_opt

(* $DIR/$NAME.$VERSION/opam *)
let of_filename f =
  if OpamFilename.basename f = OpamFilename.Base.of_string "opam" then
    of_dirname (OpamFilename.dirname f)
  else
    None

(* $NAME.$VERSION+opam.tar.gz *)
let of_archive f =
  let base = OpamFilename.basename f in
  match OpamMisc.cut_at (OpamFilename.Base.to_string base) '+' with
  | None       -> None
  | Some (s,_) -> of_string_opt s

let list dir =
  log "list %s" (OpamFilename.Dir.to_string dir);
  if OpamFilename.exists_dir dir then (
    let files = OpamFilename.rec_files dir in
    List.fold_left (fun set f ->
        match of_filename f with
        | None   -> set
        | Some p ->
          if not (Set.mem p set) then Set.add p set
          else
            let suffix = Filename.concat (to_string p) "opam" in
            let files = List.filter (OpamFilename.ends_with suffix) files in
            OpamGlobals.error_and_exit "Multiple definition of package %s in %s:\n  %s"
              (to_string p) (OpamFilename.Dir.to_string dir)
              (String.concat "\n  " (List.map OpamFilename.to_string files));
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
        | Some p ->
          let dirname = OpamFilename.dirname_dir (OpamFilename.dirname f) in
          let suffix = OpamFilename.Dir.to_string dirname in
          let prefix =
            match
              OpamMisc.remove_prefix ~prefix:(OpamFilename.Dir.to_string dir) suffix
            with
            | "" -> None
            | p  -> Some p in
          Map.add p prefix map
      ) Map.empty files
  ) else
    Map.empty

let versions_of_packages nvset =
  Set.fold
    (fun nv vset -> Version.Set.add (version nv) vset)
    nvset
    Version.Set.empty

let names_of_packages nvset =
  Set.fold
    (fun nv vset -> Name.Set.add (name nv) vset)
    nvset
    Name.Set.empty

let packages_of_name nvset n =
  Set.fold
    (fun nv set -> if name nv = n then Set.add nv set else set)
    nvset
    Set.empty

let versions_of_name packages n =
  versions_of_packages
    (Set.filter
       (fun nv -> name nv = n)
       packages)

let unknown name version =
  match version with
  | None   ->
    OpamGlobals.error_and_exit
      "%S is not a valid package."
      (Name.to_string name)
  | Some v ->
    OpamGlobals.error_and_exit
      "The package %S has no version %s."
      (Name.to_string name)
      (Version.to_string v)

let unavailable name version =
  match version with
  | None   ->
    OpamGlobals.error_and_exit
      "%S is not available for your compiler or your OS."
      (Name.to_string name)
  | Some v ->
    OpamGlobals.error_and_exit
      "Version %s of %S is not available for your compiler or your OS."
      (Version.to_string v)
      (Name.to_string name)

let unavailable_because_pinned name = function
  | None   ->
    OpamGlobals.error_and_exit
      "%S is not available because the package is pinned."
      (Name.to_string name)
  | Some v ->
    OpamGlobals.error_and_exit
      "Version %s of %S is not available because the package is pinned."
      (Version.to_string v)
      (Name.to_string name)

module Graph = struct
  module Vertex = struct
    include O
    let equal x y = compare x y = 0
    let hash = Hashtbl.hash
  end
  module PG = Graph.Imperative.Digraph.ConcreteBidirectional (Vertex)
  module Topological = Graph.Topological.Make (PG)
  module Traverse = Graph.Traverse.Dfs(PG)
  module Components = Graph.Components.Make(PG)
  module Parallel = OpamParallel.Make(struct
    let string_of_vertex = to_string
    include PG
    include Topological
    include Traverse
    include Components
  end)
end
module Parallel = Graph.Parallel

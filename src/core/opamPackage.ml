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

let log fmt = OpamGlobals.log "PACKAGE" fmt

module Version = struct

  type t = string

  let to_string x = x

  let of_string x = x

  let compare = Debian.Version.compare

  module O = struct
    type t = string
    let to_string = to_string
    let compare = compare
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

module Name = struct

  type t = string

  let to_string x = x

  let of_string x = x

  let default = OpamGlobals.default_package

  let compare n1 n2 =
    match compare (String.lowercase n1) (String.lowercase n2) with
      | 0 -> compare n1 n2
      | i -> i

  module O = struct
    type t = string
    let to_string = to_string
    let compare = compare
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

type t = {
  name   : Name.t;
  version: Version.t;
}

let create name version = { name; version }

let name t = t.name

let version t = t.version

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

(* XXX: this function is quite hackish, as it mainly depends on the shape the paths
   built in path.ml *)
let of_filename ~all f =
  let f = OpamMisc.strip (OpamFilename.to_string f) in
  let base = Filename.basename f in
  let parent = Filename.basename (Filename.dirname f) in
  match base with
  | "descr"
  | "opam" -> if all then of_string_opt parent else None
  | "url"  -> of_string_opt parent
  | _      ->
    if Filename.check_suffix base ".opam" then
      of_string_opt (Filename.chop_suffix base ".opam")
    else if Filename.check_suffix base "+opam.tar.gz" then
      of_string_opt (Filename.chop_suffix base "+opam.tar.gz")
    else
      match parent with
      | "files" ->
        let parent2 = Filename.basename (Filename.dirname (Filename.dirname f)) in
        of_string_opt parent2
      | _ -> None

let of_dirname d =
  of_string_opt (OpamFilename.Base.to_string (OpamFilename.basename_dir d))

let to_string t =
  Printf.sprintf "%s%c%s" (Name.to_string t.name) sep (Version.to_string t.version)

let compare nv1 nv2 =
  match Name.compare nv1.name nv2.name with
  | 0 -> Version.compare nv1.version nv2.version
  | i -> i

let hash nv = Hashtbl.hash nv

let equal nv1 nv2 =
  compare nv1 nv2 = 0

module O = struct
  type tmp = t
  type t = tmp
  let compare = compare
  let to_string = to_string
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

let list dir =
  log "list %s" (OpamFilename.Dir.to_string dir);
  if OpamFilename.exists_dir dir then (
    let dot_opams =
      let files = OpamFilename.rec_files dir in
      let files = List.filter (fun f -> OpamFilename.check_suffix f ".opam") files in
      List.fold_left (fun set file ->
        match of_filename ~all:true file with
        | None    ->
          log "%s is not a valid package filename!" (OpamFilename.to_string file);
          set
        | Some nv -> Set.add nv set
      ) Set.empty files in
    let opam =
      let all = OpamFilename.rec_dirs dir in
      let basenames = List.map OpamFilename.basename_dir all in
      Set.of_list
        (OpamMisc.filter_map
           (OpamFilename.Base.to_string |> of_string_opt)
           basenames) in

    Set.union dot_opams opam
  ) else
    Set.empty

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
      "Version %s of %S is incompatible with your compiler or your OS."
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

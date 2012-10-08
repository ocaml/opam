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
let of_filename f =
  let f = OpamMisc.strip (OpamFilename.to_string f) in
  let base = Filename.basename f in
  let parent = Filename.basename (Filename.dirname f) in
  match base with
  | "opam" | "descr" | "url" ->
    of_string_opt parent
  | _ ->
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

let of_dpkg d =
  { name    = Name.of_string d.Debian.Packages.name;
    version = Version.of_string d.Debian.Packages.version }

let of_cudf table pkg =
  let real_version =
    Debian.Debcudf.get_real_version
      table
      (pkg.Cudf.package, pkg.Cudf.version) in
  { name    = Name.of_string (Common.CudfAdd.decode pkg.Cudf.package);
    version = Version.of_string real_version; }

let to_string t =
  Printf.sprintf "%s%c%s" (Name.to_string t.name) sep (Version.to_string t.version)

module O = struct
  type tmp = t
  type t = tmp
  let compare nv1 nv2 =
    match Name.compare nv1.name nv2.name with
    | 0 -> Version.compare nv1.version nv2.version
    | i -> i
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

let versions_of_packages nvset =
  Set.fold
    (fun nv vset -> Version.Set.add (version nv) vset)
    nvset
    Version.Set.empty

let opam_files dir =
  if OpamFilename.exists_dir dir then (
    let files = OpamFilename.list_files dir in
    let files = List.filter (fun f -> OpamFilename.check_suffix f ".opam") files in
    List.fold_left (fun set file ->
      match of_filename file with
      | None    -> set
      | Some nv -> Set.add nv set
    ) Set.empty files
  ) else
    Set.empty

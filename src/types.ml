(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Define the basic types on which OPAM operates *)

module type Abstract = sig
  type t
  val of_string: string -> t
  val to_string: t -> string
  module Set: Set.S with type elt = t
  module Map: Map.S with type key = t
end

module Base = struct
  type t = string
  let of_string x = x
  let to_string x = x
  module O = struct type t = string let compare = compare end
  module Set = Set.Make(O)
  module Map = Map.Make(O)
end

let sep = '.'

(** Names *)
module N : Abstract = struct
  include Base
  let make x =
    try
      let (_:int) = String.index x sep in
      Globals.error_and_exit "%s is not a valid package name" x
    with Not_found ->
      x
end

(** Versions *)
module V : Abstract = Base

(** (Name, Version) pairs *)
module NV : sig
  include Abstract
  val name : t -> N.t
  val version: t -> V.t
  val create: N.t -> V.t -> t
  val of_dpkg: Debian.Packages.package -> t
  val of_cudf: Debian.Debcudf.tables -> Cudf.package -> t
end = struct

  type t = {
    name   : N.t;
    version: V.t;
  }

  let create name version = { name; version }
  let name t = t.name

  let version t = t.version

  let of_string s =
    let n, version =
      try
        let i = String.rindex s sep in
        String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1)
      with _ ->
        Globals.error_and_exit "%s is not a valid versioned package name" s in
    { name    = N.of_string n;
      version = V.of_string n  }

  let of_dpkg d =
    { name    = N.of_string d.Debian.Packages.name;
      version = V.of_string d.Debian.Packages.version }

  let of_cudf table pkg =
    let real_version =
      Debian.Debcudf.get_real_version
        table
        (pkg.Cudf.package, pkg.Cudf.version) in
    { name    = N.of_string pkg.Cudf.package;
      version = V.of_string real_version; }

  let to_string t =
    Printf.sprintf "%s%c%s" (N.to_string t.name) sep (V.to_string t.version)

  module O = struct type tmp = t type t = tmp let compare = compare end
  module Set = Set.Make (O)
  module Map = Map.Make (O)

end

(** OPAM repositories *)
type repository = {
  repo_name: string;
  repo_kind: string;
}

let string_of_repository r =
  Printf.sprintf "%s(%s)" r.repo_name r.repo_kind

(** Variable names *)
module Variable : Abstract = Base

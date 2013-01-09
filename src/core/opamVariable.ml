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

include OpamMisc.Base

type variable = t

let installed = of_string "installed"

let enable = of_string "enable"

type variable_contents =
  | B of bool
  | S of string

let string_of_variable_contents = function
  | B b -> string_of_bool b
  | S s -> s

module Section = struct

  include OpamMisc.Base

  type section = t

  let section_of_string = of_string

  let string_of_section = to_string

  module C = struct
    type t = string
    let compare = compare
    let equal = (=)
    let hash = Hashtbl.hash
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional(C)

  module Topo = Graph.Topological.Make (G)

  let graph_iter = Topo.iter

  module Full = struct

    type t = {
      package: OpamPackage.Name.t;
      section: section option;
    }

    let create package section =
      { package; section = Some section }

    let all package =
      { package; section = None }

    let package t = t.package

    let section t = t.section

    let of_string str =
      match OpamMisc.cut_at str '.' with
      | Some (n,s) ->
        { package = OpamPackage.Name.of_string n;
          section = Some (section_of_string s) }
      | None ->
        { package = OpamPackage.Name.of_string str;
          section = None }

    let to_string t =
      let n = OpamPackage.Name.to_string t.package in
      match t.section with
      | None   -> n
      | Some s -> Printf.sprintf "%s.%s" n (string_of_section s)

    module O = struct
      type tmp = t
      type t = tmp
      let compare = compare
      let to_string = to_string
    end

    module Set = OpamMisc.Set.Make (O)

    module Map = OpamMisc.Map.Make (O)

  end
end

module Full = struct

  type t = {
    section : Section.Full.t;
    variable: variable;
  }

  let variable t = t.variable
  let section t = Section.Full.section t.section
  let package t = Section.Full.package t.section

  let create_local package section variable =
    { section = Section.Full.create package section;
      variable }

  let create_global package variable =
    { section = Section.Full.all package;
      variable }

  let of_string s =
    match OpamMisc.rcut_at s ':' with
    | None ->
      create_global
        (OpamPackage.Name.of_string OpamGlobals.default_package)
        (of_string s)
    | Some (p,v) ->
      let v = of_string v in
      match OpamMisc.cut_at p '.' with
      | None -> create_global (OpamPackage.Name.of_string p) v
      | Some (p,s) -> create_local (OpamPackage.Name.of_string p) (Section.of_string s) v

  let to_string t =
    let package =
      let n = OpamPackage.Name.to_string (package t) in
      if n = OpamGlobals.default_package then
        ""
      else
        n in
    let section = match section t with
      | None   -> ""
      | Some s -> "." ^ Section.to_string s in
    let prefix = package ^ section in
    let prefix =
      if prefix = "" then
        ""
      else
        prefix ^ ":" in
    prefix ^ to_string t.variable

  module O = struct
    type tmp = t
    type t = tmp
    let compare = compare
    let to_string = to_string
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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

include OpamStd.AbstractString

type variable = t

type variable_contents =
  | B of bool
  | S of string

let string_of_variable_contents = function
  | B b -> string_of_bool b
  | S s -> s

module Full = struct

  type scope =
    | Global
    | Self
    | Package of OpamPackage.Name.t

  type t = {
    scope: scope;
    variable: variable;
  }

  let variable t = t.variable
  let scope t = t.scope
  let package ?self t = match t.scope with
    | Package p -> Some p
    | Self -> self
    | Global -> None

  let create package variable =
    let scope =
      if OpamPackage.Name.to_string package = "_" then Self
      else Package package
    in
    { scope; variable }

  let global variable =
    { scope = Global; variable }

  let is_global variable = match variable.scope with
    | Global -> true
    | Self | Package _ -> false

  let of_string s =
    match OpamStd.String.rcut_at s ':' with
    | None -> global (of_string s)
    | Some ("_",v) ->
      { scope = Self; variable = of_string v }
    | Some (p,v) ->
      create (OpamPackage.Name.of_string p) (of_string v)

  let to_string t =
    let prefix =
      match t.scope with
      | Global -> ""
      | Self -> "_:"
      | Package p -> OpamPackage.Name.to_string p ^ ":"
    in
    prefix ^ to_string t.variable

  let to_json x =
    `String (to_string x)

  module O = struct
    type tmp = t
    type t = tmp
    let compare = compare
    let to_string = to_string
    let to_json = to_json
  end

  module Set = OpamStd.Set.Make(O)

  module Map = OpamStd.Map.Make(O)

end

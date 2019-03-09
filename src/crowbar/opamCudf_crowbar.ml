(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Cudf_types
open Cudf
open! Crowbar

open OpamCrowbar

let version = nice_int
let relop : relop gen  = choose [
    const `Eq;
    const `Neq;
    const `Geq;
    const `Gt;
    const `Leq;
    const `Lt;
]
let constr = option (pair relop version)

let pkgname = nice_string
let vpkg = pair pkgname constr
let vpkglist = list vpkg

let enum_keep : enum_keep gen = choose [
    const `Keep_version;
    const `Keep_package;
    const `Keep_feature;
    const `Keep_none;
]

let vpkgformula = list (list vpkg)
let veqpkg = pair pkgname (option (pair (const `Eq) version))
let veqpkglist = list veqpkg

let rec typedecl1 : typedecl1 gen Lazy.t = lazy (choose [
    (map [option nice_int] @@ fun v -> `Int v);
    (map [option nice_uint] @@ fun v -> `Posint v);
    (map [option nice_uint] @@ fun v -> `Nat v);
    (map [option bool] @@ fun v -> `Bool v);
    (map [option nice_string] @@ fun v -> `String v);
    (map [option nice_string] @@ fun v -> `Pkgname v);
    (map [option nice_string] @@ fun v -> `Ident v);
    (map [list nice_string; option nice_string; list nice_string]
     @@ fun left s right ->
     `Enum (left @ (match s with None -> [] | Some s -> [s]) @ right, s));
    (map [option vpkg] @@ fun v -> `Vpkg v);
    (map [option vpkgformula] @@ fun v -> `Vpkgformula v);
    (map [option vpkglist] @@ fun v -> `Vpkglist v);
    (map [option veqpkg] @@ fun v -> `Veqpkg v);
    (map [option veqpkglist] @@ fun v -> `Veqpkglist v);
    (map [option (Crowbar.unlazy typedecl)] @@ fun v -> `Typedecl v);
  ])
and typedecl = lazy (list (pair nice_string (unlazy typedecl1)))
let (lazy typedecl1) = typedecl1
and (lazy typedecl) = typedecl

let typed_value : typed_value gen = choose [
    (map [nice_int] @@ fun v -> `Int v);
    (map [nice_uint] @@ fun v -> `Posint v);
    (map [nice_uint] @@ fun v -> `Nat v);
    (map [bool] @@ fun v -> `Bool v);
    (map [nice_string] @@ fun v -> `String v);
    (map [nice_string] @@ fun v -> `Pkgname v);
    (map [nice_string] @@ fun v -> `Ident v);
    (map [nice_string] @@ fun v -> `Pkgname v);
    (map [list nice_string; nice_string; list nice_string]
     @@ fun left s right ->
     `Enum (left @ [s] @ right, s));
    (map [vpkg] @@ fun v -> `Vpkg v);
    (map [vpkgformula] @@ fun v -> `Vpkgformula v);
    (map [vpkglist] @@ fun v -> `Vpkglist v);
    (map [veqpkg] @@ fun v -> `Veqpkg v);
    (map [veqpkglist] @@ fun v -> `Veqpkglist v);
    (map [typedecl] @@ fun v -> `Typedecl v);
]

let stanza gen = list (pair nice_string gen)

let package : package gen =
  map [
    pkgname;
    version;
    vpkgformula;
    vpkglist;
    veqpkglist;
    bool;
    bool;
    enum_keep;
    stanza typed_value;
  ] (fun
      package
      version
      depends
      conflicts
      provides
      installed
      was_installed
      keep
      pkg_extra
      ->
        {
          package;
          version;
          depends;
          conflicts;
          provides;
          installed;
          was_installed;
          keep;
          pkg_extra;
        })

let check () =
  let open OpamCudf.Json in
  check_json_roundtrip ~name:"Cudf_types.relop"
    relop (=)
    relop_to_json relop_of_json;
  check_json_roundtrip ~name:"Cudf_types.constr"
    constr (=)
    constr_to_json constr_of_json;
  check_json_roundtrip ~name:"Cudf_types.vpkg"
    vpkg (=)
    vpkg_to_json vpkg_of_json;
  check_json_roundtrip ~name:"Cudf_types.enum_keep"
    enum_keep (=)
    enum_keep_to_json enum_keep_of_json;
  check_json_roundtrip ~name:"Cudf_types.veqpkg"
    veqpkg (=)
    veqpkg_to_json veqpkg_of_json;
  check_json_roundtrip ~name:"Cudf_types.typedecl1"
    typedecl1 (=)
    typedecl1_to_json typedecl1_of_json;
  check_json_roundtrip ~name:"Cudf_types.typed_value"
    typed_value (=)
    typed_value_to_json typed_value_of_json;
  check_json_roundtrip ~name:"Cudf.package"
    package OpamCudf.Package.equal
    package_to_json package_of_json;

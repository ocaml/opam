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

type name = Name of Cudf_types.pkgname

type version =
  | Deb of Debian.Format822.version
  | Head of [`uptodate|`behind] (* Head of a version controled repository *)

type name_version = name * version

module Namespace =
struct
  open Printf

  let name_compare = compare

  let string_of_version = function
    | Deb s           -> s
    | Head `uptodate  -> "HEAD"
    | Head `behind    -> "HEAD*"

  let string_of_nv (Name n) version =
    sprintf "%s-%s" n (string_of_version version)

  let string_of_name (Name n) = n

  let version_of_string version =
    if version = "HEAD" then
      Head `uptodate
    else if version = "HEAD*" then
      Head `behind
    else
      let valid =
        try let _ = String.index version '-' in false
        with Not_found -> true in
      if not valid then
        Globals.error_and_exit "%s is not a valid version (it contains '-')" version;
      Deb version

  let name_of_string s = Name s

  let is_valid_nv s =
    try let _ = String.rindex s '-' in true
    with Not_found -> false
 
  let nv_of_string s = 
    let n, version =
      try
        let i = String.rindex s '-' in
        String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1)
      with _ ->
        Globals.error_and_exit "%s is not a valid versioned package name" s in
    Name n, version_of_string version

  let default_version = "0"

  let nv_of_dpkg d =
    Name d.Debian.Packages.name, Deb d.Debian.Packages.version

  let to_string (Name n, v) =
    Printf.sprintf "%s %s" n (string_of_version v)
end

module N_map = BatMap.Make (struct open Namespace type t = name let compare = name_compare end)
module V_set = BatSet.Make (struct open Namespace type t = version let compare = compare end)

module NV_orderedtype = 
struct
  open Namespace
  type t = name_version
  let compare (n1, v1) (n2, v2) = 
    let c = name_compare n1 n2 in
      if c = 0 then
        compare v1 v2
      else
        c
end

module NV_map = BatMap.Make (NV_orderedtype)
module NV_set = BatSet.Make (NV_orderedtype)

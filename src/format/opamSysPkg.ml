(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* basic ops *)

type t = string

let of_string s = s
let to_string s = s
let compare = OpamStd.String.compare_case
let equal s r = compare s r = 0

let to_json s =
  `O [ ("sys_package", `String s) ]
let of_json = function
  | `O dict ->
    (match OpamStd.List.assoc String.equal "sys_package" dict with
     | `String s -> Some (of_string s)
     | _ -> None
     | exception Not_found -> None)
  | _ -> None

module O = struct
  type tmp = t
  type t = tmp
  let compare = compare
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
end

module Set = OpamStd.Set.Make(O)

module Map = OpamStd.Map.Make(O)

let raw_set set =
  OpamStd.String.Set.fold (fun spkg set-> Set.add (of_string spkg) set)
    set Set.empty

(** System package status *)

type status =
  {
    s_available : Set.t;
    (** Package available but not installed *)

    s_not_found : Set.t;
    (** Package unavailable on this system *)
  }


let status_empty =
  {
    s_available  = Set.empty;
    s_not_found  = Set.empty;
  }

let string_of_status sp =
  Printf.sprintf "available: %s; not_found: %s"
    (Set.to_string sp.s_available)
    (Set.to_string sp.s_not_found)

let combine_status st st' =
  Set.Op.{
    s_available = st.s_available ++ st'.s_available;
    s_not_found = st.s_available ++ st'.s_not_found;
  }

(* System package availability *)
type availability_mode =
  | Available of Set.t
  | Suppose_available
  | No_depexts

let string_of_availability_mode = function
  | Available av -> Set.to_string av
  | Suppose_available -> "Suppose available"
  | No_depexts -> "No depexts"

let check_availability_mode_equal a b =
  match a, b with
  | Available os, Available ns -> Set.equal os ns
  | Suppose_available, Suppose_available
  | No_depexts, No_depexts -> true
  | Available _, _
  | Suppose_available, _
  | No_depexts, _ -> false

let combine_availability_mode a a' =
  match a, a' with
  | Available s, Available s' -> Available (Set.union s s')
  | Suppose_available, Suppose_available -> Suppose_available
  | Available s, Suppose_available | Suppose_available, Available s ->
    Available s
  | No_depexts, _  |  _, No_depexts -> No_depexts


(** System packages to install *)

type to_install = {
  ti_new : Set.t;
  (** Package to install required by new opam packages *)

  ti_required : Set.t
  (** Package to install required by already install opam packages *)
}

let to_install_empty = {
  ti_new  = Set.empty;
  ti_required  = Set.empty;
}

let string_of_to_install ti =
  Printf.sprintf "new: %s; required: %s"
    (Set.to_string ti.ti_new)
    (Set.to_string ti.ti_required)

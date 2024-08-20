(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Cudf criteria helpers *)

(* {2 Criterion types} *)
type filter =
  | Installed
  | Solution
  | Changed
  | Removed
  | New
  | Upgraded
  | Downgraded
  | Requested

type property = string option

type sign = Plus | Minus

type criterion = sign * filter * property

(* {2 String conversion} *)

val criterion_to_string : criterion -> string

(** [of_string s] Parses the string [s] as a CUDF optimization criteria.

    @raise {!Stdlib.Failure} if the string is invalid *)
val of_string : string -> criterion list

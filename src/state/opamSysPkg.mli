(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val get_installed_packages: OpamStd.String.Set.t -> OpamStd.String.Set.t
val update : su:bool -> interactive:bool -> unit
val install: su:bool -> interactive:bool -> OpamStd.String.Set.t -> unit

(** System packages status *)
type status =
  {
    s_available : OpamStd.String.Set.t;
    (** Package available but not installed *)

    s_not_found : OpamStd.String.Set.t;
    (** Package unavailable on this system *)
  }

val status_empty: status

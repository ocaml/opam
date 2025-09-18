(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** System package *)
type t
include OpamStd.ABSTRACT with type t := t

val raw_set: OpamStd.String.Set.t -> Set.t

(** System packages status *)
type status =
  {
    s_available : Set.t;
    (** Package available but not installed *)

    s_not_found : Set.t;
    (** Package unavailable on this system *)
  }

val status_empty: status

val string_of_status: status -> string

(** Merge status sets *)
val combine_status : status -> status -> status

(** System package availability *)
type availability_mode =
  | Available of Set.t (** Set of available system packages *)
  | Suppose_available (** In this system, all packages are considered
                          available *)
  | No_depexts (** Depext system disabled *)

val string_of_availability_mode : availability_mode -> string

(** Combine two availability_modes. If it is not the same variant, the result is
    the smallest one. *)
val combine_availability_mode : availability_mode -> availability_mode ->
  availability_mode

(** Returns [true] if both values are [Suppose_available] or both are
    [Available] with equal sets, [false] otherwise. *)
val check_availability_mode_equal : availability_mode -> availability_mode -> bool

(** System packages to install. We need to split per purpose as some
    distribution need to keep up-to-date already installed system packages. See
    {!OpamSysInteract.install_packages_commands_t}. *)
type to_install =
  {
    ti_new : Set.t;
    (** Package to install required by new opam packages *)

    ti_required : Set.t
    (** Package to install required by already install opam packages *)
  }

val to_install_empty: to_install

val string_of_to_install: to_install -> string

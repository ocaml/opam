(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 ahrefs                                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* GPG, as supported by git *)
type kind = [ `GPG ]

type t

val kind: t -> kind

(** The list of all the possible values of kind *)
val all_kinds : kind list

(** The value of the hash, as a string of hexadecimal characters *)
val contents: t -> string

val string_of_kind: kind -> string

val gpg: string -> t

include OpamStd.ABSTRACT with type t := t

val of_string_opt: string -> t option
val compare_kind: kind -> kind -> int
val equal_kind: kind -> kind -> bool

(** Check if signature contains only 0 *)
val is_null: t -> bool

(** Sorts the list from strongest to weakest *)
val sort : t list -> t list

val check_commit: [ `TODO ] -> t -> bool

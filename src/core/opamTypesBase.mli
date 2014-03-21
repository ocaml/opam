(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2014 OCamlPro                                        *)
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

(** This module contains basic utility functions and stringifiers for the
    basic OPAM types present in OpamTypes.ml *)
open OpamTypes

(** Upcast a downloaded directory. *)
val download_dir: dirname download -> generic_file download

(** Upcast a downloaded file. *)
val download_file: filename download -> generic_file download

(** Print an address *)
val string_of_address: address -> string

(** Parse an address *)
val address_of_string: string -> address

(** Guess the repository kind *)
val guess_repository_kind: repository_kind option -> address -> repository_kind

(** Pretty-print repository kinds. *)
val string_of_repository_kind: [`http|`local|`git|`darcs|`hg] -> string

(** Parser of repository kinds. Raise an error if the kind is not valid. *)
val repository_kind_of_string: string -> [`http|`local|`git|`darcs|`hg]

(** Extract a package from a package action. *)
val action_contents: 'a action -> 'a

(** Extract a packages from a package action. This returns all concerned
    packages, including the old version for an up/down-grade. *)
val full_action_contents: 'a action -> 'a list

(** Pretty-prints the cause of an action *)
val string_of_cause: ('pkg -> string) -> 'pkg cause -> string

(** Pretty-print *)
val string_of_upload: upload -> string

(** Convert a pin kind to a repository kind *)
val repository_kind_of_pin_kind: pin_kind -> repository_kind option

(** Pretty-printing of pin kinds. *)
val pin_kind_of_string: string -> pin_kind

(** Parsing of pin kinds *)
val string_of_pin_kind: pin_kind -> string

(** Read pin options args *)
val pin_option_of_string: ?kind:pin_kind -> string -> pin_option

(** Convert a pin option to a string *)
val string_of_pin_option: pin_option -> string

(** Get the pin kind from a pin option *)
val kind_of_pin_option: pin_option -> pin_kind

(** Pretty-print *)
val string_of_shell: shell -> string

(** The empty file position *)
val pos_null: pos

(** Prints a file position *)
val string_of_pos: pos -> string

val string_of_relop: relop -> string
val relop_of_string: string -> relop (** Raises Invalid_argument*)
val string_of_logop: logop -> string
val logop_of_string: string -> logop (** Raises Invalid_argument*)
val string_of_pfxop: pfxop -> string
val pfxop_of_string: string -> pfxop (** Raises Invalid_argument*)

(** Pretty print *)
val string_of_filter: filter -> string

(** Map on a solver result *)
val map_success: ('a -> 'b) -> ('a,'fail) result -> ('b,'fail) result

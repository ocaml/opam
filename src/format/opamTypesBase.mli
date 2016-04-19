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

(** Helper functions on the base types (from [OpamTypes]) *)

(** This module contains basic utility functions and stringifiers for the
    basic OPAM types present in OpamTypes.ml *)
open OpamTypes

include module type of OpamCompat

(** {2 Exceptions} *)
exception Lexer_error of string

(** Upcast a downloaded directory. *)
val download_dir: dirname download -> generic_file download

(** Upcast a downloaded file. *)
val download_file: filename download -> generic_file download

(** Corresponding user message *)
val string_of_download: _ download -> string

val string_of_generic_file: generic_file -> string

(** Extract a package from a package action. *)
val action_contents: [< 'a action ] -> 'a

val map_atomic_action: ('a -> 'b) -> 'a atomic_action -> 'b atomic_action
val map_highlevel_action: ('a -> 'b) -> 'a highlevel_action -> 'b highlevel_action
val map_concrete_action: ('a -> 'b) -> 'a concrete_action -> 'b concrete_action
val map_action: ('a -> 'b) -> 'a action -> 'b action

(** Extract a packages from a package action. This returns all concerned
    packages, including the old version for an up/down-grade. *)
val full_action_contents: 'a action -> 'a list

(** Pretty-prints the cause of an action *)
val string_of_cause: ('pkg -> string) -> 'pkg cause -> string

(** Pretty-print *)
val string_of_shell: shell -> string

(** The empty file position *)
val pos_null: pos

(** [pos_best pos1 pos2] returns the most detailed position between [pos1] and
    [pos2] (defaulting to [pos1]) *)
val pos_best: pos -> pos -> pos

(** Position in the given file, with unspecified line and column *)
val pos_file: filename -> pos

(** Prints a file position *)
val string_of_pos: pos -> string

val string_of_relop: relop -> string

val relop_of_string: string -> relop (** Raises Invalid_argument*)

val string_of_logop: logop -> string

val logop_of_string: string -> logop (** Raises Invalid_argument*)

val string_of_pfxop: pfxop -> string

val pfxop_of_string: string -> pfxop (** Raises Invalid_argument*)

val string_of_env_update_op: env_update_op -> string

val env_update_op_of_string: string -> env_update_op

val env_array: env -> string array

(** Parses the data suitable for a filter.FIdent from a string. May
    raise [Failure msg] on bad package names *)
val filter_ident_of_string:
  string -> name list * variable * (string * string) option

val string_of_filter_ident:
  name list * variable * (string * string) option -> string

val pkg_flag_of_string: string -> package_flag

val string_of_pkg_flag: package_flag -> string

(** Map on a solver result *)
val map_success: ('a -> 'b) -> ('a,'fail) result -> ('b,'fail) result

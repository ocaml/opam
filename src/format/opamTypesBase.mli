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

(** Print an address. With [~kind], makes the kind of address unambiguous
    (suitable for re-parsing, but not for passing to some external commands) *)
val string_of_address: ?kind:repository_kind -> address -> string

(** Parse an address *)
val address_of_string: string -> address

(** Guess an address kind using url suffixes ([.git], etc.) and prefixes
    ([http://], etc.). Defaults to `local. The returned address is a correct
    path in case of [file://] *)
val parse_url: address -> address * repository_kind

(** Scan the given directory for version control *)
val guess_version_control: dirname -> version_control option

(** Pretty-print repository kinds. *)
val string_of_repository_kind: repository_kind -> string

(** Parser of repository kinds. Raise an error if the kind is not valid. *)
val repository_kind_of_string: string -> repository_kind

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
val string_of_upload: upload -> string

(** Convert a pin kind to a repository kind *)
val repository_kind_of_pin_kind: pin_kind -> repository_kind option

(** Pretty-printing of pin kinds. *)
val pin_kind_of_string: string -> pin_kind

(** Parsing of pin kinds *)
val string_of_pin_kind: pin_kind -> string

(** Read pin options args. If [kind] isn't specified, [guess] is set to [true]
    and the name isn't explicit, look for VC on the filesystem to get the
    pinning kind *)
val pin_option_of_string: ?kind:pin_kind -> ?guess:bool -> string -> pin_option

(** Convert a pin option to a string *)
val string_of_pin_option: pin_option -> string

(** Get the pin kind from a pin option *)
val kind_of_pin_option: pin_option -> pin_kind

(** Get a pin_option from address and kind *)
val pin_of_url: address * repository_kind -> pin_option

val url_of_pin: pin_option -> address * repository_kind

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

(** Parses the data suitable for a filter.FIdent from a string. May
    raise [Failure msg] on bad package names *)
val filter_ident_of_string:
  string -> name list * variable * (string * string) option

val string_of_filter_ident:
  name list * variable * (string * string) option -> string

val dep_flag_of_string: string -> package_dep_flag

val string_of_dep_flag: package_dep_flag -> string

val filter_deps:
  build:bool -> test:bool -> doc:bool -> dev:bool ->
  ext_formula -> formula

val pkg_flag_of_string: string -> package_flag

val string_of_pkg_flag: package_flag -> string

(** Map on a solver result *)
val map_success: ('a -> 'b) -> ('a,'fail) result -> ('b,'fail) result

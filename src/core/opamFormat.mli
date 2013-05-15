(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

(** Format of OPAM configuration files. *)

open OpamTypes

(** The empty file *)
val empty : file

(** Get all the variable definitions from a list of items *)
val variables : file_item list -> (string * value) list

(** Get all the sections from a list of items *)
val sections : file_item list -> (string * file_section) list

(** Check whether a list of items contains only valid variable definitions *)
val is_valid : file_item list -> string list -> bool

(** Find all the invalid fields *)
val invalid_fields : file_item list -> string list -> string list

(** {2 Parsing functions} *)

(** All the following parsing function raise [Bad_format] in case the
    input does not have the right format. *)
exception Bad_format of string

(** Parse a boolean *)
val parse_bool : value -> bool

(** Parse an integer *)
val parse_int: value -> int

(** Parse an ident *)
val parse_ident : value -> string

(** Parse a symbol *)
val parse_symbol : value -> string

(** Parse a string *)
val parse_string : value -> string

(** Parse a list of 'things' *)
val parse_list : (value -> 'a) -> value -> 'a list

(** Parse a group of 'things' *)
val parse_group : (value -> 'a) -> value -> 'a list

(** Parse a value and its option of 'things' *)
val parse_option : (value -> 'a) -> (value list -> 'b) -> value -> 'a * 'b option

(** Parse a value and a single optional value *)
val parse_single_option : (value -> 'a) -> (value -> 'b) -> value -> 'a * 'b option

(** Parse a string with an optional argument *)
val parse_string_option : (value list -> 'a) -> value -> string * 'a option

(** Parse a list of strings *)
val parse_string_list : value -> string list

(** Parse a single string *)
val parse_single_string: value list -> string

(** Parse a pair of strings *)
val parse_pair: (value -> 'a) -> (value -> 'b) -> value -> 'a * 'b

(** Try to parse the value using function from the list. All the
    parsing functions are tried until one succeeds. The first argument
    is a debug message. *)
val parse_or: (string * (value -> 'a)) list -> value -> 'a

(** Parse a sequence of values *)
val parse_sequence: (string * (value -> 'a)) list -> value -> 'a list

(** {2 Creation functions} *)

(** Create a boolean *)
val make_bool : bool -> value

(** Create an integer *)
val make_int: int -> value

(** Create an ident *)
val make_ident : string -> value

(** Create a symbol *)
val make_symbol : string -> value

(** Create a string *)
val make_string : string -> value

(** Create a list of 'things' *)
val make_list : ('a -> value) -> 'a list -> value

(** Create a list of strings *)
val make_string_list: string list -> value

(** Create a group of 'things' *)
val make_group : ('a -> value) -> 'a list -> value

(** Create a value and its optional arguments *)
val make_option : ('a -> value) -> ('b -> value list) -> ('a * 'b option) -> value

(** Create a pair *)
val make_pair: ('a -> value) -> ('b -> value) -> ('a * 'b) -> value

(** Create a pair of strings *)
val make_string_pair: string * string -> value

(** {2 Printing functions} *)

(** Print a value *)
val string_of_value : value -> string

(** Print a list of values *)
val string_of_values : value list -> string

(** When folding recursively a value [Variable (s, v)] for printing,
    we check if we indent the first encountered list below [v]
    (i.e. in case [indent_variable s] is [true]). *)
type indent_variable = string -> bool

(** Print an file_item *)
val string_of_item : ?indent_variable:indent_variable -> file_item -> string option

(** Print a list of items *)
val string_of_items : ?indent_variable:indent_variable -> file_item list -> string

(** Print a file *)
val string_of_file : ?indent_variable:indent_variable -> file -> string

(** {2 Finding functions} *)

(** Get the value of a field *)
val assoc : file_item list -> string -> (value -> 'a) -> 'a

(** Get the value of a field. If the field does not exist, return
    None *)
val assoc_option : file_item list -> string -> (value -> 'a) -> 'a option

(** Get the value of a field. If the variable does not exist, return a
    default value *)
val assoc_default : 'a -> file_item list -> string -> (value -> 'a) -> 'a

(** Get the value associated to a variable. If the variable does not
    exists, return [] *)
val assoc_list : file_item list -> string -> (value -> 'a list) -> 'a list

(** Get the string list associated to a variable. If the variable does
    not exist, return [] *)
val assoc_string_list : file_item list -> string -> string list

(** Get one section of a certain kind *)
val get_section_by_kind : file_item list -> string -> file_section

(** Get all the sections of a certain kind *)
val get_all_section_by_kind : file_item list -> string -> file_section list

(** Get sections *)
val assoc_sections: file_item list -> string -> (file_section -> 'a) -> 'a list

(** {2 Formula} *)

(** This section is dedicated to the parsing and creatin of dependency
    and conflict formaulas. It's maybe easier to do that directly in
    the parser ... *)

open OpamTypes

(** Parse package formula where AND are implicit: [x y -> x & y] *)
val parse_formula : value -> formula

(** Build a formula where AND are implicit. *)
val make_formula : formula -> value

(** Parse optional package formula where OR are implicit: [x y -> x | y] *)
val parse_opt_formula : value -> formula

(** Build a formula where OR are implicit. *)
val make_opt_formula : formula -> value

(** Parse compiler constraints *)
val parse_compiler_constraint: value -> compiler_constraint

(** Build a compiler constraint *)
val make_compiler_constraint: compiler_constraint -> value

(** Parse an OS constraint *)
val parse_os_constraint: value -> (bool * string) generic_formula

(** Build an OS constraint *)
val make_os_constraint: (bool * string) generic_formula -> value

(** {2 Environment variables} *)

(** Parsing *)
val parse_env_variable: value -> (string * string * string)

(** Making *)
val make_env_variable: (string * string * string) -> value

(** {2 filter expressions} *)

(** Parsing *)
val parse_filter: value list -> filter

(** Creation *)
val make_filter: filter -> value list

(** Parse a command *)
val parse_command: value -> command

(** Create a command *)
val make_command: command -> value

(** Parse a list of commands *)
val parse_commands: value -> command list

(** Create a list of commands *)
val make_commands: command list -> value

(** Parse a list of commands *)
val parse_messages: value -> (string * filter option) list

(** {2 Tags} *)

(** Parse tags *)
val parse_tags: value -> tags

(** Make tags *)
val make_tags: tags -> value

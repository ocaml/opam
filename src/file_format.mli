(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Untyped generic file format. *)

(** Base values *)
type value =
  | Bool of bool
  | Int of int
  | String of string
  | Symbol of string
  | Ident of string
  | List of value list
  | Group of value list
  | Option of value * value list

(** A file section *)
type section = {
  kind  : string;
  name  : string;
  items : item list;
}

(** A file is composed of sections and variable definitions *)
and item =
  | Section of section
  | Variable of string * value

(** A file is a list of items and the filename *)
type file = {
  contents : item list;
  filename : string;
}

(** The empty file *)
val empty : file

(** Get all the variable definitions from a list of items *)
val variables : item list -> (string * value) list

(** Get all the sections from a list of items *)
val sections : item list -> (string * section) list

(** Check whether a list of items contains only valid variable definitions *)
val is_valid : item list -> string list -> bool

(** Find all the invalid fields *)
val invalid_fields : item list -> string list -> string list

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
val parse_string_pair: value -> string * string

(** Parse a pair of strings from a list of values *)
val parse_string_pair_of_list: value list -> string * string

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

(** Create a group of 'things' *)
val make_group : ('a -> value) -> 'a list -> value

(** Create a value and its optional arguments *)
val make_option : ('a -> value) -> ('b -> value list) -> ('a * 'b option) -> value

(** Create a pair *)
val make_pair: ('a -> value) -> ('a * 'a) -> value

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

(** Print an item *)
val string_of_item : ?indent_variable:indent_variable -> item -> string option

(** Print a list of items *)
val string_of_items : ?indent_variable:indent_variable -> item list -> string

(** Print a file *)
val string_of_file : ?indent_variable:indent_variable -> file -> string

(** {2 Finding functions} *)

(** Get the value of a field *)
val assoc : item list -> string -> (value -> 'a) -> 'a

(** Get the value of a field. If the field does not exist, return
    None *)
val assoc_option : item list -> string -> (value -> 'a) -> 'a option

(** Get the value of a field. If the variable does not exist, return a
    default value *)
val assoc_default : 'a -> item list -> string -> (value -> 'a) -> 'a

(** Get the value associated to a variable. If the variable does not
    exists, return [] *)
val assoc_list : item list -> string -> (value -> 'a list) -> 'a list

(** Get the string list associated to a variable. If the variable does
    not exist, return [] *)
val assoc_string_list : item list -> string -> string list

(** Get one section of a certain kind *)
val get_section_by_kind : item list -> string -> section

(** Get all the sections of a certain kind *)
val get_all_section_by_kind : item list -> string -> section list

(** Get sections *)
val assoc_sections: item list -> string -> (section -> 'a) -> 'a list

(** {2 Formula} *)

(** This section is dedicated to the parsing and creatin of dependency
    and conflict formaulas. It's maybe easier to do that directly in
    the parser ... *)

open Types

(** Parse an AND formala such as
    ["foo", "bar" (<"1", >"2"), "aa" ] *)
val parse_and_formula : value -> and_formula

(** Parse an CNF formula (which contains only inlevel OR) such as
    ["foo" ("bar"(<"1") | "bar" (>"2"))] *)
val parse_cnf_formula : value -> cnf_formula

(** Build an AND formula *)
val make_and_formula : and_formula -> value

(** Build a CNF formula *)
val make_cnf_formula : cnf_formula -> value

(** Parse a simple constraint *)
val parse_constraint: value -> ocaml_constraint

(** Build a simple constraint *)
val make_constraint: ocaml_constraint -> value

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

(** Parse a command argument *)
val parse_arg: value -> arg

(** Create a command argument *)
val make_arg: arg -> value

(** Parse a command *)
val parse_command: value -> command

(** Create a command *)
val make_command: command -> value

(** Parse a list of commands *)
val parse_commands: value -> command list

(** Create a list of commands *)
val make_commands: command list -> value

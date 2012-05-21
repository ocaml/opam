(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Untyped generic file format. *)

(** Base values *)
type value =
  | Bool of bool
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
val parse_option : (value -> 'a) -> (value -> 'b) -> value -> 'a * 'b list

(** Parse a string with an optional argument *)
val parse_string_option : (value list -> 'a) -> value -> string * 'a option

(** Parse a list of strings *)
val parse_string_list : value -> string list

(** Parse a single string *)
val parse_single_string: value list -> string

(** Parse a pair of strings *)
val parse_string_pair: value list -> string * string

(** Try to parse the value using function from the list. All the
    parsing functions are tried until one succeeds. The first argument
    is a debug message. *)
val parse_or: (string * (value -> 'a)) list -> value -> 'a

(** {2 Creation functions} *)

(** Create a boolean *)
val make_bool : bool -> value

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
val make_option : ('a -> value) -> ('b -> value) -> 'a * 'b list -> value

(** {2 Printing functions} *)

(** Print a value *)
val string_of_value : value -> string

(** Print a list of values *)
val string_of_values : value list -> string

(** Print an item *)
val string_of_item : item -> string

(** Print a list of items *)
val string_of_items : item list -> string

(** Print a file *)
val string_of_file : file -> string

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

open Debian.Format822

(** Parse an AND formala such as
    ["foo", "bar" (<"1", >"2"), "aa" ] *)
val parse_and_formula : value -> vpkglist

(** Parse an OR formula (which contains only toplevel OR) such as
    [("foo", "bar"(<"1")) | "bar" (>"2")] *)
val parse_or_formula : value -> vpkgformula

(** Build an AND formula *)
val make_and_formula : vpkglist -> value

(** Build an OR formula *)
val make_or_formula : vpkgformula -> value

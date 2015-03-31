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

(** Manage filters *)

(** Filters are a small language of formulas over strings and booleans used for
    conditions and text replacements. It has relational operators over strings
    (using version-number comparison), And, Or and Not boolean operations,
    dynamic casting (using strings "true" and "false"), and string
    interpolation. Variables are resolved using a user function returning an
    option, undefined values are propagated.

    String interpolation uses the syntax '%{identifier}%'

    Identifiers have the form {v[package:]var[?str_if_true:str_if_false_or_undef]v}.
    The last optional part specifies a conversion from boolean to static strings.

    The syntax [pkg1+pkg2+pkgn:var] is allowed as a shortcut to
    [pkg1:var & pkg2:var & pkgn:var].

    The special variable [pkg:enable] is allowed as a shortcut for
    [pkg:installed?enable:disable]
*)


open OpamTypes

(** Pretty-print *)
val to_string: filter -> string

(** Folds on the tree of a filter *)
val fold_down_left: ('a -> filter -> 'a) -> 'a -> filter -> 'a

(** Returns all the variables appearing in a filter (including the ones within
    string interpolations *)
val variables: filter -> full_variable list

(** Type of filter environment. *)
type env = full_variable -> variable_contents option

(** The type of filter idents with (optionally multiple) qualifying package
    names and optional string converter *)
type fident = name list * variable * (string * string) option

(** Rewrites string interpolations within a string *)
val expand_string: env -> string -> string

(** Computes the value of a filter. May raise [Failure] if [default] isn't
    provided *)
val eval: ?default:variable_contents -> env -> filter -> variable_contents

(** Like [to_value] but casts the result to a bool. Raises [Invalid_argument] if
    not a valid bool and no default supplied. *)
val eval_to_bool: ?default:bool -> env -> filter -> bool

(** Same as [eval_to_bool], but takes an option as filter and returns always
    [true] on [None], [false] when the filter is [Undefined]. This is the
    most common behaviour for using "filters" for filtering *)
val opt_eval_to_bool: env -> filter option -> bool

(** Like [to_value] but casts the result to a string *)
val eval_to_string: ?default:string -> env -> filter -> string

(** Wraps a full_variable into a fident accessor *)
val ident_of_var: full_variable -> fident

(** A fident accessor directly referring a variable with the given name *)
val ident_of_string: string -> fident

(** Resolves a filter ident. Like [eval], may raise Failure if no default is
    provided *)
val ident_value: ?default:variable_contents -> env -> fident -> variable_contents

(** Like [ident_value], but casts the result to a string *)
val ident_string: ?default:string -> env -> fident -> string

(** Like [ident_value], but casts the result to a bool *)
val ident_bool: ?default:bool -> env -> fident -> bool

(** Rewrites [basename].in to [basename], expanding interpolations *)
val expand_interpolations_in_file: env -> basename -> unit


(** Processes filters evaluation in a command list: parameter expansion and
    conditional filtering *)
val commands: env -> command list -> string list list

(** Process a simpler command, without filters *)
val single_command: env -> arg list -> string list

(** Extracts variables appearing in a list of commands *)
val commands_variables: command list -> full_variable list

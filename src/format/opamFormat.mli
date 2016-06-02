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

(** OPAM config files syntax and conversion tools, printing *)

open OpamTypes

(** {2 Parsing functions} *)

(** Format error reporting: position and message *)
type bad_format = pos option * string

(** All the following parsing function raise [Bad_format] in case the
    input does not have the right format. *)
exception Bad_format of bad_format
exception Bad_format_list of bad_format list

(** Raise [Bad_format]. *)
val bad_format: ?pos:pos -> ('a, unit, string, 'b) format4 -> 'a

val string_of_bad_format: ?file:filename -> exn -> string

(** Adds a position to a Bad_format exception if it doesn't have one yet *)
val add_pos: pos -> exn -> exn

(** Get the position out of a value *)
val value_pos: value -> pos

(** {2 Printers} *)

module Print : sig

  val value : value -> string

  val value_list: value list -> string

  val items: opamfile_item list -> string

  val opamfile: opamfile -> string

  val format_opamfile: Format.formatter -> opamfile -> unit

end

(** {2 Normalised output for opam syntax files} *)

module Normalise : sig
  val escape_string : string -> string
  val value : OpamTypes.value -> string
  val item : OpamTypes.opamfile_item -> string
  val item_order : OpamTypes.opamfile_item -> OpamTypes.opamfile_item -> int
  val items : OpamTypes.opamfile_item list -> string
end

(** {2 Structures for bidirectional parsing/printing, combiners and converters} *)

module Pp : sig

  (** The type of bidirectional parsers from ['a] to ['b]. We abuse the terms
      and describe going from ['a] to ['b] as "parsing", and going from ['b] to
      ['a] as "printing". Parsing is generally error-prone, while printing is
      not expected to fail, so the handling isn't really symmetrical.

      [parse (print x)] should always be the identity, while no guarantee is
      given regarding [print (parse x)] *)
  type ('a, 'b) t

  (** Base constructor for Pp.t, from a parser function and a printer function.
      [name_constr] is used to construct the resulting name when on the left of
      a pipe. Names are for tracing errors. *)
  val pp :
    ?name:string -> ?name_constr:(string -> string) ->
    (pos:pos -> 'a -> 'b) ->
    ('b -> 'a) ->
    ('a, 'b) t

  (** Constructor fof Pp.t from a name and a pair *)
  val of_pair :
    string ->
    ('a -> 'b) * ('b -> 'a) ->
    ('a, 'b) t

  (** Base call for parsing with a pp *)
  val parse : ('a, 'b) t -> pos:pos -> 'a -> 'b

  (** Base call for printing with a pp *)
  val print : ('a, 'b) t -> 'b -> 'a

  (** Error handling *)

  (** Raises an exception handled by parser calls *)
  val unexpected : ?pos:pos -> unit -> 'a

  (** Warns in debug, fails with strict_mode or when [strict] is set to true *)
  val warn :
    ?pos:pos -> ?strict:bool -> ?exn:exn ->
    ('a, unit, string, unit) format4 -> 'a

  (** {3 Various pp constructors} *)

  module Op : sig

    (** Piping pps together: the left-hand pp is called first when parsing, last
        when printing *)
    val ( -| ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

    (** Combinator to parse lists to different types using nested pairs *)
    val ( ^+ ) : ('a, 'b) t -> ('a list, 'c) t -> ('a list, 'b * 'c) t

  end

  val identity : ('a, 'a) t

  (** Always parses to [None] *)
  val ignore : ('a, 'b option) t

  (** Identity pp, unless the check fails. The check is turned into an assertion
      when printing *)
  val check : ?name:string -> ?errmsg:string -> ('a -> bool) -> ('a, 'a) t

  val map_pair :
    ?name:string ->
    ?posf1:('a -> pos) ->
    ?posf2:('b -> pos) ->
    ('a, 'c) t -> ('b, 'd) t -> ('a * 'b, 'c * 'd) t

  (** Builds a pp of pairs by passing the second term along *)
  val map_fst :
    ('a, 'b) t -> ('a * 'c, 'b * 'c) t

  (** Builds a pp of pairs by passing the first term along *)
  val map_snd :
    ('a, 'b) t -> ('c * 'a, 'c * 'b) t

  val map_list :
    ?name:string ->
    ?posf:('a -> pos) -> ('a, 'b) t -> ('a list, 'b list) t

  val map_option : ?name:string -> ('a, 'b) t -> ('a option, 'b option) t

  (** Parsing fails on non-singleton lists *)
  val singleton : ('a list, 'a) t

  (** Use for the rightmost element to close a [^+] sequence, e.g.
      [pp1 ^+ pp2 ^+ last -| pp3] *)
  val last : ('a list, 'a) t


  module type STR = sig
    type t
    val of_string : string -> t
    val to_string : t -> string
  end

  (** Generates a string pp from a module with of/to string functions *)
  val of_module :
    string -> (module STR with type t = 'a) -> (string, 'a) t

  (** Parses to None on the empty list. Often combined with singleton
      ([opt (singleton _)]) *)
  val opt : ('a list, 'b) t -> ('a list, 'b option) t

  val default : 'a -> ('a option, 'a) t


  (** {3 low-level Pps for the Lines parser ([string list list])} *)

  type lines = string list list

  (** Provided an empty element, addition and fold operations with signatures as
      per Set.S, and a pp from lines to elements, returns a pp parsing from
      lines *)
  val lines_set:
    empty:'set ->
    add:('elt -> 'set -> 'set) ->
    fold:(('elt -> lines -> lines) -> 'set -> lines -> lines) ->
    (string list, 'elt) t ->
    (lines, 'set) t

  (** Provided an empty element, addition and fold operations with signatures as
      per Map.S, and a pp from lines to key, value pairs, returns a pp parsing
      from lines *)
  val lines_map :
    empty:'map ->
    add:('k -> 'v -> 'map -> 'map) ->
    fold:(('k -> 'v -> lines -> lines) -> 'map -> lines -> lines) ->
    (string list, 'k * 'v) t ->
    (lines, 'map) t

  (** {3 Pps for the type [value], used by opam-syntax files ([opamfile])} *)

  module V : sig
    (** These base converters raise [Unexpected] when not run on the right input
        (which is then converted to [Bad_format] by the parser. *)

    val bool : (value, bool) t
    val int : (value, int) t

    (** positive or null integer *)
    val pos_int : (value, int) t

    val ident : (value, string) t
    val string : (value, string) t

    (** Trimmed string *)
    val string_tr : (value, string) t

    (** Command arguments, i.e. strings or idents *)
    val simple_arg : (value, simple_arg) t

    (** Strings or bools *)
    val variable_contents : (value, variable_contents) t

    (** "[a b c]"; also allows just "a" to be parsed as a singleton list *)
    val list : (value, value list) t

    (** "(a b c)" *)
    val group : (value, value list) t

    (** Options in the [value] type sense, i.e. a value with an optional list
        of parameters in braces:
        "value {op1 op2}" *)
    val option :
      (value, value * value list) t

    val map_group : (value, 'a) t -> (value, 'a list) t

    (** An expected list depth may be specified to enable removal of extra
        brackets (never use [~depth] for an inner list) *)
    val map_list : ?depth:int -> (value, 'a) t -> (value, 'a list) t

    (** Normalises to the given list depth when parsing, and removes brackets
        that can be made implicit when printing *)
    val list_depth : int -> (value, value) t

    val map_option : (value, 'a) t -> (value list, 'b) t -> (value, 'a * 'b) t

    (** A pair is simply a list with two elements in the [value] type *)
    val map_pair :
      (value, 'a) t ->
      (value, 'b) t -> (value, 'a * 'b) t

    val url : (value, url) t

    (** Specialised url parser when the backend is already known *)
    val url_with_backend : OpamUrl.backend -> (value, url) t

    val compiler_version : (value, string) t

    val filter_ident :
      (value,
       name list * variable *
       (string * string) option)
        t

    val filter : (value list, filter) t

    (** Arguments in commands (term + optional filter) *)
    val arg : (value, simple_arg * filter option) t

    val command : (value, (simple_arg * filter option) list * filter option) t

    (** Simple dependency constraints *)
    val constraints :
      (value, 'a) t ->
      (value list, (OpamFormula.relop * 'a) OpamFormula.formula) t

    (** Dependency constraints mixed with filters *)
    val filtered_constraints :
      (value, 'version) t ->
      (value list, 'version filter_or_constraint OpamFormula.formula) t

    (** Package versions *)
    val version: (value, version) t

    (** Package versions as filters, as they may appear in dependency (may be an
        expanded string or an ident) *)
    val ext_version: (value, filter) t

      (** Returns an atom parser ("package" {>= "version"}) from a constraint
          and a version parser*)
    val package_atom:
      (value list, 'a) t -> (value, name * 'a) t

    (** Takes a parser for constraints. Lists without operator will be
        understood as conjunctions or disjunctions depending on the first
        argument. *)
    val package_formula :
      [< `Conj | `Disj ] ->
      (value list, 'a) t ->
      (value, (name * 'a) OpamFormula.formula) t

    (** Environment variable updates syntax *)
    val env_binding : (value, env_update) t

    (** For the "features" field, e.g. a list of [(variable "doc" {filter})] *)
    val features : (value, (variable * string * filter) list) t

    val os_constraint : (value, (bool * string) OpamFormula.formula) t
  end

  (** {3 Combinators to parse to a record from a list of (field name, field
      setter, field getter)} *)

  (** Used to parse a single field of a record: ['a] on the left is the
      accumulator, or value of the record parsed so far. *)
  type ('a, 'value) field_parser = ('a * 'value option, 'a) t

  (** Make a field parser from setter, getter and base pp. [cleanup] is an
      optional sanitisation function that is called on parsed elements
      before calling the setter. *)
  val ppacc :
    ?cleanup:(pos:pos -> 'acc -> 'a -> 'a) ->
    ('a -> 'acc -> 'acc) ->
    ('acc -> 'a) ->
    ('value, 'a) t ->
    ('acc, 'value) field_parser

  (** Same as [ppacc], but when the field may be unset in the record, i.e. the
      getter returns an option *)
  val ppacc_opt :
    ?cleanup:(pos:pos -> 'acc -> 'a -> 'a) ->
    ('a -> 'acc -> 'acc) ->
    ('acc -> 'a option) ->
    ('value, 'a) t ->
    ('acc, 'value) field_parser

  (** A field parser that ignores its argument *)
  val ppacc_ignore : ('a, value) field_parser

  (** {3 Specific Pps for items lists and fields (opamfile)} *)

  module I :
  sig
    val file : (opamfile, filename * opamfile_item list) t

    val map_file : (opamfile_item list, 'a) t -> (opamfile, filename * 'a) t

    val item : (opamfile_item, string * value) t

    val items : (opamfile_item list, (string * value) list) t

    type ('a, 'value) fields_def = (string * ('a, 'value) field_parser) list

    (** Parses an item list into a record using a fields_def *)
    val fields :
      ?name:string ->
      ?strict:bool ->
      empty:'a ->
      ?sections:(('a, opamfile_item list) fields_def) ->
      ('a, value) fields_def ->
      (opamfile_item list, 'a) t

    (** Partitions a file's items into the ones that are known but not defined
        in the file, the ones that are defined, and the ones that are in the
        file but unknown. All but the well-defined ones are ignored when
        printing back. *)
    val good_fields :
      ?name:string ->
      ?allow_extensions:bool ->
      ?sections:(('a, opamfile_item list) fields_def) ->
      ('a, value) fields_def ->
      (opamfile_item list,
       ('a, value) fields_def * ('a, opamfile_item list) fields_def *
       opamfile_item list * opamfile_item list) t

    (** Filters out any unrecognised items from the file during parsing, warning
        in debug and failing in strict mode in case of mismatches *)
    val check_fields :
      ?name:string ->
      ?allow_extensions:bool ->
      ?strict:bool ->
      ?sections:(('a, opamfile_item list) fields_def) ->
      ('a, value) fields_def ->
      (opamfile_item list, opamfile_item list) t

    (** Partitions items in an opamfile base on a condition on the variable
        names *)
    val partition_fields :
      (string -> bool) ->
      (opamfile_item list, opamfile_item list * opamfile_item list) t

    (** Partitions items in an opamfile base on a generic condition on the
        items *)
    val partition :
      (opamfile_item -> bool) ->
      (opamfile_item list, opamfile_item list * opamfile_item list) t

    (** Parse a single field from a file, return the result and the unchanged
        item list. The single field is ignored when printing back. *)
    val field :
      string ->
      (pos:pos -> value -> 'a) ->
      (opamfile_item list, 'a option * opamfile_item list) t

    (** Parse a single section with the given "kind", towards its name and
        contents *)
    val section :
      string ->
      (opamfile_item, (string option * opamfile_item list)) t

    (** Extracts a single item with the given variable name from an item list.
        The item is removed from the returned item list, and the two are
        re-combined when printing *)
    val extract_field :
      string ->
      (opamfile_item list, value option * opamfile_item list) t

    (** Checks the [opam_version] field; otherwise the identity *)
    val check_opam_version :
      ?optional:bool -> ?f:(opam_version -> bool) -> unit ->
      (opamfile_item list, opamfile_item list) t

    (** Signature handling (wip) *)

    (** A signature is a keyid, an algorithm and the signature proper *)
    type signature = string * string * string

    val signature : (value, signature) t

    exception Invalid_signature of
        pos * (string * string * string) list option

    (** Pp for signed files. Will assert fail if attempting to write a file with
        an invalid signature. *)
    val signed:
      check:(signature list -> string -> bool) ->
      (opamfile_item list, signature list * opamfile_item list) t
  end
end

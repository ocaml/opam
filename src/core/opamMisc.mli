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

(** Basic functions *)

(** {2 Abstract types} *)

(** Collection of abstract values *)
module type SET = sig

  include Set.S

  (** auto-map *)
  val map: (elt -> elt) -> t -> t

  (** Return one element. Fail if the set is not a singleton. *)
  val choose_one : t -> elt

  (** Make a set from a list *)
  val of_list: elt list -> t

  (** Pretty-print a set *)
  val to_string: t -> string

  (** Return a JSON representation of the given set *)
  val to_json: t -> OpamJson.t

  (** Find an element in the list *)
  val find: (elt -> bool) -> t -> elt

  module Op : sig
    val (++): t -> t -> t (** Infix set union *)
    val (--): t -> t -> t (** Infix set difference *)
    val (%%): t -> t -> t (** Infix set intersection *)
  end
end

(** Dictionaries of abstract values *)
module type MAP = sig

  include Map.S

  (** Pretty-printing *)
  val to_string: ('a -> string) -> 'a t  -> string

  (** Return a JSON representation of the given map. *)
  val to_json: ('a -> OpamJson.t) -> 'a t -> OpamJson.t

  (** Return the values in the map. *)
  val values: 'a t -> 'a list

  (** Return the keys in the map. *)
  val keys: 'a t -> key list

  (** A key will be in the union of [m1] and [m2] if it is appears
      either [m1] or [m2], with the corresponding value. If a key
      appears in both [m1] and [m2], then the resulting value is built
      using the function given as argument. *)
  val union: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (** Convert an assoc list to a map *)
  val of_list: (key * 'a) list -> 'a t

end

(** All abstract types should implement this signature *)
module type ABSTRACT = sig

  (** ABSTRACT type *)
  type t

  (** Create an abstract value from a string *)
  val of_string: string -> t

  (** Convert an abstract value to a string *)
  val to_string: t -> string

  (** Convert an abstract value to a JSON object *)
  val to_json: t -> OpamJson.t

  module Set: SET with type elt = t
  module Map: MAP with type key = t
end

(** Extended sets and maps *)
module type OrderedType = sig
  include Set.OrderedType
  val to_string: t -> string
  val to_json: t -> OpamJson.t
end

(** Set constructor *)
module Set: sig
  module Make (S: OrderedType): SET with type elt = S.t
end

(** Map constructor *)
module Map: sig
  module Make (S: OrderedType): MAP with type key = S.t
end

(** Base module, useful to abstract strings *)
module Base: sig
  type t = string
  val of_string: string -> t
  val to_string: t -> string
  val to_json: t -> OpamJson.t
  module Map: MAP with type key = string
  module Set: SET with type elt = string
end

(** {2 Integer manipulation} *)

(** Map of ints *)
module IntMap: MAP with type key = int

(** Set of ints *)
module IntSet: SET with type elt = int

(** Convert list items to string and concat. [sconcat_map sep f x] is equivalent
    to String.concat sep (List.map f x) but tail-rec. *)
val sconcat_map:
  ?left:string -> ?right:string -> ?nil:string ->
  string -> ('a -> string) -> 'a list -> string

(** Display a list of strings *)
val string_of_list: ('a -> string) -> 'a list -> string

(** Convert a list of items to string as a dashed list *)
val itemize: ?bullet:string -> ('a -> string) -> 'a list -> string

val string_map: (char -> char) -> string -> string

(** Display a pretty list: ["x";"y";"z"] -> "x, y and z".
    "and" can be changed by specifying [last] *)
val pretty_list: ?last:string -> string list -> string

(** Removes consecutive duplicates in a list *)
val remove_duplicates: 'a list -> 'a list

(** {2 String manipulation} *)

(** Map of strings *)
module StringMap: MAP with type key = string

(** Set of strings *)
module StringSet: SET with type elt = string

(** Set of string sets *)
module StringSetSet: SET with type elt = StringSet.t

(** Map of string sets *)
module StringSetMap: MAP with type key = StringSet.t

(** Strip a string *)
val strip: string -> string

(** Does a string starts with the given prefix ? *)
val starts_with: prefix:string -> string -> bool

(** Does a string ends with the given suffix ? *)
val ends_with: suffix:string -> string -> bool

(** Remove a prefix *)
val remove_prefix: prefix:string -> string -> string

(** Remove a suffix *)
val remove_suffix: suffix:string -> string -> string

(** Cut a string at the first occurence of the given char *)
val cut_at: string -> char -> (string * string) option

(** Same as [cut_at], but starts from the right *)
val rcut_at: string -> char -> (string * string) option

(** Does a string contains the given chars ? *)
val contains: string -> char -> bool

(** Split a string at occurences of a given characters. Empty strings are
    skipped. *)
val split: string -> char -> string list

(** The same as [split], but keep empty strings (leading, trailing or between
    contiguous delimiters) *)
val split_delim: string -> char -> string list

(** Returns the length of the string in terminal chars, ignoring ANSI color
    sequences from OpamGlobals.colorise *)
val visual_length: string -> int

(** left indenting. [~visual] can be used to indent eg. ANSI colored
    strings and should correspond to the visible characters of s *)
val indent_left: string -> ?visual:string -> int -> string

(** right indenting *)
val indent_right: string -> ?visual:string -> int -> string

(** Pads fields in a table with spaces for alignment. *)
val align_table: string list list -> string list list

(** Prints a table *)
val print_table: out_channel -> sep:string -> string list list -> unit

(** Cut a string *)
val sub_at: int -> string -> string

(** Cut long lines in string according to the terminal width *)
val reformat: ?start_column:int -> ?indent:int -> string -> string

(** {2 Option} *)

module Option: sig
  val map: ('a -> 'b) -> 'a option -> 'b option

  val iter: ('a -> unit) -> 'a option -> unit

  val default: 'a -> 'a option -> 'a

  val default_map: 'a option -> 'a option -> 'a option

  val compare: ('a -> 'a -> int) -> 'a option -> 'a option -> int

  val to_string: ?none:string -> ('a -> string) -> 'a option -> string

  module Op: sig
    val (>>=): 'a option -> ('a -> 'b option) -> 'b option
    val (>>|): 'a option -> ('a -> 'b) -> 'b option
    val (+!): 'a option -> 'a -> 'a
    val (++): 'a option -> 'a option -> 'a option
  end
end

(** {2 Misc} *)

(** Remove from a c-separated list of string the one with the given prefix *)
val reset_env_value: prefix:string -> char -> string -> string list

(** split a c-separated list of string in two according to the first
    occurrences of the string with the given [prefix]. The list of
    elements occurring before is returned in reverse order. If there are
    other elements with the same [prefix] they are kept in the second list.
 *)
val cut_env_value: prefix:string -> char -> string -> string list * string list

(** if rsync -arv return 4 lines, this means that no files have changed *)
val rsync_trim: string list -> string list

(** Exact regexp matching *)
val exact_match: Re.re -> string -> bool

(** Filter and map *)
val filter_map: ('a -> 'b option) -> 'a list -> 'b list

(** Insert a value in an ordered list *)
val insert: ('a -> 'a -> int) -> 'a -> 'a list -> 'a list

(** Lazy environment variable *)
val getenv: string -> string

(** Lazy environment *)
val env: unit -> (string * string) list

(** To use when catching default exceptions: ensures we don't catch fatal errors
    like C-c. try-with should _always_ (by decreasing order of preference):
    - either catch specific exceptions
    - or re-raise the same exception
    - or call this function on the caught exception *)
val fatal: exn -> unit

(** Register a backtrace for when you need to process a finalizer (that
    internally uses exceptions) and then re-raise the same exception.
    To be printed by pretty_backtrace. *)
val register_backtrace: exn -> unit

(** Return a pretty-printed backtrace *)
val pretty_backtrace: exn -> string

(** Prettify a local path (eg. replace /home/me/ by '~') *)
val prettify_path: string -> string

module OP: sig

  (** Function application (with lower priority) (predefined in OCaml 4.01+) *)
  val (@@): ('a -> 'b) -> 'a -> 'b

  (** Pipe operator -- reverse application (predefined in OCaml 4.01+) *)
  val (|>): 'a -> ('a -> 'b) -> 'b

  (** Function composition : (f @* g) x =~ f (g x) *)
  val (@*): ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

  (** Reverse function composition : (f @> g) x =~ g (f x) *)
  val (@>): ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

end

(** true if stdout is bound to a terminal *)
val tty_out : bool

(** When [stdout] refers to a terminal, query the number of columns.
    Otherwise return [max_int]. *)
val terminal_columns : unit -> int

(** Get the output of [uname -s] *)
val uname_s: unit -> string option

(** Get the output of [uname -m] *)
val uname_m: unit -> string option

(** Guess the shell compat-mode *)
val guess_shell_compat: unit -> [`csh|`zsh|`sh|`bash|`fish]

(** Guess the location of .profile *)
val guess_dot_profile: [`csh|`zsh|`sh|`bash|`fish] -> string

(** Like Pervasives.at_exit but with the possibility to call manually
    (eg. before exec()) *)
val at_exit: (unit -> unit) -> unit

(** Calls the functions registered in at_exit *)
val exec_at_exit: unit -> unit

(** / *)

val debug: bool ref

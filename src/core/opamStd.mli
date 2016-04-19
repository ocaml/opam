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

(** Generic stdlib functions (String, List, Option, Sys submodules...) *)

(** {2 Signatures and functors} *)

(** Sets with extended interface and infix operators *)
module type SET = sig

  include Set.S

  val map: (elt -> elt) -> t -> t

  (** Returns one element, assuming the set is a singleton. Raises [Not_found]
      on an empty set, [Failure] on a non-singleton. *)
  val choose_one : t -> elt

  val of_list: elt list -> t
  val to_string: t -> string
  val to_json: t -> OpamJson.t
  val find: (elt -> bool) -> t -> elt
  val find_opt: (elt -> bool) -> t -> elt option

  (** Raises Failure in case the element is already present *)
  val safe_add: elt -> t -> t

  module Op : sig
    val (++): t -> t -> t (** Infix set union *)

    val (--): t -> t -> t (** Infix set difference *)

    val (%%): t -> t -> t (** Infix set intersection *)
  end

end

(** Maps with extended interface *)
module type MAP = sig

  include Map.S

  val to_string: ('a -> string) -> 'a t  -> string
  val to_json: ('a -> OpamJson.t) -> 'a t -> OpamJson.t
  val keys: 'a t -> key list
  val values: 'a t -> 'a list
  val find_opt: key -> 'a t -> 'a option

  (** A key will be in the union of [m1] and [m2] if it is appears
      either [m1] or [m2], with the corresponding value. If a key
      appears in both [m1] and [m2], then the resulting value is built
      using the function given as argument. *)
  val union: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val of_list: (key * 'a) list -> 'a t

  (** Raises Failure in case the element is already present *)
  val safe_add: key -> 'a -> 'a t -> 'a t

  (** [update k f zero map] updates the binding of [k] in [map] using function
      [f], applied to the current value bound to [k] or [zero] if none *)
  val update: key -> ('a -> 'a) -> 'a -> 'a t -> 'a t
end

(** A signature for handling abstract keys and collections thereof *)
module type ABSTRACT = sig

  type t

  val of_string: string -> t
  val to_string: t -> string
  val to_json: t -> OpamJson.t

  module Set: SET with type elt = t
  module Map: MAP with type key = t
end

(** A basic implementation of ABSTRACT using strings *)
module AbstractString : ABSTRACT with type t = string

(** {3 Generators for set and map modules with printers} *)

module type OrderedType = sig
  include Set.OrderedType
  val to_string: t -> string
  val to_json: t -> OpamJson.t
end

module Set: sig
  module Make (S: OrderedType): SET with type elt = S.t
end

module Map: sig
  module Make (S: OrderedType): MAP with type key = S.t
end


(** {2 Integer collections} *)

(** Map of ints *)
module IntMap: MAP with type key = int

(** Set of ints *)
module IntSet: SET with type elt = int


(** {2 Utility modules extending the standard library on base types} *)

module Option: sig
  val map: ('a -> 'b) -> 'a option -> 'b option

  val iter: ('a -> unit) -> 'a option -> unit

  val default: 'a -> 'a option -> 'a

  val default_map: 'a option -> 'a option -> 'a option

  val compare: ('a -> 'a -> int) -> 'a option -> 'a option -> int

  val to_string: ?none:string -> ('a -> string) -> 'a option -> string

  val some: 'a -> 'a option

  val none: 'a -> 'b option

  (** [of_Not_found f x] calls [f x], catches [Not_found] and returns [None] *)
  val of_Not_found: ('a -> 'b) -> 'a -> 'b option

  module Op: sig
    val (>>=): 'a option -> ('a -> 'b option) -> 'b option
    val (>>|): 'a option -> ('a -> 'b) -> 'b option
    val (>>+): 'a option -> (unit -> 'a option) -> 'a option
    val (+!): 'a option -> 'a -> 'a
    val (++): 'a option -> 'a option -> 'a option
  end
end

module List : sig

  val cons: 'a -> 'a list -> 'a list

  (** Convert list items to string and concat. [sconcat_map sep f x] is equivalent
      to String.concat sep (List.map f x) but tail-rec. *)
  val concat_map:
    ?left:string -> ?right:string -> ?nil:string ->
    string -> ('a -> string) -> 'a list -> string

  (** Like [List.find], but returning option instead of raising *)
  val find_opt: ('a -> bool) -> 'a list -> 'a option

  val to_string: ('a -> string) -> 'a list -> string

  (** Removes consecutive duplicates in a list *)
  val remove_duplicates: 'a list -> 'a list

  (** Sorts the list, removing duplicates *)
  val sort_nodup: ('a -> 'a -> int) -> 'a list -> 'a list

  (** Filter and map *)
  val filter_map: ('a -> 'b option) -> 'a list -> 'b list

  (** Retrieves [Some] values from a list *)
  val filter_some: 'a option list -> 'a list

  (** Insert a value in an ordered list *)
  val insert: ('a -> 'a -> int) -> 'a -> 'a list -> 'a list

end

module String : sig

  (** {3 Collections} *)

  module Map: MAP with type key = string

  module Set: SET with type elt = string

  (** Set of string sets *)
  module SetSet: SET with type elt = Set.t

  (** Map of string sets *)
  module SetMap: MAP with type key = Set.t

  (** {3 Checks} *)

  val starts_with: prefix:string -> string -> bool
  val ends_with: suffix:string -> string -> bool
  val contains_char: string -> char -> bool
  val contains: sub:string -> string -> bool
  val exact_match: Re.re -> string -> bool

  (** {3 Manipulation} *)

  val map: (char -> char) -> string -> string
  val strip: string -> string
  val sub_at: int -> string -> string
  val remove_prefix: prefix:string -> string -> string
  val remove_suffix: suffix:string -> string -> string

  (** {4 Transformations} *)

  (** Cut a string at the first occurence of the given char *)
  val cut_at: string -> char -> (string * string) option

  (** Same as [cut_at], but starts from the right *)
  val rcut_at: string -> char -> (string * string) option

  (** Split a string at occurences of a given characters. Empty strings are
      skipped. *)
  val split: string -> char -> string list

  (** The same as [split], but keep empty strings (leading, trailing or between
      contiguous delimiters) *)
  val split_delim: string -> char -> string list

  val fold_left: ('a -> char -> 'a) -> 'a -> string -> 'a

end

module Format : sig

  (** {4 Querying information} *)

  (** Returns the length of the string in terminal chars, ignoring ANSI color
      sequences from OpamConsole.colorise *)
  val visual_length: string -> int

  (** {4 Text formatting functions} *)

  (** Truncates the string to not visually get over [width] columns *)
  val cut_at_visual: string -> int -> string

  (** left indenting. [~visual] can be used to indent eg. ANSI colored
      strings and should correspond to the visible characters of s *)
  val indent_left: string -> ?visual:string -> int -> string

  val indent_right: string -> ?visual:string -> int -> string

  (** Pads fields in a table with spaces for alignment. *)
  val align_table: string list list -> string list list

  (** Cut long lines in string according to the terminal width *)
  val reformat: ?start_column:int -> ?indent:int -> string -> string

  (** Convert a list of items to string as a dashed list *)
  val itemize: ?bullet:string -> ('a -> string) -> 'a list -> string

  (** Display a pretty list: ["x";"y";"z"] -> "x, y and z".
      "and" can be changed by specifying [last] *)
  val pretty_list: ?last:string -> string list -> string

  (** {4 Printing} *)

  (** Prints a table. If [cut] is set (the default for stdout and stderr),
      overflowing lines are truncated. *)
  val print_table:
    ?cut:bool -> out_channel -> sep:string -> string list list -> unit
end

module Exn : sig

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

end

(** {2 Manipulation and query of environment variables} *)

module Env : sig
  (** Remove from a c-separated list of string the one with the given prefix *)
  val reset_value: prefix:string -> char -> string -> string list

  (** split a c-separated list of string in two according to the first
      occurrences of the string with the given [prefix]. The list of
      elements occurring before is returned in reverse order. If there are
      other elements with the same [prefix] they are kept in the second list.
  *)
  val cut_value: prefix:string -> char -> string -> string list * string list

  val get: string -> string

  val getopt: string -> string option

  val list: unit -> (string * string) list
end

(** {2 System query and exit handling} *)

module Sys : sig

  (** {3 Querying} *)

  (** true if stdout is bound to a terminal *)
  val tty_out : bool

  (** true if stdin is bound to a terminal *)
  val tty_in : bool

  (** Queried lazily, but may change on SIGWINCH *)
  val terminal_columns : unit -> int

  (** The user's home directory. Queried lazily *)
  val home: unit -> string

  type os = Darwin
          | Linux
          | FreeBSD
          | OpenBSD
          | NetBSD
          | DragonFly
          | Cygwin
          | Win32
          | Unix
          | Other of string

  (** Queried lazily *)
  val os: unit -> os

  val os_string: unit -> string

  (** Queried lazily *)
  val arch: unit -> string

  (** Guess the shell compat-mode *)
  val guess_shell_compat: unit -> [`csh|`zsh|`sh|`bash|`fish]

  (** Guess the location of .profile *)
  val guess_dot_profile: [`csh|`zsh|`sh|`bash|`fish] -> string

  (** The separator character used in the PATH variable (varies depending on
      OS) *)
  val path_sep: unit -> char

  (** {3 Exit handling} *)

  (** Like Pervasives.at_exit but with the possibility to call manually
      (eg. before exec()) *)
  val at_exit: (unit -> unit) -> unit

  (** Calls the functions registered in at_exit. Unneeded if exiting normally *)
  val exec_at_exit: unit -> unit

  (** Indicates intention to exit the program with given exit code *)
  exception Exit of int

  (** Indicates intention to exec() the given command (paramters as per
      [Unix.execvpe]), after proper finalisations. It's the responsibility of
      the main function to catch this, call [exec_at_exit], and
      [Unix.execvpe]. *)
  exception Exec of string * string array * string array

  (** Raises [Exit i] *)
  val exit: int -> 'a

end

(** {2 General use infix function combinators} *)

module Op: sig

  (** Function application (with lower priority) (predefined in OCaml 4.01+) *)
  val (@@): ('a -> 'b) -> 'a -> 'b

  (** Pipe operator -- reverse application (predefined in OCaml 4.01+) *)
  val (|>): 'a -> ('a -> 'b) -> 'b

  (** Function composition : (f @* g) x =~ f (g x) *)
  val (@*): ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

  (** Reverse function composition : (f @> g) x =~ g (f x) *)
  val (@>): ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

end

(** {2 Helper functions to initialise configuration from the environment} *)

module Config : sig

  type env_var = string

  val env_bool: env_var -> bool option

  val env_int: env_var -> int option

  (* Like [env_int], but accept boolean values for 0 and 1 *)
  val env_level: env_var -> int option

  val env_string: env_var -> string option

  val env_float: env_var -> float option

  val env_when: env_var -> [ `Always | `Never | `Auto ] option

  val env_when_ext: env_var -> [ `Extended | `Always | `Never | `Auto ] option

  val resolve_when: auto:(bool Lazy.t) -> [ `Always | `Never | `Auto ] -> bool

  (** Sets the OpamCoreConfig options, reading the environment to get default
      values when unspecified *)
  val init: ?noop:_ -> (unit -> unit) OpamCoreConfig.options_fun

  (** Like [init], but returns the given value. For optional argument
      stacking *)
  val initk: 'a -> 'a OpamCoreConfig.options_fun

  module type Sig = sig

    (** Read-only record type containing the lib's configuration options *)
    type t

    (** Type of functions with optional arguments for setting each of [t]'s
        fields, similarly named, and returning ['a] *)
    type 'a options_fun

    (** The default values of the options to use at startup *)
    val default: t

    (** Use to update any option in a [t], using the optional arguments of
        [options_fun]. E.g. [set opts ?option1:1 ?option4:"x" ()] *)
    val set: t -> (unit -> t) options_fun

    (** Same as [set], but passes the result to a continuation, allowing
        argument stacking *)
    val setk: (t -> 'a) -> t -> 'a options_fun

    (** The global reference containing the currently set library options.
        Access using [OpamXxxConfig.(!r.field)]. *)
    val r: t ref

    (** Updates the currently set options in [r] according to the optional
        arguments *)
    val update: ?noop:_ -> (unit -> unit) options_fun

    (** Sets the options, reading the environment to get default values when
        unspecified *)
    val init: ?noop:_ -> (unit -> unit) options_fun

    (** Sets the options like [init], but returns the given value (for arguments
        stacking) *)
    val initk: 'a -> 'a options_fun

  end

end

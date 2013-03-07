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

  (** Find an element in the list *)
  val find: (elt -> bool) -> t -> elt

end

(** Dictionaries of abstract values *)
module type MAP = sig

  include Map.S

  (** Pretty-printing *)
  val to_string: ('a -> string) -> 'a t  -> string

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

  module Set: SET with type elt = t
  module Map: MAP with type key = t
end

(** Extended sets and maps *)
module type OrderedType = sig
  include Set.OrderedType
  val to_string: t -> string
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
  module Map: MAP with type key = string
  module Set: SET with type elt = string
end

(** {2 Integer manipulation} *)

(** Map of ints *)
module IntMap: MAP with type key = int

(** Set of ints *)
module IntSet: SET with type elt = int

(** Display a list of strings *)
val string_of_list: ('a -> string) -> 'a list -> string

(** Display a pretty list: ["x";"y";"z"] -> "x, y and z" *)
val pretty_list: string list -> string

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

(** Split a string *)
val split: string -> char -> string list

(** left indenting *)
val indent_left: string -> int -> string

(** right indenting *)
val indent_right: string -> int -> string

(** Cut a string *)
val sub_at: int -> string -> string

(** Cut a git string of the form /git/address[#SHA] into (address * commit) *)
val git_of_string: string -> string * string option

(** {2 Misc} *)

(** Remove from a ':' separated list of string the one with the given prefix *)
val reset_env_value: prefix:string -> string -> string list

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

(** Return a pretty-printed backtrace *)
val pretty_backtrace: unit -> string

module OP: sig

  (** Pipe operator *)
  val (|>): ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

  (** [finally f cleaner] call the [cleaner] function when [f] is
      complete even in the presence of exceptions. *)
  val finally: (unit -> 'a) -> (unit -> unit) -> 'a

end

(** When [stdout] refers to a terminal, query the number of columns.
    Otherwise return [max_int]. *)
val terminal_columns : unit -> int

(** Get the output of [uname -s] *)
val uname_s: unit -> string option

(** Guess the shell compat-mode *)
val guess_shell_compat: unit -> [`sh|`csh|`zsh]

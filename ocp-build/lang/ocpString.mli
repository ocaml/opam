(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

(** Extension of the stdlib String module *)

(** [for_all fn s] tests whether every characters of [s] verify [fn] *)
val for_all : (char -> bool) -> string -> bool

(** [exists fn s] tests whether at least a character of [s] verifies
    [fn] *)
val exists : (char -> bool) -> string -> bool

(** [before s pos] returns the substring before [pos] (the character
    at position [pos] is discarded) *)
val before : string -> int -> string

(** [after s pos] returns the substring after [pos] (the character at
    position [pos] is discarded) *)
val after : string -> int -> string

(** [starts_with s ~prefix] checks whether [s] starts with [prefix] *)
val starts_with : string -> prefix:string -> bool

(** [ends_with s ~suffix] checks whether [s] ends with [suffix] *)
val ends_with : string -> suffix:string -> bool

(** [cut ?keep pos s] returns the substrings of [s] before and after
    the position [pos]. If [keep] is set, then the character at position
    [pos] is kept (default is [keep=false]). *)
val cut : ?keep:bool -> string -> int -> string * string

(** [cuts str] returns all the cuts of string [str]. *)
val cuts : string -> (string * string) list

(** [cut_at ?keep c s] returns the substring of [s] before and after
    the position first occurence of character [c]. If [keep] is set,
    then the character at position [pos] is kept (default is
    [keep=false]). *)
val cut_at : ?keep:bool -> string -> char -> string * string

(** [rcut_at ?keep c s] returns the substring of [s] before and after
    the position last occurence of character [c]. If [keep] is set,
    then the character at position [pos] is kept (default is
    [keep=false]). *)
val rcut_at : ?keep:bool -> string -> char -> string * string

(** [strip s] removes white spaces, tabs and return chars from both sides of [s] *)
val strip : ?fn:(char -> bool) -> string -> string

(** [lstrip s] removes white spaces, tabs and return chars from the left of [s] *)
val lstrip : ?fn:(char -> bool) -> string -> string

(** [rstrip s] removes white spaces, tabs and return chars from the right of [s] *)
val rstrip : ?fn:(char -> bool) -> string -> string

(** [split s c] splits the string [s] on characters [c],
   starting from the left. *)
val split : string -> char -> string list

(** [split s c] splits the string [s] on characters [c],
   starting from the left, removing empty sub strings. *)
val split_simplify : string -> char -> string list

(** [rsplit s c] splits the string [s] on characters [c],
    starting from the right. *)
val rsplit : string -> char -> string list

(** [split ?n s c] splits the string [s] on characters [c] at most [n]
    times, starting from the left. *)
val splitn : int  -> string -> char -> string list

(** [rsplit ?n s c] splits the string [s] on characters [c] at most
    [n] times, starting from the right. *)
val rsplitn : int -> string -> char -> string list

exception Out_of_bounds

(** [indexes lines n] returns the pair ([i],[j]) such that
    [lines.(i).[j]] = [(String.concat "" lines).[n]]. Can raise
    [Out_of_bounds]. *)
val indexes : string array -> int -> int * int

(** [first_word s] returns the first word of the sentence [s]. Raise
    [Not_found] if there is no such word. *)
val first_word : string -> string

(** [compact s] give a compact representation of [s], without spaces
    and capital letters *)
val compact : string -> string

(** Is the character blank (ie. white-space, tab, return) ? *)
val is_ws : char -> bool

(** Is the character blank (ie. white-space, tab, return) or a newline ? *)
val is_ws_or_nl : char -> bool

(** Same as [String.index] but it returns the first occurence of [c1]
    immediatly followed by [c2] in [s].*)
val index2 : string -> char -> char -> int

(** Same as [String.index_from] but it returns the first occurence of
    [c1] immediatly followed by [c2] in [s], starting at position
    [pos]. *)
val index2_from : string -> int -> char -> char -> int

(** [prefixes s] returns the non-empty prefixes of the string [s]
    (including [s]) *)
val prefixes : string -> string list

(** [suffixes s] returns the non-empty suffixes of the string [s]
    (including [s]) *)
val suffixes : string -> string list

(** [substrings s] retuns all the sub-strings of [s] *)
val substrings : string -> string list

(** Split a string using a separator list *)
val split_chars : string -> char list -> string list

(** [find sub s] looks for the string [sub] in [s] and returns the
position of its first occurrence. *)
val find : string -> string -> int

(** [find_from sub s pos] looks for the string [sub] in [s] after
   position [pos] and returns the position of its first occurrence. *)
val find_from : string -> string -> int -> int

(** [unspace s] returns a string, where blanks (space or tab) have been
   removed before and after [s]. It can be [s] if no space was
   removed. *)
val unspace : string -> string

(** [replace_chars s list] returns a copy of [s], where all chars
   appearing in [list] have been replaced by the associated string
   in [list]. *)
val replace_chars : string -> (char * string) list -> string

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

(*************************************************************)
(*                                                           *)
(*                                                           *)
(*                     High-level interface                  *)
(*                                                           *)
(*                                                           *)
(*************************************************************)

type config_file

val create_config_file : File.t -> config_file






(* Loading and saving config_files *)

(* [load file] loads the option file. All options whose value is specified
in the option file are updated. *)
val load : config_file -> unit

(* [append file filename] loads the specified option file. All options whose
value is specified in this file are updated. *)
val append : config_file -> File.t -> unit

(*d [save ()] saves all the options values to the option file. *)
val save : config_file -> unit

(*d [save_with_help ()] saves all the options values to the option file,
with the help provided for each option. *)
val save_with_help : config_file -> unit

val config_file : config_file -> File.t
val set_config_file : config_file -> File.t -> unit

(* Loading and saving Hooks *)

val set_before_save_hook : config_file -> int -> (unit -> unit) -> unit
val set_after_save_hook : config_file -> (unit -> unit) -> unit
val set_after_load_hook : config_file -> (unit -> unit) -> unit



(* Options *)

(* the type of an option *)
type 'a config_option

(* The type of a class of options.  A class is a set of options which
   use the same conversion functions from loading and saving. *)
type 'a option_class

val create_option : config_file -> string list -> ?short_help:string ->            string list -> ?level:int -> 'a option_class -> 'a -> 'a config_option

(* Basic classes of options *)

val string_option : string option_class
val int_option : int option_class
val int64_option : int64 option_class
val bool_option : bool option_class
val float_option : float option_class
val string2_option : (string * string) option_class
val file_option : File.t option_class

(* parameterized options *)
val list_option : 'a option_class -> 'a list option_class
val array_option : 'a option_class -> 'a array option_class
val intmap_option : ('a -> int) -> 'a option_class -> 'a IntMap.t option_class
val option_option : 'a option_class -> 'a option option_class
val smalllist_option : 'a option_class -> 'a list option_class
val sum_option : (string * 'a) list -> 'a option_class
val tuple2_option :
  'a option_class * 'b option_class -> ('a * 'b) option_class
val tuple3_option : 'a option_class * 'b option_class * 'c option_class ->
  ('a * 'b * 'c) option_class
val tuple4_option : 'a option_class * 'b option_class * 'c option_class
  * 'd option_class->
  ('a * 'b * 'c * 'd) option_class
val tuple5_option : 'a option_class * 'b option_class * 'c option_class
  * 'd option_class * 'e option_class->
  ('a * 'b * 'c * 'd * 'e) option_class

(*4 Using options *)

val ( !! ) : 'a config_option -> 'a
val ( =:= ) : 'a config_option -> 'a -> unit



(* Options Sections *)

type config_section

val create_config_section :
  config_file -> string list -> string -> config_section





(*************************************************************)
(*                                                           *)
(*                                                           *)
(*                      Low-level interface                  *)
(*                                                           *)
(*                                                           *)
(*************************************************************)

module LowLevel : sig

    (* Simple options:
  This will enable very simple configuration, by a mouse-based configurator.
  Options will be defined by a special function, which will also check
  if a value has been provided  by the user in its .gwmlrc file.
  The .gwmlrc will be created by a dedicated tool, which could be used
  to generate both .gwmlrc and .efunsrc files.

    *)

(*d This module implements a simple mechanism to handle program options files.
  An options file is defined as a set of [variable = value] lines,
  where value can be a simple string, a list of values (between brackets
or parentheses) or a set of [variable = value] lines between braces.
The option file is automatically loaded and saved, and options are
manipulated inside the program as easily as references. *)

(*d The abstract type for a class of options.*)

type option_value =
  Module of option_module
| StringValue of  string
| IntValue of int
| FloatValue of float
| List of option_value list
| SmallList of option_value list
| OnceValue of option_value
| DelayedValue of (Buffer.t -> string -> unit)

and option_module =
  (string * option_value) list

exception SideEffectOption

val prune_file : config_file -> unit
(*4 Operations on option files *)



(*4 Creating options *)
val create_section_option : config_section -> string list ->
  ?short_help:string -> string list -> ?level:int ->
  'a option_class -> 'a -> 'a config_option

val option_hook : 'a config_option -> (unit -> unit) -> unit
val add_option_hook : 'a config_option -> (unit -> unit) -> unit
val clear_option_hooks : 'a config_option -> unit

val color_option : string option_class
val font_option : string option_class

val hasharray_option : 'a -> (int * 'a * 'b) option_class -> ('a, 'b) Hashtbl.t array option_class

(*4 Using options *)

val shortname : 'a config_option -> string
val option_type : 'a config_option -> string
val get_help : 'a config_option -> string
val advanced : 'a config_option -> bool

(*4 Creating new option classes *)

val get_class : 'a config_option -> 'a option_class

val class_hook : 'a option_class -> ('a config_option -> unit) -> unit

val define_option_class :
  string -> (option_value -> 'a) -> ('a -> option_value) -> 'a option_class

val to_value : 'a option_class -> 'a -> option_value
val from_value : 'a option_class -> option_value -> 'a

val value_to_string : option_value -> string
val string_to_value : string -> option_value
val value_to_int : option_value -> int
val int_to_value : int -> option_value
val value_to_int64 : option_value -> int64
val int64_to_value : int64 -> option_value
val bool_of_string : string -> bool
val value_to_bool : option_value -> bool
val bool_to_value : bool -> option_value
val value_to_float : option_value -> float
val float_to_value : float -> option_value
val value_to_string2 : option_value -> string * string
val string2_to_value : string * string -> option_value
val value_to_list : (option_value -> 'a) -> option_value -> 'a list
val list_to_value : ('a -> option_value) -> 'a list -> option_value
val smalllist_to_value : ('a -> option_value) -> 'a list -> option_value

(*
  val value_to_path : option_value -> string list
  val path_to_value : string list -> option_value
*)

val value_to_tuple2 :
  (option_value * option_value -> 'a) -> option_value -> 'a
val tuple2_to_value :
  ('a -> option_value * option_value) -> 'a -> option_value


val filename_to_value : File.t -> option_value
val value_to_filename : option_value -> File.t

val set_simple_option : config_file -> string -> string -> unit
val get_simple_option : config_file -> string -> string
val set_option_hook : config_file -> string -> (unit -> unit) -> unit

val set_string_wrappers : 'a option_class ->
  ('a -> string) -> (string -> 'a) -> unit

val once_value : option_value -> option_value
(* val info_of_option : 'a config_option  -> option_info *)

val array_to_value : ('a -> option_value) -> 'a array -> option_value
val value_to_array : (option_value -> 'a) -> option_value -> 'a array

val restore_default : 'a config_option -> unit
(* val set_option_desc : 'a config_option -> string -> unit *)

val sections : config_file -> config_section list
(*
  val strings_of_section_options :
  string -> config_section -> option_info list
*)

val section_name : config_section -> string
val iter_file : (Obj.t config_option -> unit) -> config_file -> unit
val iter_section : (Obj.t config_option -> unit) -> config_section -> unit


val option_value_to_string : option_value -> string (* debug *)

val set_volatile: 'a config_option -> unit

(*
module SimpleOptions : sig

    val name : 'a config_option -> string
    val help : 'a config_option -> string

    val iter_sections : (config_section -> unit) -> config_file -> unit
    val iter_options : (Obj.t t -> unit) -> config_section -> unit
    val find : config_file -> string -> 'a config_option
    val to_string : 'a config_option -> string
    val of_string : 'a config_option -> string -> unit

  end
*)

(*
type option_info = {
    option_name : string;
    option_shortname : string;
    option_desc : string;
    option_value : string;
    option_help : string;
    option_advanced : bool;
    option_default : string;
    option_type : string;
  }
val simple_options : string -> config_file -> option_info list

val simple_args : string -> config_file -> (string * Arg.spec * string) list

val prefixed_args :
  string -> config_file -> (string * Arg.spec * string) list

*)
end

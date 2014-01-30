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

(* A simple interface: no labels, no optional arguments. Use FileLabels if
   you want labels *)

(****************************)
(* Filename management *)
(****************************)

type t

val win32 : bool

val of_string : string -> t
val of_unix_string : string -> t
val of_win32_string : string -> t

    (* Force either /absolute_path or ./implicit_path *)
val to_rooted_string : t -> string
val path_separator : char

val to_string : t -> string

val concat : t -> t -> t
val add_basename : t -> string -> t
val add_basenames : t -> string list -> t
val add_suffix : t -> string -> t
val check_suffix : t -> string -> bool

    (*d [safe_basename s] returns s if s is not a path,
     * returns the last basename of s otherwise. *)
val safe_basename : string -> string

val dirname : t -> t
val basename : t -> string

(*val output : t -> string*)

val extensions : t -> string list
val last_extension : t -> string
val chop_extension : t -> t

val is_implicit : t -> bool
val is_absolute : t -> bool

val temp_file : t -> string -> t
val current_dir_name : t

val of_path : string -> string list -> t

val equal : t -> t -> bool
val getcwd : unit -> t


(****************************)
(* File management *)
(****************************)


(** Cut a filename at the last extension position. The extension is
    converted to lowercase, and DOES NOT contain the initial dot. *)
val cut_last_extension : string -> string * string

(** Get the contents of a channel *)
val string_of_channel : in_channel -> string

(** Output a line in a channel *)
val output_line : out_channel -> string -> unit

(** Get all the lines of a files *)
val lines_of_file : string -> string list

(** [file_of_lines name lines] saves the [lines] into the file [name] *)
val file_of_lines : string -> string list -> unit

(** Get the contents of a file *)
val string_of_file : string -> string

(** [file_of_string name str] saves [str] into the file [name] *)
val file_of_string : string -> string -> unit

(** [iter_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iter_lines : (string -> unit) -> string -> unit

(** [iteri_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iteri_lines : (int -> string -> unit) -> string -> unit

(** [find_in_path path filename] searches a file in a list of directories. *)
val find_in_path: string list -> string -> string

(** [sub_lines filename off len] returns [len] lines of [filename], starting at [off] *)
val sub_lines : string -> int -> int -> string list

module RawIO : sig

  val copy_file : string -> string -> unit
  val iter_blocks : (string -> int -> int -> unit) -> string -> unit
  val safe_mkdir : string -> unit
  val copy_rec : string -> string -> unit
  val uncopy_rec : string -> string -> unit
  val iter_dir : (string -> unit) -> string -> unit

end

module X : sig

  val copy_file : t -> t -> unit

(** Get all the lines of a files *)
val read_lines : t -> string list

(** [file_of_lines name lines] saves the [lines] into the file [name] *)
val write_lines : t -> string list -> unit

(** Get the contents of a file *)
val read_to_string : t -> string

(** Get the contents of a file *)
val read_part_to_string : t -> int -> int -> string

(** [file_of_string name str] saves [str] into the file [name] *)
val write_of_string : t -> string -> unit

(** [iter_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iter_lines : (string -> unit) -> t -> unit

(** [iteri_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iteri_lines : (int -> string -> unit) -> t -> unit

(** [sub_lines filename off len] returns [len] lines of [filename], starting at [off] *)
val sub_lines : t -> int -> int -> string list

val exists : t -> bool
(*val size64 : t -> int64 *)
val size : t -> int
val getcwd : unit -> t

val open_in : t -> in_channel
val open_out : t -> out_channel
val open_in_bin : t -> in_channel
val open_out_bin : t -> out_channel
val open_fd :
  t -> MinUnix.open_flag list -> MinUnix.file_perm -> MinUnix.file_descr

val iter : (string -> int -> int -> unit) -> t -> unit

val is_directory : t -> bool
val remove : t -> unit
val rename : t -> t -> unit

val stat : t -> MinUnix.stats
val lstat : t -> MinUnix.stats
end

module Dir : sig
  val mkdir : t -> int -> unit
  val make : t -> unit
  val make_all : t -> unit
  val list : t -> string list
  val list_files : t -> t list
  val iter : (string -> unit) -> t -> unit
  val iter_files : (t -> unit) -> t -> unit
  val remove : t -> unit
  val remove_all : t -> unit
end

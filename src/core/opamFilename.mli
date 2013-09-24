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

(** Typed filename manipulation *)

(** Basenames *)
module Base: OpamMisc.ABSTRACT

(** Directory names *)
module Dir: OpamMisc.ABSTRACT

(** Return the current working directory *)
val cwd: unit -> Dir.t

(** Remove a directory *)
val rmdir: Dir.t -> unit

(** Clean the contents of a directory. *)
val cleandir: Dir.t -> unit

(** Create a directory *)
val mkdir: Dir.t -> unit

(** List the sub-directory recursively *)
val rec_dirs: Dir.t -> Dir.t list

(** List the sub-directory (do not recurse) *)
val dirs: Dir.t -> Dir.t list

(** Evaluate a function in a given directory *)
val in_dir: Dir.t -> (unit -> 'a) -> 'a

(** Execute a list of commands in a given directory *)
val exec: Dir.t -> ?env:(string * string) list -> ?name:string ->
  ?metadata:(string * string) list -> ?keep_going:bool -> string list list -> unit

(** Move a directory *)
val move_dir: src:Dir.t -> dst:Dir.t -> unit

(** Copy a directory *)
val copy_dir: src:Dir.t -> dst:Dir.t -> unit

(** Link a directory *)
val link_dir: src:Dir.t -> dst:Dir.t -> unit

(** Does the directory existsb ? *)
val exists_dir: Dir.t -> bool

(** Return the parent directory *)
val dirname_dir: Dir.t -> Dir.t

(** Return the deeper directory name *)
val basename_dir: Dir.t -> Base.t

(** Turn a full path into a list of directory names *)
val to_list_dir: Dir.t -> Dir.t list

(** Creation from a raw string (as {i http://<path>}) *)
val raw_dir: string -> Dir.t

(** Execute a function in a temp directory *)
val with_tmp_dir: (Dir.t -> 'a) -> 'a

include OpamMisc.ABSTRACT

(** Generic filename *)
type generic_file =
  | D of Dir.t
  | F of t

(** Create a filename from a Dir.t and a basename *)
val create: Dir.t -> Base.t -> t

(** Create a file from a basename and the current working directory
    as dirname *)
val of_basename: Base.t -> t

(** Creation from a raw string (as {i http://<path>}) *)
val raw: string -> t

(** Prettify a filename:
    - replace /path/to/opam/foo by <opam>/foo
    - replace /path/to/home/foo by ~/foo *)
val prettify: t -> string

(** Prettify a dirname. *)
val prettify_dir: Dir.t -> string


(** Return the directory name *)
val dirname: t -> Dir.t

(** Return the base name *)
val basename: t -> Base.t

(** Retrieves the contents from the hard disk. *)
val read: t -> string

(** Open a channel from a given file. *)
val open_in: t -> in_channel

(** Removes everything in [filename] if existed. *)
val remove: t -> unit

(** Removes everything in [filename] if existed, then write [contents] instead. *)
val write: t -> string -> unit

(** see [Sys.file_exists] *)
val exists: t -> bool

(** Check whether a file has a given suffix *)
val check_suffix: t -> string -> bool

(** Add a file extension *)
val add_extension: t -> string -> t

(** Remove the file extension *)
val chop_extension: t -> t

(** List all the filenames, recursively *)
val rec_files: Dir.t -> t list

(** List all the filename. Do not recurse. *)
val files: Dir.t -> t list

(** Apply a function on the contents of a file *)
val with_contents: (string -> 'a) -> t -> 'a

(** Copy a file in a directory. If [root] is set, copy also the
    sub-directories. For instance, [copy_in ~root:"/foo" "/foo/bar/gni"
    "/toto"] creates ["/toto/bar/gni"]. *)
val copy_in: ?root:Dir.t -> t -> Dir.t -> unit

(** Move a file *)
val move: src:t -> dst:t -> unit

(** Symlink a file in a directory *)
val link_in: t -> Dir.t -> unit

(** Read a symlinked file *)
val readlink: t -> t

(** Is a symlink ? *)
val is_symlink: t -> bool

(** Copy a file *)
val copy: src:t -> dst:t -> unit

(** Symlink a file. If symlink is not possible on the system, use copy instead. *)
val link: src:t -> dst:t -> unit

(** Extract an archive in a given directory (it rewrites the root to
    match [Dir.t] dir if needed) *)
val extract: t -> Dir.t -> unit

(** Extract an archive in a given directory (which should already exists) *)
val extract_in: t -> Dir.t -> unit

(** Extract a generic file *)
val extract_generic_file: generic_file -> Dir.t -> unit

(** Check whether a filename starts by a given Dir.t *)
val starts_with: Dir.t -> t -> bool

(** Check whether a filename ends with a given suffix *)
val ends_with: string -> t -> bool

(** Remove a prefix from a file name *)
val remove_prefix: Dir.t -> t -> string

(** Remove a suffix from a filename *)
val remove_suffix: Base.t -> t -> string

(** download a remote file in a given directory. Return the location
    of the downloaded file if the download is successful.  *)
val download: overwrite:bool -> t -> Dir.t -> t

(** same as [download], but with a specified destination filename instead of a
    directory *)
val download_as: overwrite:bool -> t -> t -> unit

(** iterate downloads until one is sucessful *)
val download_iter: overwrite:bool -> t list -> Dir.t -> t

(** Apply a patch to a directory *)
val patch: t -> Dir.t -> unit

(** Compute the MD5 digest of a file *)
val digest: t -> string

(** Compute the MD5 digest a file. Return the empty list if the file
    does not exist. *)
val checksum: t -> string list

(** Compute the MD5 digest for all files in a directory. *)
val checksum_dir: Dir.t -> string list

(** Create an empty file *)
val touch: t -> unit

(** Change file permissions *)
val chmod: t -> int -> unit

(** File locks *)
val with_flock: t -> ('a -> 'b) -> 'a -> 'b

(** [copy_if_check t src dst] copies all the files from one directory
    to another. Do nothing if OPAMDONOTCOPYFILE is set to a non-empty
    value. *)
val copy_files: src:Dir.t -> dst:Dir.t -> unit

module OP: sig

  (** Create a new directory *)
  val (/): Dir.t -> string -> Dir.t

  (** Create a new filename *)
  val (//): Dir.t -> string -> t

end

(** Simple structure to hanle file attributes *)
module Attribute: sig

  include OpamMisc.ABSTRACT

  (** Get remote filename *)
  val base: t -> Base.t

  (** MD5 digest of the remote file *)
  val md5: t -> string

  (** File permission *)
  val perm: t -> int option

  (** Constructor*)
  val create: Base.t -> string -> int -> t

end

(** Convert a filename to an attribute, relatively to a root *)
val to_attribute: Dir.t -> t -> Attribute.t

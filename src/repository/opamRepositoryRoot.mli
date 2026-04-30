(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025-2026 Kate Deplaix                                    *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** This module abstract the notion of repository root over its concrete
    implementation (could be a database, a file, a directory, etc.) *)

open OpamTypes

(** Repository root implemented as a directory *)
module Dir : sig
  type t

  val of_dir : dirname -> t
  val to_dir : t -> dirname
  val to_string : t -> string

  (** [quarantine dir] returns a path to a temporary directory dedicated to the
      original repository root. The returned directory is not created and
      points to a statically known directory in the same parent directory
      as [dir]. *)
  val quarantine : t -> t

  val with_tmp : (t -> 'a) -> 'a
  val backup : inn:dirname -> t -> t

  val cwd : unit -> t
  val exists : t -> bool
  val remove : t -> unit
  val move : src:t -> dst:t -> unit
  val copy : src:t -> dst:t -> unit
  val copy_except_vcs : src:t -> dst:t -> unit
  val make_empty : t -> unit
  val dirs : t -> dirname list
  val is_empty : t -> bool option
  val dirname : t -> dirname

  module Op: sig
    val ( / ) : t -> string -> dirname
    val ( // ) : t -> string -> filename
  end

  (* Repository paths *)
  module Path : OpamRepositoryPath.PATH
    with type repo_root = t
     and type repo_dirname = dirname
     and type 'a typed_file = 'a OpamFile.t

end

val make_tar_gz : filename -> Dir.t -> unit
val extract_in_job : filename -> Dir.t -> exn option OpamProcess.job

type t =
  | Dir of Dir.t

(** [quarantine repo_root] returns a temporary repository root dedicated
    to [repo_root]. the returned repository is not created on disk and
    points to a statically known repository located in the same parent
    directory as [repo_root]. *)
val quarantine : t -> t

(* backup into [inn] *)
val backup: inn:dirname -> t -> t

val remove : t -> unit
val is_empty : t -> bool option
val make_empty : t -> unit
val dirname : t -> dirname
val basename : t -> basename
val to_string : t -> string

val remove_prefix: t -> filename -> unix_filename
val remove_prefix_dir: t -> dirname -> unix_dirname

val string_of_backend: t -> string

val copy : src:t -> dst:t -> unit
val move : src:t -> dst:t -> unit

val exists : t -> bool
val is_symlink : t -> bool

(* [read_file IO_FILE ?safe t ?filename content] reads [content] using
   [IO_FILE.read_from_string]. Is [safe] is true, uses
   [IO_FILE.safe_read_from_string]. *)
val read_file:
  (module OpamFile.IO_FILE with type t = 'a) ->
  ?safe:bool -> t -> ?filename:'a OpamFile.t -> string -> 'a

val patch :
  allow_unclean:bool ->
  [`Patch_file of filename | `Patch_diffs of Patch.t list ] -> t ->
  (Patch.operation list, exn) result

(** Returns a pair [(exists, f)] where [exists] tells whether the
    [repo] file exists in the repository and [f] reads it *)
val delayed_read_repo : t -> bool * (unit -> OpamFile.Repo.t)

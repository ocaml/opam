(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** This module abstract the notion of repository root over its concrete
    implementation (could be a database, a file, a directory, etc.) *)

(** Repository root implemented as a directory *)
module Dir : sig
  type t

  val of_dir : OpamFilename.Dir.t -> t
  val to_dir : t -> OpamFilename.Dir.t
  val to_string : t -> string

  (** [quarantine dir] returns a path to a temporary directory dedicated to the
      original repository root. The returned directory is not created and
      points to a statically known directory in the same parent directory
      as [dir]. *)
  val quarantine : t -> t

  val with_tmp : (t -> 'a) -> 'a
  val backup : tmp_dir:OpamFilename.Dir.t -> t -> t

  val cwd : unit -> t
  val in_dir : t -> (unit -> 'a) -> 'a
  val exists : t -> bool
  val remove : t -> unit
  val move : src:t -> dst:t -> unit
  val copy : src:t -> dst:t -> unit
  val copy_except_vcs : src:t -> dst:t -> unit
  val make_empty : t -> unit
  val dirs : t -> OpamFilename.Dir.t list
  val is_empty : t -> bool option
  val dirname : t -> OpamFilename.Dir.t

  val repo : t -> OpamFile.Repo.t OpamFile.t

  module Op: sig
    val ( / ) : t -> string -> OpamFilename.Dir.t
    val ( // ) : t -> string -> OpamFilename.t
  end
end

val make_tar_gz_job : OpamFilename.t -> Dir.t -> exn option OpamProcess.job
val extract_in_job : OpamFilename.t -> Dir.t -> exn option OpamProcess.job

type t =
  | Dir of Dir.t

(** [quarantine repo_root] returns a temporary repository root dedicated
    to [repo_root]. the returned repository is not created on disk and
    points to a statically known repository located in the same parent
    directory as [repo_root]. *)
val quarantine : t -> t

val remove : t -> unit
val is_empty : t -> bool option
val make_empty : t -> unit
val dirname : t -> OpamFilename.Dir.t
val basename : t -> OpamFilename.Base.t
val to_string : t -> string
val copy : src:t -> dst:t -> unit
val move : src:t -> dst:t -> unit
val is_symlink : t -> bool
val patch :
  allow_unclean:bool ->
  [`Patch_file of OpamFilename.t | `Patch_diffs of Patch.t list ] -> t ->
  (Patch.operation list, exn) result

(* TODO ORR doc *)
val delayed_read_repo : t -> bool * (unit -> OpamFile.Repo.t)

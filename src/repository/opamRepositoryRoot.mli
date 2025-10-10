module Dir : sig
  type t

  val of_dir : OpamFilename.Dir.t -> t
  val to_dir : t -> OpamFilename.Dir.t
  val to_string : t -> string

  val quarantine : t -> t
  val with_tmp : (t -> 'a) -> 'a
  val backup : tmp_dir:OpamFilename.Dir.t -> t -> t

  val in_dir : t -> (unit -> 'a) -> 'a
  val exists : t -> bool
  val remove : t -> unit
  val clean : t -> unit
  val move : src:t -> dst:t -> unit
  val copy : src:t -> dst:t -> unit
  val copy_except_vcs : src:t -> dst:t -> unit
  val is_symlink : t -> bool
  val patch : allow_unclean:bool -> [`Patch_file of OpamFilename.t | `Patch_diffs of Patch.t list ] -> t -> (Patch.operation list, exn) result
  val make : t -> unit
  val dirs : t -> OpamFilename.Dir.t list
  val is_empty : t -> bool option
  val dirname : t -> OpamFilename.Dir.t

  val repo : t -> OpamFile.Repo.t OpamFile.t
end

val make_tar_gz_job : OpamFilename.t -> Dir.t -> exn option OpamProcess.job
val extract_in_job : OpamFilename.t -> Dir.t -> exn option OpamProcess.job

type t =
  | Dir of Dir.t

val quarantine : t -> t
val remove : t -> unit
val exists : t -> bool
val is_empty : t -> bool option
val make : t -> unit
val dirname : t -> OpamFilename.Dir.t
val basename : t -> OpamFilename.Base.t
val to_string : t -> string
val copy : src:t -> dst:t -> unit
val move : src:t -> dst:t -> unit
val is_symlink : t -> bool
val patch : allow_unclean:bool -> [`Patch_file of OpamFilename.t | `Patch_diffs of Patch.t list ] -> t -> (Patch.operation list, exn) result
val clean : t -> unit
val delayed_read_repo : t -> bool * (unit -> OpamFile.Repo.t)

(** Repository helpers *)

open Types

(** The state associated to a reposiory *)
type state = {
  local_path : dirname;
  local_repo : Path.R.t;
  remote_path: dirname;
  remote_repo: Path.R.t;
}

(** Create a repository state *)
val make_state: unit -> state

module type REPO = sig

  (** [sync state t] returns the updated local filenames *)
  val sync: state -> Filename.Set.t

  (** upload the contents of [dirname]. Return the uploaded local
      filename. *)
  val upload: state -> dirname -> Filename.Set.t

end

module Make(R: REPO): sig

  (** Get the list of updated package updated *)
  val get_updates: state -> NV.Set.t      

  (** upload the contents of [upload/]. Return the uploaded
      packages. *)
  val upload: state -> NV.Set.t

end

val local_of_remote_file: state -> filename -> filename

val local_of_remote_dir: state -> dirname -> dirname

val remote_of_local_file: state -> filename -> filename

val remote_of_local_dir: state -> dirname -> dirname

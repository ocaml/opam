(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
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

open OpamTypes

(** Backend signature *)
module type S = sig

  val name: OpamUrl.backend

  (** [pull_url package local_dir checksum remote_url] pull the contents of
      [remote_url] into [local_dir]. Can return either a file or a
      directory. [checksum] is the optional expected checksum. *)
  val pull_url: package -> dirname -> string option -> url -> generic_file download OpamProcess.job

  (** [pull_repo] pull the contents of a repository. *)
  val pull_repo: repository -> unit OpamProcess.job

  (** [pull_archive repo archive] pull [archive] in the given
      repository. *)
  val pull_archive: repository -> url -> filename download OpamProcess.job

  (** Return the (optional) revision of a given repository. Only useful
      for VCS backends. *)
  val revision: repository -> version option OpamProcess.job

end

(** Pretty-print *)
val to_string: repository -> string
val to_json: repository -> json

(** Compare repositories *)
val compare: repository -> repository -> int

(** The address for the default OPAM repository (currently:
    https://opam.ocaml.org/) *)
val default_url: url

(** The default OPAM repository (named "default", upstream at
    [default_address]) *)
val default: unit -> repository

(** Create a local repository on a given path, without remote (only for external
    tools, not to be mistaken for an opam repo with a local url) *)
val local: dirname -> repository

(** [check_digest file expected] check that the [file] digest is the
    one [expected]. *)
val check_digest: filename -> string option -> bool

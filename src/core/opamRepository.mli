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

(** Mangagement of OPAM repositories. *)

open OpamTypes

include OpamMisc.ABSTRACT with type t := repository

exception Unknown_backend

(** Default repository address*)
val default_address: address

(** Pretty-print *)
val to_string: repository -> string

(** Compare repositories *)
val compare: repository -> repository -> int

(** Default repository *)
val default: unit -> repository

(** Create a local repository on a given path *)
val local: dirname -> repository

(** Get the list of packages *)
val packages: repository -> package_set

(** Get the list of packages (and their eventual prefixes) *)
val packages_with_prefixes: repository -> string option package_map

(** Get the list of all compiler *)
val compilers: repository -> compiler_set

(** Get the list of compilers (and their eventual prefixes) *)
val compilers_with_prefixes: repository -> string option compiler_map

(** {2 State} *)

(** Get all the package files *)
val package_files: repository -> string option -> package -> archive:bool ->
  filename list

(** Compute a package state (ie. a list of checksums). *)
val package_state: repository -> string option -> package -> [`all|`partial of bool]
  -> checksums

(** Get all the compiler files *)
val compiler_files: repository -> string option -> compiler -> filename list

(** Compute a compiler state (ie. a list of checksums). *)
val compiler_state: repository -> string option -> compiler -> checksums

(** {2 Repository backends} *)

(** Initialize {i $opam/repo/$repo} *)
val init: repository -> unit

(** Update {i $opam/repo/$repo}. *)
val update: repository -> unit

(** Backend signature *)
module type BACKEND = sig

  (** [pull_url package local_dir remote_url] pull the contents of
      [remote_url] into [local_dir]. Can return either a file or a
      directory. *)
  val pull_url: package -> dirname -> address -> generic_file download

  (** [pull_repo] pull the contents of a repository. *)
  val pull_repo: repository -> unit

  (** [pull_archive repo archive] pull [archive] in the given
      repository. *)
  val pull_archive: repository -> filename -> filename download

  (** Return the (optional) revision of a given repository. Only useful
      for VCS backends. *)
  val revision: repository -> version option

end

(** Signature for pull functions *)
type pull_fn = repository_kind -> package -> dirname -> address -> generic_file download

(** Download an url *)
val pull_url: pull_fn

(** Pull and check the resulting digest *)
val pull_and_check_digest: checksum:string -> pull_fn

(** Pull and fix the resultging digest *)
val pull_and_fix_digest: file:filename -> checksum:string -> pull_fn

(** Pull an archive in a repository *)
val pull_archive: repository -> package -> filename download

(** Get the optional revision associated to a backend. *)
val revision: repository -> version option

(** [make_archive ?gener_digest repo prefix package] builds the
    archive for the given [package]. By default, the digest that
    appears in {i $NAME.$VERSION/url} is not modified, unless
    [gener_digest] is set. *)
val make_archive: ?gener_digest:bool -> repository -> string option -> package -> unit

(** Register a repository backend *)
val register_backend: repository_kind -> (module BACKEND) -> unit

(** Find a backend *)
val find_backend: repository_kind -> (module BACKEND)

(** {2 Misc} *)

(** Parallel iterations *)
module Parallel: OpamParallel.SIG with type G.V.t = repository

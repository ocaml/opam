(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Generic repository pluggin *)

(** The following functions are wrapper to the corresponding
    scripts *)

open Types

type kind = string

(** Backend signature *)
module type BACKEND = sig

  (** Initialize the repository *)
  val init: repository -> unit

  (** Update the repository. Return the list of updated files *)
  val update: repository -> Filename.Set.t

  (** Download the package archive on the server *)
  val download_archive: repository -> nv -> filename download

  (** Download a file *)
  val download_file: repository -> nv -> filename -> filename download

  (** Download a directory *)
  val download_dir: repository -> nv -> dirname  -> dirname download

  (** Upload a local directory *)
  val upload_dir: repository -> dirname -> Filename.Set.t
end

(** Register a repository backend *)
val register_backend: kind -> (module BACKEND) -> unit

(** Initialize {i $opam/repo/$repo} *)
val init: repository -> unit

(** Updated {i $opam/repo/$repo} *)
val update: repository -> unit

(** Run {i opam-$kind-download} in {i $opam/repo/$repo} *)
val download: repository -> nv -> unit

(** Upload the content of {i $opam/repo/$repo/upload} *)
val upload: repository -> unit

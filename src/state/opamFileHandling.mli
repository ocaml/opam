(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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

open OpamTypes

(** Read the opam metadata from a given directory (opam file, with possible
    overrides from url and descr files). Also includes the names and hashes
    of files below files/ *)
val read_opam: dirname -> OpamFile.OPAM.t option

(*
type effective_hash = private string

type full_hash = private string

(** Returns the "effective hash" of the given metadata, i.e. the hash of all
    data that has a computational effect on the package. In other words,
    human-directed informative fields like description and maintainer are
    excluded, but upstream hash and instructions are included.

    Used, for example, to detect when a package should be rebuilt. *)
val effective_hash: OpamFile.OPAM.t -> effective_hash

(** Returns the full hash of the metadata, including purely informative
    fields. *)
val full_hash: OpamFile.OPAM.t -> full_hash
*)

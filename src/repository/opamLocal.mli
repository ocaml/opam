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

(** Rsync repository backend *)

module B: OpamRepositoryBackend.S

open OpamTypes

val rsync_dirs: ?args:string list -> ?exclude_vcdirs:bool ->
  OpamUrl.t -> OpamFilename.Dir.t ->
  OpamFilename.Dir.t download OpamProcess.job
val rsync_file: ?args:string list ->
  OpamUrl.t -> OpamFilename.t ->
  OpamFilename.t download OpamProcess.job

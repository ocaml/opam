(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2016 OCamlPro                                        *)
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

(** Tools for manipulating and checking package definition ("opam") files *)

open OpamTypes

(** Create an OPAM package template filled with common options *)
val template: package -> OpamFile.OPAM.t

(** Runs several sanity checks on the opam file; returns a list of warnings.
    [`Error] level should be considered unfit for publication, while
    [`Warning] are advisory but may be accepted. The int is an identifier for
    this specific warning/error. *)
val lint: OpamFile.OPAM.t -> (int * [`Warning|`Error] * string) list

(** Same as [validate], but operates on a file, which allows catching parse
    errors too. You can specify an expected name and version *)
val lint_file: OpamFile.OPAM.t OpamFile.typed_file ->
  (int * [`Warning|`Error] * string) list * OpamFile.OPAM.t option

(** Like [validate_file], but takes the file contents as a string *)
val lint_string: OpamFile.OPAM.t OpamFile.typed_file -> string ->
  (int * [`Warning|`Error] * string) list * OpamFile.OPAM.t option

(** Utility function to print validation results *)
val warns_to_string: (int * [`Warning|`Error] * string) list -> string

(** Read the opam metadata from a given directory (opam file, with possible
    overrides from url and descr files). Also includes the names and hashes
    of files below files/ *)
val read_opam: dirname -> OpamFile.OPAM.t option

(** Adds (or overrides) data from url, descr or files/ in the specified dir or
    the opam file's metadata dir (only files and hashes are included for files
    below files/) *)
val add_aux_files: ?dir:dirname -> OpamFile.OPAM.t -> OpamFile.OPAM.t

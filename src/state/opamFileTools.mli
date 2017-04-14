(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2016 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
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

(** Same as [lint], but operates on a file, which allows catching parse
    errors too. You can specify an expected name and version *)
val lint_file: OpamFile.OPAM.t OpamFile.typed_file ->
  (int * [`Warning|`Error] * string) list * OpamFile.OPAM.t option

(** Same as [lint_file], but taking input from a channel *)
val lint_channel: OpamFile.OPAM.t OpamFile.typed_file -> in_channel ->
  (int * [`Warning|`Error] * string) list * OpamFile.OPAM.t option

(** Like [lint_file], but takes the file contents as a string *)
val lint_string: OpamFile.OPAM.t OpamFile.typed_file -> string ->
  (int * [`Warning|`Error] * string) list * OpamFile.OPAM.t option

(** Utility function to print validation results *)
val warns_to_string: (int * [`Warning|`Error] * string) list -> string

(** Read the opam metadata from a given directory (opam file, with possible
    overrides from url and descr files). Also includes the names and hashes
    of files below files/ *)
val read_opam: dirname -> OpamFile.OPAM.t option

(** Adds data from 'url' and 'descr' files found in the specified dir or the
    opam file's metadata dir, if not already present in the opam file. if
    [files_subdir_hashes] is [true], also adds the names and hashes of files
    found below 'files/' *)
val add_aux_files:
  ?dir:dirname -> files_subdir_hashes:bool -> OpamFile.OPAM.t -> OpamFile.OPAM.t

(** {2 Tools to manipulate the [OpamFile.OPAM.t] contents} *)
val map_all_variables:
  (full_variable -> full_variable) -> OpamFile.OPAM.t -> OpamFile.OPAM.t

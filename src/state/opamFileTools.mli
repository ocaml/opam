(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
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
   [`Error] level should be considered unfit for publication, while [`Warning]
   are advisory but may be accepted. The int is an identifier for this specific
   warning/error. If [check_extra_files] is unspecified or false, warning 53
   won't be checked. *)
val lint:
  ?check_extra_files:(basename * (OpamHash.t -> bool)) list ->
  ?check_upstream:bool ->
  OpamFile.OPAM.t -> (int * [`Warning|`Error] * string) list

(** Same as {!lint}, but operates on a file, which allows catching parse errors
   too. [check_extra_files] defaults to a function that will look for a [files/]
   directory besides [filename]. [handle_dirname] is used for warning 4, and
   should be set when reading packages from a repository, so that package name
   and version are inferred from the filename. *)
val lint_file:
  ?check_extra_files:(basename * (OpamHash.t -> bool)) list ->
  ?check_upstream:bool ->
  ?handle_dirname:bool ->
  OpamFile.OPAM.t OpamFile.typed_file ->
  (int * [`Warning|`Error] * string) list * OpamFile.OPAM.t option

(** Same as {!lint_file}, but taking input from a channel. [check_extra_files]
   defaults to a function that will look for a [files/] directory besides
   [filename] *)
val lint_channel:
  ?check_extra_files:(basename * (OpamHash.t -> bool)) list ->
  ?check_upstream: bool ->
  ?handle_dirname:bool ->
  OpamFile.OPAM.t OpamFile.typed_file -> in_channel ->
  (int * [`Warning|`Error] * string) list * OpamFile.OPAM.t option

(** Like {!lint_file}, but takes the file contents as a string.
   [check_extra_files] defaults to a function that will look for a [files/]
   directory besides [filename] *)
val lint_string:
  ?check_extra_files:(basename * (OpamHash.t -> bool)) list ->
  ?check_upstream: bool ->
  ?handle_dirname:bool ->
  OpamFile.OPAM.t OpamFile.typed_file -> string ->
  (int * [`Warning|`Error] * string) list * OpamFile.OPAM.t option

val all_lint_warnings: unit -> (int * [`Warning|`Error] * string) list

(** Utility function to print validation results *)
val warns_to_string: (int * [`Warning|`Error] * string) list -> string

(** Utility function to construct a json of validation results.
    The format is as follow:
    {[
    { "file"     : string <filename>,
      "result"   : string (passed | error | warning),
      "warnings" :
        [ { "id"      : int,
            "message" : string <warning message> },
          ...
        ],
      "errors"   :
        [ { "id"      : int,
            "message" : string <error message> },
          ...
        ]
    }
    ]}
*)
val warns_to_json:
  ?filename:string -> (int * [`Warning|`Error] * string) list -> OpamJson.t

(** Read the opam metadata from a given directory (opam file, with possible
    overrides from url and descr files).
    Warning: use {!read_repo_opam} instead for correctly reading files from
    repositories!*)
val read_opam: dirname -> OpamFile.OPAM.t option

(** Like {!read_opam}, but additionally fills in the [metadata_dir] info
    correctly for the given repository. *)
val read_repo_opam:
  repo_name:repository_name -> repo_root:dirname ->
  dirname -> OpamFile.OPAM.t option

(** Adds data from 'url' and 'descr' files found in the specified dir or the
    opam file's metadata dir, if not already present in the opam file. if
    [files_subdir_hashes] is [true], also adds the names and hashes of files
    found below 'files/' *)
val add_aux_files:
  ?dir:dirname -> ?files_subdir_hashes:bool -> OpamFile.OPAM.t -> OpamFile.OPAM.t

(** {2 Tools to manipulate the {!OpamFile.OPAM.t} contents} *)
val map_all_variables:
  (full_variable -> full_variable) -> OpamFile.OPAM.t -> OpamFile.OPAM.t

val map_all_filters:
  (filter -> filter) -> OpamFile.OPAM.t -> OpamFile.OPAM.t

(** Converts a dependency formula to the same format as used in opam package
    definition files. *)
val dep_formula_to_string: formula -> string

(** Sort opam fields: author, tags, depexts, depends, depopts, conflicts,
    pin_depends, extra_files, extra_sources *)
val sort_opam: OpamFile.OPAM.t -> OpamFile.OPAM.t

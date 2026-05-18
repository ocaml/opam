(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 David Allsopp Ltd.                                   *)
(*    Copyright 2025 Kate Deplaix                                         *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** [translate_patch ~dir input_patch output_patch] writes a copy of
    [input_patch] to [output_patch] as though [input_patch] had been applied in
    [dir]. The patch is rewritten such that if text files have different line
    endings then the patch is transformed to patch using the encoding on disk.
    In particular, this means that patches generated against Unix checkouts of
    Git sources will correctly apply to Windows checkouts of the same sources.
*)
val translate_patch: dir:string -> string -> string -> unit

(** [patch ~allow_unclean ?patch_filename ~dir diffs] applies a patch to
    directory [dir].

    @param allow_unclean decides if applying a patch on a directory which
    differs slightly from the one described in the patch file is allowed.
    Allowing unclean applications imitates the default behaviour of GNU Patch. *)
val patch:
  allow_unclean:bool -> ?patch_filename:string -> dir:string
  -> Patch.t list -> unit


(** [parse_patch ~dir patch_file] processes and parses a patch file.
    Returns the parsed patch diffs or raises [Not_found] if the patch file
    doesn't exist or can't be parsed. *)
val parse_patch: dir:string -> file:string -> Patch.t list

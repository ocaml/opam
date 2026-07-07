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

module type FS_ABSTR = sig
  (** The [root] is the type of the element that we want to patch: a directory,
      an archive, etc. *)
  type root

  (** The [file] is the type of internal files that we manipulate *)
  type file

  (** The [target] is the inner type of the element that we are patching *)
  type target

  (* The name of [root] *)
  val root_label : string

  (* Do we need to translate the patch file for this [root] *)
  val translate_patch : bool

  val root_to_string : root -> string
  val file_to_string : file -> string
  val equal_file: file -> file -> bool

  (* Identity unless some checks need to be done. Take a [fail] function. *)
  val get_path : fail:(unit -> unit) -> root -> string -> file

  (* Write a [file] *)
  val write : file -> string -> target -> target

  (* Function called when a patch application succeed when [allow_unclean] is set
      to true.

      @param file is the filename on which the patch was rejected
      @param content is the file content, if it exists
  *)
  val on_unclean_accept : file -> string option -> unit

  (* Function called when a patch application fails when [allow_unclean] is set
     to true.

     @param file is the filename on which the patch was rejected
     @param content is the file content, if it exists
     @param diff is the patch that failed to apply
  *)
  val on_unclean_reject : file -> string option -> Patch.t -> unit

  (* Return true is [file] exists *)
  val exists : file -> target -> bool

  (* Return true if dirname of [file] exists and is a directory *)
  val exists_dir : file -> target -> bool

  (* Read a [file] *)
  val read : file -> target -> string

  (* Remove a [file] *)
  val remove : file -> target -> target

  (* Remove dirname of [file] if it exists *)
  val remove_dir : file -> target -> target

  (* Return true if [src] and [dst] are in the same directory *)
  val same_dirname : src:file -> dst:file -> bool

  (* Move [src] into [dst] *)
  val mv : src:file -> dst:file -> target -> target

  (* [open_ root f] opens [root] and apply function [f] on its
     [target] content *)
  val open_ : root -> (target -> unit) -> unit

  (* Save the target (e.g. on disk) *)
  val save : target -> unit
end

(** [patch FS_ABSTR ~allow_unclean ?patch_filename ~dir diffs] applies a
    patch to given element by [FS_ABSTRACTION].

    @param FS_ABSTR is a module that defines the element that we are
    patching and define all needed functions

    @param ~allow_unclean decides if applying a patch on a directory which
    differs slightly from the one described in the patch file is allowed.
    Allowing unclean applications imitates the default behaviour of GNU Patch. *)
val patch:
  (module FS_ABSTR with type root = 'a) ->
  allow_unclean:bool ->
  [`Patch_file of string | `Patch_diffs of Patch.t list ] -> 'a
  -> (Patch.operation list, exn) result

(** [parse_patch ~translate patch_file] processes and parses a patch file.
    Returns the parsed patch diffs or raises [Not_found] if the patch file
    doesn't exist or can't be parsed.

    @param translate if [Some dir], launch {translate_patch} on [dir].
    Otherwise, no translation done.
*)
val parse_patch: translate:string option -> string -> Patch.t list

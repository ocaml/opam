(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025-2026 Kate Deplaix                                    *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Tar gz archives manipulation *)
(* The current implementation handles only files not directories *)

open OpamTypes

(* The archive file *)
type archive = filename

(* Filename of compressed file in archive *)
type archived_file = unix_filename

(* Archive compressed file content *)
type archived_file_content = string

(* Fold over files of an archive *)
val fold_reg_files :
  ('acc -> archived_file -> archived_file_content -> 'acc) -> 'acc -> archive
  -> 'acc

(* [create archive dir] Creates an compressed archive [archive] containing the
   [dir] at root.
   If the directory contains a VCS directory, it is not integrated in the
   archive. *)
val create : archive -> dirname -> unit

(* Apply a patch on an archive *)
val patch:
  allow_unclean:bool ->
  [`Patch_file of string | `Patch_diffs of Patch.t list ] -> archive ->
  (Patch.operation list, exn) result

(* This module contains helpers to act on the archive once openned *)
module Inplace : sig
  type t

  (* Open an archive and fold over it *)
  val with_open_out : archive -> (t -> 'a) -> 'a

  (* Fold over files of an archive *)
  val fold_reg_files :
    ('acc -> archived_file -> archived_file_content -> 'acc) ->
    'acc -> t -> 'acc

  (* Return the content of the filename from the archive *)
  val read: archived_file -> t -> archived_file_content

  (* Add a file in an archive *)
  val add : archived_file -> archived_file_content -> t -> t

  (* Remove a file from an archive *)
  val remove : archived_file -> t -> t

  (* Remove the content of a directory from an archive *)
  val remove_dir : unix_dirname -> t -> t

  (* Move a file in the archive *)
  val mv: src:archived_file -> dst:archived_file -> t -> t

  (* Return true if the filename exists in the archive *)
  val exists: archived_file -> t -> bool

  (* Write the archive on disk.
     Files will be written with permission 640, no timestamps (0), user id 0,
     and group id 0. *)
  val write : t -> unit
end

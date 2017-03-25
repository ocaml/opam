(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Some command helpers, and auxiliary opam management functions used by the
    CLI *)

open OpamTypes
open OpamStateTypes

(** Gets the file changes done in the installation of the given packages in the
    given switch, and copies the corresponding files to the same relative paths
    below the given prefix ; files that are not current according to the
    recorded package changes print warnings and aren't copied. *)
val copy_files_to_destdir: 'a switch_state -> dirname -> package_set -> unit

(** Removes all files that may have been installed by [copy_files_to_destdir];
    it's more agressive than [OpamDirTrack.revert] and doesn't check if the
    files are current. *)
val remove_files_from_destdir: 'a switch_state -> dirname -> package_set -> unit

(** Resolves the opam files and directories in the list to package name and
    location, and returns the corresponding pinnings and atoms. May fail and
    exit if package names for provided [`Filename] could not be inferred, or if
    the same package name appears multiple times *)
val resolve_locals:
  [ `Atom of atom | `Filename of filename | `Dirname of dirname ] list ->
  (name * OpamUrl.t * OpamFile.OPAM.t OpamFile.t) list * atom list

(** Resolves the opam files in the list to package name and location, pins the
    corresponding packages accordingly if necessary, and returns the
    resolved atom list *)
val autopin:
  rw switch_state ->
  [ `Atom of atom | `Filename of filename | `Dirname of dirname ] list ->
  rw switch_state * atom list

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2018 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Functions handling the "opam pin" subcommand *)

open OpamTypes
open OpamStateTypes

(** Pins a package to the given version, and writes to disk. Returns the updated
    state. The main difference with [source_pin] is that a definition overlay is
    not created. Therefore, the package must exist already. *)
val version_pin: rw switch_state -> name -> version -> rw switch_state

exception Aborted
exception Nothing_to_do

(** Sets the package as pinned to the given target. A package definition is
    looked for in the package source and current metadata (in this order),
    unless specified using [~opam].

    If [edit], or if no package definition is found, this opens an editor (with
    a template if no definition is available).

    If [force], don't abort even if the source can't be fetched from [target]

    May raise [Aborted] or [Nothing_to_do]. *)
val source_pin:
  rw switch_state -> name ->
  ?version:version -> ?edit:bool -> ?opam:OpamFile.OPAM.t -> ?quiet:bool ->
  ?force:bool -> ?ignore_extra_pins:bool -> ?subpath: string ->
  url option ->
  rw switch_state

(** Interactively handles the [pin-depends] in an opam file *)
val handle_pin_depends:
  rw switch_state -> package -> OpamFile.OPAM.t -> rw switch_state

(** Fetch in parallel jobs a list of pins [name, url, subpath], and return the
    successful ones.
    Ask for confirmation to continue if a fetching fails.
*)
val fetch_all_pins:
  'a switch_state -> (string * url * string option) list ->
  (url * string option) list

(** Let the user edit a pinned package's opam file. If given, the version is put
    into the template in advance. Writes and returns the updated switch
    state. *)
val edit: rw switch_state -> ?version:version -> name -> rw switch_state

(** Unpin packages *)
val unpin: rw switch_state -> name list -> rw switch_state

(** Pure function that reverts a single package pinning *)
val unpin_one: 'a switch_state -> package -> 'a switch_state

(** List the pinned packages to the user. *)
val list: 'a switch_state -> short:bool -> unit

(** Scan pinning separator, used for printing and parsing by [scna] and
    [parse_pins]. *)
val scan_sep: char

(** Scan for available packages to pin, and display it on stdout. If
    [normalise] is true, displays it's normalised format
    `name.version[scan_sep]curl[scan_sep]subpath`. *)
val scan: normalise:bool -> recurse:bool -> ?subpath: string -> url -> unit

(** Detect if a string is a normalised format of [scan]. *)
val looks_like_normalised: string list -> bool

(** Parse the normalised form of [scan], and returns pinning informations. *)
val parse_pins: string list -> (name * version option * url * string option) list

(** Lints the given opam file, prints warnings or errors accordingly (unless
    [quiet]), upgrades it to current format, adds references to files below the
    'files/' subdir (unless the file is directly below the specified, local
    [url]), and returns it *)
val read_opam_file_for_pinning:
  ?quiet:bool -> name -> OpamFile.OPAM.t OpamFile.t -> url -> OpamFile.OPAM.t option

(** The default version for pinning a package: depends on the state, what is
    installed and available, and defaults to [~dev]. *)
val default_version: 'a switch_state -> name -> version

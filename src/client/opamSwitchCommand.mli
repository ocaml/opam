(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Functions handling the "opam switch" subcommand *)

open OpamTypes
open OpamStateTypes

(** Creates and configures a new switch. The given [global_state] is unlocked
    once done. [update_config] sets the switch as current globally, unless it is
    external.

    [post] can be used to run guarded operations after the switch creation
    (cleanup will be proposed to the user if they fail). You probably want to
    call [install_compiler] there. *)
val create:
  rw global_state ->
  rt:'a repos_state ->
  ?synopsis:string ->
  ?repos:repository_name list ->
  update_config:bool ->
  invariant:formula ->
  switch ->
  (rw switch_state -> 'ret * rw switch_state) ->
  'ret * rw switch_state

(** Used to initially install a compiler's base packages, according to its
    invariant. [ask] triggers prompting the user as for normal installs;
    defaults to [false]. *)
val install_compiler:
  ?additional_installs:atom list -> ?deps_only:bool -> ?ask:bool ->
  rw switch_state -> rw switch_state

(** Import a file which contains the packages to install.
    If [deps_only] is true do not install root packages. *)
val import:
  rw switch_state ->
  ?deps_only:bool ->
  OpamFile.SwitchExport.t OpamFile.t option ->
  rw switch_state

(** Export a file which contains the installed packages. If [full] is specified
    and true, export metadata of all installed packages (excluding overlay
    files) as part of the export. The export will be extended with a map of all
    extra-files. If [freeze] is specified and true, VCS urls will be frozen to
    the specific commit ID. If [None] is provided as file argument, the export
    is done to stdout. *)
val export:
  'a repos_state ->
  ?freeze:bool ->
  ?full:bool ->
  ?switch:switch ->
  OpamFile.SwitchExport.t OpamFile.t option ->
  unit

(** Remove the given compiler switch, and returns the updated state (unchanged
    in case [confirm] is [true] and the user didn't confirm) *)
val remove: rw global_state -> ?confirm:bool -> switch -> rw global_state

(** Changes the currently active switch *)
val switch: 'a lock -> rw global_state -> switch -> unit

(** Changes the currently active switch to the previous active switch *)
val switch_previous: 'a lock -> rw global_state -> unit

(** Reinstall the given compiler switch. *)
val reinstall: rw switch_state -> rw switch_state

(** Updates the switch invariant and the associated config files, and writes the
    config file unless [show] or [dry_run] are activated globally. Low-level
    function, see [set_invariant] for the user-facing function. *)
val set_invariant_raw:
  rw switch_state -> formula -> rw switch_state

(** Sets the packages configured as the current switch compiler base, after some
    checks and messages. *)
val set_invariant:
  ?force:bool -> rw switch_state -> formula -> rw switch_state

(** Display the current compiler switch. *)
val show: unit -> unit

(** List all the available compiler switches. *)
val list: 'a global_state -> print_short:bool -> unit

(** Returns all available compiler packages from a repo state *)
val get_compiler_packages:
  ?repos:repository_name list -> 'a repos_state -> package_set

(** Guess a real compiler spec from a list of strings, which may refer to
    packages with optional version constraints, or just versions.
    This uses some heuristics. *)
val guess_compiler_invariant:
  ?repos:repository_name list -> 'a repos_state -> string list -> OpamFormula.t


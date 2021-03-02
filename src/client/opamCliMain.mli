(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Handles calling opam plugins (à la git). E.g. [opam publish] runs
    [opam-publish] from PATH, with specific addition of OpamPath.plugins_bin and
    the current switch bin directory).

    Note that this does load some configuration and env, but only handles a
    leading [--yes] argument.
    @raise InvalidCLI *)
val check_and_run_external_commands:
  unit -> OpamCLIVersion.Sourced.t * string list

(** Handles flushing buffers and catching exceptions from the main call,
    including special cases like [OpamStd.Sys.Exec] that is expected to do a
    [Unix.exec], but after all proper cleanup has been done. *)
val main_catch_all: (unit -> unit) -> unit

(** Handling of debug JSON output, according to [OpamClientConfig.json_out] *)
val json_out: unit -> unit

(** [run default command_list] runs command-line argument parsing and processing
    of the command *)
val run: unit -> unit

(** Default entry point with handling of debug finalisers *)
val main: unit -> unit

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2021 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** CLI version helpers *)

(* Defines Cmdliner optional argument function-helpers, with the cli
   version. *)

open Cmdliner

type validity

val cli_from: ?experimental:bool -> OpamCLIVersion.t -> validity
val cli_between:
  ?option:[`experimental | `default] -> OpamCLIVersion.t ->
  ?replaced:string -> OpamCLIVersion.t -> validity
val cli_original: validity

val cli2_0: OpamCLIVersion.t
val cli2_1: OpamCLIVersion.t
val cli2_2: OpamCLIVersion.t

val mk_flag:
  cli:OpamCLIVersion.Sourced.t -> validity -> section:string -> string list ->
  string -> bool Term.t

val mk_flag_replaced:
  cli:OpamCLIVersion.Sourced.t -> section:string ->
  (validity * string list) list ->
  string -> bool Term.t

val mk_opt:
  cli:OpamCLIVersion.Sourced.t -> validity -> section:string -> ?vopt:'a ->
  string list -> string -> string -> 'a Arg.conv -> 'a -> 'a Term.t

val mk_opt_all:
  cli:OpamCLIVersion.Sourced.t -> validity -> section:string -> ?vopt:'a ->
  ?default:'a list -> string list -> string -> string -> 'a Arg.conv ->
  'a list Term.t

val mk_vflag:
  cli:OpamCLIVersion.Sourced.t -> section:string -> 'a ->
  (validity * 'a * string list * string) list -> 'a Term.t

val mk_vflag_all:
  cli:OpamCLIVersion.Sourced.t -> section:string -> ?default:'a list ->
  (validity * 'a * string list * string) list -> 'a list Term.t

val mk_enum_opt:
  cli:OpamCLIVersion.Sourced.t -> validity -> section:string -> string list ->
  string -> (validity * string * 'a) list -> string -> 'a option Term.t

(** [opt_all] with enums. Check each flag content cli, purge non corresponding
    ones from the final result. If after purge the resulting list is empty (all
    removed or newer flag contents), it raises an error ; otherwise only
    display warnings on wrong cli contents. *)
val mk_enum_opt_all:
  cli:OpamCLIVersion.Sourced.t -> validity -> section:string -> string list ->
  string -> (validity * string * 'a) list -> string -> 'a list Term.t

val string_of_enum: (validity * string * 'a) list -> string

type 'a subcommand = validity * string * 'a * string list * string
type 'a subcommands = 'a subcommand list

val mk_subcommands:
  cli:OpamCLIVersion.Sourced.t -> 'a subcommands ->
  'a option Term.t * string list Term.t

type 'a default = [> `default of string] as 'a

val mk_subcommands_with_default:
  cli:OpamCLIVersion.Sourced.t -> 'a default subcommands ->
  'a option Term.t * string list Term.t

val bad_subcommand:
  cli:OpamCLIVersion.Sourced.t -> 'a default subcommands ->
  (string * 'a option * string list) -> 'b Term.ret

val mk_subdoc :
  cli:OpamCLIVersion.Sourced.t -> ?defaults:(string * string) list ->
  ?extra_defaults:(validity * string * string) list ->
  'a subcommands -> Manpage.block list

type command = unit Term.t * Cmd.info

val mk_command:
  cli:OpamCLIVersion.Sourced.t -> validity ->
  (cli:OpamCLIVersion.Sourced.t -> string -> doc:string ->
   man:Manpage.block list -> Cmd.info) ->
  string -> doc:string ->
  man:Manpage.block list -> (unit -> unit) Term.t -> command

val mk_command_ret:
  cli:OpamCLIVersion.Sourced.t -> validity ->
  (cli:OpamCLIVersion.Sourced.t -> string -> doc:string ->
   man:Manpage.block list -> Cmd.info) ->
  string -> doc:string -> man:Manpage.block list ->
  (unit -> unit Term.ret) Term.t -> command

val env_with_cli:
  (string * validity * (string -> OpamStd.Config.E.t) * string) list ->
  (OpamCLIVersion.Sourced.t -> Manpage.block list) *
  (OpamCLIVersion.Sourced.t -> unit)

val is_original_cli: validity -> bool

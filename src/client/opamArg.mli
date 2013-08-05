(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(** OPAM command-line arguments *)

open OpamTypes
open Cmdliner

(** {2 Commands} *)

(** Type of commands *)
type command = unit Term.t * Term.info

(** [run default commdands at_exit] build a binary which takes
    [commands] as subcommand and [default] as default argument
    (ie. which will be executed when no subcommand is
    given). [at_exit] is executed before the program exits. *)
val run: ?at_exit:(unit -> unit) -> command -> command list -> unit

(** opam *)
val default: command

(** opam init *)
val init: command

(** opam list *)
val list: command

(** opam info *)
val info: command

(** opam search *)
val search: command

(** opam install *)
val install: command

(** opam remove *)
val remove: command

(** opam reinstall *)
val reinstall: command

(** opam update *)
val update: command

(** opam upgrade *)
val upgrade: command

(** opam config *)
val config: command

(** opam remote (alias for 'opam repository') *)
val remote: command

(** opam repository *)
val repository: command

(** opam switch *)
val switch: command

(** opam pin *)
val pin: command

(** opam help *)
val help: command

(** {2 Flags} *)

(** --short *)
val print_short_flag: bool Term.t

(** --installed *)
val installed_flag: bool Term.t

(** --installed-root *)
val installed_roots_flag: bool Term.t

(** --fish *)
val fish_flag: bool Term.t

(** --zsh *)
val zsh_flag: bool Term.t

(** --csh *)
val csh_flag: bool Term.t

(** --sh *)
val sh_flag: bool Term.t

(** --dot-profile *)
val dot_profile_flag: filename option Term.t

(** --http/ --git/ --local *)
val repo_kind_flag: repository_kind option Term.t

(** --jobs *)
val jobs_flag: int option Term.t

(** --json *)
val json_flag: string option Term.t

(** patterns *)
val pattern_list: string list Term.t

(** package names *)
val name_list: name list Term.t

(** repositories *)
val repository_list: repository_name list Term.t

(** parameters *)
val param_list: string list Term.t

(** {3 Global options} *)

(** Abstract type for global options *)
type global_options

(** Global options *)
val global_options: global_options Term.t

(** Apply global options *)
val apply_global_options: global_options -> unit

(** {3 Build options} *)

(** Abstract type for build options *)
type build_options

(** Build options *)
val build_options: build_options Term.t

(** Applly build options *)
val apply_build_options: build_options -> unit

(** {3 Converters} *)

(** Repository name converter *)
val repository_name: repository_name Arg.converter

(** Repository address converter *)
val address: address Arg.converter

(** Filename converter *)
val filename: filename Arg.converter

(** Dirnam converter *)
val dirname: dirname Arg.converter

(** Compiler converter *)
val compiler: compiler Arg.converter

(** Package name converter *)
val package_name: name Arg.converter

(** {2 Misc} *)

(** Enumeration with a default command *)
val enum_with_default:
  (string * ([> `default of string] as 'a)) list -> 'a Arg.converter

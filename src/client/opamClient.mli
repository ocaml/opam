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

(** Client entry-point. *)

open OpamTypes

(** OPAM API. *)
module API: sig

  (** Initialize the client a consistent state. *)
  val init:
    repository -> compiler -> jobs:int ->
    shell -> filename -> [`ask|`yes|`no] ->
    unit

  (** Display all available packages that matches any of the
      regexps. *)
  val list:
    print_short:bool ->
    filter:[`all|`installed|`roots|`installable] ->
    exact_name:bool ->
    case_sensitive:bool ->
    string list ->
    unit

  (** Display a general summary of a collection of packages. *)
  val info: fields:string list -> string list -> unit

  (** Install the given set of packages. *)
  val install: name_set -> unit

  (** Reinstall the given set of packages. *)
  val reinstall: name_set -> unit

  (** Refresh the available packages. *)
  val update: repository_name list -> unit

  (** Find a consistent state where most of the installed packages are
      upgraded to their latest version. [None] means all the installed
      packages. *)
  val upgrade: name_set option -> unit

  (** Remove the given set of packages. *)
  val remove: autoremove:bool -> name_set -> unit

  (** Config API. *)
  module CONFIG: sig

    (** Display configuration options. *)
    val config: config -> unit

    (** Display environment. *)
    val env: csh:bool -> sexp:bool -> fish:bool -> unit

    (** Global and user setup of OPAM. *)
    val setup: user_config option -> global_config option -> unit

    (** Display global and user informations about OPAM setup. *)
    val setup_list: shell -> filename -> unit

    (** Execute a command in a subshell with the right environment variables. *)
    val exec: string -> unit

    (** Display includes files. *)
    val includes: is_rec:bool -> name list -> unit

    (** Display variables and their contents. *)
    val list: name list -> unit

    (** Display a given variable content. *)
    val variable: full_variable -> unit

    (** Substitute files. *)
    val subst: basename list -> unit

  end

  (** Repository API *)
  module REPOSITORY: sig

    (** Display the list of repositories. *)
    val list: short:bool -> unit

    (** Add a new repository. *)
    val add: repository_name -> repository_kind -> address -> priority:int option -> unit

    (** Remove a repository. *)
    val remove: repository_name -> unit

    (** Set-up repository priority. *)
    val priority: repository_name -> priority:int -> unit

  end

  (** Switch API *)
  module SWITCH: sig

    (** Switch to the given compiler. Take the global file lock. *)
    val switch: quiet:bool -> warning:bool -> switch -> unit

    (** Install the given compiler. *)
    val install: quiet:bool -> warning:bool -> update_config:bool -> switch -> compiler -> unit

    (** Import the packages from a file. If no filename is specified,
        read stdin. *)
    val import: filename option -> unit

    (** Export the packages to a file. If no filename is specified,
        write to stdout. *)
    val export: filename option -> unit

    (** Remove the given compiler. *)
    val remove: switch -> unit

    (** Reinstall the given compiler. *)
    val reinstall: switch -> unit

    (** List the available compiler descriptions. *)
    val list: print_short:bool -> installed:bool -> unit

    (** Display the name of the current compiler. *)
    val show: unit -> unit

  end

  (** Pin API *)
  module PIN: sig

    (** Pin a package to a specific version. *)
    val pin: force:bool -> pin -> unit

    (** List the current pinned packages. *)
    val list: unit -> unit

  end

end

(** Call an unsafe function while taking the global lock. *)
val global_lock: (unit -> unit) -> unit

(** Call an unsafe function while taking the current switch lock. *)
val switch_lock: (unit -> unit) -> unit

(** Call an unsafe function while checking that no lock is already held. *)
val read_lock: (unit -> unit) -> unit

(** This version of the API can be used concurrently. *)
module SafeAPI: (module type of API)

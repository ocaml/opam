(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
open OpamStateTypes

(* (\** OPAM API. *\) *)
(* module API: sig *)

  (** Initialize the client a consistent state. *)
  val init:
    repository -> shell -> filename -> [`ask|`yes|`no] ->
    unlocked repos_state

  (** Install the given list of packages. Second argument, if not None, specifies
      that given packages should be added or removed from the roots.
      Third argument installs all dependencies but not the packages themselves *)
  val install:
    [< unlocked ] global_state ->
    atom list -> bool option -> deps_only:bool -> upgrade:bool -> unit

  (** Reinstall the given set of packages. *)
  val reinstall:
    [< unlocked ] global_state -> atom list -> unit

  (** Refresh the available packages. *)
  val update:
    [< unlocked ] global_state ->
    repos_only:bool -> dev_only:bool -> ?no_stats:bool ->
    string list -> unit

  (** Find a consistent state where most of the installed packages are
      upgraded to their latest version, within the given constraints.
      An empty list means upgrade all installed packages. *)
  val upgrade: [< unlocked ] global_state -> atom list -> unit

  (** Recovers from an inconsistent universe *)
  val fixup: [< unlocked ] global_state -> unit

  (** Remove the given list of packages. *)
  val remove:
    [< unlocked ] global_state -> autoremove:bool -> force:bool -> atom list -> unit

(*
  (** Config API. *)
  module CONFIG: sig

    (** Display environment. *)
    val env: csh:bool -> sexp:bool -> fish:bool -> inplace_path:bool -> unit

    (** Global and user setup of OPAM. *)
    val setup:
      ?dot_profile:OpamTypes.filename -> ocamlinit:bool ->
      switch_eval:bool -> completion:bool ->
      shell:OpamTypes.shell ->
      user:bool -> global:bool -> unit

    (** Display global and user informations about OPAM setup. *)
    val setup_list: shell -> filename -> unit

    (** Execute a command in a subshell with the right environment variables. *)
    val exec: inplace_path:bool -> string list -> unit

    (** Display variables and their contents. *)
    val list: name list -> unit

    (** Sets or unsets a global switch variable *)
    val set: full_variable -> string option -> unit

    (** Prints the variable expansion of the given string *)
    val expand: string -> unit

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
    val add: repository_name -> url -> priority:int option -> unit

    (** Remove a repository. *)
    val remove: repository_name -> unit

    (** Set-up repository priority. *)
    val priority: repository_name -> priority:int -> unit

    (** Set-up repository url. *)
    val set_url: repository_name -> url -> unit
  end

  (** Switch API *)
  module SWITCH: sig

    (** Set the given switch, installing it if necessary. Take the
        global file lock. *)
    val switch:
      quiet:bool -> packages:atom conjunction ->
      switch -> unit

    (** Install the given compiler. *)
    val install:
      quiet:bool -> update_config:bool ->
      packages:atom conjunction ->
      switch -> unit

    (** Import the packages from a file. If no filename is specified,
        read stdin. *)
    val import:
      [< rw ] global_state -> switch ->
      OpamFile.SwitchExport.t OpamFile.t option -> unit

    (** Export the packages to a file. If no filename is specified,
        write to stdout. *)
    val export: ?full:bool -> OpamFile.SwitchExport.t OpamFile.t option -> unit

    (** Remove the given compiler. *)
    val remove: [< rw ] global_state -> ?confirm:bool -> switch -> unit

    (** Reinstall the given compiler. *)
    val reinstall: [< rw ] global_state -> switch -> unit

    (** List the available compiler descriptions. *)
    val list:
      'a global_state -> print_short:bool -> installed:bool -> all:bool -> unit

    (** Display the name of the current compiler. *)
    val show: unit -> unit

  end
*)
  (** Pin API *)
  module PIN: sig

    (** Set a package pinning. if [pin_option] is [None], set the package defined
        upstream. If [action], prompt for install/reinstall as appropriate after
        pinning. *)
    val pin:
      [< unlocked ] global_state ->
      OpamPackage.Name.t ->
      ?edit:bool -> ?version:version -> ?action:bool ->
      pin_option option -> unit

    val edit:
      [< unlocked ] global_state -> ?action:bool -> OpamPackage.Name.t -> unit

    val unpin:
      [< unlocked ] global_state ->
      ?action:bool -> OpamPackage.Name.t list -> unit

    (** List the current pinned packages. *)
    val list: [< unlocked ] global_state -> short:bool -> unit -> unit

  end
(*
  module LIST: sig

    (** Display all available packages that matches any of the
        regexps. *)
    val list:
      print_short:bool ->
      filter:[`all|`installed|`roots|`installable] ->
      order:[`normal|`depends] ->
      exact_name:bool ->
      case_sensitive:bool ->
      ?depends:(atom list) ->
      ?reverse_depends:bool -> ?recursive_depends:bool -> ?resolve_depends:bool ->
      ?depopts:bool -> ?depexts:string list -> ?dev:bool ->
      string list ->
      unit

    (** Display a general summary of a collection of packages. *)
    val info: fields:string list -> raw_opam:bool -> where:bool -> atom list -> unit

  end
*)
(* end *)
(*
(** Call an unsafe function while taking the global lock. *)
val global_lock: (unit -> unit) -> unit

(** Call an unsafe function while taking the current switch lock. *)
val switch_lock: (unit -> unit) -> unit

(** Call an unsafe function while checking that no lock is already held. *)
val read_lock: (unit -> unit) -> unit

(** Loads state with [command], and calls [f] on it. The loaded state is backed
    up, and in case of error, a message is displayed on how to revert. *)
val with_switch_backup: string -> ([< rw ] switch_state -> unit) -> unit

(** This version of the API can be used concurrently. *)
module SafeAPI: (module type of API)
*)

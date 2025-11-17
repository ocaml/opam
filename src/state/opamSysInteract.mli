(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStateTypes

(** [available_packages ?env config pkgs] queries the system to determine
    which of the given system [pkgs] are available (i.e., present in the
    system’s package database  or known to the system's package manager).

    The result is an {!OpamSysPkg.availability_mode} type, which may either be
    a set of available packages or a flag indicating that availability should
    be assumed (e.g., on systems where querying is unsupported or skipped),
    or a flag that indicates that depexts are disabled.

    [env] is used to determine host specification.
    [config] is used to determine Windows depext installation. *)
val available_packages:
  ?env:gt_variables -> OpamFile.Config.t -> OpamSysPkg.Set.t ->
  OpamSysPkg.availability_mode

(** Returns the subset of the given system [pkgs] that are currently installed
    on the system. This function queries the system package manager (e.g., apt,
    yum, pacman, brew, etc.) to determine which of the provided packages are
    already installed. It handles different package manager formats and output
    parsing, including virtual packages. *)
val installed_packages:
  ?env:gt_variables -> OpamFile.Config.t -> OpamSysPkg.Set.t ->
  OpamSysPkg.Set.t

(* Given a list of system packages, retrieve their installation status from the
   system and returns a {!OpamSysInteract.packages_status} record with,
     * available set: package that exist on the default
       repositories, but not installed
     * not found set: packages not found on the defined repositories

   [env] is used to determine host specification.
   [sys_available], if provided, is used directly to determine the
   availability of system packages depending on system; otherwise,
   {!available_packages} is called to query the system.
   Installed packages are computed using {!installed_packages}.
   [config] is used to determine Windows depext installation. *)
val packages_status:
  ?env:gt_variables -> ?sys_available:OpamSysPkg.availability_mode ->
  OpamFile.Config.t -> OpamSysPkg.Set.t -> OpamSysPkg.status

(* Returns [true] if the distribution is a stateless installation. It permits to
   define where there is a need to handle installed system packages or not. *)
val stateless_install: ?env:gt_variables -> unit -> bool

(* Return the commands to run to install given system packages.
   [env] is used to determine host specification.
   [config] is used to determine Windows depext installation. *)
val install_packages_commands:
  ?env:gt_variables -> rw switch_state option -> OpamFile.Config.t ->
  OpamSysPkg.to_install ->
  ([`AsAdmin of string | `AsUser of string] * string list) list

(* Install given system packages, by calling local system package manager.
   [env] is used to determine host specification.
   [config] is used to determine Windows depext installation. *)
val install:
  ?env:gt_variables -> rw switch_state option -> OpamFile.Config.t ->
  OpamSysPkg.to_install -> unit

val update: ?env:gt_variables -> OpamFile.Config.t -> unit

val package_manager_name: ?env:gt_variables -> OpamFile.Config.t -> string

(* Determine if special packages may need installing to enable other
   repositories.
   Presently used to check for epel-release on CentOS and RHEL.
   [env] is used to determine host specification. *)
val repo_enablers: ?env:gt_variables -> OpamFile.Config.t -> string option


module Cygwin : sig

  (* Location of the internal Cygwin installation *)
  val internal_cygroot: unit -> OpamFilename.Dir.t

  (* Install an internal Cygwin install, in <root>/.cygwin *)
  val install: OpamSysPkg.t list -> unit

  (* [analyse_install path] searches for and identifies Cygwin/MSYS2
     installations. [path] may be able the location of cygcheck.exe itself
     (with or without the .exe) or just a directory. If [path] is just a
     directory, then the function searches for 'path\cygcheck.exe',
     'path\bin\cygcheck.exe', or 'path\usr\bin\cygcheck.exe'. If exactly one
     is found, and cygpath.exe is found with it, then cygpath is used both to
     identify whether the installation is Cygwin or MSYS2 and to translate the
     root directory [/] to its Windows path (i.e. to get the canonical root
     directory of the installation). MSYS2 is additionally required to have
     pacman.exe in the same directory as cygcheck.exe and cygpath.exe.

     On success, the result is the kind of installation (Cygwin/MSYS2) along
     with the root directory (e.g. {v C:\cygwin64 v} or {v C:\msys64 v}),
     otherwise a description of the problem encountered is returned. *)
  val analyse_install:
    string -> ([ `Cygwin | `Msys2 ] * OpamFilename.Dir.t, string) result

  (* [bindir_for_root kind root] returns the bin directory for the given
     installation root and [kind], as returned by {!analyse_install}. *)
  val bindir_for_root:
    [ `Cygwin | `Msys2 ] -> OpamFilename.Dir.t -> OpamFilename.Dir.t

  (* Returns true if Cygwin install is internal *)
  val is_internal: OpamFile.Config.t -> bool

  (* Return Cygwin binary path *)
  val cygbin_opt: OpamFile.Config.t -> OpamFilename.Dir.t option

  (* Return MSYS2 binary path *)
  val msys2bin_opt: OpamFile.Config.t -> OpamFilename.Dir.t option
end

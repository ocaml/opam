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

(** Defines the types holding global, repository and switch states *)

open OpamTypes

include module type of OpamStateTypesCommon

(** State of a given switch: options, available and installed packages, etc.*)
type +'lock switch_state = private {
  switch_lock: OpamSystem.lock;

  switch_global: unlocked global_state;

  switch_repos: unlocked repos_state;

  switch: switch;
  (** The current active switch *)

  switch_invariant: formula;
  (** Defines the "base" of the switch, e.g. what compiler is desired *)

  compiler_packages: package_set;
  (** The packages that form the base of the current compiler. Normally equal to
      the subset of installed packages matching the invariant defined in
      switch_config *)

  switch_config: OpamFile.Switch_config.t;
  (** The configuration file for this switch *)

  repos_package_index: OpamFile.OPAM.t package_map;
  (** Metadata of all packages that could be found in the configured
      repositories (ignoring installed or pinned packages) *)

  opams: OpamFile.OPAM.t package_map;
  (** The metadata of all packages, gathered from repo, local cache and pinning
      overlays. This includes URL and descr data (even if they were originally
      in separate files), as well as the original metadata directory (that can
      be used to retrieve the files/ subdir) *)

  conf_files: OpamFile.Dot_config.t name_map;
  (** The opam-config of installed packages (from
      ".opam-switch/config/pkgname.config") *)

  packages: package_set;
  (** The set of all known packages *)

  sys_packages: sys_pkg_status package_map Lazy.t;
  (** Map of package and their system dependencies packages status. Only
      initialised for otherwise available packages *)

  available_packages: package_set Lazy.t;
  (** The set of available packages, filtered by their [available] field *)

  pinned: package_set;
  (** The set of pinned packages (their metadata, including pinning target, is
      in {!field:opams}) *)

  installed: package_set;
  (** The set of all installed packages *)

  installed_opams: OpamFile.OPAM.t package_map;
  (** The cached metadata of installed packages (may differ from the metadata
      that is in {!field:opams} for updated packages) *)

  installed_roots: package_set;
  (** The set of packages explicitly installed by the user. Some of them may
      happen not to be installed at some point, but this indicates that the
      user would like them installed. *)

  reinstall: package_set Lazy.t;
  (** The set of packages which need to be reinstalled *)

  invalidated: package_set Lazy.t;
  (** The set of packages which are installed but no longer valid, e.g. because
      of removed system dependencies. Only packages which are unavailable end up
      in this set, they are otherwise put in {!field:reinstall}. *)

  overwrote_opams: (bool * OpamFile.OPAM.t) package_map;
  (** In case of simulated pins, keep the old information of opam files. The
      boolean is set to true if the package was previously pinned. *)

  (* Missing: a cache for
     - switch-global and package variables
     - the solver universe? *)
} constraint 'lock = 'lock lock

module Abs : sig
  val create_switch_state :
    switch_global:unlocked global_state ->
    switch_repos:unlocked repos_state ->
    switch_lock:OpamSystem.lock ->
    switch:OpamTypes.switch ->
    switch_invariant:OpamTypes.formula ->
    compiler_packages:OpamTypes.package_set ->
    switch_config:OpamFile.Switch_config.t ->
    repos_package_index:OpamFile.OPAM.t OpamTypes.package_map ->
    installed_opams:OpamFile.OPAM.t OpamTypes.package_map ->
    installed:OpamTypes.package_set ->
    pinned:OpamTypes.package_set ->
    installed_roots:OpamTypes.package_set ->
    opams:OpamFile.OPAM.t OpamTypes.package_map ->
    conf_files:OpamFile.Dot_config.t OpamTypes.name_map ->
    packages:OpamTypes.package_set ->
    available_packages:OpamTypes.package_set Lazy.t ->
    sys_packages:OpamTypes.sys_pkg_status OpamTypes.package_map Lazy.t ->
    reinstall:OpamTypes.package_set Lazy.t ->
    invalidated:OpamTypes.package_set Lazy.t ->
    overwrote_opams:(bool * OpamFile.OPAM.t) OpamTypes.package_map ->
    'a switch_state

  val with_switch_lock : 'a switch_state -> OpamSystem.lock -> 'b switch_state

  val add_package :
    resolve_switch_raw:(?package:OpamPackage.Map.key ->
                        unlocked global_state ->
                        OpamTypes.switch ->
                        OpamFile.Switch_config.t ->
                        OpamFilter.env) ->
    'a switch_state ->
    package ->
    OpamFile.OPAM.t ->
    'a switch_state

  val remove_package : 'a switch_state -> package -> 'a switch_state

  val add_pinned :
    resolve_switch_raw:(?package:OpamPackage.Map.key ->
                        unlocked global_state ->
                        OpamTypes.switch ->
                        OpamFile.Switch_config.t ->
                        OpamFilter.env) ->
    'a switch_state ->
    package ->
    OpamFile.OPAM.t ->
    'a switch_state

  val remove_pinned : 'a switch_state -> package -> 'a switch_state

  val update_gt : 'a switch_state -> unlocked global_state -> 'a switch_state

  val update_reinstall : 'a switch_state -> package_set Lazy.t -> 'a switch_state

  val update_installed :
    compute_invariant_packages:('a switch_state -> package_set) ->
    ?installed:package_set ->
    ?installed_roots:package_set ->
    ?reinstall:package_set ->
    ?pinned:package_set ->
    'a switch_state ->
    'a switch_state

  val update_installed_only : 'a switch_state -> package_set -> 'a switch_state

  val update_reinstall_only : 'a switch_state -> package_set Lazy.t -> 'a switch_state

  val update_installed_plus_conf_files : 'a switch_state -> package_set -> 'a switch_state

  val update_installed_roots : 'a switch_state -> package_set -> 'a switch_state

  val update_conf_files : 'a switch_state -> OpamFile.Dot_config.t name_map -> 'a switch_state

  val update_config : OpamFile.Switch_config.t -> 'a switch_state -> 'a switch_state

  val update_invariant_and_config : 'a switch_state -> OpamFile.Switch_config.t -> 'a switch_state

  val update_invariant : 'a switch_state -> OpamFormula.t -> 'a switch_state

  val update_available : 'a switch_state -> package_set Lazy.t -> 'a switch_state

  val update_overwrote : 'a switch_state -> (bool * OpamFile.OPAM.t) package_map -> 'a switch_state

  val update_compilers : 'a switch_state -> package_set -> 'a switch_state

  val update_sys_pkgs : 'a switch_state -> sys_pkg_status package_map Lazy.t -> 'a switch_state

  val update_name_and_available : 'a switch_state -> switch -> package_set Lazy.t -> 'a switch_state

  val import :
    'a switch_state ->
    available_packages:OpamTypes.package_set Lazy.t ->
    packages:OpamTypes.package_set ->
    compiler_packages:OpamTypes.package_set ->
    pinned:OpamTypes.package_set ->
    opams:OpamFile.OPAM.t OpamTypes.package_map ->
    'a switch_state

  val pin_overwrotes : 'a switch_state -> 'a switch_state

  val empty_installed : 'a switch_state -> 'a switch_state
end

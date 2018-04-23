(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Defines the types holding global, repository and switch states *)

open OpamTypes

(** Client state *)

(** Phantom types to indicate the locking state of a state, and allow or not
    on-disk operations.

    Note that each state load is itself locking enough to return a consistent
    state: a read lock is only needed when the consistency of the actions depend
    on the fact that the given state doesn't change during the run (e.g. an
    update that depends on it). In particular, all query commands don't need a
    read lock.

    Subtyping is by guarantees given on the operations allowed, [rw] giving the
    most and being the smallest type, so that it is safe to coerce
    [(rw t :> ro t)].
*)

(** Phantom type for readwrite-locked state (ensures that there are no
    concurrent reads or writes) *)
type rw = [ `Lock_write ]

(** Type for read-locked state (ensures that there are no concurrent writes) *)
type ro = [ `Lock_read | rw ]

(** Type for unlocked state (single file reads should still be ok) *)
type unlocked = [ `Lock_none | ro ]

(** The super-type for all lock types *)
type +'a lock = [< unlocked > `Lock_write ] as 'a

(** Global state corresponding to an opam root and its configuration *)
type +'lock global_state = {
  global_lock: OpamSystem.lock;

  root: OpamPath.t;
  (** The global OPAM root path (caution: this is stored here but some code may
      rely on OpamStateConfig.root_dir ; in other words, multiple root handling
      isn't really supported at the moment) *)

  config: OpamFile.Config.t;
  (** The main configuration file. A note of caution: this corresponds to the
      configuration as loaded from the file: to get the current options, which
      may be overriden through the command-line or environment, see
      OpamStateConfig *)

  global_variables:
    (variable_contents option Lazy.t * string) OpamVariable.Map.t;
  (** A map of variables that have been defined globally, e.g. through
      `.opam/config`. They may need evaluation so are stored as lazy values.
      The extra string is the supplied variable documentation *)
} constraint 'lock = 'lock lock

(** State corresponding to the repo/ subdir: all available packages and
    metadata, for each repository. *)
type +'lock repos_state = {
  repos_lock: OpamSystem.lock;

  repos_global: unlocked global_state;

  repositories: repository repository_name_map;
  (** The list of repositories *)

  repos_definitions: OpamFile.Repo.t repository_name_map;
  (** The contents of each repo's [repo] file *)

  repo_opams: OpamFile.OPAM.t package_map repository_name_map;
  (** All opam files that can be found in the configured repositories *)
} constraint 'lock = 'lock lock

(** State of a given switch: options, available and installed packages, etc.*)
type +'lock switch_state = {
  switch_lock: OpamSystem.lock;

  switch_global: unlocked global_state;

  switch_repos: unlocked repos_state;

  switch: switch;
  (** The current active switch *)

  compiler_packages: package_set;
  (** The packages that form the base of the current compiler *)

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

  conf_files: OpamFile.Dot_config.t package_map;
  (** The opam-config of installed packages (from
      ".opam-switch/config/pkgname.config") *)

  packages: package_set;
  (** The set of all known packages *)

  available_packages: package_set Lazy.t;
  (** The set of available packages, filtered by their [available:] field *)

  pinned: package_set;
  (** The set of pinned packages (their metadata, including pinning target, is
      in [opams]) *)

  installed: package_set;
  (** The set of all installed packages *)

  installed_opams: OpamFile.OPAM.t package_map;
  (** The cached metadata of installed packages (may differ from the metadata
      that is in [opams] for updated packages) *)

  installed_roots: package_set;
  (** The set of packages explicitly installed by the user. Some of them may
      happen not to be installed at some point, but this indicates that the
      user would like them installed. *)

  reinstall: package_set;
  (** The set of packages which needs to be reinstalled *)

  (* Missing: a cache for
     - switch-global and package variables
     - the solver universe? *)
} constraint 'lock = 'lock lock

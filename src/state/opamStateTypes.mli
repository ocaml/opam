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

open OpamTypes

(** Client state *)
(* !X Rather use a phantom type for [readonly xxx_state], [readwrite xxx_state],
   etc. and ensure well-typedness of state writing functions to guarantee
   soundness of locking ? *)
type lock_kind = Lock_none | Lock_readonly | Lock_readwrite

(** Global state corresponding to an opam root and its configuration *)
type global_state = {
  global_lock: lock_kind;

  root: OpamPath.t;
  (** The global OPAM root path (caution: this is stored here but some code may
      rely on OpamStateConfig.root_dir ; in other words, multiple root handling
      isn't really supported at the moment) *)

  config: OpamFile.Config.t;
  (** The main configuration file. A note of caution: this corresponds to the
      configuration as loaded from the file: to get the current options, which
      may be overriden through the command-line or environment, see
      OpamStateConfig *)
}

(** State corresponding to the repo/ subdir: all available packages, their
    origin and metadata.

    Note: this state corresponds to a given configuration of repository
    priorities. For per-switch repositories, we will need also a generic
    repository state with all packages from all repos, before computation of a
    given layout. *)
type repos_state = {
  repos_lock: lock_kind;

  repos_global: global_state;

  repositories: OpamFile.Repo_config.t repository_name_map;
  (** The list of repositories *)

  package_index: (repository_name * string option) package_map;
  (** Package index
      (map from packages to their repository and relative path) *)

  repo_opams: OpamFile.OPAM.t package_map;
  (** All opam files that can be found in the configured repositories *)
}

(** State of a given switch: options, available and installed packages, etc.*)
type switch_state = {
  switch_lock: lock_kind;

  switch_global: global_state;

  switch_repos: repos_state;

  switch: switch;
  (** The current active switch *)

  compiler_packages: package_set;
  (** The packages that form the base of the current compiler *)

  switch_config: OpamFile.Dot_config.t;
  (** The contents of the global configuration file for this
      switch *)

  opams: OpamFile.OPAM.t package_map;
  (** The metadata of all packages, gathered from repo, local cache and pinning
      overlays. This includes URL and descr data (even if they were originally
      in separate files), as well as the original metadata directory (that can
      be used to retrieve the files/ subdir) *)

  packages: package_set;
  (** The set of all known packages *)

  available_packages: package_set Lazy.t;
  (** The set of available packages, filtered by their [available:] field *)

  pinned: (OpamPackage.Version.t * pin_option) name_map;
  (** The names of pinned packages and their pinning target *)

  installed: package_set;
  (** The set of all installed packages *)

  installed_roots: package_set;
  (** The set of packages explicitly installed by the user (subset of
      [installed]) *)

  reinstall: package_set;
  (** The set of packages which needs to be reinsalled *)

  (* Missing: a cache for
     - switch-global and package variables
     - the solver universe ? *)
}

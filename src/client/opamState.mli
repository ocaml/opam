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

(** OPAM client state *)

open OpamTypes

(** Client state *)
module Types: sig

  type t = {

    (** Is the state partial ?
        TODO: split-up global vs. repository state *)
    partial: bool;

    (** The global OPAM root path *)
    root: OpamPath.t;

    (** The current active switch *)
    switch: switch;

    (** The current compiler name (corresponding to a .comp file) *)
    compiler: compiler;

    (** The current version of the compiler *)
    compiler_version: compiler_version lazy_t;

    (** The list of OPAM files *)
    opams: OpamFile.OPAM.t package_map;

    (** The list of description files *)
    descrs: OpamFile.Descr.t lazy_t package_map;

    (** The list of repositories *)
    repositories: OpamFile.Repo_config.t repository_name_map;

    (** The list of packages *)
    packages: package_set;

    (** The list of packages, keeping the one available for the current
        compiler version *)
    available_packages: package_set Lazy.t;

    (** The association list between switch and compiler *)
    aliases: OpamFile.Aliases.t;

    (** The list of compiler available to install *)
    compilers: compiler_set;

    (** The list of pinned packages *)
    pinned: OpamFile.Pinned.t;

    (** The list of installed packages *)
    installed: OpamFile.Installed.t;

    (** The list of packages explicitly installed by the user *)
    installed_roots: OpamFile.Installed_roots.t;

    (** The list of packages which needs to be reinsalled *)
    reinstall: OpamFile.Reinstall.t;

    (** The main configuration file *)
    config: OpamFile.Config.t;

    (** Package index *)
    package_index: (repository_name * string option) package_map lazy_t;

    (** Compiler index *)
    compiler_index: (repository_name * string option) compiler_map lazy_t;
  }

end

type state = Types.t
open Types

(** Load the client state. The string argument is to identify to call
    site. *)
val load_state: ?save_cache:bool -> string -> state

(** Rebuild the state cache. *)
val rebuild_state_cache: unit -> unit

(** Remove the state cache *)
val remove_state_cache: unit -> unit

(** Display stats *)
val print_stats: unit -> unit

(** Load repository related states only. All the other fields are left empty. *)
val load_repository_state: string -> state

(** Load state associated to env variables. All other fields are left empty. *)
val load_env_state: string -> state

(** Create a universe from the current state *)
val universe: state -> user_action -> universe

(** {2 Environment} *)

(** Get the current environment. *)
val get_full_env: state -> env

(** Get only environment modified by OPAM. *)
val get_opam_env: state -> env

(** Update an environment. *)
val add_to_env: state -> env -> (string * string * string) list -> env

(** Print a warning if the environment is not set-up properly on init. *)
val print_env_warning_at_init: state -> user_config -> unit

(** Print a warning if the environment is not set-up properly on switch. *)
val print_env_warning_at_switch: state -> unit

(** {2 Initialisation} *)

(** Update the global and user configuration by asking some questions. *)
val update_setup_interactive: state -> shell -> filename -> bool

(** Display the global and user configuration for OPAM. *)
val display_setup: state -> shell -> filename -> unit

(** Update the user configuration. *)
val update_setup: state -> user_config option -> global_config option -> unit

(** {2 Substitutions} *)

(** Compute the value of a variable *)
val contents_of_variable: state -> variable_map -> full_variable -> variable_contents option

(** Compute the value of a variable. Raise [Exit] if the variable is
    not valid. *)
val contents_of_variable_exn: state -> variable_map -> full_variable -> variable_contents

(** Substitute a string *)
val substitute_string: state -> variable_map -> string -> string

(** Substitute file *)
val substitute_file: state -> variable_map -> basename -> unit

(** {2 Filters} *)

(** Evaluate a filter *)
val eval_filter: state -> variable_map -> filter option -> bool

(** Filter a list of commands by:
    - evaluating the substitution strings; and
    - removing the commands with a filter evaluating to "false" *)
val filter_commands: state -> variable_map -> command list -> string list list

(** {2 Helpers} *)

(** Return the OPAM file for the given package *)
val opam: state -> package -> OpamFile.OPAM.t

(** Return the URL file for the given package *)
val url: state -> package -> OpamFile.URL.t option

(** Return the Descr file for the given package *)
val descr: state -> package -> OpamFile.Descr.t option

(** Return the files/ directory overlay for the given package *)
val files: state -> package -> dirname option

(** Return the compiler description *)
val compiler_comp: state -> compiler -> OpamFile.Comp.t

(** {2 Repositories} *)

(** Pretty print a map of repositories *)
val string_of_repositories: OpamFile.Repo_config.t repository_name_map -> string

(** Builds a map which says in which repository the latest metadata
    for a given package are. The function respect the bustom
    priorities given by the order of [priorities]. *)
val package_index: state -> (repository_name * string option) package_map

(** Build a map which says in which repository the latest metadata for
    a given compiler is. *)
val compiler_index: state -> (repository_name * string option) compiler_map

(** Sort repositories by priority. *)
val sorted_repositories: state -> repository list

(** Check whether a repository exists. *)
val mem_repository: state -> repository_name -> bool

(** Find a given repostiory. Exit the program if no such repository name exists. *)
val find_repository: state -> repository_name -> repository

(** Find a given repostiory. *)
val find_repository_opt: state -> repository_name -> repository option

(** {2 Compilers} *)

(** (Re-)install the configuration for a given root and switch *)
val install_global_config: dirname -> switch -> unit

(** Install the given compiler *)
val install_compiler: state -> quiet:bool -> switch -> compiler -> unit

(** Write the right compiler switch in ~/.opam/config *)
val update_switch_config: state -> switch -> unit

(** Get the packages associated with the given compiler *)
val get_compiler_packages: state -> compiler -> atom list

(** Is a compiler installed ? *)
val is_compiler_installed: state -> compiler -> bool

(** Is a switch installed ? *)
val is_switch_installed: state -> switch -> bool

(** Global compiler state *)
val compiler_state: state -> checksums compiler_map

(** Repository state *)
val compiler_repository_state: state -> checksums compiler_map

(** Return the active repository for a given compiler *)
val repository_of_compiler: state -> compiler -> (repository * string option) option

(** {2 Packages} *)

(** Check whether a package name is installed *)
val is_name_installed: state -> name -> bool

(** Return whether a package is installed *)
val is_package_installed: state -> package -> bool

(** Return the installed package with the right name *)
val find_installed_package_by_name: state -> name -> package

(** Return all the packages with the given name *)
val find_packages_by_name: state -> name -> package_set option

(** Return a map from package names to package installed version *)
val installed_map: state -> version name_map

(** Return the base packages *)
val base_packages: name list

(** Return all the collection of installed packages, for all the
    available packages *)
val all_installed: state -> package_set

(** Return a map containing the switch where a given package is installed. *)
val installed_versions: state -> name -> switch list package_map

(** Download the OPAM-package archive ($name.$version+opam.tar.gz) *)
val download_archive: state -> package -> filename option

(** Download the upstream archive, add the eventual additional files
    and return the directory.. *)
val download_upstream: state -> package -> dirname -> generic_file option

(** Global package state. *)
val package_state: state -> checksums package_map

(** Global & partial package state. *)
val package_partial_state: state -> package -> archive:bool -> bool * checksums

(** Repository state *)
val package_repository_state: state -> checksums package_map

(** Repository & partial package state. *)
val package_repository_partial_state: state -> package -> archive:bool ->
  bool * checksums

(** Get the active repository for a given package *)
val repository_of_package: state -> package -> (repository * string option) option

(** Add the given packages to the set of package to reinstall. If [all]
    is set, this is done for ALL the switches (useful when a package
    change upstream for instance). If not, only the reinstall state of the
    current switch is changed. *)
val add_to_reinstall: state -> all:bool -> package_set -> unit

(** Return the files for a given package *)
val copy_files: state -> package -> dirname -> unit

(** {2 Development packages} *)

(** Get all the development packages. This include the one locally
    pinned (for the current switch) and the global dev packages. *)
val dev_packages: state -> package_set

(** [update_dev_packages t] checks for upstream changes for packages
    first in the switch cache and then in the global cache. Return the
    packages whose contents have changed upstream. Side-effect: update
    the reinstall files. *)
val update_dev_packages: state -> package_set

(** {2 Configuration files} *)

(** Return the .config file for the given package *)
val dot_config: state -> name -> OpamFile.Dot_config.t

(** {2 Locks} *)

(** Apply a function while taking the right locks *)
val check: lock -> unit

(** {2 Pinned packages} *)

(** Is the package locally pinned ? (ie. not a version pinning) *)
val is_locally_pinned: state -> name -> bool

(** Return the URL file associated with a locally pinned package. *)
val url_of_locally_pinned_package: state -> name -> OpamFile.URL.t

(** Return the repository associtated with a locally pinned
    package. *)
val repository_of_locally_pinned_package: state -> name -> repository

(** {2 Overlays} *)

(** Compute the overlay package for a given name. It return the higher
    package available with this name.  *)
val overlay_of_name: state -> name -> package

(** Cache an OPAM file *)
val add_opam_overlay: state -> package -> OpamFile.OPAM.t -> unit

(** Cache an URL file *)
val add_url_overlay: state -> package -> OpamFile.URL.t -> unit

(** Cache a descr file *)
val add_descr_overlay: state -> package -> OpamFile.Descr.t -> unit

(** Cache additional files *)
val add_files_overlay: state -> package -> dirname -> filename list -> unit

(** Add overlay files for a pinned package *)
val add_pinned_overlay: state -> name -> unit

(** Remove all overlay files *)
val remove_overlay: state -> package -> unit

(** {2 System compilers} *)

(** Create {i $opam/compilers/system.com}. Take the global root and
    the new system compiler version as arguments. *)
val create_system_compiler_description: dirname -> compiler_version option -> unit

(** {2 Jobs} *)
val jobs: state -> int

(** {2 Misc} *)

(** Ask the user to press Y/y/N/n to continue *)
val confirm: ('a, unit, string, bool) format4 -> 'a

(** Consistency checks: do the base package for the current compiler
    are installed ? *)
val check_base_packages: state -> unit

(** / **)

(** Switch reinstall hook. *)
val switch_reinstall_hook: (switch -> unit) ref

(** Update hook *)
val fix_descriptions_hook: (state -> verbose:bool -> unit) ref

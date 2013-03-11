(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** OPAM client state *)

open OpamTypes

(** Client state *)
type state = {

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
  compiler_version: compiler_version;

  (** The list of OPAM files *)
  opams: OpamFile.OPAM.t package_map;

  (** The list of description files *)
  descrs: OpamFile.Descr.t package_map;

  (** The list of repositories *)
  repositories: OpamFile.Repo_config.t repository_name_map;

  (** The list of packages *)
  packages: package_set;

  (** The list of packages, keeping the one available for the current
      compiler version *)
  available_packages: package_set Lazy.t;

  (** The association list between switch and compiler *)
  aliases: OpamFile.Aliases.t;

  (** The list of pinned packages *)
  pinned: OpamFile.Pinned.t;

  (** The list of installed packages *)
  installed: OpamFile.Installed.t;

  (** The list of packages explicitely installed by the user *)
  installed_roots: OpamFile.Installed_roots.t;

  (** The list of packages which needs to be reinsalled *)
  reinstall: OpamFile.Reinstall.t;

  (** The main configuration file *)
  config: OpamFile.Config.t;

  (** The main configuration files for the repositories *)
  repo_index: OpamFile.Repo_index.t;

}

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

(** Print a warning if the environment is not set-up properly. *)
val print_env_warning: state -> user_config option -> unit

(** {2 Initialisation} *)

(** Update the global and user configuration by asking some questions. *)
val update_setup_interactive: state -> shell -> filename -> unit

(** Display the global and user configuration for OPAM. *)
val display_setup: state -> shell -> filename -> unit

(** Update the user configuration. *)
val update_setup: state -> user_config option -> global_config option -> unit

(** Update the global environment variables. *)
val update_env_variables: state -> unit

(** {2 Substitutions} *)

(** Compute the value of a variable *)
val contents_of_variable: state -> full_variable -> variable_contents

(** Substitute a string *)
val substitute_string: state -> string -> string

(** Substitute file *)
val substitute_file: state -> basename -> unit

(** {2 Filters} *)

(** Evaluate a filter *)
val eval_filter: state -> filter option -> bool

(** Filter a list of commands by:
    - evaluating the substitution strings; and
    - removing the commands with a filter evaluating to "false" *)
val filter_commands: state -> command list -> string list list

(** {2 Repositories} *)

(** Check if a package belongs to a repository *)
val mem_repository: state -> package -> bool

(** Apply a function on the repository which contains a given package *)
val with_repository: state -> package -> (repository_root -> repository -> 'a) -> 'a

(** Check whether a repository name is valid *)
val mem_repository_name: state -> repository_name -> bool

(** Find a repository state, given its name *)
val find_repository_name: state -> repository_name -> repository

(** Pretty print a map of repositories *)
val string_of_repositories: OpamFile.Repo_config.t repository_name_map -> string

(** Build a map which says in which repository the latest metadata for
    a given package is. This function is *very* costly (need to scan all the
   files in the repositories, so don't abuse). *)
val package_repository_map: state -> repository package_map

(** Build a map which says in which repository the latest metadata for
    a given compiler is. *)
val compiler_repository_map: state -> (filename * filename option) compiler_map

(** Sort repositories by priority *)
val sorted_repositories: state -> repository list

(** {2 Compilers} *)

(** Return the list of available compilers *)
val compilers: root:dirname -> compiler_set

(** Install the given compiler *)
val install_compiler: state -> quiet:bool -> switch -> compiler -> unit

(** Get the packages associated with the given compiler *)
val get_compiler_packages: state -> compiler -> atom list

(** {2 Packages} *)

(** Check whether a package name is installed *)
val mem_installed_package_by_name: state -> name -> bool

(** Return the installed package with the right name *)
val find_installed_package_by_name: state -> name -> package

(** Check whether a package name is installed, but this time
    using the collection of installed packages as argument *)
val mem_installed_package_by_name_aux: package_set -> name -> bool

(** Return the installed package with the right name, but this time
    using the collection of installed packages as argument *)
val find_installed_package_by_name_aux: package_set -> name -> package

(** Return all the packages with the given name *)
val find_packages_by_name: state -> name -> package_set option

(** Return a map from package names to package installed version *)
val installed_map: state -> version name_map

(** Return the base packages *)
val base_packages: name list

(** Return all the collection of installed packages, for all the
   available packages *)
val all_installed: state -> package_set

(** {2 Configuration files} *)

(** Return the .config file for the given package *)
val dot_config: state -> name -> OpamFile.Dot_config.t

(** Return the OPAM file for the given package *)
val opam: state -> package -> OpamFile.OPAM.t

(** Return the compiler descritpion file for the given compiler name *)
val compiler: state -> compiler -> OpamFile.Comp.t

(** {2 Locks} *)

(** Apply a function while taking the right locks *)
val check: lock -> unit

(** {2 Pinned packages} *)

(** Is a package pinned ? *)
val is_pinned: state -> name -> bool

(** Get the corresponding pinned package. If the package is pinned to
    a path (locally or via git/darcs), it returns the latest package as we
    assume that the most up-to-date build descriptions. *)
val pinned_package: state -> name -> package

(** Get the path associated to the given pinned package. Return [None]
   if the package is not pinned or if it is pinned to a version
   number. *)
val pinned_path: state -> name -> dirname option

(** Update pinned package *)
val update_pinned_package: state -> name -> dirname download

(** Add the given packages to the set of package to reinstall. If [all]
    is set, this is done for ALL the switches (useful when a package
    change upstream for instance). If not, only the reinstall state of the
    current switch is changed. *)
val add_to_reinstall: state -> all:bool -> package_set -> unit

(** {2 System compilers} *)

(** Create {i $opam/compilers/system.com}. Take the global root and
    the new system compiler version as arguments. *)
val create_system_compiler_description: dirname -> compiler_version option -> unit

(** {2 Misc} *)

(** Ask the user to press Y/y/N/n to continue *)
val confirm: ('a, unit, string, bool) format4 -> 'a

(** Consistency checks: do the base package for the current compiler
    are installed ? *)
val check_base_packages: state -> unit

(** To be able to open [OpamState.Types] *)
module Types: sig
  type t = state = {
    partial: bool;
    root: OpamPath.t;
    switch: switch;
    compiler: compiler;
    compiler_version: compiler_version;
    opams: OpamFile.OPAM.t package_map;
    descrs: OpamFile.Descr.t package_map;
    repositories: OpamFile.Repo_config.t repository_name_map;
    packages: package_set;
    available_packages: package_set Lazy.t;
    aliases: OpamFile.Aliases.t;
    pinned: OpamFile.Pinned.t;
    installed: OpamFile.Installed.t;
    installed_roots: OpamFile.Installed_roots.t;
    reinstall: OpamFile.Reinstall.t;
    config: OpamFile.Config.t;
    repo_index: OpamFile.Repo_index.t;
  }
end


(** / **)

(** Update hook. *)
val update_hook: (save_cache:bool -> repository_name list -> unit) ref

(** Switch reinstall hook. *)
val switch_reinstall_hook: (switch -> unit) ref

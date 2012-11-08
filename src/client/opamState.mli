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

  (** The list of packages which needs to be reinsalled *)
  reinstall: OpamFile.Reinstall.t;

  (** The main configuration file *)
  config: OpamFile.Config.t;

  (** The main configuration files for the repositories *)
  repo_index: OpamFile.Repo_index.t;

}

(** Load the client state *)
val load_state: unit -> state

(** Create a universe from the current state *)
val universe: state -> user_action -> universe

(** {2 Environment} *)

(** Get the current environment *)
val get_env: state -> env

(** Update an environment. *)
val update_env: state -> env -> (string * string * string) list -> env

(** Print a warning if the environment is not set-up properly. *)
val print_env_warning: ?add_profile:bool -> state -> unit

(** {2 Substitutions} *)

(** Compute the value of a variable *)
val contents_of_variable: state -> full_variable -> variable_contents

(** Substitute a string *)
val substitute_string: state -> string -> string

(** Substitute file *)
val substitute_file: state -> basename -> unit

(** Substitute a list of commands *)
val substitute_commands: state -> command list -> command list

(** {2 Filters} *)

(** Evaluate a filter *)
val eval_filter: state -> filter option -> bool

(** Filter a list of commands *)
val filter_commands: state -> command list -> string list list

(** {2 Repositories} *)

(** Check if a package belongs to a repository *)
val mem_repository: state -> package -> bool

(** Apply a function on the repository which contains a given package *)
val with_repository: state -> package -> (OpamPath.Repository.r -> repository -> 'a) -> 'a

(** Check whether a repository name is valid *)
val mem_repository_name: state -> repository_name -> bool

(** Find a repository state, given its name *)
val find_repository_name: state -> repository_name -> repository

(** Pretty print a map of repositories *)
val string_of_repositories: OpamFile.Repo_config.t repository_name_map -> string

(** {2 Compilers} *)

(** Return the list of available compilers *)
val compilers: state -> compiler_set

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

(** To be able to open [OpamState.Types] *)
module Types: sig
  type t = state = {
    root: OpamPath.t;
    switch: switch;
    compiler: compiler;
    compiler_version: compiler_version;
    opams: OpamFile.OPAM.t package_map;
    repositories: OpamFile.Repo_config.t repository_name_map;
    packages: package_set;
    available_packages: package_set Lazy.t;
    aliases: OpamFile.Aliases.t;
    pinned: OpamFile.Pinned.t;
    installed: OpamFile.Installed.t;
    reinstall: OpamFile.Reinstall.t;
    config: OpamFile.Config.t;
    repo_index: OpamFile.Repo_index.t;
  }
end

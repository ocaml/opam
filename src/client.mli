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

(** OPAM client. *)

open Types

(** Initialize the client a consistent state.
    [init repo alias oversion cores] means:
    - [repo] is the initial repository description,
    - [oversion] is the version of the compiler. [None] means we
      are using the system-wide installed compiler.
    - [cores] is the number of cores *)
val init : repository -> OCaml_V.t option -> int -> unit

(** Display all available packages that matches any of the regexps. *)
val list : print_short:bool -> installed_only:bool -> ?name_only:bool -> ?case_sensitive:bool
  -> string list  -> unit

(** Display a general summary of a package. *)
val info : N.t -> unit

(** Depending on request, return options or directories where the
    package is installed. *)
val config : config -> unit

(** Install the given set of packages. Take the global file lock. *)
val install : N.Set.t -> unit

(** Reinstall the given set of packages. Take the global file lock. *)
val reinstall : N.Set.t -> unit

(** Refresh the available packages. Take the global file lock. *)
val update : string list -> unit

(** Find a consistent state where most of the installed packages are
    upgraded to their latest version.
    If no package packages are specified then try to upgrade everything.
    Take the global file lock. *)
val upgrade : N.Set.t -> unit

(** Upload a package to a remote repository. If repo is [None] then it
    will look for the repository associated with the package
    name. Otherwise, it will look for a repository having the right
    name. Take the global file lock. *)
val upload : upload -> string option -> unit

(** Remove the given set of packages. Take the global file lock. *)
val remove : N.Set.t -> unit

(** Manage remote repositories. Take the global file lock. *)
val remote : remote -> unit

(** Install the given compiler. Take the global file lock. *)
val compiler_install: bool -> Alias.t -> OCaml_V.t -> unit

(** Import the packages from a file. Take the global file lock. *)
val compiler_import: filename -> unit

(** Export the packages to a file. Take the global file lock. *)
val compiler_export: filename -> unit

(** Remove the given compiler. Take the global file lock. *)
val compiler_remove: Alias.t -> unit

(** Switch to the given compiler. Take the global file lock. *)
val compiler_switch: bool -> Alias.t -> unit

(** Reinstall the given compiler. Take the global file lock. *)
val compiler_reinstall: Alias.t -> unit

(** List the available compiler descriptions *)
val compiler_list: unit -> unit

(** Display the name of the current compiler *)
val compiler_current: unit -> unit

(** Pin a package to a specific version. Take the global file lock. *)
val pin: pin -> unit

(** List the current pinned packages *)
val pin_list: unit -> unit

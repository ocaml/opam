(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** OPAM client. *)

open Types

(** Initializes the client a consistent state.
    [init repo alias oversion cores] means:
    - [repo] is the initial repository description,
    - [alias] the compiler alias
    - [oversion] is the version of the compiler
    - [cores] is the number of cores *)
val init : repository -> Alias.t -> OCaml_V.t -> int -> unit

(** Displays all available packages. 
    If [bool] is [true], then we only display 
    packages that are known to exist. *)
val list : bool -> unit

(** Displays a general summary of a package. *)
val info : N.t -> unit

(** Depending on request, returns options or directories where the
    package is installed. *)
val config : config -> unit

(** Installs the given set of packages. Take the global file lock. *)
val install : N.Set.t -> unit

(** Refresh the available packages. Take the global file lock. *)
val update : unit -> unit

(** Finds a consistent state where most of the installed packages are
    upgraded to their latest version. Take the global file lock. *)
val upgrade : unit -> unit

(** Upload a package to a remote repository. If repo is [None] then it
    will look for the repository associated with the package
    name. Otherwise, it will look for a repository having the right
    name. Take the global file lock. *)
val upload : upload -> string option -> unit

(** Removes the given set of packages. Take the global file lock. *)
val remove : N.Set.t -> unit

(** Manage remote repositories. Take the global file lock. *)
val remote : remote -> unit

(** [switch and_clone alias descr] switch to an OCaml compiler 
    and clone at the end in case [and_clone] is [true]. It creates
    {i $opam/$alias} if it does not exists by reading the contents
    of {i $opam/compilers/$descr.comp}.
    It takes the global file lock. *)
val switch: bool -> Alias.t -> OCaml_V.t -> unit

(** [compiler_list] list the available compiler descriptions *)
val compiler_list: unit -> unit

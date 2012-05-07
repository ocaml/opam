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

open Types

(** Initializes the client a consistent state. *)
val init : repository -> unit

(** Displays all available packages *)
val list : unit -> unit

(** Displays a general summary of a package. *)
val info : N.t -> unit

(** Depending on request, returns options or directories where the
    package is installed. *)
val config : config -> unit

(** Installs the given package. *)
val install : N.t -> unit

(** Refresh the available packages. *)
val update : unit -> unit

(** Finds a consistent state where most of the installed packages are
    upgraded to their latest version. *)
val upgrade : unit -> unit

(** Upload a package to a remote repository. If repo is [None] then it
    will look for the repository associated with the package name. *)
val upload : upload -> repository option -> unit

(** Removes the given package. *)
val remove : N.t -> unit

(** Manage remote repositories *)
val remote : remote -> unit

(** Switch to an OCaml compiler *)
val switch: OCaml_V.t -> unit

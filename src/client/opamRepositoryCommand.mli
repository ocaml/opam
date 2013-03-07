(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012-2013 OCamlPro                                     *)
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

(** Repository sub-command functions. *)

open OpamState.Types
open OpamTypes

(** Update the repository index. Return the new (package -> repo)
   map. *)
val update_index: t -> repository package_map

(** List the available repositories. *)
val list: short:bool -> unit

(** Add a new repository. *)
val add: repository_name -> repository_kind -> address -> priority:int option -> unit

(** Remove a repository. *)
val remove: repository_name -> unit

(** Set a repository priority. *)
val priority: repository_name -> priority:int -> unit

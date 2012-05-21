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
open Path
open File

val init : R.t -> Repo_config.t -> unit
val update : R.t -> Repo_config.t -> unit
val upload : R.t -> Repo_config.t -> unit
val download : R.t -> Repo_config.t -> NV.t -> unit

module Raw : sig (* MOVE each value in a [repo/$REPO/raw.ml] file ? *)

  val rsync : [ `A | `R ] list -> string -> filename -> unit
(* val svn : *)
(* val git : *)
end

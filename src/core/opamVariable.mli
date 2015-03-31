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

(** {2 Variable names} *)

include OpamMisc.ABSTRACT

(** Shortcut to variables *)
type variable = t

(** Variable contents *)
type variable_contents =
  | B of bool
  | S of string

(** Pretty print of variable contents *)
val string_of_variable_contents: variable_contents -> string


module Full: sig

  (** Fully qualified variable. *)

  include OpamMisc.ABSTRACT

  (** Create a variable local for a given library/syntax extension *)
  val create: OpamPackage.Name.t -> variable -> t

  (** Create a global variable *)
  val global: variable -> t

  val is_global: t -> bool

  (** Return the package the variable is defined in *)
  val package: t -> OpamPackage.Name.t

  (** Return the variable name *)
  val variable: t -> variable

end

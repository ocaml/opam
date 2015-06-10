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

(** {2 Variable names} *)

include OpamStd.ABSTRACT

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

  include OpamStd.ABSTRACT

  type scope =
    | Global (** Note: this is attributed to unqualified variables, and may
                 also design self-referring ones *)
    | Self (** Variable in a package-specific file referring to that
               package [_:varname] *)
    | Package of OpamPackage.Name.t (** [pkgname:varname] *)

  (** Returns the scope of the variable *)
  val scope: t -> scope

  (** Returns the unqualified variable name *)
  val variable: t -> variable

  val is_global: t -> bool

  (** Return the package corresponding to the scope of the variable *)
  val package: ?self:OpamPackage.Name.t -> t -> OpamPackage.Name.t option

  (** Create a variable local for a given library/syntax extension *)
  val create: OpamPackage.Name.t -> variable -> t

  (** Create a global variable *)
  val global: variable -> t

end

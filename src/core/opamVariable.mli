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

(** the variable [enable] *)
val enable: t

(** the variable [installed] *)
val installed: t

(** Section names *)
module Section: sig

  include OpamMisc.ABSTRACT

  (** Graph of fully-qualified sections *)
  module G : Graph.Sig.I with type V.t = t

  (** Iteration in topological order *)
  val graph_iter : (G.V.t -> unit) -> G.t -> unit

  (** Shortcut to sections *)
  type section = t

  (** Fully qualified section names *)
  module Full: sig

    include OpamMisc.ABSTRACT

    (** Create a fully qualified section *)
    val create: OpamPackage.Name.t -> section -> t

    (** All the sections in a package *)
    val all: OpamPackage.Name.t ->  t

    (** Return the package name in which the section is *)
    val package: t -> OpamPackage.Name.t

    (** Return the optional section OpamPackage.Name.t: [None] means all available
        sections. *)
    val section: t -> section option

  end

end

(** Fully qualified variables *)
module Full: sig

  include OpamMisc.ABSTRACT

  (** Create a variable local for a given library/syntax extension *)
  val create_local: OpamPackage.Name.t -> Section.t -> variable -> t

  (** Create a global variable for a package *)
  val create_global: OpamPackage.Name.t -> variable -> t

  (** Return the package the variable is defined in *)
  val package: t -> OpamPackage.Name.t

  (** Return the section (library or syntax extension) the package is
      defined in *)
  val section: t -> Section.t option

  (** Return the variable name *)
  val variable: t -> variable

end

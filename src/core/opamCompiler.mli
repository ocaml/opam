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

(** Compiler names and versions *)

(** OCaml compiler versions *)
module Version: sig

  include OpamMisc.ABSTRACT

  (** Compiler constraint *)
  type constr = (OpamFormula.relop * t) OpamFormula.formula

  (** Return the version of the compiler currently installed *)
  val current: unit -> t option

  (** Returm the version of the system compiler *)
  val system: unit -> t option

  (** Compare OCaml versions *)
  val compare: t -> OpamFormula.relop -> t -> bool

end

(** Compiler names *)
include OpamMisc.ABSTRACT

(** Convert a filename into a compiler name *)
val of_filename: OpamFilename.t -> t

(** List the compiler available in a directory *)
val list: OpamFilename.Dir.t -> Set.t

(** Default compiler *)
val default: t


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

(** Resolution and handling of opam variables + filters *)

open OpamTypes
open OpamStateTypes

(** Lists of available switch-global variables and their description *)
val global_variable_names: (string * string) list

(** Lists of predefined package variables and their description *)
val package_variable_names: (string * string) list

(** Obsolete ocaml variables (for compat) and their description *)
val ocaml_variable_names: (string * string) list

(** Variables that are pre-defined in the dependency filtered-formula scope, and
    which resolution is delayed to after the universe is computed (these are the
    only ones allowed in the universe, and resolved by
    [OpamSolver.filter_deps] *)
val predefined_depends_variables: full_variable list

(** Resolves globally available variables only *)
val resolve_global: 'a global_state -> full_variable -> variable_contents option

(** Resolves global variables within the context of a switch *)
val resolve_switch: 'a switch_state -> full_variable -> variable_contents option

(** Resolves filter variables, including global, switch and package variables ;
    a map of locally defined variables can be supplied, as well as the opam file
    of origin, which is used to resolve self-references (implicit ["%{bin}%"] or
    explicit ["%{_:bin}%"] *)
val resolve:
  'a switch_state -> ?opam:OpamFile.OPAM.t ->
  ?local:OpamVariable.variable_contents option OpamVariable.Map.t ->
  OpamFilter.env

(** Like [resolve_switch], but takes more specific parameters so that it can be
    used before the switch state is fully loaded *)
val resolve_switch_raw:
  'a global_state -> switch -> OpamFile.Dot_config.t -> full_variable ->
  variable_contents option

val is_dev_package: 'a switch_state -> OpamFile.OPAM.t -> bool

(** The defaults are [true] for [build], false for [dev] and defined by
    OpamStateConfig for [test] and [bool]. *)
val filter_depends_formula:
  ?build:bool -> ?test:bool -> ?doc:bool -> ?dev:bool -> ?default:bool ->
  env:OpamFilter.env ->
  filtered_formula -> formula

(** Assumes [filter_default=false] by default, i.e. dependencies with undefined
    filters are discarded. *)
val all_depends:
  ?build:bool -> ?test:bool -> ?doc:bool -> ?dev:bool -> ?filter_default:bool ->
  ?depopts:bool ->
  'a switch_state -> OpamFile.OPAM.t -> formula

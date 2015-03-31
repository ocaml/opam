(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
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

open OpamTypes

type t = {
  cudf_file: string option;
  solver_timeout: float;
  external_solver: OpamTypes.arg list option Lazy.t;
  solver_preferences_default: string Lazy.t option;
  solver_preferences_upgrade: string Lazy.t option;
  solver_preferences_fixup: string Lazy.t option;
}

type 'a options_fun =
  ?cudf_file:string option ->
  ?solver_timeout:float ->
  ?external_solver:OpamTypes.arg list option Lazy.t ->
  ?solver_preferences_default:string Lazy.t option ->
  ?solver_preferences_upgrade:string Lazy.t option ->
  ?solver_preferences_fixup:string Lazy.t option ->
  unit -> 'a

let default =
  let external_solver = lazy (
    if OpamSystem.command_exists "aspcud" then Some [CIdent "aspcud", None] else
    if OpamSystem.command_exists "packup" then Some [CIdent "packup", None] else
      None
  ) in
  {
    cudf_file = None;
    solver_timeout = 5.;
    external_solver;
    solver_preferences_default = None;
    solver_preferences_upgrade = None;
    solver_preferences_fixup = None;
  }

let setk k t
    ?cudf_file
    ?solver_timeout
    ?external_solver
    ?solver_preferences_default
    ?solver_preferences_upgrade
    ?solver_preferences_fixup
    ()
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    cudf_file = t.cudf_file + cudf_file;
    solver_timeout = t.solver_timeout + solver_timeout;
    external_solver = t.external_solver + external_solver;
    solver_preferences_default =
      t.solver_preferences_default + solver_preferences_default;
    solver_preferences_upgrade =
      t.solver_preferences_upgrade + solver_preferences_upgrade;
    solver_preferences_fixup =
      t.solver_preferences_fixup + solver_preferences_fixup;
  }

let set = setk (fun x -> x)

let r = ref default

let update = setk (fun cfg -> r := cfg) !r

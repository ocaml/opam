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

(** OPAMNOSELFUPGRADE is set to this value when the current opam process has
    been called by an older opam process using the self-upgrade mechanism *)
val self_upgrade_bootstrapping_value: string

(** Sets the OpamClientConfig options, reading the environment to get default
    values when unspecified *)
val init_config: unit -> unit OpamClientConfig.options_fun

(* val default_repository_name: string *)
(* val default_repository_address: string *)

val ocaml_version: string option Lazy.t
val ocaml_opt_available: bool Lazy.t
val ocaml_native_available: bool Lazy.t
val ocaml_natdynlink_available: bool Lazy.t
val system_ocamlc_version: string option Lazy.t
val system_ocamlc_where: string option Lazy.t
val system_compiler: OpamCompiler.t option Lazy.t

val filter_deps: ext_formula -> formula

val search_files: string list

(* Loads the global configuration file, protecting against concurrent writes *)
val load_conf_file: dirname -> OpamFile.Config.t option

(* Writes the global configuration file, protecting against concurrent reads *)
val write_conf_file: dirname -> OpamFile.Config.t -> unit

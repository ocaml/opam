(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** This module defines a few defaults, used at 'opam init', that bind opam to
    its default OCaml repository at https://opam.ocaml.org. All can be overriden
    through the init command flags or an init config file. *)

open OpamTypes

(** Url of the default Opam repository *)
val repository_url: url

val default_compiler: formula

val eval_variables: (OpamVariable.t * string list * string) list

(** Default initial configuration file for use by [opam init] if nothing is
    supplied. *)
val init_config: OpamFile.InitConfig.t

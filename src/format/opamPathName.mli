(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** {1 Names of internal opam root elements} *)

(** {2 Directories} *)
val files_d: string
val repo_d: string
val build_d: string
val config_d: string
val hooks_d: string
val install_d: string
val log_d: string
val overlay_d: string
val packages_d: string
val plugins_d: string
val reinstall_d: string
val remove_d: string
val sources_d: string
val opam_d: string
val opamswitch_d: string

(** {2 Files} *)
val config_f: string
val opam_f: string
val lock_f: string
val environment_f: string

(** {2 Suffixes} *)
val changes_suffix: string
val install_suffix: string
val config_suffix: string
val opam_suffix: string
val subst_suffix: string
val subst_ext: string

(** {2 Internal config files *)
val switch_config: string
val switch_state: string
val repo_config: string

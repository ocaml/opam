(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let files_d = OpamRepositoryPathName.files_d
let repo_d = "repo"
let build_d = "build"
let config_d = "config"
let hooks_d = "hooks"
let install_d = "install"
let log_d = "log"
let overlay_d = "overlay"
let packages_d = "packages"
let plugins_d = "plugins"
let reinstall_d = "reinstall"
let remove_d = "remove"
let sources_d = "sources"
let opam_d = "opam"
let opamswitch_d = ".opam-switch"

let config_f = "config"
let opam_f = OpamRepositoryPathName.opam_f
let lock_f = "lock"
let environment_f = "environment"

let changes_suffix = ".changes"
let install_suffix = ".install"
let config_suffix = ".config"
let opam_suffix = ".opam"
let subst_ext = "in"
let subst_suffix = "." ^ subst_ext

let switch_config = "switch-config"
let switch_state = "switch-state"
let repo_config = "repo-config"

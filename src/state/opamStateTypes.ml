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

open OpamTypes

type lock_kind = Lock_none | Lock_readonly | Lock_readwrite

type global_state = {
  global_lock: lock_kind;
  root: OpamPath.t;
  config: OpamFile.Config.t;
  aliases: compiler switch_map;
}

type repos_state = {
  repos_lock: lock_kind;
  repos_global: global_state;
  repositories: OpamFile.Repo_config.t repository_name_map;
  compilers: compiler_set;
  package_index: (repository_name * string option) package_map;
  compiler_index: (repository_name * string option) compiler_map;
  repo_opams: OpamFile.OPAM.t package_map;
}

type switch_state = {
  switch: switch;
  compiler_packages: package_set;
  switch_config: OpamFile.Dot_config.t;
  opams: OpamFile.OPAM.t package_map;
  packages: package_set;
  available_packages: package_set Lazy.t;
  pinned: (OpamPackage.Version.t * pin_option) name_map;
  installed: package_set;
  installed_roots: package_set;
  reinstall: package_set;
}

type state = {
  switch_lock: lock_kind;
  switch_global: global_state;
  switch_repos: repos_state;
  current_switch: switch;
  switchmap: switch_state OpamSwitch.Map.t;
}

let get_switch st switch =
  try OpamSwitch.Map.find switch st.switchmap
  with Not_found ->
    (OpamConsole.error
   "%s is not a valid switch"
   (OpamSwitch.to_string switch));
   raise Not_found


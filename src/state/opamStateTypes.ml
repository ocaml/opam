(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

type rw = [ `Lock_write ]

type ro = [ `Lock_read | rw ]

type unlocked = [ `Lock_none | ro ]

type +'a lock = [< unlocked > `Lock_write ] as 'a

type gt_variables =
  (variable_contents option Lazy.t * string) OpamVariable.Map.t

type gt_changes = { gtc_repo: bool; gtc_switch: bool }

type +'lock global_state = {
  global_lock: OpamSystem.lock;
  lock: OpamSystem.lock;
  root: OpamPath.t;
  config: OpamFile.Config.t;
  global_variables: gt_variables;
  global_state_to_upgrade: gt_changes;
} constraint 'lock = 'lock lock


type os_dummy_test_setup = {
  osd_install: bool;
  osd_installed: [ `all | `none | `set of OpamSysPkg.Set.t];
  osd_available: [ `all | `none | `set of OpamSysPkg.Set.t];
}

type os_family =
  | Alpine
  | Altlinux
  | Arch
  | Centos
  | Cygwin
  | Debian
  | Dummy of os_dummy_test_setup
  | Freebsd
  | Gentoo
  | Homebrew
  | Macports
  | Msys2
  | Netbsd
  | Nix
  | Openbsd
  | Suse

type repo_syspkgs_available = (os_family * OpamSysPkg.availability_mode) option

type +'lock repos_state = {
  repos_lock: OpamSystem.lock;
  repos_global: unlocked global_state;
  repositories: repository repository_name_map;
  repos_definitions: OpamFile.Repo.t repository_name_map;
  repo_opams: OpamFile.OPAM.t package_map repository_name_map;
  repos_syspkgs_available : repo_syspkgs_available;

} constraint 'lock = 'lock lock

type +'lock switch_state = {
  switch_lock: OpamSystem.lock;
  switch_global: unlocked global_state;
  switch_repos: unlocked repos_state;
  switch: switch;
  switch_invariant: formula;
  compiler_packages: package_set;
  switch_config: OpamFile.Switch_config.t;
  repos_package_index: OpamFile.OPAM.t package_map;
  opams: OpamFile.OPAM.t package_map;
  conf_files: OpamFile.Dot_config.t name_map;
  packages: package_set;
  sys_packages: sys_pkg_status package_map Lazy.t;
  available_packages: package_set Lazy.t;
  pinned: package_set;
  installed: package_set;
  installed_opams: OpamFile.OPAM.t package_map;
  installed_roots: package_set;
  reinstall: package_set Lazy.t;
  invalidated: package_set Lazy.t;
  overwrote_opams: (bool * OpamFile.OPAM.t) package_map;
} constraint 'lock = 'lock lock

type provenance = [ `Env | `Command_line | `Default ]

type 'url _topin_opamfile = {
  pin_file: OpamFile.OPAM.t OpamFile.t;
  pin_locked: string option;
  pin_subpath: subpath option;
  pin_url: 'url;
}
type ('name, 'url) _topin_name_and_opamfile = {
  pin_name: 'name;
  pin: 'url _topin_opamfile;
}

type name_and_file = (name, unit) _topin_name_and_opamfile
type name_and_file_w_url = (name, url) _topin_name_and_opamfile
type nameopt_and_file = (name option, unit) _topin_name_and_opamfile
type nameopt_and_file_w_url = (name option, url) _topin_name_and_opamfile

type pinned_opam = {
  pinned_name : name;
  pinned_version : version option;
  pinned_opam : OpamFile.OPAM.t option;
  pinned_subpath: subpath option;
  pinned_url: url;
}

module Abs = struct
  let create_switch_state ~switch_global ~switch_repos ~switch_lock
      ~switch ~switch_invariant ~compiler_packages ~switch_config
      ~repos_package_index ~installed_opams
      ~installed ~pinned ~installed_roots
      ~opams ~conf_files
      ~packages ~available_packages ~sys_packages ~reinstall ~invalidated
      ~overwrote_opams =
    {
      switch_global; switch_repos; switch_lock;
      switch; switch_invariant; compiler_packages; switch_config;
      repos_package_index; installed_opams;
      installed; pinned; installed_roots;
      opams; conf_files;
      packages; available_packages; sys_packages; reinstall; invalidated;
      overwrote_opams;
    }

  let with_switch_lock st switch_lock = { st with switch_lock }

  let add_package ~resolve_switch_raw st nv opam =
    { st with
      opams = OpamPackage.Map.add nv opam st.opams;
      packages = OpamPackage.Set.add nv st.packages;
      available_packages = lazy (
        if OpamFilter.eval_to_bool ~default:false
            (resolve_switch_raw ?package:(Some nv)
               st.switch_global st.switch st.switch_config)
            (OpamFile.OPAM.available opam)
        then OpamPackage.Set.add nv (Lazy.force st.available_packages)
        else OpamPackage.Set.remove nv (Lazy.force st.available_packages)
      );
      reinstall = lazy (
        match OpamPackage.Map.find_opt nv st.installed_opams with
        | Some inst ->
          if OpamFile.OPAM.effectively_equal inst opam
          then OpamPackage.Set.remove nv (Lazy.force st.reinstall)
          else OpamPackage.Set.add nv (Lazy.force st.reinstall)
        | _ -> Lazy.force st.reinstall
      );
    }

  let remove_package st nv =
    { st with
      opams = OpamPackage.Map.remove nv st.opams;
      packages = OpamPackage.Set.remove nv st.packages;
      available_packages =
        lazy (OpamPackage.Set.remove nv (Lazy.force st.available_packages));
    }

  let add_pinned ~resolve_switch_raw st nv opam =
    let pinned =
      OpamPackage.Set.add nv (OpamPackage.filter_name_out st.pinned nv.name)
    in
    let available_packages = lazy (
      OpamPackage.filter_name_out (Lazy.force st.available_packages) nv.name
    ) in
    add_package ~resolve_switch_raw { st with pinned; available_packages } nv opam
end

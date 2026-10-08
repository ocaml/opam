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

include OpamStateTypesCommon

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
    (* TODO: this looks wrong
       e.g. remove one package from a repo shadowing the same one from a different repo.
       We should use st.repos_package_index *)
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

  let remove_pinned st nv =
    (* TODO: this should be homologous to add_pinned but it's clearly not *)
    { st with pinned = OpamPackage.Set.remove nv st.pinned }

  let update_gt st gt =
    let rt = { st.switch_repos with repos_global = gt } in
    { st with switch_global = gt; switch_repos = rt }

  let update_reinstall st reinstall =
    (* TODO: this looks wrong *)
    { st with reinstall }

  let update_installed ~compute_invariant_packages ?installed ?installed_roots ?reinstall ?pinned st =
    (* TODO: this looks wrong *)
    let open OpamStd.Option.Op in
    let open OpamPackage.Set.Op in
    let installed = installed +! st.installed in
    let reinstall = lazy (
      (reinstall +! Lazy.force st.reinstall) %% installed
    ) in
    let st =
      { st with
        installed;
        installed_roots = installed_roots +! st.installed_roots;
        reinstall;
        pinned = pinned +! st.pinned;
      }
    in
    let compiler_packages = compute_invariant_packages st in
    { st with compiler_packages }

  let update_installed_only st installed =
    (* TODO: this looks wrong *)
    { st with installed }

  let update_reinstall_only st reinstall =
    (* TODO: this looks wrong *)
    { st with reinstall }

  let update_installed_roots st installed_roots =
    (* TODO: this looks wrong *)
    { st with installed_roots }

  let update_installed_plus_conf_files st installed =
    (* TODO: this looks better than update_installed_only,
       why isn't this the default? *)
    let conf_files =
      OpamPackage.Name.Map.filter (fun name _ ->
          OpamPackage.Set.exists (fun pkg ->
              OpamPackage.Name.equal name pkg.name)
            installed)
        st.conf_files
    in
    { st with installed; conf_files }

  let update_conf_files st conf_files =
    (* TODO: this looks wrong *)
    { st with conf_files }

  let update_config switch_config st =
    { st with switch_config }

  let update_invariant_and_config st switch_config =
    (* TODO: should this simply be in update_config by default? *)
    let switch_invariant =
      match switch_config.OpamFile.Switch_config.invariant with
      | None -> assert false (* TODO *)
      | Some invariant -> invariant
    in
    { st with switch_invariant; switch_config }

  let update_invariant st switch_invariant =
    let switch_config =
      { st.switch_config with invariant = Some switch_invariant }
    in
    { st with switch_invariant; switch_config }

  let update_available st available_packages =
    (* TODO: this looks wrong *)
    { st with available_packages }

  let update_overwrote st overwrote_opams =
    (* TODO: this looks wrong *)
    { st with overwrote_opams }

  let update_compilers st compiler_packages =
    (* TODO: this looks wrong *)
    { st with compiler_packages }

  let update_sys_pkgs st sys_packages =
    (* TODO: this looks wrong *)
    { st with sys_packages }

  let update_name_and_available st switch available_packages =
    (* TODO: this looks wrong *)
    { st with switch; available_packages }

  let import st ~available_packages ~packages ~compiler_packages ~pinned ~opams =
    (* TODO: this looks wrong *)
    { st with
      available_packages;
      packages;
      compiler_packages;
      pinned;
      opams }

  let pin_overwrotes st =
    let opams, pinned =
      OpamPackage.Map.fold (fun nv (was_pinned, opam) (opams, pinned) ->
          OpamPackage.Map.add nv opam opams,
          if was_pinned then pinned else OpamPackage.Set.remove nv pinned)
        st.overwrote_opams (st.opams, st.pinned)
    in
    { st with opams; pinned; overwrote_opams = OpamPackage.Map.empty }

  let empty_installed st =
    (* TODO: what is the purpose on this??! *)
    { st with
      installed = OpamPackage.Set.empty;
      installed_roots = OpamPackage.Set.empty;
      reinstall = Lazy.from_val OpamPackage.Set.empty }
end

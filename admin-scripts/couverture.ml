#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** This script gives scenarios to install all named packages in a given set.
    This may require several steps, in case of conflicts.

    Consistent installation steps are printed one per line to stdout. Stderr
    gives more detail.

    Relies on the current opam root for the list of available packages, i.e.
    depends on configured remotes, OS and OCaml version, but not on the set of
    currently installed packages.
*)

open OpamTypes

let max_install t inst_packages =
  let universe = OpamState.universe t Query in
  let wish_field = "wished" in
  let base = OpamState.base_packages t in
  let universe =
    { universe with u_installed = base;
                    u_installed_roots = base;
                    u_attrs = [wish_field, inst_packages]; }
  in
  if not (OpamCudf.external_solver_available ()) then
    failwith "No external solver found";
  let preferences =
    let preferences = OpamSolverConfig.criteria `Default in
    Some (lazy (Printf.sprintf "+sum(solution,%s),%s" wish_field preferences))
  in
  OpamSolverConfig.update ~solver_preferences_default:preferences ();
  let version_map =
    OpamSolver.cudf_versions_map universe universe.u_available
  in
  let request = {
    wish_install = [];
    wish_remove = [];
    wish_upgrade = [];
    extra_attributes = [wish_field];
    criteria = `Default;
  } in
  let cudf_universe =
    OpamSolver.load_cudf_universe ~build:true universe ~version_map
      universe.u_available
  in
  match OpamCudf.resolve ~extern:true ~version_map cudf_universe request with
  | Conflicts _ -> failwith "Solver error (unexpected conflicts)"
  | Success u ->
    OpamPackage.Set.diff
      (OpamPackage.Set.of_list
         (List.map OpamCudf.cudf2opam (OpamCudf.packages u)))
      base

module P = OpamPackage
open P.Set.Op

let rec couverture acc t pkgs =
  Printf.eprintf "# %d packages remaining...\n%!"
    (P.Name.Set.cardinal (P.names_of_packages pkgs));
  let step = max_install t pkgs in
  let added =
    P.Name.Set.inter (P.names_of_packages step) (P.names_of_packages pkgs)
  in
  if P.Name.Set.is_empty added then
    let () =
      Printf.eprintf "# -> %d uninstallable packages remaining.\n%!"
        (P.Name.Set.cardinal (P.names_of_packages pkgs))
    in
    List.rev acc, pkgs
  else
  let n = P.Name.Set.cardinal added in
  Printf.eprintf "# -> Step %d: covering %d/%d packages%s.\n%!"
    (List.length acc + 1) n (P.Name.Set.cardinal (P.names_of_packages pkgs))
    (if n > 5 then "" else
       OpamStd.List.concat_map ~left:" (" ~right:")" " " P.Name.to_string
         (OpamPackage.Name.Set.elements added));
  let pkgs =
    P.Set.filter
      (fun nv -> not (P.has_name step (P.name nv))) pkgs
  in
  couverture (step::acc) t pkgs

let () =
  let root = OpamStateConfig.opamroot () in
  OpamFormatConfig.init ();
  if not (OpamStateConfig.load_defaults root) then
    failwith "Opam root not found";
  OpamStd.Config.init ();
  OpamSolverConfig.init ();
  OpamStateConfig.init ();
  let t =
    OpamState.load_state ~save_cache:false "couverture"
      OpamStateConfig.(!r.current_switch)
  in
  let avail = Lazy.force t.OpamState.Types.available_packages in
  let wanted = match Array.to_list Sys.argv with
    | [] | _::[] ->
      avail -- P.packages_of_names avail (OpamState.base_package_names t)
    | _::l ->
      List.fold_left (fun wanted name ->
          let nvs =
            if String.contains name '.' then
              P.Set.singleton (P.of_string name)
            else
              P.packages_of_name avail
                (P.Name.of_string name)
          in
          if P.Set.is_empty (nvs %% avail) then
            failwith (Printf.sprintf "Package %s not found" name)
          else
            wanted ++ nvs
        ) P.Set.empty l
  in
  let couv,remaining = couverture [] t wanted in
  let avail_names = P.names_of_packages avail in
  let remaining_names = P.names_of_packages remaining in
  Printf.eprintf "# Found a couverture for %d over %d packages in %d steps:\n%!"
    (P.Name.Set.cardinal (P.Name.Set.diff avail_names remaining_names))
    (P.Name.Set.cardinal avail_names)
    (List.length couv);
  List.iter (fun s ->
      print_endline
        (OpamStd.List.concat_map " " OpamPackage.to_string
           (P.Set.elements s)))
    couv;
  Printf.eprintf "# %d uninstallable packages remain: %s\n%!"
    (P.Name.Set.cardinal remaining_names)
    (OpamStd.List.concat_map " " OpamPackage.Name.to_string
       (P.Name.Set.elements remaining_names))

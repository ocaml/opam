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
open OpamStateTypes
open OpamFilename.Op

let log fmt = OpamConsole.log "PINNED" fmt
let slog = OpamConsole.slog

(* Copies package definition from the repository to the overlay *)
let add_overlay ?(template=false) ?version st name pin =
  let open OpamFile in
  let module Ov = OpamPath.Switch.Overlay in
  log "Add pinning overlay for %a (template:%b, version:%a)"
    (slog OpamPackage.Name.to_string) name template
    OpamStd.Option.Op.(
      slog @@ fun v -> (v >>| OpamPackage.Version.to_string) +! "none"
    ) version;
  let root = st.switch_global.root in
  let switch = st.switch in
  let pkg_overlay f = f root switch name in
  let copy_files opam =
    match OpamFile.OPAM.metadata_dir opam with
    | Some bd ->
      List.iter
        (fun f -> OpamFilename.copy_in ~root:bd f (pkg_overlay Ov.package))
        (OpamFilename.rec_files (bd / "files"))
    | None -> ()
  in
  match pin with
  | Version v ->
    let rv = OpamPackage.create name v in
    let opam = OpamPackage.Map.find rv st.repos_package_index in
    copy_files opam;
    OPAM.write (pkg_overlay Ov.opam) (OPAM.with_version opam v)
  | Source url ->
    let nv = OpamStd.Option.map (OpamPackage.create name) version in
    let url_f = OpamFile.URL.create url in
    try
      let rv = (* repo version *)
        match nv with
        (* Lookup in repositories to ignore pinned versions *)
        | Some nv when OpamPackage.Map.mem nv st.repos_package_index -> nv
        | _ ->
          let versions =
            OpamPackage.Map.fold (fun nv _ acc ->
                if nv.name = name then
                  OpamPackage.Set.add nv acc
                else acc)
              st.repos_package_index OpamPackage.Set.empty
          in
          OpamPackage.max_version versions name (* raises Not_found *)
      in
      let v = OpamStd.Option.default (OpamPackage.version rv) version in
      let opam = OpamPackage.Map.find rv st.repos_package_index in
      copy_files opam;
      OPAM.write (pkg_overlay Ov.opam)
        (OPAM.with_version (OPAM.with_url opam url_f) v)
    with Not_found -> (* No original meta *)
      let version =
        OpamStd.Option.default
          (OpamPackage.Version.of_string
             (if template then "0.1" else "~unknown"))
          version
      in
      let nv = OpamPackage.create name version in
      let opam = if template then OPAM.template nv else OPAM.create nv in
      let opam = OpamFile.OPAM.with_url opam url_f in
      OPAM.write (pkg_overlay (if template then Ov.tmp_opam else Ov.opam)) opam

let remove_overlay gt switch name =
  OpamFilename.rmdir
    (OpamPath.Switch.Overlay.package gt.root switch name)

let version st name = fst (OpamPackage.Name.Map.find name st.pinned)

let package st name = OpamPackage.create name (version st name)

let package_opt st name = try Some (package st name) with Not_found -> None

let packages st =
  OpamPackage.Name.Map.fold (fun name (v,_) acc ->
      OpamPackage.Set.add (OpamPackage.create name v) acc)
    st.pinned OpamPackage.Set.empty

let find_opam_file_in_source name dir =
  OpamStd.Option.map OpamFile.make
    (OpamStd.List.find_opt OpamFilename.exists [
        dir / (OpamPackage.Name.to_string name ^ ".opam") // "opam";
        dir // (OpamPackage.Name.to_string name ^ ".opam");
        dir / "opam" // "opam";
        dir // "opam"
      ])

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

(* Utility helper to check if a given set of packages is installed *)

let usage = "opam-check [--root root] [-l label] <package>+"

let label = ref ""
let spec = Arg.align [
    ("--root", Arg.Set_string OpamGlobals.root_dir, " Set opam path");
    ("-l"    , Arg.Set_string label            , " Set a test label");
    ("--version", Arg.Unit OpamVersion.message , " Display version information");
  ]

let packages = ref []
let ano x = packages := x :: !packages

let () = Arg.parse spec ano usage

let packages = OpamPackage.Set.of_list (List.map OpamPackage.of_string !packages)

let installed () =
  let root = OpamPath.root () in
  let config = OpamFile.Config.read (OpamPath.config root) in
  let version = OpamFile.Config.switch config in
  let installed =
    OpamFile.Installed.safe_read (OpamPath.Switch.installed root version) in
  OpamPackage.Set.filter (fun nv ->
      OpamPackage.name nv <> OpamPackage.Name.global_config
    ) installed

let () =
  let installed = installed () in
  let diff1 = OpamPackage.Set.diff packages installed in
  let diff2 = OpamPackage.Set.diff installed packages in
  let diff = OpamPackage.Set.union diff1 diff2 in
  let label = if !label = "" then "" else Printf.sprintf "[%s] " !label in
  if not (OpamPackage.Set.is_empty diff) then (
    OpamGlobals.error "%swaiting for: %s" label (OpamPackage.Set.to_string diff1);
    OpamGlobals.error "%sgot:         %s" label (OpamPackage.Set.to_string diff2);
    exit 1
  )

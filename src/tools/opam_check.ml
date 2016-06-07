(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Utility helper to check if a given set of packages is installed *)

let usage = "opam-check [--root root] [-l label] <package>+"

let label = ref ""
let root_dir_ref = ref ""
let spec = Arg.align [
    ("--root", Arg.Set_string root_dir_ref, " Set opam path");
    ("-l"    , Arg.Set_string label            , " Set a test label");
    ("--version", Arg.Unit OpamVersion.message , " Display version information");
  ]

let packages = ref []
let ano x = packages := x :: !packages

let () =
  Arg.parse spec ano usage;
  let root_dir = match !root_dir_ref with
    | "" -> None
    | d -> Some (OpamFilename.Dir.of_string d)
  in
  OpamSystem.init();
  OpamStd.Config.init();
  OpamFormatConfig.init();
  OpamRepositoryConfig.init();
  OpamSolverConfig.init();
  OpamStateConfig.init
    ?root_dir
    ()


let packages = OpamPackage.Set.of_list (List.map OpamPackage.of_string !packages)

let installed () =
  let root = OpamStateConfig.(!r.root_dir) in
  let config = OpamFile.Config.read (OpamPath.config root) in
  let version = match OpamFile.Config.switch config with
    | Some sw -> sw
    | None -> failwith "No switch set" in
  let state = OpamFile.SwitchSelections.safe_read (OpamPath.Switch.selections root version) in
  state.OpamTypes.sel_installed

let () =
  let installed = installed () in
  let diff1 = OpamPackage.Set.diff packages installed in
  let diff2 = OpamPackage.Set.diff installed packages in
  let diff = OpamPackage.Set.union diff1 diff2 in
  let label = if !label = "" then "" else Printf.sprintf "[%s] " !label in
  if not (OpamPackage.Set.is_empty diff) then (
    OpamConsole.error "%swaiting for: %s" label (OpamPackage.Set.to_string diff1);
    OpamConsole.error "%sgot:         %s" label (OpamPackage.Set.to_string diff2);
    exit 1
  )

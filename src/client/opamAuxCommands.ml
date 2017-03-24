(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStateTypes

let package_file_changes st packages =
  OpamPackage.Set.fold (fun nv acc ->
      let f =
        OpamPath.Switch.changes st.switch_global.root st.switch nv.name
      in
      match OpamFile.Changes.read_opt f with
      | None -> acc
      | Some ch -> OpamStd.String.Map.union (fun _ x -> x) acc ch)
    packages
    OpamStd.String.Map.empty

let copy_files_to_destdir st pfx packages =
  let switch_pfx = OpamPath.Switch.root st.switch_global.root st.switch in
  package_file_changes st packages |>
  OpamDirTrack.check switch_pfx |>
  List.iter @@ function
  | src, `Unchanged ->
    let relf = OpamFilename.remove_prefix switch_pfx src in
    let dst = OpamFilename.Op.(pfx // relf) in
    OpamConsole.msg "%-40s %s %s\n"
      relf (OpamConsole.colorise `blue "=>")
      (OpamFilename.to_string dst);
    if not OpamStateConfig.(!r.dryrun) then
      if OpamFilename.exists src then OpamFilename.copy ~src ~dst else
      let as_dir f = OpamFilename.(Dir.of_string (to_string f)) in
      if OpamFilename.exists_dir (as_dir src)
      then OpamFilename.mkdir (as_dir dst)
  | src, (`Changed | `Removed as ch) ->
    OpamConsole.warning "Not installing %s, which was %s since"
      (OpamConsole.colorise `bold (OpamFilename.to_string src))
      (match ch with `Changed -> "changed" | `Removed -> "removed")

let remove_files_from_destdir st pfx packages =
  package_file_changes st packages |>
  OpamStd.String.Map.bindings |>
  List.rev |> (* Get the longer names first, their parent folders last *)
  List.iter @@ fun (rel_file, change) ->
  match change with
  | OpamDirTrack.Added _
  | OpamDirTrack.Contents_changed _
  | OpamDirTrack.Kind_changed _ ->
    let f = OpamFilename.Op.(pfx // rel_file) in
    let d = OpamFilename.Op.(pfx / rel_file) in
    if OpamFilename.exists f then
      (OpamConsole.msg "Removing %s\n"
         (OpamConsole.colorise `bold (OpamFilename.to_string f));
       if not OpamStateConfig.(!r.dryrun) then OpamFilename.remove f)
    else if OpamFilename.exists_dir d then
      if OpamFilename.dir_is_empty d then
        (OpamConsole.msg "Removing %s\n"
           (OpamConsole.colorise `bold (OpamFilename.Dir.to_string d));
         if not OpamStateConfig.(!r.dryrun) then OpamFilename.rmdir d)
      else
        OpamConsole.note "Not removing non-empty directory %s"
          (OpamConsole.colorise `bold (OpamFilename.Dir.to_string d))
  | _ -> ()

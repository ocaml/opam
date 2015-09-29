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
open OpamTypesBase
open OpamFilename.Op
open OpamProcess.Job.Op

module Hg = struct

  let name = `hg

  let exists repo =
    OpamFilename.exists_dir (repo.repo_root / ".hg")

  let hg repo =
    let dir = OpamFilename.Dir.to_string repo.repo_root in
    fun ?verbose ?env args ->
      OpamSystem.make_command ~dir ?verbose ?env "hg" args

  let init repo =
    hg repo [ "init" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    OpamFilename.write
      OpamFilename.Op.(repo.repo_root / ".hg" // "hgrc")
      (Printf.sprintf "[paths]\ndefault = %s\n"
         (path_of_address repo.repo_address));
    Done ()

  let fetch repo =
    let check_and_fix_remote =
      hg repo [ "showconfig" ; "paths.default" ]
      @@> fun r ->
      OpamSystem.raise_on_process_error r;
      if r.OpamProcess.r_stdout <> [path_of_address repo.repo_address] then (
        OpamFilename.rmdir OpamFilename.Op.(repo.repo_root / ".hg");
        init repo
      ) else Done ()
    in
    check_and_fix_remote @@+ fun () ->
    hg repo [ "pull" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let revision repo =
    hg repo [ "id"; "-i" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | [] -> Done "<none>"
    | full::_ ->
      if String.length full > 8 then Done (String.sub full 0 8)
      else Done full

  let unknown_commit commit =
    OpamSystem.internal_error "Unknown mercurial revision/branch/bookmark: %s."
      commit

  let reset repo =
    let commit = match snd repo.repo_address with
      | None   -> "tip"
      | Some c -> c
    in
    hg repo [ "update" ; "--clean"; commit ] @@> fun r ->
      if OpamProcess.is_failure r then unknown_commit commit;
      Done ()

  let diff repo =
    let commit = match snd repo.repo_address with
      | None   -> "tip"
      | Some c -> c in
    hg repo [ "diff" ; "--stat" ; "-r" ; commit ] @@> fun r ->
    if OpamProcess.is_failure r then unknown_commit commit;
    Done (r.OpamProcess.r_stdout <> [])

  let versionned_files repo =
    hg repo [ "locate" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done r.OpamProcess.r_stdout

  let vc_dir repo = OpamFilename.Op.(repo.repo_root / ".hg")

end

module B = OpamVCS.Make(Hg)

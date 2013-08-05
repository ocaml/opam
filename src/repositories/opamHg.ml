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

open OpamTypes
open OpamFilename.OP

let log fmt = OpamGlobals.log "HG" fmt

module Hg = struct

  let exists repo =
    OpamFilename.exists_dir (repo.repo_root / ".hg")

  let init repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
        OpamSystem.command [ "hg" ; "init" ];
        OpamFilename.write
          (OpamFilename.of_string ".hg/hgrc")
          (Printf.sprintf "[paths]\ndefault = %s\n" (fst repo.repo_address));
      )

  let fetch repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
      OpamSystem.command [ "hg" ; "pull" ]
    )

  let revision repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
        match OpamSystem.read_command_output [ "hg" ; "id" ; "-i" ] with
        | []      -> "<none>"
        | full::_ ->
          if String.length full > 8 then
            String.sub full 0 8
          else
            full
      )

  let unknown_commit commit =
    OpamSystem.internal_error "Unknown mercurial revision/branch/bookmark: %s."
      commit

  let reset repo =
    let merge commit =
      try OpamSystem.command [ "hg" ; "update" ; "--clean"; commit ]; true
      with _ -> false in
    let commit = match snd repo.repo_address with
      | None   -> "tip"
      | Some c -> c in
    OpamFilename.in_dir repo.repo_root (fun () ->
      if not (merge commit) then
        unknown_commit commit
    )

  let diff repo =
    let diff commit =
      try Some (
        OpamSystem.read_command_output
          ["hg" ; "diff" ; "--stat" ; "-r" ; commit ])
      with _ -> None in
    let commit = match snd repo.repo_address with
      | None   -> "tip"
      | Some c -> c in
    OpamFilename.in_dir repo.repo_root (fun () ->
      match diff commit with
      | Some [] -> false
      | Some _  -> true
      | None    -> unknown_commit commit
    )

end

module B = OpamVCS.Make(Hg)

let register () =
  OpamRepository.register_backend `hg (module B: OpamRepository.BACKEND)

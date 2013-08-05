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

let log fmt = OpamGlobals.log "GIT" fmt

module Git = struct

  let exists repo =
    OpamFilename.exists_dir (repo.repo_root / ".git")

  let init repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
      OpamSystem.commands [
        [ "git" ; "init" ] ;
        [ "git" ; "remote" ; "add" ; "origin" ; fst repo.repo_address ] ;
      ]
    )

  let fetch repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
        OpamSystem.command [ "git" ; "fetch" ; "origin" ]
      )

  let revision repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
        match OpamSystem.read_command_output [ "git" ; "rev-parse" ; "HEAD" ] with
        | []      -> "<none>"
        | full::_ ->
          if String.length full > 8 then
            String.sub full 0 8
          else
            full
      )

  let reset repo =
    let merge commit =
      try OpamSystem.command [ "git" ; "reset" ; "--hard"; commit ]; true
      with _ -> false in
    let commit = match snd repo.repo_address with
      | None   -> "origin/master"
      | Some c -> c in
    OpamFilename.in_dir repo.repo_root (fun () ->
      if not (merge commit) then
        if not (merge ("origin/"^commit)) then
          OpamSystem.internal_error "Unknown revision: %s." commit
    )

  let diff repo =
    let diff commit =
      try Some (
        OpamSystem.read_command_output ["git" ; "diff" ; commit ; "--name-only"])
      with _ -> None in
    let commit = match snd repo.repo_address with
      | None   -> "origin/master"
      | Some c -> c in
    OpamFilename.in_dir repo.repo_root (fun () ->
      match diff commit with
      | Some [] -> false
      | Some _  -> true
      | None       ->
        match diff ("origin/"^commit) with
        | Some [] -> false
        | Some _  -> true
        | None    -> OpamSystem.internal_error "Unknown revision: %s." commit
    )

end

module B = OpamVCS.Make(Git)

let register () =
  OpamRepository.register_backend `git (module B: OpamRepository.BACKEND)

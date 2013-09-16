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

  let get_commits repo =
    match snd repo.repo_address with
    | None -> [ "refs/remotes/origin/master" ]
    | Some c -> [ "refs/remotes/origin/"^c;
                  "refs/tags/"^c;
                  c ]

  let reset repo =
    let merge commit =
      try OpamSystem.command [ "git" ; "reset" ; "--hard"; commit; "--" ]; true
      with _ -> false in
    OpamFilename.in_dir repo.repo_root (fun () ->
        let ok =
          List.fold_left (fun ok commit -> if ok then ok else merge commit)
            false (get_commits repo) in
        if not ok then OpamSystem.internal_error "Unknown revision: %s."
            (match snd repo.repo_address with Some a -> a | None -> "master")
      )

  let diff repo =
    let diff commit =
      try Some (
        OpamSystem.read_command_output ["git" ; "diff" ; "--name-only" ; commit ; "--" ])
      with _ -> None in
    OpamFilename.in_dir repo.repo_root (fun () ->
        let diff =
          List.fold_left (fun r commit -> match r with
              | None -> diff commit
              | Some x -> Some x)
            None (get_commits repo) in
        match diff with
        | Some [] -> false
        | Some _  -> true
        | None    ->
          OpamSystem.internal_error "Unknown revision: %s."
            (match snd repo.repo_address with Some a -> a | None -> "master")
    )

end

module B = OpamVCS.Make(Git)

let register () =
  OpamRepository.register_backend `git (module B: OpamRepository.BACKEND)

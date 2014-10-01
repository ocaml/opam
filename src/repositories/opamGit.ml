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
    let env =
      Array.append (Unix.environment ()) [|
        "GIT_AUTHOR_NAME=Opam";
        "GIT_AUTHOR_EMAIL=opam@ocaml.org";
        "GIT_COMMITTER_NAME=Opam";
        "GIT_COMMITTER_EMAIL=opam@ocaml.org"
      |] in
    OpamFilename.in_dir repo.repo_root (fun () ->
      OpamSystem.commands ~env [
        [ "git" ; "init" ] ;
        [ "git" ; "remote" ; "add" ; "origin" ; fst repo.repo_address ] ;
        [ "git" ; "commit" ; "--allow-empty" ; "-m" ; "opam-git-init" ] ;
      ]
    )

  let remote_ref = "refs/remotes/opam-ref"

  let fetch repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
        let current_remote =
          try
            match OpamSystem.read_command_output ~verbose:false
                    [ "git" ; "config" ; "--get"; "remote.origin.url" ]
            with
            | [url] -> Some url
            | _ -> None
          with e -> OpamMisc.fatal e; None
        in
        if current_remote <> Some (fst repo.repo_address) then (
          log "Git remote for %s needs updating (was: %s)"
            (OpamRepository.to_string repo)
            (OpamMisc.Option.default "<none>" current_remote);
          OpamSystem.commands
            [ [ "git" ; "remote" ; "rm" ; "origin" ];
              [ "git" ; "remote" ; "add" ; "origin"; fst repo.repo_address ] ]
        );
        let branch = OpamMisc.Option.default "HEAD" (snd repo.repo_address) in
        let refspec = Printf.sprintf "+%s:%s" branch remote_ref in
        OpamSystem.command [ "git" ; "fetch" ; "origin"; refspec ]
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
    OpamFilename.in_dir repo.repo_root (fun () ->
        try OpamSystem.command [ "git" ; "reset" ; "--hard"; remote_ref; "--" ]
        with e ->
          OpamMisc.fatal e;
          OpamSystem.internal_error "Git error: %s not found." remote_ref
      )

  let diff repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
        try
          OpamSystem.read_command_output
            ["git" ; "diff" ; "--name-only" ; "HEAD"; remote_ref; "--" ]
          <> []
        with e ->
          OpamMisc.fatal e;
          OpamSystem.internal_error "Git error: %s not found." remote_ref
      )

end

module B = OpamVCS.Make(Git)

let register () =
  OpamRepository.register_backend `git (module B: OpamRepository.BACKEND)

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

let log fmt = OpamConsole.log "GIT" fmt

module Git : OpamVCS.VCS= struct

  let name = `git

  let exists repo =
    OpamFilename.exists_dir (repo.repo_root / ".git")

  let git repo =
    let dir = OpamFilename.Dir.to_string repo.repo_root in
    fun ?verbose ?env args ->
      OpamSystem.make_command ~dir ?verbose ?env "git" args

  let init repo =
    let env =
      Array.append (Unix.environment ()) [|
        "GIT_AUTHOR_NAME=Opam";
        "GIT_AUTHOR_EMAIL=opam@ocaml.org";
        "GIT_COMMITTER_NAME=Opam";
        "GIT_COMMITTER_EMAIL=opam@ocaml.org"
      |] in
    OpamProcess.Job.of_list [
      git repo ~env [ "init" ];
      git repo ~env [ "remote" ; "add" ; "origin" ;
                      path_of_address repo.repo_address ];
      git repo ~env [ "commit" ; "--allow-empty" ; "-m" ; "opam-git-init" ];
    ] @@+ function
    | None -> Done ()
    | Some (_,err) -> OpamSystem.process_error err

  let remote_ref = "refs/remotes/opam-ref"

  let fetch repo =
    let check_and_fix_remote =
      git repo ~verbose:false [ "config" ; "--get"; "remote.origin.url" ]
      @@> fun r ->
      OpamSystem.raise_on_process_error r;
      let current_remote = match r.OpamProcess.r_stdout with
        | [url] -> Some url
        | _ -> None
      in
      if current_remote <> Some (path_of_address repo.repo_address) then (
        log "Git remote for %s needs updating (was: %s)"
          (OpamRepositoryBackend.to_string repo)
          (OpamStd.Option.default "<none>" current_remote);
        OpamProcess.Job.of_list [
          git repo ~verbose:false [ "remote" ; "rm" ; "origin" ];
          git repo ~verbose:false
            [ "remote" ; "add" ; "origin"; path_of_address repo.repo_address ]
        ] @@+ function
        | None -> Done ()
        | Some (_,err) -> OpamSystem.process_error err
      ) else
        Done ()
    in
    check_and_fix_remote @@+ fun () ->
    let branch = OpamStd.Option.default "HEAD" (snd repo.repo_address) in
    let refspec = Printf.sprintf "+%s:%s" branch remote_ref in
    git repo [ "fetch" ; "-q"; "origin"; refspec ]
    @@> fun r ->
    if OpamProcess.is_success r then Done ()
    else
      (* fallback to fetching all first (workaround, git 2.1 fails silently
         on 'fetch HASH' when HASH isn't available locally already) *)
      OpamProcess.Job.of_list
        [ git repo [ "fetch" ; "-q"; "origin" ];
          git repo [ "fetch" ; "-q"; "origin"; refspec ] ]
      @@+ function
      | None -> Done ()
      | Some (_,err) -> OpamSystem.process_error err

  let revision repo =
    git repo ~verbose:false [ "rev-parse"; "HEAD" ] @@>
    fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | []      -> Done "<none>"
    | full::_ ->
      if String.length full > 8 then
        Done (String.sub full 0 8)
      else
        Done full

  let reset repo =
    git repo [ "reset" ; "--hard"; remote_ref; "--" ]
    @@> fun r ->
    if OpamProcess.is_failure r then
      OpamSystem.internal_error "Git error: %s not found." remote_ref
    else Done ()

  let diff repo =
    git repo [ "diff" ; "--name-only" ; "HEAD"; remote_ref; "--" ]
    @@> fun r ->
    if OpamProcess.is_failure r then
      OpamSystem.internal_error "Git error: %s not found." remote_ref
    else
      Done (r.OpamProcess.r_stdout <> [])

  let versionned_files repo =
    git repo ~verbose:false [ "ls-files" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done r.OpamProcess.r_stdout

  let vc_dir repo = OpamFilename.Op.(repo.repo_root / ".git")

end

module B = OpamVCS.Make(Git)

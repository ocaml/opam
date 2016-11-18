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

open OpamFilename.Op
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "GIT" fmt

module Git : OpamVCS.VCS= struct

  let name = `git

  let exists repo_root =
    OpamFilename.exists_dir (repo_root / ".git") ||
    OpamFilename.exists (repo_root // ".git")

  let git repo_root =
    let dir = OpamFilename.Dir.to_string repo_root in
    fun ?verbose ?env args ->
      OpamSystem.make_command ~dir ?verbose ?env "git" args

  let init repo_root repo_url =
    let env =
      Array.append (Unix.environment ()) [|
        "GIT_AUTHOR_NAME=Opam";
        "GIT_AUTHOR_EMAIL=opam@ocaml.org";
        "GIT_COMMITTER_NAME=Opam";
        "GIT_COMMITTER_EMAIL=opam@ocaml.org"
      |] in
    OpamProcess.Job.of_list [
      git repo_root ~env [ "init" ];
      git repo_root ~env [ "remote" ; "add" ; "origin" ;
                      OpamUrl.base_url repo_url ];
      git repo_root ~env [ "commit" ; "--allow-empty" ; "-m" ; "opam-git-init" ];
    ] @@+ function
    | None -> Done ()
    | Some (_,err) -> OpamSystem.process_error err

  let remote_ref = "refs/remotes/opam-ref"

  let fetch repo_root repo_url =
    let check_and_fix_remote =
      git repo_root ~verbose:false [ "config" ; "--get"; "remote.origin.url" ]
      @@> fun r ->
      OpamSystem.raise_on_process_error r;
      let current_remote = match r.OpamProcess.r_stdout with
        | [url] -> Some url
        | _ -> None
      in
      if current_remote <> Some (OpamUrl.base_url repo_url) then (
        log "Git remote %s needs updating (was: %s)"
          (OpamUrl.to_string repo_url)
          (OpamStd.Option.default "<none>" current_remote);
        OpamProcess.Job.of_list [
          git repo_root ~verbose:false [ "remote" ; "rm" ; "origin" ];
          git repo_root ~verbose:false
            [ "remote" ; "add" ; "origin"; OpamUrl.base_url repo_url ]
        ] @@+ function
        | None -> Done ()
        | Some (_,err) -> OpamSystem.process_error err
      ) else
        Done ()
    in
    check_and_fix_remote @@+ fun () ->
    let branch = OpamStd.Option.default "HEAD" repo_url.OpamUrl.hash in
    let refspec = Printf.sprintf "+%s:%s" branch remote_ref in
    git repo_root [ "fetch" ; "-q"; "origin"; "--update-shallow"; refspec ]
    @@> fun r ->
    if OpamProcess.is_success r then Done ()
    else
      (* fallback to fetching all first (workaround, git 2.1 fails silently
         on 'fetch HASH' when HASH isn't available locally already).
         Also, remove the [--update-shallow] option in case git is so old that
         it didn't exist yet, as that is not needed in the general case *)
      OpamProcess.Job.of_list
        [ git repo_root [ "fetch" ; "-q"; "origin" ];
          git repo_root [ "fetch" ; "-q"; "origin"; refspec ] ]
      @@+ function
      | None -> Done ()
      | Some (_,err) -> OpamSystem.process_error err

  let revision repo_root =
    git repo_root ~verbose:false [ "rev-parse"; "HEAD" ] @@>
    fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | []      -> Done "<none>"
    | full::_ ->
      if String.length full > 8 then
        Done (String.sub full 0 8)
      else
        Done full

  let reset repo_root _repo_url =
    git repo_root [ "reset" ; "--hard"; remote_ref; "--" ]
    @@> fun r ->
    if OpamProcess.is_failure r then
      OpamSystem.internal_error "Git error: %s not found." remote_ref
    else
      if OpamFilename.exists (repo_root // ".gitmodules") then
        git repo_root [ "submodule"; "update"; "--init"; "--recursive" ]
        @@> fun r ->
        if OpamProcess.is_failure r then
          OpamConsole.warning "Git submodule update failed in %s"
            (OpamFilename.Dir.to_string repo_root);
        Done ()
      else Done ()

  let diff repo_root _repo_url =
    git repo_root [ "diff" ; "--name-only" ; "HEAD"; remote_ref; "--" ]
    @@> fun r ->
    if OpamProcess.is_failure r then
      OpamSystem.internal_error "Git error: %s not found." remote_ref
    else
      Done (r.OpamProcess.r_stdout <> [])

  let versionned_files repo_root =
    git repo_root ~verbose:false [ "ls-files" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done r.OpamProcess.r_stdout

  let vc_dir repo_root = OpamFilename.Op.(repo_root / ".git")

end

module B = OpamVCS.Make(Git)

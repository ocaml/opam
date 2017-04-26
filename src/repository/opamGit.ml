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

(* let log fmt = OpamConsole.log "GIT" fmt *)

module VCS : OpamVCS.VCS = struct

  let name = `git

  let exists repo_root =
    OpamFilename.exists_dir (repo_root / ".git") ||
    OpamFilename.exists (repo_root // ".git")

  let git repo_root =
    let dir = OpamFilename.Dir.to_string repo_root in
    fun ?verbose ?env ?stdout args ->
      OpamSystem.make_command ~dir ?verbose ?env ?stdout "git" args

  let init repo_root _repo_url =
    OpamFilename.mkdir repo_root;
    OpamProcess.Job.of_list [
      git repo_root [ "init" ];
      (* Enforce this option, it can break our use of git if set *)
      git repo_root [ "config" ; "--local" ; "fetch.prune"; "false"];
    ] @@+ function
    | None -> Done ()
    | Some (_,err) -> OpamSystem.process_error err

  let remote_ref url =
    match url.OpamUrl.hash with
    | Some h -> "refs/remotes/opam-ref-"^h
    | None -> "refs/remotes/opam-ref"

  let fetch repo_root repo_url =
    let origin = OpamUrl.base_url repo_url in
    let branch = OpamStd.Option.default "HEAD" repo_url.OpamUrl.hash in
    let opam_ref = remote_ref repo_url in
    let refspec = Printf.sprintf "+%s:%s" branch opam_ref in
    git repo_root [ "fetch" ; "-q"; origin; "--update-shallow"; refspec ]
    @@> fun r ->
    if OpamProcess.check_success_and_cleanup r then Done ()
    else
      (* fallback to fetching all first (workaround, git 2.1 fails silently
         on 'fetch HASH' when HASH isn't available locally already).
         Also, remove the [--update-shallow] option in case git is so old that
         it didn't exist yet, as that is not needed in the general case *)
      git repo_root [ "fetch" ; "-q"; origin ] @@> fun r ->
      OpamSystem.raise_on_process_error r;
      (* retry to fetch the specific branch *)
      git repo_root [ "fetch" ; "-q"; origin; refspec ] @@> fun r ->
      if OpamProcess.check_success_and_cleanup r then Done ()
      else if
        OpamStd.String.fold_left (fun acc c -> match acc, c with
            | true, ('0'..'9' | 'a'..'f' | 'A'..'F') -> true
            | _ -> false)
          true branch
      then
        (* the above might still fail on raw, untracked hashes: try to bind to
           the direct refspec, if found *)
        (git repo_root [ "update-ref" ; opam_ref; branch ] @@> fun r ->
         OpamSystem.raise_on_process_error r;
         Done ())
      else OpamSystem.process_error r

  let revision repo_root =
    git repo_root ~verbose:false [ "rev-parse"; "HEAD" ] @@>
    fun r ->
    if r.OpamProcess.r_code = 128 then
      (OpamProcess.cleanup ~force:true r; Done None)
    else
      (OpamSystem.raise_on_process_error r;
       match r.OpamProcess.r_stdout with
       | []      -> Done None
       | full::_ ->
         if String.length full > 8 then
           Done (Some (String.sub full 0 8))
         else
           Done (Some full))

  let reset repo_root repo_url =
    let rref = remote_ref repo_url in
    git repo_root [ "reset" ; "--hard"; rref; "--" ]
    @@> fun r ->
    if OpamProcess.is_failure r then
      OpamSystem.internal_error "Git error: %s not found." rref
    else
      if OpamFilename.exists (repo_root // ".gitmodules") then
        git repo_root [ "submodule"; "update"; "--init"; "--recursive" ]
        @@> fun r ->
        if OpamProcess.is_failure r then
          OpamConsole.warning "Git submodule update failed in %s"
            (OpamFilename.Dir.to_string repo_root);
        Done ()
      else Done ()

  let diff repo_root repo_url =
    let rref = remote_ref repo_url in
    let patch_file = OpamSystem.temp_file ~auto_clean: false "git-diff" in
    let finalise () = OpamSystem.remove_file patch_file in
    OpamProcess.Job.catch (fun e -> finalise (); raise e) @@ fun () ->
    git repo_root [ "add"; "." ] @@> fun r ->
    (* Git diff is to the working dir, but doesn't work properly for
       unregistered directories. *)
    OpamSystem.raise_on_process_error r;
    git repo_root ~stdout:patch_file [ "diff" ; "-R" ; "-p" ; rref; "--" ]
    @@> fun r ->
    if not (OpamProcess.check_success_and_cleanup r) then
      (finalise ();
       OpamSystem.internal_error "Git error: %s not found." rref)
    else if OpamSystem.file_is_empty patch_file then
      (finalise (); Done None)
    else
      Done (Some (OpamFilename.of_string patch_file))

  let is_up_to_date repo_root repo_url =
    let rref = remote_ref repo_url in
    git repo_root [ "diff" ; "--quiet" ; rref; "--" ]
    @@> function
    | { OpamProcess.r_code = 0; _ } -> Done true
    | { OpamProcess.r_code = 1; _ } as r ->
      OpamProcess.cleanup ~force:true r; Done false
    | r -> OpamSystem.process_error r

  let versionned_files repo_root =
    git repo_root ~verbose:false [ "ls-files" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done r.OpamProcess.r_stdout

  let vc_dir repo_root = OpamFilename.Op.(repo_root / ".git")

  let current_branch dir =
    git dir [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ]
    @@> function
    | { OpamProcess.r_code = 0; OpamProcess.r_stdout = [s]; _ } ->
      Done (Some s)
    | _ ->
      Done (Some "HEAD")

  let is_dirty dir =
    git dir [ "diff"; "--quiet" ]
    @@> function
    | { OpamProcess.r_code = 0; _ } -> Done false
    | { OpamProcess.r_code = 1; _ } as r ->
      OpamProcess.cleanup ~force:true r; Done true
    | r -> OpamSystem.process_error r

end

module B = OpamVCS.Make(VCS)

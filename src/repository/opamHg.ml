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

module VCS = struct

  let name = `hg

  let exists repo_root =
    OpamFilename.exists_dir (repo_root / ".hg")

  let hg repo_root =
    let dir = OpamFilename.Dir.to_string repo_root in
    fun ?verbose ?env ?stdout args ->
      OpamSystem.make_command ~dir ?verbose ?env ?stdout "hg" args

  let init repo_root _repo_url =
    OpamFilename.mkdir repo_root;
    hg repo_root [ "init" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let withrev url = match url.OpamUrl.hash with
    | None -> fun cmd -> cmd
    | Some r -> fun cmd -> cmd @ ["--rev"; r]

  let fetch repo_root repo_url =
    hg repo_root
      (withrev repo_url [ "pull"; "-f"; OpamUrl.base_url repo_url ])
    @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let revision repo_root =
    hg repo_root [ "id"; "-i" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | [] -> Done None
    | full::_ ->
      if String.length full > 8 then Done (Some (String.sub full 0 8))
      else Done (Some full)

  let get_id repo_root repo_url =
    hg repo_root
      (withrev repo_url [ "id" ; "-i" ; OpamUrl.base_url repo_url])
    @@> fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | [] -> failwith "unfound mercurial revision"
    | id::_ -> Done id

  let reset repo_root repo_url =
    get_id repo_root repo_url @@+ fun id ->
    hg repo_root [ "update" ; "--clean" ; "--rev" ; id ]
    @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let diff repo_root repo_url =
    let patch_file = OpamSystem.temp_file ~auto_clean:false "hg-diff" in
    let finalise () = OpamSystem.remove_file patch_file in
    OpamProcess.Job.catch (fun e -> finalise (); raise e) @@ fun () ->
    get_id repo_root repo_url @@+ fun id ->
    hg repo_root ~stdout:patch_file [ "diff"; "--reverse"; "--rev"; id ]
    @@> fun r ->
    if OpamProcess.is_failure r then
      (finalise ();
       OpamSystem.internal_error "Hg error: %s not found." id)
    else if OpamSystem.file_is_empty patch_file then
      (finalise (); Done None)
    else
      Done (Some (OpamFilename.of_string patch_file))

  let is_up_to_date repo_root repo_url =
    get_id repo_root repo_url @@+ fun id ->
    hg repo_root [ "stat"; "--rev"; id ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done (r.OpamProcess.r_stdout = [])

  let versionned_files repo_root =
    hg repo_root [ "locate" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done r.OpamProcess.r_stdout

  let vc_dir repo_root = OpamFilename.Op.(repo_root / ".hg")

  let current_branch dir =
    hg dir [ "branch" ] @@> function
    | { OpamProcess.r_code = 0; OpamProcess.r_stdout = [b]; _ } -> Done (Some b)
    | _ -> Done None

  let is_dirty dir =
    hg dir [ "stat" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done (r.OpamProcess.r_stdout = [])

end

module B = OpamVCS.Make(VCS)

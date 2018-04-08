(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

let log = OpamConsole.log "REPO_BACKEND"
let slog = OpamConsole.slog

type update =
  | Update_full of dirname
  | Update_patch of filename
  | Update_empty
  | Update_err of exn

module type S = sig
  val name: OpamUrl.backend
  val pull_url:
    ?cache_dir:dirname -> dirname -> OpamHash.t option -> url ->
    filename option download OpamProcess.job
  val fetch_repo_update:
    repository_name -> ?cache_dir:dirname -> dirname -> url ->
    update OpamProcess.job
  val repo_update_complete: dirname -> url -> unit OpamProcess.job
  val revision: dirname -> version option OpamProcess.job
  val sync_dirty: dirname -> url -> filename option download OpamProcess.job
end

let compare r1 r2 = compare r1.repo_name r2.repo_name

let to_string r =
  Printf.sprintf "%s at %s from %s"
    (OpamRepositoryName.to_string r.repo_name)
    (OpamFilename.Dir.to_string r.repo_root)
    (OpamUrl.to_string r.repo_url)

let local dirname = {
  repo_name     = OpamRepositoryName.of_string "local";
  repo_root     = dirname;
  repo_url      = OpamUrl.empty;
  repo_trust    = None;
}

let to_json r =
  `O  [ ("name", OpamRepositoryName.to_json r.repo_name);
        ("kind", `String (OpamUrl.string_of_backend r.repo_url.OpamUrl.backend));
      ]

let check_digest filename = function
  | Some expected
    when OpamRepositoryConfig.(!r.force_checksums) <> Some false ->
    (match OpamHash.mismatch (OpamFilename.to_string filename) expected with
     | None -> true
     | Some bad_hash ->
       OpamConsole.error
         "Bad checksum for %s: expected %s\n\
         \                     got      %s\n\
          Metadata might be out of date, in this case use `opam update`."
         (OpamFilename.to_string filename)
         (OpamHash.to_string expected)
         (OpamHash.to_string bad_hash);
       false)
  | _ -> true

open OpamProcess.Job.Op

let job_text name label =
  OpamProcess.Job.with_text
    (Printf.sprintf "[%s: %s]"
       (OpamConsole.colorise `green (OpamRepositoryName.to_string name))
       label)

let get_diff parent_dir dir1 dir2 =
  log "diff: %a/{%a,%a}"
    (slog OpamFilename.Dir.to_string) parent_dir
    (slog OpamFilename.Base.to_string) dir1
    (slog OpamFilename.Base.to_string) dir2;
  let patch = OpamSystem.temp_file ~auto_clean: false "patch" in
  let patch_file = OpamFilename.of_string patch in
  let finalise () = OpamFilename.remove patch_file in
  OpamProcess.Job.catch (fun e -> finalise (); raise e) @@ fun () ->
  OpamSystem.make_command
    ~verbose:OpamCoreConfig.(!r.verbose_level >= 2)
    ~dir:(OpamFilename.Dir.to_string parent_dir) ~stdout:patch
    "diff"
    [ "-ruaN";
      OpamFilename.Base.to_string dir1;
      OpamFilename.Base.to_string dir2; ]
  @@> function
  | { OpamProcess.r_code = 0; _ } -> finalise(); Done None
  | { OpamProcess.r_code = 1; _ } as r ->
    OpamProcess.cleanup ~force:true r;
    Done (Some patch_file)
  | r -> OpamSystem.process_error r

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

let log fmt = OpamGlobals.log "VCS" fmt

open OpamTypes
open OpamTypesBase
open OpamProcess.Job.Op

module type VCS = sig
  val exists: repository -> bool
  val init: repository -> unit OpamProcess.job
  val fetch: repository -> unit OpamProcess.job
  val reset: repository -> unit OpamProcess.job
  val diff: repository -> bool OpamProcess.job
  val revision: repository -> string OpamProcess.job
  val versionned_files: repository -> string list OpamProcess.job
end


module Make (VCS: VCS) = struct

  (* Local repos without a branch set actually use the rsync backend, but
     limited to versionned files *)
  let is_synched_repo repo =
    match repo.repo_address with
    | _, Some _ -> false
    | addr, None ->
      not (Re_str.string_match (Re_str.regexp_string "://") addr 0) &&
      OpamFilename.exists_dir (OpamFilename.Dir.of_string addr)

  let rsync repo =
    let source_repo =
      { repo with repo_root =
                    OpamFilename.Dir.of_string (fst repo.repo_address) }
    in
    VCS.versionned_files source_repo
    @@+ fun files ->
    let stdout_file =
      let f = OpamSystem.temp_file "rsync-files" in
      let fd = open_out f in
      List.iter (fun s -> output_string fd s; output_char fd '\n') files;
      close_out fd;
      f
    in
    OpamLocal.rsync_dirs ~args:["--files-from"; stdout_file]
      (OpamFilename.Dir.of_string (fst repo.repo_address))
      repo.repo_root
    @@+ fun dl ->
    OpamSystem.remove stdout_file;
    Done dl

  let init repo =
    VCS.init repo

  let pull_repo repo =
    if is_synched_repo repo then
      rsync repo
    else if VCS.exists repo then
      VCS.fetch repo @@+ fun () ->
      VCS.diff repo @@+ fun diff ->
      VCS.reset repo @@+ fun () ->
      if diff then Done (Result repo.repo_root)
      else Done (Up_to_date repo.repo_root)
    else
      (OpamFilename.mkdir repo.repo_root;
       VCS.init repo @@+ fun () ->
       VCS.fetch repo @@+ fun () ->
       VCS.reset repo @@+ fun () ->
       Done (Result repo.repo_root))

  let repo dirname address =
    let repo = OpamRepository.default () in
    {
      repo with
      repo_root    = dirname;
      repo_address = address;
    }

  let pull_url package dirname checksum remote_url =
    let () = match checksum with
      | None   -> ()
      | Some _ -> OpamGlobals.note "Skipping checksum for dev package %s"
                    (OpamPackage.to_string package) in
    let repo = repo dirname remote_url in
    pull_repo repo @@+ fun r ->
    OpamGlobals.msg "[%s] %s %s\n"
      (OpamGlobals.colorise `green (OpamPackage.name_to_string package))
      (string_of_address remote_url)
      (match r with
       | Result _ -> "updated"
       | Up_to_date _ -> "already up-to-date"
       | Not_available _ -> OpamGlobals.colorise `red "unavailable");
    Done (download_dir r)

  let pull_repo repo =
    pull_repo repo @@+ fun r ->
    OpamGlobals.msg "[%s] %s %s\n"
      (OpamGlobals.colorise `blue
         (OpamRepositoryName.to_string repo.repo_name))
      (string_of_address repo.repo_address)
      (match r with
       | Result _ -> "updated"
       | Up_to_date _ -> "already up-to-date"
       | Not_available _ -> OpamGlobals.colorise `red "unavailable");
    Done ()

  let pull_archive repo filename =
    let dirname = OpamPath.Repository.archives_dir repo in
    let basename = OpamFilename.basename filename in
    let local_file = OpamFilename.create dirname basename in
    if OpamFilename.exists local_file then (
      OpamGlobals.msg "[%s] Using %s\n"
        (OpamGlobals.colorise `blue
           (OpamRepositoryName.to_string repo.repo_name))
        (OpamFilename.prettify local_file);
      Done (Up_to_date local_file)
    ) else
      Done (Not_available (OpamFilename.to_string filename))

  let revision repo =
    let repo =
      if is_synched_repo repo then
        (* Actually get the revision at the source *)
        { repo with repo_root =
                      OpamFilename.Dir.of_string (fst repo.repo_address) }
      else repo
    in
    VCS.revision repo
    @@+ fun r ->
        Done (Some (OpamPackage.Version.of_string r))

end

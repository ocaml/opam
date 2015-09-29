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
open OpamProcess.Job.Op

module type VCS = sig
  val name: repository_kind
  val exists: repository -> bool
  val init: repository -> unit OpamProcess.job
  val fetch: repository -> unit OpamProcess.job
  val reset: repository -> unit OpamProcess.job
  val diff: repository -> bool OpamProcess.job
  val revision: repository -> string OpamProcess.job
  val versionned_files: repository -> string list OpamProcess.job
  val vc_dir: repository -> dirname
end


module Make (VCS: VCS) = struct

  let name = VCS.name

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
      let repo_root =
        OpamFilename.Dir.of_string (path_of_address repo.repo_address)
      in
      { repo with repo_root }
    in
    VCS.versionned_files source_repo
    @@+ fun files ->
    let files =
      List.map OpamFilename.(remove_prefix source_repo.repo_root)
        (OpamFilename.rec_files (VCS.vc_dir source_repo))
      @ files
    in
    let stdout_file =
      let f = OpamSystem.temp_file "rsync-files" in
      let fd = open_out f in
      List.iter (fun s -> output_string fd s; output_char fd '\n') files;
      close_out fd;
      f
    in
    OpamLocal.rsync_dirs ~args:["--files-from"; stdout_file]
      ~exclude_vcdirs:false
      (OpamFilename.Dir.of_string (path_of_address repo.repo_address))
      repo.repo_root
    @@+ fun dl ->
    OpamSystem.remove stdout_file;
    Done dl

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
    let repo = OpamRepositoryBackend.default () in
    {
      repo with
      repo_root    = dirname;
      repo_address = address;
    }

  let pull_url package dirname checksum remote_url =
    let () = match checksum with
      | None   -> ()
      | Some _ -> OpamConsole.note "Skipping checksum for dev package %s"
                    (OpamPackage.to_string package) in
    let repo = repo dirname remote_url in
    pull_repo repo @@+ fun r ->
    OpamConsole.msg "[%s] %s %s\n"
      (OpamConsole.colorise `green (OpamPackage.name_to_string package))
      (string_of_address remote_url)
      (match r with
       | Result _ -> "updated"
       | Up_to_date _ -> "already up-to-date"
       | Not_available _ -> OpamConsole.colorise `red "unavailable");
    Done (download_dir r)

  let pull_repo repo =
    pull_repo repo @@+ fun r ->
    OpamConsole.msg "[%s] %s %s\n"
      (OpamConsole.colorise `blue
         (OpamRepositoryName.to_string repo.repo_name))
      (string_of_address repo.repo_address)
      (match r with
       | Result _ -> "updated"
       | Up_to_date _ -> "already up-to-date"
       | Not_available _ -> OpamConsole.colorise `red "unavailable");
    Done ()

  let pull_archive repo filename =
    let dirname = OpamRepositoryPath.archives_dir repo in
    let basename = OpamFilename.basename filename in
    let local_file = OpamFilename.create dirname basename in
    if OpamFilename.exists local_file then (
      OpamConsole.msg "[%s] Using %s\n"
        (OpamConsole.colorise `blue
           (OpamRepositoryName.to_string repo.repo_name))
        (OpamFilename.prettify local_file);
      Done (Up_to_date local_file)
    ) else
      Done (Not_available (OpamFilename.to_string filename))

  let revision repo =
    let repo =
      if is_synched_repo repo then
        (* Actually get the revision at the source *)
        let repo_root =
          OpamFilename.Dir.of_string (path_of_address repo.repo_address)
        in
        { repo with repo_root }
      else repo
    in
    VCS.revision repo
    @@+ fun r ->
        Done (Some (OpamPackage.Version.of_string r))

end

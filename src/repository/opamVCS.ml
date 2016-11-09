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

open OpamTypes
open OpamTypesBase
open OpamStd.Op
open OpamProcess.Job.Op

module type VCS = sig
  val name: OpamUrl.backend
  val exists: dirname -> bool
  val init: dirname -> url -> unit OpamProcess.job
  val fetch: dirname -> url -> unit OpamProcess.job
  val reset: dirname -> url -> unit OpamProcess.job
  val diff: dirname -> url -> bool OpamProcess.job
  val revision: dirname -> string OpamProcess.job
  val versionned_files: dirname -> string list OpamProcess.job
  val vc_dir: dirname -> dirname
end


module Make (VCS: VCS) = struct

  let name = VCS.name

  (* Local repos without a branch set actually use the rsync backend, but
     limited to versionned files *)
  let synched_repo repo_url = match repo_url with
    | { OpamUrl.transport = "file"; hash = None; path = _; backend = _ } as url ->
      OpamUrl.local_dir url
    | _ -> None

  let rsync repo_root repo_url dir =
    VCS.versionned_files dir
    @@+ fun files ->
    let files =
      List.map OpamFilename.(remove_prefix dir)
        (OpamFilename.rec_files (VCS.vc_dir dir))
      @ files
    in
    let stdout_file =
      let f = OpamSystem.temp_file "rsync-files" in
      let fd = open_out f in
      List.iter (fun s -> output_string fd s; output_char fd '\n') files;
      close_out fd;
      f
    in
    (* Remove non-versionned files from destination *)
    (* fixme: doesn't clean directories *)
    let fset = OpamStd.String.Set.of_list files in
    List.iter (fun f ->
        let basename = OpamFilename.remove_prefix repo_root f in
        if not (OpamStd.String.Set.mem basename fset)
        then OpamFilename.remove f)
      (OpamFilename.rec_files repo_root);
    OpamLocal.rsync_dirs ~args:["--files-from"; stdout_file]
      ~exclude_vcdirs:false
      repo_url repo_root
    @@+ fun dl ->
    OpamSystem.remove stdout_file;
    Done dl

  let pull_repo_raw repo_name repo_root repo_url =
    match synched_repo repo_url with
    | Some dir -> rsync repo_root repo_url dir
    | None ->
      OpamProcess.Job.catch
        (fun e ->
           OpamConsole.error "Could not synchronize %s from %S:\n%s"
             (OpamRepositoryName.to_string repo_name)
             (OpamUrl.to_string repo_url)
             (Printexc.to_string e);
           Done (Not_available (Printexc.to_string e)))
      @@ fun () ->
      if VCS.exists repo_root then
        VCS.fetch repo_root repo_url @@+ fun () ->
        VCS.diff repo_root repo_url @@+ fun diff ->
        VCS.reset repo_root repo_url @@+ fun () ->
        if diff then Done (Result repo_root)
        else Done (Up_to_date repo_root)
      else
        (OpamFilename.mkdir repo_root;
         VCS.init repo_root repo_url @@+ fun () ->
         VCS.fetch repo_root repo_url @@+ fun () ->
         VCS.reset repo_root repo_url @@+ fun () ->
         Done (Result repo_root))

  let pull_url dirname _checksum remote_url =
    pull_repo_raw OpamRepositoryName.default dirname remote_url @@+ fun r ->
    Done (download_dir r)

  let pull_repo repo_name repo_root repo_url =
    pull_repo_raw repo_name repo_root repo_url @@+ fun r ->
    OpamConsole.msg "[%s] %s %s\n"
      (OpamConsole.colorise `blue
         (OpamRepositoryName.to_string repo_name))
      (OpamUrl.to_string repo_url)
      (match r with
       | Result _ -> "changed"
       | Up_to_date _ -> "already up-to-date"
       | Not_available _ -> OpamConsole.colorise `red "unavailable");
    Done ()

  let revision repo_root =
    VCS.revision repo_root @@+ fun r ->
    Done (Some (OpamPackage.Version.of_string r))

end

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
  val diff: dirname -> url -> filename option OpamProcess.job
  val is_up_to_date: dirname -> url -> bool OpamProcess.job
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

  let fetch_repo_update repo_name repo_root repo_url =
    if VCS.exists repo_root then
      OpamProcess.Job.catch (fun e -> Done (OpamRepositoryBackend.Update_err e))
      @@ fun () ->
      OpamRepositoryBackend.job_text repo_name "sync"
        (VCS.fetch repo_root repo_url)
      @@+ fun () ->
      OpamRepositoryBackend.job_text repo_name "diff"
        (VCS.diff repo_root repo_url)
      @@| function
      | None -> OpamRepositoryBackend.Update_empty
      | Some patch -> OpamRepositoryBackend.Update_patch patch
    else
      OpamProcess.Job.catch (fun e ->
          OpamFilename.rmdir repo_root;
          Done (OpamRepositoryBackend.Update_err e))
      @@ fun () ->
      OpamRepositoryBackend.job_text repo_name "init"
        (VCS.init repo_root repo_url)
      @@+ fun () ->
      OpamRepositoryBackend.job_text repo_name "sync"
        (VCS.fetch repo_root repo_url)
      @@+ fun () ->
      let tmpdir = OpamFilename.Dir.(of_string (to_string repo_root ^".new")) in
      OpamFilename.copy_dir ~src:repo_root ~dst:tmpdir;
      OpamProcess.Job.catch (fun e -> OpamFilename.rmdir tmpdir; raise e)
      @@ fun () ->
      VCS.reset tmpdir repo_url @@| fun () ->
      OpamRepositoryBackend.Update_full tmpdir

  let pull_url dirname checksum url =
    if checksum <> None then invalid_arg "VC pull_url doesn't allow checksums";
    match synched_repo url with
    | Some dir ->
      rsync dirname url dir @@|
      download_dir
    | None ->
      OpamProcess.Job.catch
        (fun e ->
           OpamConsole.error "Could not synchronize %s from %S:\n%s"
             (OpamFilename.Dir.to_string dirname)
             (OpamUrl.to_string url)
             (Printexc.to_string e);
           Done (Not_available (Printexc.to_string e)))
      @@ fun () ->
      if VCS.exists dirname then
        VCS.fetch dirname url @@+ fun () ->
        VCS.is_up_to_date dirname url @@+ function
        | true -> Done (Up_to_date (D dirname))
        | false ->
          VCS.reset dirname url @@+ fun () ->
          Done (Result (D dirname))
      else
        (OpamFilename.mkdir dirname;
         VCS.init dirname url @@+ fun () ->
         VCS.fetch dirname url @@+ fun () ->
         VCS.reset dirname url @@+ fun () ->
         Done (Result (D dirname)))

  let revision repo_root =
    VCS.revision repo_root @@+ fun r ->
    Done (Some (OpamPackage.Version.of_string r))

end

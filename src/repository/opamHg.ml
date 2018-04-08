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

  let mark_from_url url =
    match url.OpamUrl.hash with
    | None -> "opam-mark"
    | Some fragment -> "opam-mark-" ^ fragment

  let fetch ?cache_dir:_ repo_root repo_url =
    let src = OpamUrl.base_url repo_url in
    let rev = OpamStd.Option.default "default" repo_url.OpamUrl.hash in
    let mark = mark_from_url repo_url in
    hg repo_root [ "pull"; "--rev"; rev; src ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    hg repo_root [ "bookmark"; "--force"; "--rev"; rev; mark ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let revision repo_root =
    hg repo_root [ "identify"; "--id" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | [] -> Done None
    | full::_ ->
      if String.length full > 8 then Done (Some (String.sub full 0 8))
      else Done (Some full)

  let reset repo_root repo_url =
    let mark = mark_from_url repo_url in
    hg repo_root [ "update"; "--clean"; "--rev"; mark ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let diff repo_root repo_url =
    let patch_file = OpamSystem.temp_file ~auto_clean:false "hg-diff" in
    let finalise () = OpamSystem.remove_file patch_file in
    OpamProcess.Job.catch (fun e -> finalise (); raise e) @@ fun () ->
    let mark = mark_from_url repo_url in
    hg repo_root ~stdout:patch_file [ "diff"; "--subrepos"; "--reverse";
        "--rev"; mark ] @@> fun r ->
    if OpamProcess.is_failure r then
      (finalise ();
       OpamSystem.internal_error "Hg error: '%s' not found." mark)
    else if OpamSystem.file_is_empty patch_file then
      (finalise (); Done None)
    else
      Done (Some (OpamFilename.of_string patch_file))

  let is_up_to_date repo_root repo_url =
    let mark = mark_from_url repo_url in
    hg repo_root [ "status"; "--subrepos"; "--rev"; mark ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done (r.OpamProcess.r_stdout = [])

  let versionned_files repo_root =
    hg repo_root [ "locate" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done r.OpamProcess.r_stdout

  let vc_dir repo_root = OpamFilename.Op.(repo_root / ".hg")

  let filter_opam_mark mark =
    String.length mark < 9 || String.sub mark 0 9 <> "opam-mark"

  let split_on_char sep s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = sep then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    String.sub s 0 !j :: !r

  let current_branch repo_root =
    hg repo_root [ "identify"; "--bookmarks" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | [] -> Done None
    | marks::_ ->
        let marks = split_on_char ' ' marks in
        let marks = List.filter filter_opam_mark marks in
        match marks with
        | mark::_ -> Done (Some mark)
        | [] ->
            hg repo_root [ "identify"; "--branch" ] @@> fun r ->
            OpamSystem.raise_on_process_error r;
            match r.OpamProcess.r_stdout with
            | branch::_ when branch <> "default" -> Done (Some branch)
            | _ -> Done None

  let is_dirty repo_root =
    hg repo_root [ "status"; "--subrepos" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done (r.OpamProcess.r_stdout = [])

end

module B = OpamVCS.Make(VCS)

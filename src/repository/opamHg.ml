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

module Hg = struct

  let name = `hg

  let exists repo_root =
    OpamFilename.exists_dir (repo_root / ".hg")

  let hg repo_root =
    let dir = OpamFilename.Dir.to_string repo_root in
    fun ?verbose ?env args ->
      OpamSystem.make_command ~dir ?verbose ?env "hg" args

  let init repo_root repo_url =
    hg repo_root [ "init" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    OpamFilename.write
      OpamFilename.Op.(repo_root / ".hg" // "hgrc")
      (Printf.sprintf "[paths]\ndefault = %s\n"
         (OpamUrl.base_url repo_url));
    Done ()

  let fetch repo_root repo_url =
    let check_and_fix_remote =
      hg repo_root [ "showconfig" ; "paths.default" ]
      @@> fun r ->
      OpamSystem.raise_on_process_error r;
      if r.OpamProcess.r_stdout <> [OpamUrl.base_url repo_url] then (
        OpamFilename.rmdir OpamFilename.Op.(repo_root / ".hg");
        init repo_root repo_url
      ) else Done ()
    in
    check_and_fix_remote @@+ fun () ->
    hg repo_root [ "pull" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let revision repo_root =
    hg repo_root [ "id"; "-i" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | [] -> Done "<none>"
    | full::_ ->
      if String.length full > 8 then Done (String.sub full 0 8)
      else Done full

  let unknown_commit commit =
    OpamSystem.internal_error "Unknown mercurial revision/branch/bookmark: %s."
      commit

  let reset repo_root repo_url =
    let commit = match repo_url.OpamUrl.hash with
      | None   -> "tip"
      | Some c -> c
    in
    hg repo_root [ "update" ; "--clean"; commit ] @@> fun r ->
      if OpamProcess.is_failure r then unknown_commit commit;
      Done ()

  let diff repo_root repo_url =
    let commit = match repo_url.OpamUrl.hash with
      | None   -> "tip"
      | Some c -> c in
    hg repo_root [ "diff" ; "--stat" ; "-r" ; commit ] @@> fun r ->
    if OpamProcess.is_failure r then unknown_commit commit;
    Done (r.OpamProcess.r_stdout <> [])

  let versionned_files repo_root =
    hg repo_root [ "locate" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done r.OpamProcess.r_stdout

  let vc_dir repo_root = OpamFilename.Op.(repo_root / ".hg")

end

module B = OpamVCS.Make(Hg)

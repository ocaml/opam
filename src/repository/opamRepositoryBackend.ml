(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

let log ?level fmt = OpamConsole.log "REPO_BACKEND" ?level fmt
let slog = OpamConsole.slog

type update =
  | Update_full of dirname
  | Update_patch of filename
  | Update_empty
  | Update_err of exn

module type S = sig
  val name: OpamUrl.backend
  val pull_url:
    ?full_fetch:bool ->
    ?cache_dir:dirname -> ?subpath:subpath -> dirname -> OpamHash.t option -> url ->
    filename option download OpamProcess.job
  val fetch_repo_update:
    repository_name -> ?cache_dir:dirname -> dirname -> url ->
    update OpamProcess.job
  val repo_update_complete: dirname -> url -> unit OpamProcess.job
  val revision: dirname -> version option OpamProcess.job
  val sync_dirty:
    ?subpath:subpath -> dirname -> url -> filename option download OpamProcess.job
  val get_remote_url:
    ?hash:string -> dirname ->
    url option OpamProcess.job
end

let compare r1 r2 = compare r1.repo_name r2.repo_name

let to_string r =
  Printf.sprintf "%s from %s"
    (OpamRepositoryName.to_string r.repo_name)
    (OpamUrl.to_string r.repo_url)

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

let getfiles parent_dir dir =
  let dir = Filename.concat (OpamFilename.Dir.to_string parent_dir) dir in
  OpamSystem.get_files dir

let get_files_for_diff parent_dir dir1 dir2 = match dir1, dir2 with
  | None, None -> assert false
  | Some dir, None ->
    List.map (fun file -> Patch.Delete (dir^"/"^file))
      (getfiles parent_dir dir)
  | None, Some dir ->
    List.map (fun file -> Patch.Create (dir^"/"^file))
      (getfiles parent_dir dir)
  | Some dir1, Some dir2 ->
    let files1 = List.fast_sort String.compare (getfiles parent_dir dir1) in
    let files2 = List.fast_sort String.compare (getfiles parent_dir dir2) in
    let rec aux acc files1 files2 = match files1, files2 with
      | (file1::files1 as orig1), (file2::files2 as orig2) ->
        let cmp = String.compare file1 file2 in
        if cmp = 0 then
          aux (Patch.Edit
                 (dir1^"/"^file1, dir2^"/"^file2)
               :: acc)
            files1 files2
        else if cmp < 0 then
          aux (Patch.Delete (dir1^"/"^file1) :: acc) files1 orig2
        else
          aux (Patch.Create (dir2^"/"^file2) :: acc) orig1 files2
      | file1::files1, [] ->
        aux (Patch.Delete (dir1^"/"^file1) :: acc) files1 []
      | [], file2::files2 ->
        aux (Patch.Create (dir2^"/"^file2) :: acc) [] files2
      | [], [] ->
        acc
    in
    aux [] files1 files2

let readfile parent_dir file =
  let file = Filename.concat (OpamFilename.Dir.to_string parent_dir) file in
  OpamSystem.read file

let lstat parent_dir file =
  let file = Filename.concat (OpamFilename.Dir.to_string parent_dir) file in
  Unix.lstat file

let get_diff parent_dir dir1 dir2 =
  let chrono = OpamConsole.timer () in
  log "diff: %a/{%a,%a}"
    (slog OpamFilename.Dir.to_string) parent_dir
    (slog OpamFilename.Base.to_string) dir1
    (slog OpamFilename.Base.to_string) dir2;
  let rec aux diffs dir1 dir2 =
    let files = get_files_for_diff parent_dir dir1 dir2 in
    let diffs =
      List.fold_left (fun diffs operation ->
          let file1, file2 = match operation with
            | Patch.Delete filename -> (Some filename, None)
            | Patch.Create filename -> (None, Some filename)
            | Patch.Edit (file1, file2)
            | Patch.Rename_only (file1, file2) -> (Some file1, Some file2)
          in
          let add_to_diffs content1 content2 diffs =
            match Patch.diff operation content1 content2 with
            | None -> diffs
            | Some diff -> diff :: diffs
          in
          match
            OpamStd.Option.map (lstat parent_dir) file1,
            OpamStd.Option.map (lstat parent_dir) file2
          with
          | Some {Unix.st_kind = Unix.S_REG; _}, None
          | None, Some {Unix.st_kind = Unix.S_REG; _}
          | Some {st_kind = S_REG; _}, Some {st_kind = S_REG; _} ->
            let content1 = Option.map (readfile parent_dir) file1 in
            let content2 = Option.map (readfile parent_dir) file2 in
            add_to_diffs content1 content2 diffs
          | Some {Unix.st_kind = Unix.S_DIR; _}, None
          | None, Some {Unix.st_kind = Unix.S_DIR; _}
          | Some {st_kind = S_DIR; _}, Some {st_kind = S_DIR; _} ->
            aux diffs file1 file2
          | Some {st_kind = S_DIR; _}, Some {st_kind = S_REG; _} ->
            let content2 = Option.map (readfile parent_dir) file2 in
            let diffs = add_to_diffs None content2 diffs in
            aux diffs file1 None
          | Some {st_kind = S_REG; _}, Some {st_kind = S_DIR; _} ->
            let diffs = aux diffs None file2 in
            let content1 = Option.map (readfile parent_dir) file1 in
            add_to_diffs content1 None diffs
          | Some {st_kind = S_LNK; _}, _ | _, Some {st_kind = S_LNK; _} ->
            failwith "Symlinks are unsupported"
          | Some {st_kind = S_CHR; _}, _ | _, Some {st_kind = S_CHR; _} ->
            failwith "Character devices are unsupported"
          | Some {st_kind = S_BLK; _}, _ | _, Some {st_kind = S_BLK; _} ->
            failwith "Block devices are unsupported"
          | Some {st_kind = S_FIFO; _}, _ | _, Some {st_kind = S_FIFO; _} ->
            failwith "Named pipes are unsupported"
          | Some {st_kind = S_SOCK; _}, _ | _, Some {st_kind = S_SOCK; _} ->
            failwith "Sockets are unsupported"
          | None, None -> assert false)
        diffs files
    in
    diffs
  in
  match
    aux []
      (Some (OpamFilename.Base.to_string dir1))
      (Some (OpamFilename.Base.to_string dir2))
  with
  | [] ->
    log "Internal diff (empty) done in %.2fs." (chrono ());
    Done None
  | diffs ->
    log "Internal diff (non-empty) done in %.2fs." (chrono ());
    let patch = OpamSystem.temp_file ~auto_clean:false "patch" in
    let patch_file = OpamFilename.of_string patch in
    OpamFilename.write patch_file (Format.asprintf "%a" Patch.pp_list diffs);
    Done (Some patch_file)

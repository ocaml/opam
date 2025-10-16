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
  | Update_full of OpamRepositoryRoot.t
  | Update_patch of (filename * Patch.t list)
  | Update_empty
  | Update_err of exn

module type S = sig
  val name: OpamUrl.backend
  val pull_url:
    ?full_fetch:bool ->
    ?cache_dir:dirname -> ?subpath:subpath -> dirname -> OpamHash.t option -> url ->
    filename option download OpamProcess.job
  val fetch_repo_update:
    repository_name -> ?cache_dir:dirname -> OpamRepositoryRoot.t -> url ->
    update OpamProcess.job
  val repo_update_complete: OpamRepositoryRoot.t -> url -> unit OpamProcess.job
  val revision: dirname -> string option OpamProcess.job
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

let job_text name label =
  OpamProcess.Job.with_text
    (Printf.sprintf "[%s: %s]"
       (OpamConsole.colorise `green (OpamRepositoryName.to_string name))
       label)

let get_files_for_diff parent_dir dir1 dir2 =
  let getfiles parent_dir dir =
    let dir = Filename.concat (OpamFilename.Dir.to_string parent_dir) dir in
    OpamSystem.get_files_except_vcs dir
  in
  match dir1, dir2 with
  | None, None -> assert false
  | Some dir, None ->
    List.map (fun file -> (Some (dir^"/"^file), None))
      (getfiles parent_dir dir)
  | None, Some dir ->
    List.map (fun file -> (None, Some (dir^"/"^file)))
      (getfiles parent_dir dir)
  | Some dir1, Some dir2 ->
    let files1 = List.fast_sort String.compare (getfiles parent_dir dir1) in
    let files2 = List.fast_sort String.compare (getfiles parent_dir dir2) in
    let rec aux acc files1 files2 = match files1, files2 with
      | (file1::files1 as orig1), (file2::files2 as orig2) ->
        let cmp = String.compare file1 file2 in
        if cmp = 0 then
          aux ((Some (dir1^"/"^file1), Some (dir2^"/"^file2)) :: acc)
            files1 files2
        else if cmp < 0 then
          aux ((Some (dir1^"/"^file1), None) :: acc) files1 orig2
        else
          aux ((None, Some (dir2^"/"^file2)) :: acc) orig1 files2
      | file1::files1, [] ->
        aux ((Some (dir1^"/"^file1), None) :: acc) files1 []
      | [], file2::files2 ->
        aux ((None, Some (dir2^"/"^file2)) :: acc) [] files2
      | [], [] ->
        acc
    in
    aux [] files1 files2

(* Serves to remove the repository suffix since the quarantine mechanism in
   local and http patches causes incoherencies with vcs patches *)
let strip_repo_suffix patch =
  let rm_prefix f =
    match OpamStd.String.cut_at f '/' with
    | None ->
      log "Internal diff: failed to remove prefix of %s" f;
      f
    | Some (_, r) -> r
  in
  let operation =
    match patch.Patch.operation with
    | Patch.Create f -> Patch.Create (rm_prefix f)
    | Patch.Delete f -> Patch.Delete (rm_prefix f)
    | Patch.Edit (f1, f2) -> Patch.Edit (rm_prefix f1, rm_prefix f2)
    | Patch.Git_ext (f1, f2, ext) ->
      Patch.Git_ext (rm_prefix f1, rm_prefix f2, ext)
  in
  {patch with operation}

let return_patch_diffs diffs kind chrono =
  match diffs with
  | [] ->
    log "Internal diff (%s, empty) done in %.2fs." kind (chrono ());
    None
  | diffs ->
    log "Internal diff (%s, non-empty, %a changed files) done in %.2fs."
      kind (slog (fun l -> string_of_int (List.length l))) diffs (chrono ());
    let patch = OpamSystem.temp_file ~auto_clean:false "patch" in
    let patch_file = OpamFilename.of_string patch in
    OpamFilename.write patch_file (Format.asprintf "%a" Patch.pp_list diffs);
    Some (patch_file, List.map strip_repo_suffix diffs)

let get_diff_dirs parent_dir dir1 dir2 =
  let chrono = OpamConsole.timer () in
  log "diff: %a/{%a,%a}"
    (slog OpamFilename.Dir.to_string) parent_dir
    (slog OpamFilename.Base.to_string) dir1
    (slog OpamFilename.Base.to_string) dir2;
  let readfile parent_dir file =
    let real_file =
      Filename.concat (OpamFilename.Dir.to_string parent_dir) file
    in
    (file, OpamSystem.read real_file)
  in
  let lstat_opt parent_dir = function
    | None -> None
    | Some file ->
      let file = Filename.concat (OpamFilename.Dir.to_string parent_dir) file in
      Some (Unix.lstat file)
  in
  let rec aux diffs dir1 dir2 =
    let files = get_files_for_diff parent_dir dir1 dir2 in
    let diffs =
      List.fold_left (fun diffs (file1, file2) ->
          let add_to_diffs content1 content2 diffs =
            match Patch.diff content1 content2 with
            | None -> diffs
            | Some diff -> diff :: diffs
          in
          match lstat_opt parent_dir file1, lstat_opt parent_dir file2 with
          | Some {st_kind = S_REG; _}, None
          | None, Some {st_kind = S_REG; _}
          | Some {st_kind = S_REG; _}, Some {st_kind = S_REG; _} ->
            let content1 = Option.map (readfile parent_dir) file1 in
            let content2 = Option.map (readfile parent_dir) file2 in
            add_to_diffs content1 content2 diffs
          | Some {st_kind = S_DIR; _}, None | None, Some {st_kind = S_DIR; _}
          | Some {st_kind = S_DIR; _}, Some {st_kind = S_DIR; _} ->
            aux diffs file1 file2
          | Some {st_kind = S_DIR; _}, Some {st_kind = S_REG; _} ->
            failwith "Change from a directory to a regular file is unsupported"
          | Some {st_kind = S_REG; _}, Some {st_kind = S_DIR; _} ->
            failwith "Change from a regular file to a directory is unsupported"
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
  return_patch_diffs
    (aux [] (Some (OpamFilename.Base.to_string dir1))
       (Some (OpamFilename.Base.to_string dir2))) "dir-dir" chrono

(** Compute content diffs for a single file.
    Compares [content2] (new) against [contents1] (old state map).
    Adds [filename] to [seen] set and generates a diff if contents differ.
    Returns updated (diffs, seen) accumulator pair *)
let get_content_diffs filename contents1 content2 diffs seen =
  let seen = OpamStd.String.Set.add filename seen in
  match OpamStd.String.Map.find_opt filename contents1 with
  | Some content1 when String.equal content1 content2 ->
    (diffs, seen)
  | content1_opt ->
    let content1 = Option.map (fun c -> (filename, c)) content1_opt in
    let content2 = Some (filename, content2) in
    match Patch.diff content1 content2 with
    | None -> (diffs, seen)
    | Some diff -> (diff :: diffs, seen)

let get_tar_contents tar =
  OpamTar.fold_reg_files
    (fun acc filename content ->
       OpamStd.String.Map.add filename content acc)
    OpamStd.String.Map.empty tar

(** Recursively read directory contents into a string map.
    Returns a map from relative file paths to their contents. *)
let read_dir_contents dir =
  let rec aux acc prefix current_dir =
    let dir_path = OpamFilename.Dir.to_string current_dir in
    let entries = OpamSystem.get_files_except_vcs dir_path in
    List.fold_left (fun acc entry ->
        let full_path = Filename.concat dir_path entry in
        let relative_path = if prefix = "" then entry else prefix ^ "/" ^ entry in
        let stat = Unix.lstat full_path in
        match stat.Unix.st_kind with
        | Unix.S_REG ->
          let content = OpamSystem.read full_path in
          OpamStd.String.Map.add relative_path content acc
        | Unix.S_DIR ->
          aux acc relative_path (OpamFilename.Dir.of_string full_path)
        | Unix.S_LNK -> failwith "Symlinks are unsupported"
        | Unix.S_CHR -> failwith "Character devices are unsupported"
        | Unix.S_BLK -> failwith "Block devices are unsupported"
        | Unix.S_FIFO -> failwith "Named pipes are unsupported"
        | Unix.S_SOCK -> failwith "Sockets are unsupported"
      ) acc entries
  in
  aux OpamStd.String.Map.empty "" dir

let get_deletion_diffs contents diffs seen  =
  OpamStd.String.Map.fold (fun filename content diffs ->
      if OpamStd.String.Set.mem filename seen then diffs
      else
        match Patch.diff (Some (filename, content)) None with
        | None -> diffs
        | Some diff -> diff :: diffs
    ) contents diffs

let get_diff_tars tar1 tar2 =
  let chrono = OpamConsole.timer () in
  let hash1 = OpamHash.compute (OpamFilename.to_string tar1) in
  let hash2 = OpamHash.compute (OpamFilename.to_string tar2) in
  if OpamHash.equal hash1 hash2 then
    (log "Tars identical, no diff needed in %.2fs." (chrono ());
     None)
  else
    ( log "diff: tar %a vs tar %a"
        (slog OpamFilename.to_string) tar1
        (slog OpamFilename.to_string) tar2;
      let contents1 = get_tar_contents tar1
      in
      let diffs, seen = OpamTar.fold_reg_files
          (fun (diffs, seen) filename content2 ->
             get_content_diffs filename contents1 content2 diffs seen
          ) ([],  OpamStd.String.Set.empty) tar2
      in
      let diffs =
        get_deletion_diffs contents1 diffs seen
      in
      return_patch_diffs diffs "tar-tar" chrono)

let get_diff_tar_dir tar_file dir =
  let chrono = OpamConsole.timer () in
  log "diff: tar %a vs dir %a"
    (slog OpamFilename.to_string) tar_file
    (slog OpamFilename.Dir.to_string) dir;

  let tar_contents = get_tar_contents tar_file in
  let dir_contents = read_dir_contents dir in
  let diffs, seen = OpamStd.String.Map.fold
      (fun filename content_dir (diffs, seen) ->
         get_content_diffs filename tar_contents content_dir diffs seen
      ) dir_contents ([], OpamStd.String.Set.empty)
  in
  let diffs =
    get_deletion_diffs tar_contents diffs seen
  in
  return_patch_diffs diffs "tar-dir" chrono

let get_diff_dir_tar dir tar_file =
  let chrono = OpamConsole.timer () in
  log "diff: dir %a vs tar %a"
    (slog OpamFilename.Dir.to_string) dir
    (slog OpamFilename.to_string) tar_file;

  let dir_contents = read_dir_contents dir in
  let tar_contents = get_tar_contents tar_file in
  let diffs, seen = OpamStd.String.Map.fold
      (fun filename content_tar (diffs, seen) ->
         get_content_diffs filename dir_contents content_tar diffs seen
      ) tar_contents ([], OpamStd.String.Set.empty)
  in
  let diffs =
    get_deletion_diffs dir_contents diffs seen
  in
  return_patch_diffs diffs "dir-tar" chrono

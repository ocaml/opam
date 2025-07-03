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

let get_diff parent_dir dir1 dir2 =
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
  let rec aux acc dir1 dir2 =
    let files = get_files_for_diff parent_dir dir1 dir2 in
    let diffs =
      List.fold_left (fun ((size, diffs) as acc) (file1, file2) ->
          match lstat_opt parent_dir file1, lstat_opt parent_dir file2 with
          | Some {st_kind = S_REG; _}, None
          | None, Some {st_kind = S_REG; _}
          | Some {st_kind = S_REG; _}, Some {st_kind = S_REG; _} ->
            let content1 = Option.map (readfile parent_dir) file1 in
            let content2 = Option.map (readfile parent_dir) file2 in
            begin match Patch.diff content1 content2 with
            | None -> (size, diffs)
            | Some diff ->
                let approximated_max_length_diff (filename, content) =
                  let len_content = String.length content in
                  String.length filename * 4 + len_content + len_content / 40
                in
                let size = match content1, content2 with
                  | None, None -> assert false
                  | Some x, None | None, Some x ->
                      size + 4096 + approximated_max_length_diff x
                  | Some x, Some y ->
                      size + 4096 + approximated_max_length_diff x + approximated_max_length_diff y
                in
                (size, diff :: diffs)
            end
          | Some {st_kind = S_DIR; _}, None | None, Some {st_kind = S_DIR; _}
          | Some {st_kind = S_DIR; _}, Some {st_kind = S_DIR; _} ->
            aux acc file1 file2
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
        acc files
    in
    diffs
  in
  match
    aux (0, [])
      (Some (OpamFilename.Base.to_string dir1))
      (Some (OpamFilename.Base.to_string dir2))
  with
  | _, [] ->
    log "Internal diff (empty) done in %.2fs." (chrono ());
    None
  | max_length, diffs ->
    log "Internal diff (non-empty) done in %.2fs." (chrono ());
    let patch = OpamSystem.temp_file ~auto_clean:false "patch" in
    let patch_file = OpamFilename.of_string patch in
    let buf =
      (* 512 is the default internal size of the Buffer.t in Format *)
      Buffer.create max_length
    in
    Patch.pp_list (Format.formatter_of_buffer buf) diffs;
    OpamFilename.write patch_file (Buffer.contents buf);
    Some patch_file

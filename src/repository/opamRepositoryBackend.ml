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

(**  DIFF *)

(* We put back the prefix for patch -p1 harmonisation *)
let add_prefix repo1 repo2 =
  let prefix repo =
    OpamRepositoryRoot.basename repo
    |> OpamFilename.Base.to_string
  in
  let p1 x = prefix repo1 ^ "/" ^ x in
  let p2 x = prefix repo2 ^ "/" ^ x in
  fun patch ->
    let operation =
      match patch.Patch.operation with
      | Patch.Create f -> Patch.Create (p2 f)
      | Patch.Delete f -> Patch.Delete (p1 f)
      | Patch.Edit (f1, f2) -> Patch.Edit (p1 f1, p2 f2)
      | Patch.Git_ext (f1, f2, ext) -> Patch.Git_ext (p1 f1, p2 f2, ext)
    in
    {patch with operation}

let get_diff repo1 repo2 =
  let chrono = OpamConsole.timer () in
  log "diff: %a"
    (fun fmt () ->
       if OpamFilename.Dir.equal
           (OpamRepositoryRoot.dirname repo1)
           (OpamRepositoryRoot.dirname repo2) then
         Format.fprintf fmt "%s/{%s,%s}"
           (OpamFilename.Dir.to_string (OpamRepositoryRoot.dirname repo1))
           (OpamFilename.Base.to_string (OpamRepositoryRoot.basename repo1))
           (OpamFilename.Base.to_string (OpamRepositoryRoot.basename repo2))
       else
         let prefix r = if OpamRepositoryRoot.is_tar r then "tar" else "dir" in
         Format.fprintf fmt "%s %s vs %s %s"
           (prefix repo1)
           (OpamRepositoryRoot.to_string repo1)
           (prefix repo2)
           (OpamRepositoryRoot.to_string repo2))
    ();
  let get_contents =
    let get_tar_contents tar =
      OpamRepositoryRoot.Tar.fold (fun acc filename content ->
          OpamStd.String.Map.add
            (OpamFilename.Unix.to_string filename) content acc)
        OpamStd.String.Map.empty tar
    in
    let read_dir_contents dir =
      let fail s = failwith (s ^ " are unsupported") in
      (* Recursively read directory contents into a string map.
         Returns a map from relative file paths to their contents. *)
      let rec aux acc prefix current_dir =
        let entries = OpamSystem.get_files_except_vcs current_dir in
        List.fold_left (fun acc entry ->
            let full_path = Filename.concat current_dir entry in
            let relative_path =
              match prefix with
              | None -> entry
              | Some prefix -> prefix ^ "/" ^ entry
            in
            let stat = Unix.lstat full_path in
            match stat.Unix.st_kind with
            | Unix.S_REG ->
              let content = OpamSystem.read full_path in
              OpamStd.String.Map.add relative_path content acc
            | Unix.S_DIR ->
              aux acc (Some relative_path) full_path
            | Unix.S_LNK -> fail "Symlinks"
            | Unix.S_CHR -> fail "Character devices"
            | Unix.S_BLK -> fail "Block devices"
            | Unix.S_FIFO -> fail "Named pipes"
            | Unix.S_SOCK -> fail "Sockets")
          acc entries
      in
      aux OpamStd.String.Map.empty None dir
    in
    function
    | OpamRepositoryRoot.Dir dir ->
      read_dir_contents (OpamRepositoryRoot.Dir.to_string dir)
    | OpamRepositoryRoot.Tar tar ->
      get_tar_contents tar
  in
  let contents1 = get_contents repo1 in
  let contents2 = get_contents repo2 in
  let get_content_diffs filename contents1 content2 diffs seen =
    (* Compute content diffs for a single file.
       Compares [content2] (new) against [contents1] (old state map).
       Adds [filename] to [seen] set and generates a diff if contents differ.
       Returns updated (diffs, seen) accumulator pair *)
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
  in
  let diffs, seen =
    OpamStd.String.Map.fold
      (fun filename content2 (diffs, seen) ->
         get_content_diffs filename contents1 content2 diffs seen)
      contents2 ([], OpamStd.String.Set.empty)
  in
  let diffs =
    (* NOTE: putting the deletions first in the list allows us to have
       a simpler implementation of OpamPatch. This might not be needed
       in the future if OpamPatch supports git apply style reordering. *)
    OpamStd.String.Map.fold (fun filename content diffs ->
        if OpamStd.String.Set.mem filename seen then diffs
        else
          match Patch.diff (Some (filename, content)) None with
          | None -> diffs
          | Some diff -> diff :: diffs)
      contents1 diffs
  in
  match diffs with
  | [] ->
    log "Internal diff (empty) done in %.2fs." (chrono ());
    None
  | diffs ->
    log "Internal diff (non-empty, %a changed files) done in %.2fs."
      (slog (fun l -> string_of_int (List.length l))) diffs (chrono ());
    let patch = OpamSystem.temp_file ~auto_clean:false "patch" in
    let patch_file = OpamFilename.of_string patch in
    let file_diffs = List.map (add_prefix repo1 repo2) diffs in
    OpamFilename.write patch_file (Format.asprintf "%a" Patch.pp_list file_diffs);
    Some (patch_file, diffs)

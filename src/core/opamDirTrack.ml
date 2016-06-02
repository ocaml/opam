(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
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

open OpamStd.Op
open OpamProcess.Job.Op

let log ?level fmt = OpamConsole.log ?level "TRACK" fmt
let slog = OpamConsole.slog

module SM = OpamStd.String.Map

type digest = string

let digest_of_string dg = dg
let string_of_digest dg = dg

type change =
  | Added of digest
  | Removed
  | Contents_changed of digest
  | Perm_changed of digest
  | Kind_changed of digest

type t = change SM.t

let string_of_change = function
  | Added _ -> "addition"
  | Removed -> "removal"
  | Contents_changed _ -> "modifications"
  | Perm_changed _ -> "permission change"
  | Kind_changed _ -> "kind change"

let to_string t =
  OpamStd.Format.itemize (fun (f, change) ->
      Printf.sprintf "%s of %s"
        (String.capitalize (string_of_change change)) f)
    (SM.bindings t)

(** uid, gid, perm *)
type perms = int * int * int

type item_value =
  | File of string
  | Dir
  | Link of string
  | Special of (int * int)

type item = perms * item_value

let item_of_filename f : item =
  let stats = Unix.stat f in
  Unix.(stats.st_uid, stats.st_gid, stats.st_perm),
  match stats.Unix.st_kind with
  | Unix.S_REG -> File (Digest.file f)
  | Unix.S_DIR -> Dir
  | Unix.S_LNK -> Link (Unix.readlink f)
  | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK ->
    Special Unix.(stats.st_dev, stats.st_rdev)

let item_digest = function
  | _perms, File d -> "F:" ^ Digest.to_hex d
  | _perms, Dir -> "D"
  | _perms, Link l -> "L:" ^ l
  | _perms, Special (a,b) -> Printf.sprintf "S:%d:%d" a b

let track dir ?(except=OpamFilename.Base.Set.empty) job_f =
  let module SM = OpamStd.String.Map in
  let rec make_index acc prefix dir =
    let files =
      try Sys.readdir (Filename.concat prefix dir)
      with Sys_error _ as e ->
        log "Error at dir %s: %a" (Filename.concat prefix dir)
          (slog Printexc.to_string) e;
        [||]
    in
    Array.fold_left
      (fun acc f ->
         let rel = Filename.concat dir f in
         if OpamFilename.Base.(Set.mem (of_string rel) except) then acc else
         let f = Filename.concat prefix rel in
         try
           let item = item_of_filename f in
           let acc = SM.add rel item acc in
           match item with
           | _, Dir -> make_index acc prefix rel
           | _ -> acc
         with Unix.Unix_error _ as e ->
           log "Error at %s: %a" f (slog Printexc.to_string) e;
           acc)
      acc files
  in
  let str_dir = OpamFilename.Dir.to_string dir in
  let before = make_index SM.empty str_dir "" in
  log ~level:2 "before install: %a elements"
    (slog @@ string_of_int @* SM.cardinal) before;
  job_f () @@| fun result ->
  let after = make_index SM.empty str_dir "" in
  let diff =
    SM.merge (fun _ before after ->
        match before, after with
        | None, None -> assert false
        | Some _, None -> Some Removed
        | None, Some item -> Some (Added (item_digest item))
        | Some (perma, a), Some ((permb, b) as item) ->
          if a = b then
            if perma = permb then None
            else Some (Perm_changed (item_digest item))
          else
          match a, b with
          | File _, File _ | Link _, Link _
          | Dir, Dir | Special _, Special _ ->
            Some (Contents_changed (item_digest item))
          | _ -> Some (Kind_changed (item_digest item)))
      before after
  in
  log "after install: %a elements, %a added"
    (slog @@ string_of_int @* SM.cardinal) after
    (slog @@ string_of_int @* SM.cardinal @*
             SM.filter (fun _ -> function Added _ -> true | _ -> false))
    diff;
  result, diff

let check prefix changes =
  let str_pfx = OpamFilename.Dir.to_string prefix in
  SM.fold (fun fname op acc ->
      let f = Filename.concat str_pfx fname in
      match op with
      | Added dg | Kind_changed dg ->
        let status =
          try
            let it = item_of_filename f in
            if item_digest it = dg then `Unchanged
            else `Changed
          with Unix.Unix_error _ -> `Removed
        in
        (OpamFilename.of_string f, status) :: acc
      | Contents_changed _ | Perm_changed _ | Removed -> acc)
    changes []
  |> List.rev

let revert ?(verbose=true) ?(force=false) prefix changes =
  let changes =
    (* Reverse the list so that dirnames come after the files they contain *)
    List.rev (OpamStd.String.Map.bindings changes)
  in
  List.iter (fun (fname,op) ->
      let f = Filename.concat (OpamFilename.Dir.to_string prefix) fname in
      match op with
      | Added dg | Kind_changed dg ->
        let cur_item_ct, cur_dg =
          try
            let item = item_of_filename f in
            Some (snd item), Some (item_digest item)
          with Unix.Unix_error _ -> None, None
        in
        if cur_dg = None then
          log ~level:2 "file %s was already removed" f
        else if cur_dg <> Some dg && not force then
          (if verbose then
             OpamConsole.warning "%s has been modified, not removing" f)
        else if cur_item_ct = Some Dir then
          (let d = OpamFilename.Dir.of_string f in
           if OpamFilename.dir_is_empty d then
             OpamFilename.rmdir d
           else if verbose then
             OpamConsole.warning "Not removing non-empty directory %s"
               (OpamFilename.Dir.to_string d))
        else
        let f = OpamFilename.of_string f in
        OpamFilename.remove f
      | (Removed | Perm_changed _ | Contents_changed _ as op) ->
        if verbose then
          OpamConsole.warning "Cannot revert %s of %s"
            (string_of_change op) fname)
    changes

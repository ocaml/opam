(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCompat
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
        (String.capitalize_ascii (string_of_change change)) f)
    (SM.bindings t)

(** uid, gid, perm *)
type perms = int * int * int

type item_value =
  | File of string
  | Dir
  | Link of string
  | Special of (int * int)

type item = perms * item_value

let cached_digest =
  let item_cache = Hashtbl.create 749 in
  fun f size mtime ->
    try
      let csize, cmtime, digest = Hashtbl.find item_cache f in
      if csize = size || mtime = cmtime then digest
      else raise Not_found
    with Not_found ->
      let digest = Digest.file f in
      Hashtbl.replace item_cache f (size, mtime, digest);
      Digest.to_hex digest

let quick_digest _f size mtime =
  Printf.sprintf "S%dT%s" size (string_of_float mtime)

let get_digest ?(precise=OpamCoreConfig.(!r.precise_tracking)) f size mtime =
  if precise then cached_digest f size mtime
  else quick_digest f size mtime

let item_of_filename ?precise f : item =
  let stats = Unix.lstat f in
  Unix.(stats.st_uid, stats.st_gid, stats.st_perm),
  match stats.Unix.st_kind with
  | Unix.S_REG ->
    File (get_digest ?precise f stats.Unix.st_size stats.Unix.st_mtime)
  | Unix.S_DIR -> Dir
  | Unix.S_LNK -> Link (Unix.readlink f)
  | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK ->
    Special Unix.(stats.st_dev, stats.st_rdev)

let item_digest = function
  | _perms, File d -> "F:" ^ d
  | _perms, Dir -> "D"
  | _perms, Link l -> "L:" ^ l
  | _perms, Special (a,b) -> Printf.sprintf "S:%d:%d" a b

let is_precise_digest d =
  not (OpamStd.String.starts_with ~prefix:"F:S" d)

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
  let scan_timer = OpamConsole.timer () in
  let before = make_index SM.empty str_dir "" in
  log ~level:2 "before install: %a elements scanned in %.3fs"
    (slog @@ string_of_int @* SM.cardinal) before (scan_timer ());
  job_f () @@| fun result ->
  let scan_timer = OpamConsole.timer () in
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
  log "after install: %a elements, %a added, scanned in %.3fs"
    (slog @@ string_of_int @* SM.cardinal) after
    (slog @@ string_of_int @* SM.cardinal @*
             SM.filter (fun _ -> function Added _ -> true | _ -> false))
    diff (scan_timer ());
  result, diff

let check_digest file digest =
  let precise = is_precise_digest digest in
  let it = item_of_filename ~precise file in
  try if item_digest it = digest then `Unchanged else `Changed
  with Unix.Unix_error _ -> `Removed

let check prefix changes =
  let str_pfx = OpamFilename.Dir.to_string prefix in
  SM.fold (fun fname op acc ->
      let f = Filename.concat str_pfx fname in
      match op with
      | Added dg | Kind_changed dg | Contents_changed dg ->
        (OpamFilename.of_string f, check_digest f dg) :: acc
      | Perm_changed _ | Removed -> acc)
    changes []
  |> List.rev

let revert ?title ?(verbose=OpamConsole.verbose()) ?(force=false)
    ?(dryrun=false) prefix changes =
  let title = match title with
    | None -> ""
    | Some t -> t ^ ": "
  in
  let rmdir d = if not dryrun then OpamFilename.rmdir d in
  let rmfile f = if not dryrun then OpamFilename.remove f in
  let changes =
    (* Reverse the list so that dirnames come after the files they contain *)
    List.rev (OpamStd.String.Map.bindings changes)
  in
  let already, modified, nonempty, cannot =
    List.fold_left (fun (already,modified,nonempty,cannot as acc) (fname,op) ->
        let f = Filename.concat (OpamFilename.Dir.to_string prefix) fname in
        match op with
        | Added dg | Kind_changed dg ->
          let cur_item_ct, cur_dg =
            try
              let precise = is_precise_digest dg in
              let item = item_of_filename ~precise f in
              Some (snd item), Some (item_digest item)
            with Unix.Unix_error _ -> None, None
          in
          if cur_dg = None then (fname::already, modified, nonempty, cannot)
          else if cur_dg <> Some dg && not force then
            (already, fname::modified, nonempty, cannot)
          else if cur_item_ct = Some Dir then
            let d = OpamFilename.Dir.of_string f in
            if OpamFilename.dir_is_empty d then
              (rmdir d; acc)
            else
              let nonempty =
                if List.exists
                    (OpamStd.String.starts_with ~prefix:fname) nonempty
                then nonempty else fname::nonempty
              in
              (already, modified, nonempty, cannot)
          else
          let f = OpamFilename.of_string f in
          rmfile f;
          acc
        | Contents_changed dg ->
          if check_digest f dg = `Changed then
            (already, modified, nonempty, (op,fname)::cannot)
          else
            acc (* File has changed, assume the removal script reverted it *)
        | (Removed | Perm_changed _) ->
          (already, modified, nonempty, (op,fname)::cannot))
      ([], [], [], []) changes
  in
  if already <> [] then
    log ~level:2 "%sfiles %s were already removed" title
      (String.concat ", " (List.rev already));
  if modified <> [] && verbose then
    OpamConsole.warning "%snot removing files that changed since:\n%s" title
      (OpamStd.Format.itemize (fun s -> s) (List.rev modified));
  if nonempty <> [] && verbose then
    OpamConsole.note "%snot removing non-empty directories:\n%s" title
      (OpamStd.Format.itemize (fun s -> s) (List.rev nonempty));
  if cannot <> [] && verbose then
    OpamConsole.warning "%scannot revert:\n%s" title
      (OpamStd.Format.itemize
         (fun (op,f) -> string_of_change op ^" of "^ f)
         (List.rev cannot))

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 David Allsopp Ltd.                                   *)
(*    Copyright 2025 Kate Deplaix                                         *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let log ?level fmt = OpamConsole.log "PATCH" ?level fmt

let translate_patch ~dir orig corrected =
  (* It's unnecessarily complicated to infer whether the entire file is CRLF
     encoded and also the status of individual files, so accept scanning the
     file three times instead of two. *)
  let strip_cr = OpamSystem.get_eol_encoding orig = Some true in
  let ch =
    try open_in_bin orig
    with Sys_error _ -> raise (OpamSystem.File_not_found orig)
  in
  (* CRLF detection with patching can be more complicated than that used here,
     especially in the presence of files with mixed LF/CRLF endings. The
     processing done here aims to allow patching to succeed on files which are
     wholly encoded CRLF or LF against patches which may have been translated to
     be the opposite.

     The resulting patch will *always* have LF line endings for the patch
     metadata (headers, chunk locations, etc.) but uses either CRLF or LF
     depending on the target file. Endings in the patch are always preserved for
     new files. The benefit of always using LF endings for the metadata is that
     patch's "Stripping trailing CRs from patch" behaviour won't be triggered.

     There are various patch formats, though only the Unified and Context
     formats allow multiple files to be patched. I tired of trying to get
     sufficient documented detail of Context diffs to be able to parse them
     without resorting to reverse-engineering code. It is unusual to see them
     these days, so for now opam just emits a warning if a Context diff file is
     encountered and does no processing to it.

     There are various semantic aspects of Unified diffs which are not handled
     (at least at present) by this function which are documented in the code
     with the marker "Weakness". *)
  let process_chunk_header result line =
    match OpamStd.String.split line ' ' with
    | "@@"::a::b::"@@"::_ ->
      (* Weakness: for a new file [a] should always be -0,0 (not checked) *)
      let l_a = String.length a in
      let l_b = String.length b in
      if l_a > 1 && l_b > 1 && a.[0] = '-' && b.[0] = '+' then
        try
          let f (_, v) = int_of_string v in
          let neg =
            OpamStd.String.cut_at (String.sub a 1 (l_a - 1)) ','
            |> OpamStd.Option.map_default f 1
          in
          let pos =
            OpamStd.String.cut_at (String.sub b 1 (l_b - 1)) ','
            |> OpamStd.Option.map_default f 1
          in
          result neg pos
        with e ->
          OpamStd.Exn.fatal e;
          (* TODO Should display some kind of re-sync warning *)
          `Header
      else
        (* TODO Should display some kind of re-sync warning *)
        `Header
    | _ ->
      (* TODO Should display some kind of warning that there were no chunks *)
      `Header
  in
  let process_state_transition next_state state transforms =
    match (state, next_state) with
    | (`Processing _, `Processing _) ->
      transforms
    | (`Processing (_, target, crlf, patch_crlf, chunks, _), _) ->
      let compute_transform patch_crlf =
        (* Emit the patch *)
        let transform =
          match (crlf, patch_crlf) with
          | (None, _)
          | (_, None) ->
            log ~level:3 "CRLF adaptation skipped for %s" target;
            None
          | (Some crlf, Some patch_crlf) ->
            if crlf = patch_crlf then begin
              log ~level:3 "No CRLF adaptation necessary for %s" target;
              None
            end else if crlf then begin
              log ~level:3 "Adding \\r to patch chunks for %s" target;
              Some true
            end else begin
              log ~level:3 "Stripping \\r to patch chunks for %s" target;
              Some false
            end
        in
        let record_transform transform =
          let augment_record (first_line, last_line) =
            (first_line, last_line, transform)
          in
          List.rev_append (List.rev_map augment_record chunks) transforms
        in
        OpamStd.Option.map_default record_transform transforms transform
      in
      OpamStd.Option.map_default compute_transform transforms patch_crlf
    | _ ->
      transforms
  in
  let rec fold_lines state n transforms =
    match input_line ch with
    | line ->
      let line =
        if strip_cr then
          String.sub line 0 (String.length line - 1)
        else
          line
      in
      let length = String.length line in
      let next_state =
        match state with
        | `Header ->
          begin
            match (if length > 4 then String.sub line 0 4 else "") with
            | "--- " ->
              (* Start of a unified diff header. *)
              let file =
                let file = String.sub line 4 (length - 4) in
                let open OpamStd in
                Option.map_default fst file (String.cut_at file '\t')
              in
              (* Weakness: new files are also marked with a time-stamp at
                           the start of the epoch, however it's localised,
                           making it a bit tricky to identify! New files are
                           also identified by their absence on disk, so this
                           weakness isn't particularly critical. *)
              if file = "/dev/null" then
                `NewHeader
              else
                let target =
                  OpamStd.String.cut_at (OpamSystem.back_to_forward file) '/'
                  |> OpamStd.Option.map_default snd file
                  |> Filename.concat dir
                in
                if Sys.file_exists target then
                  let crlf = OpamSystem.get_eol_encoding target in
                  `Patching (file, crlf)
                else
                  `NewHeader
            | "*** " ->
              OpamConsole.warning "File %s uses context diffs which are \
                                   less portable; consider using unified \
                                   diffs" orig;
              `SkipFile
            | _ ->
              (* Headers will contain other lines, which are ignored (e.g.
                 the diff command which generated the diff, or Git commit
                 messages) *)
              `Header
          end
        | `NewHeader ->
          if (if length > 4 then String.sub line 0 4 else "") = "+++ " then
            `New
          else
            (* TODO Should display some kind of re-sync warning *)
            `Header
        | `New ->
          process_chunk_header (fun neg pos -> `NewChunk (neg, pos))
            line
        | `NewChunk (neg, pos) ->
          (* Weakness: new files should only have + lines *)
          let neg =
            if line = "" || line.[0] = ' ' || line.[0] = '-' then
              neg - 1
            else
              neg
          in
          let pos =
            if line = "" || line.[0] = ' ' || line.[0] = '+' then
              pos - 1
            else
              pos
          in
          if neg = 0 && pos = 0 then
            `New
          else
            (* Weakness: there should only be one chunk for a new file *)
            `NewChunk (neg, pos)
        | `Patching (orig, crlf) ->
          if (if length > 4 then String.sub line 0 4 else "") = "+++ " then
            let file =
              let file = String.sub line 4 (length - 4) in
              let open OpamStd in
              Option.map_default fst file (String.cut_at file '\t')
            in
            `Processing (orig, file, crlf, None, [], `Head)
          else
            `Header
        | `Processing (orig, target, crlf, patch_crlf, chunks, `Head) ->
          if line = "\\ No newline at end of file" then
            (* If the no eol-at-eof indicator is found, never add \r to
               final chunk line *)
            let chunks =
              match chunks with
              | (a, b)::chunks ->
                (a, b - 1)::chunks
              | _ ->
                chunks
            in
            `Processing (orig, target, crlf, patch_crlf, chunks, `Head)
          else
            process_chunk_header
              (fun neg pos ->
                 `Processing (orig, target, crlf, patch_crlf, chunks,
                              `Chunk (succ n, neg, pos)))
              line
        | `Processing (orig, target, crlf, patch_crlf, chunks,
                       `Chunk (first_line, neg, pos)) ->
          let neg =
            if line = "" || line.[0] = ' ' || line.[0] = '-' then
              neg - 1
            else
              neg
          in
          let pos =
            if line = "" || line.[0] = ' ' || line.[0] = '+' then
              pos - 1
            else
              pos
          in
          let patch_crlf =
            let has_cr = (length > 0 && line.[length - 1] = '\r') in
            match patch_crlf with
            | None ->
              Some (Some has_cr)
            | Some (Some think_cr) when think_cr <> has_cr ->
              log ~level:2 "Patch adaptation disabled for %s: \
                            mixed endings or binary file" target;
              Some None
            | _ ->
              patch_crlf
          in
          if neg = 0 && pos = 0 then
            let chunks = (first_line, n)::chunks in
            `Processing (orig, target, crlf, patch_crlf, chunks, `Head)
          else
            `Processing (orig, target, crlf, patch_crlf, chunks,
                         `Chunk (first_line, neg, pos))
        | `SkipFile ->
          `SkipFile
      in
      if next_state = `SkipFile then
        []
      else
        process_state_transition next_state state transforms
        |> fold_lines next_state (succ n)
    | exception End_of_file ->
      process_state_transition `Header state transforms |> List.rev
  in
  let transforms = fold_lines `Header 1 [] in
  if transforms = [] then begin
    log ~level:1 "No patch translation needed for %s -> %s" orig corrected;
    OpamSystem.copy_file orig corrected
  end else begin
    seek_in ch 0;
    log ~level:1 "Transforming patch %s to %s" orig corrected;
    let ch_out =
      try open_out_bin corrected
      with Sys_error _ ->
        close_in ch;
        raise (OpamSystem.File_not_found corrected)
    in
    let (normal, add_cr, strip_cr) =
      let strip n s = String.sub s 0 (String.length s - n) in
      let id x = x in
      if strip_cr then
        (strip 1, id, strip 2)
      else
        (id, (fun s -> s ^ "\r"), strip 1)
    in
    if OpamConsole.debug () then begin
      let log_transform (first_line, last_line, add_cr) =
        let indicator = if add_cr then '+' else '-' in
        log ~level:3 "Transform %d-%d %c\\r" first_line last_line indicator
      in
      List.iter log_transform transforms
    end;
    let rec fold_lines n transforms =
      match input_line ch with
      | line ->
        let (f, transforms) =
          match transforms with
          | (first_line, last_line, add_cr_to_chunks)::next_transforms ->
            let transforms =
              if n = last_line then
                next_transforms
              else
                transforms
            in
            let f =
              if n >= first_line then
                if add_cr_to_chunks then
                  add_cr
                else
                  strip_cr
              else
                normal
            in
            (f, transforms)
          | [] ->
            (normal, [])
        in
        output_string ch_out (f line);
        output_char ch_out '\n';
        fold_lines (succ n) transforms
      | exception End_of_file ->
        close_out ch_out
    in
    fold_lines 1 transforms
  end;
  close_in ch

(* Patch configurator *)

module type FS_ABSTR = sig
  type root
  type file
  type target
  val root_label : string
  val translate_patch : bool
  val root_to_string : root -> string
  val file_to_string : file -> string
  val end_slash : root -> root
  val get_path : fail:(unit -> unit) -> root -> string -> file
  val ext : file -> string -> file
  val write : file -> string -> target -> target
  val on_rejection : file -> string option -> Patch.t -> unit
  val exists : file -> target -> bool
  val exists_dir : file -> target -> bool
  val read : file -> target -> string
  val remove : file -> target -> target
  val remove_dir : file -> target -> target
  val same_dirname : src:file -> dst:file -> bool
  val mv : src:file -> dst:file -> target -> target
  val open_ : root -> (target -> unit) -> unit
  val save : target -> unit
end

exception Internal_patch_error of string

let patch_t (type a) (module FS : FS_ABSTR with type root = a)
    ~allow_unclean ?patch_filename (to_patch:a) diffs =
 if diffs = [] then () else
  let internal_patch_error fmt =
    Printf.ksprintf (fun str -> raise (Internal_patch_error str)) fmt
  in
  let patch_info_path =
    OpamStd.Option.default
      (Printf.sprintf "in %s %s" FS.root_label (FS.root_to_string to_patch))
      patch_filename
  in
  let to_patch = FS.end_slash to_patch in
  let get_path file =
    let fail () =
      internal_patch_error "Patch %S tried to escape its scope."
        patch_info_path
    in
    FS.get_path ~fail to_patch file
  in
 let patch file content diff patching =
    (* NOTE: The None case returned by [Patch.patch] is only returned
       if [diff = Patch.Delete _]. This sub-function is not called in
       this case so we [assert false] instead. *)
    match Patch.patch ~cleanly:true content diff with
    | Some x -> patching, x
    | None -> assert false (* See NOTE above *)
    | exception _ when not allow_unclean ->
      internal_patch_error "Patch %S does not apply cleanly."
        patch_info_path
    | exception _ ->
      match Patch.patch ~cleanly:false content diff with
      | Some x ->
        let patching =
          OpamStd.Option.map_default (fun content ->
              FS.write (FS.ext file ".orig") content patching)
            patching content
        in
        patching, x
      | None -> assert false (* See NOTE above *)
      | exception _ ->
        FS.on_rejection file content diff;
        internal_patch_error "Patch %S does not apply cleanly."
          patch_info_path
  in
  let apply patching diff =
    match diff.Patch.operation with
    | Patch.Edit (file1, file2) ->
      let file1 = get_path file1 in
      let file2 = get_path file2 in
      let file1_exists = FS.exists file1 patching in
      (* That seems to be the GNU patch behaviour *)
      let file = if file1_exists then file1 else file2 in
      let content = FS.read file patching in
      let patching, content = patch file (Some content) diff patching in
      let patching = FS.write file content patching in
      let patching =
        if file1_exists && file1 <> (file2 : FS.file) then
          FS.remove_dir file1 patching
        else
          patching
      in
      patching
    | Patch.Delete file | Patch.Git_ext (file, _, Patch.Delete_only) ->
      let file = get_path file in
      let patching = FS.remove file patching in
      let patching = FS.remove_dir file patching in
      patching
    | Patch.Create file | Patch.Git_ext (_, file, Patch.Create_only) ->
      let file = get_path file in
      let patching, content = patch file None diff patching in
      let patching = FS.write file content patching in
      patching
    | Patch.Git_ext (_, _, Patch.Rename_only (src, dst)) ->
      let src = get_path src in
      let dst = get_path dst in
      let patching = FS.mv ~src ~dst patching in
      let patching =
        if FS.same_dirname ~src ~dst then
          FS.remove_dir src patching
        else
          patching
      in
      patching
  in
  FS.open_ to_patch (fun patching ->
      let patched : FS.target = List.fold_left apply patching diffs in
      FS.save patched)

let parse_patch ~translate file =
  if not (Sys.file_exists file) then
    (OpamConsole.error "Patch file %S not found." file;
     raise Not_found);
  let file' =
    match translate with
    | Some dir ->
      let file' = OpamSystem.temp_file ~auto_clean:false "processed-patch" in
      translate_patch ~dir file file';
      file'
    | None -> file
  in
  let content = OpamSystem.read file' in
  Fun.protect (fun () -> Patch.parse ~p:1 content)
    ~finally:(fun () -> if not (OpamConsole.debug ()) then Sys.remove file')

let patch (type a) (module FS : FS_ABSTR with type root = a)
    ~allow_unclean patch_source (to_patch:a) =
  let operations_result diffs =
    Ok (List.map (fun d -> d.Patch.operation) diffs)
  in
  let patch ?patch_filename diffs =
    patch_t (module FS) ~allow_unclean ?patch_filename to_patch diffs
  in
  try
    match patch_source with
    | `Patch_diffs diffs ->
      patch diffs;
      operations_result diffs
    | `Patch_file p ->
      let diffs =
        let translate =
          if FS.translate_patch then Some (FS.root_to_string to_patch)
          else None
        in
        parse_patch ~translate p
      in
      patch ~patch_filename:p diffs;
      operations_result diffs
  with exn -> Error exn

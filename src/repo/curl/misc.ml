let log msg = Globals.log (Filename.basename Sys.argv.(0)) msg

open Types

let local_path = Dirname.of_string (Run.cwd ())
let local_repo = Path.R.of_path local_path

let remote_path = Dirname.of_raw Sys.argv.(1)
let remote_repo = Path.R.of_path remote_path

let index_file = remote_path // "urls.txt"

let local_index_archive = local_path // "index.tar.gz"
let remote_index_archive = remote_path // "index.tar.gz"

(* url/$name.$version *)
module Path = struct
  module G = Path.G
  module C = Path.C
  module R = struct
    include Path.R
    let url t nv = Path.R.root t / "url" // NV.to_string nv
    let files_dir t = Path.R.root t / "files"
    let files t nv = files_dir t / NV.to_string nv
    let available_files t nv =
      if Dirname.exists (files t nv) then
        Filename.rec_list (files t nv)
      else
        []
  end
end

module FileUrl = struct
  let read f =
    if Filename.exists f then
      Some (Utils.string_strip (Raw.to_string (Filename.read f)))
    else
      None
end

let raw_wget str =
  let open Globals in
  match os with
  | Darwin | FreeBSD | OpenBSD -> [ "curl"; "-OL"; str ]
  | _ -> [ "wget"; str ]

let wget remote_file =
  raw_wget (Filename.to_string remote_file)

let remote_files, file_perms =
  let err = Dirname.exec local_path [wget index_file] in
  if err <> 0 then
    Globals.error_and_exit "Cannot get urls.txt";
  let lines = Process.read_lines "urls.txt" in
  let all =
    Utils.filter_map (fun f ->
      match Utils.cut_at f ' ' with
      | None ->
          if f <> "" then Globals.warning "ignoring the line %S" f;
          None
      | Some (perm, f) ->
          let file =
            if Utils.starts_with "./" f then
              String.sub f 2 (String.length f - 2)
            else
              f in
          let file = remote_path // file in
          Some (file, (file, int_of_string perm))
    ) lines in
  let files, map = List.split all in
  (Filename.Set.of_list files), map

let download_remote_file ?(force = false) remote_file =
  if not (Filename.Set.mem remote_file remote_files) then
    None
  else begin
    let local_dir =
      let basename = Dirname.remove_prefix remote_path (Filename.dirname remote_file) in
      local_path / basename in
    let local_file =
      let basename = Filename.remove_prefix remote_path remote_file in
      local_path // basename in
    if not force && Filename.exists local_file then
      (* Do not overwrite the file if it is already there *)
      None
    else begin
      Dirname.mkdir (Filename.dirname local_file);
      log "dowloading %s" (Filename.to_string remote_file);
      let err = Dirname.exec local_dir [wget remote_file] in
      if err <> 0 then
        Globals.error_and_exit "Cannot download %s" (Filename.to_string remote_file);
      if not (Filename.exists local_file) then
        (* This may happen with empty files *)
        Run.write (Filename.to_string local_file) "";
      let perm = List.assoc remote_file file_perms in
      Unix.chmod (Filename.to_string local_file) perm;
      Some local_file
    end
  end

(* Get all the files in remote_dir *)
let download_remote_dir ?(force = false) remote_dir =
  Filename.Set.filter (fun f ->
    if Filename.starts_with remote_dir f then begin
      match download_remote_file ~force f with
      | Some _ -> true
      | None   -> false
    end else
    false
  ) remote_files

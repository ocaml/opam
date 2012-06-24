let log msg = Globals.log (Filename.basename Sys.argv.(0)) msg

open Types

let local_path = Dirname.of_string (Run.cwd ())
let local_repo = Path.R.of_path local_path

let remote_path = Dirname.of_raw Sys.argv.(1)
let remote_repo = Path.R.of_path remote_path

let index_file = remote_path // "urls.txt"

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
  match Globals.os with
  | Globals.Darwin -> [ "curl"; "-OL"; str ]
  | _              -> [ "wget"; str ]

let wget remote_file =
  raw_wget (Filename.to_string remote_file)

let remote_files =
  let err = Dirname.exec local_path [wget index_file] in
  if err <> 0 then
    Globals.error_and_exit "Cannot get urls.txt";
  let lines = Process.read_lines "urls.txt" in
  let lines = List.map (fun f ->
    if Utils.starts_with "./" f then
      String.sub f 2 (String.length f - 2)
    else
      f 
  ) lines in
  Filename.Set.of_list (List.map ((//) remote_path) lines)

let download_remote_file remote_file =
  if not (Filename.Set.mem remote_file remote_files) then
    None
  else begin
    let local_dir =
      let basename = Dirname.remove_prefix remote_path (Filename.dirname remote_file) in
      local_path / basename in
    let local_file =
      let basename = Filename.remove_prefix remote_path remote_file in
      local_path // basename in
    if Filename.exists local_file then
      (* Overwrite the file if it is already there *)
      Filename.remove local_file;
    Dirname.mkdir (Filename.dirname local_file);
    let err = Dirname.exec local_dir [wget remote_file] in
    if err <> 0 then
      Globals.error_and_exit "Cannot download %s" (Filename.to_string remote_file);
    if not (Filename.exists local_file) then
      (* This may happen with empty files *)
      Run.write (Filename.to_string local_file) "";
    Some local_file
  end

(* Get all the files in remote_dir *)
let download_remote_dir remote_dir =
  Filename.Set.filter (fun f ->
    if Filename.starts_with remote_dir f then begin
      match download_remote_file f with
      | Some _ -> true
      | None   -> false
    end else
    false
  ) remote_files

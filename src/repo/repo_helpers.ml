open Types

let log fmt = Globals.log "repo-helpers" fmt

type state = {
  local_path : dirname;
  local_repo : Path.R.t;
  remote_path: dirname;
  remote_repo: Path.R.t;
}

let make_state () =
  let local_path = Dirname.of_string (Run.cwd ()) in
  let local_repo = Path.R.of_dirname local_path in
  let remote_path = Dirname.raw Sys.argv.(1) in
  let remote_repo = Path.R.of_dirname remote_path in
  { local_path; local_repo; remote_path; remote_repo }

type download_state = {
  filename : filename;
  force    : bool;
  kind     : string;
}

let make_download_state () =
  let kind =
    try (Array.of_list (Utils.split Sys.argv.(0) '-')).(1)
    with _ -> "none" in
  let filename = Filename.of_string Sys.argv.(1) in
  let force = bool_of_string Sys.argv.(2) in
  { kind; filename; force }

module type REPO = sig
  type t
  val sync  : t -> state -> Filename.Set.t
  val upload: t -> state -> dirname -> Filename.Set.t
end

let local_of_remote_file state remote_file =
  let basename = Filename.remove_prefix state.remote_path remote_file in
  state.local_path // basename

let local_of_remote_dir state remote_dir =
  let basename = Dirname.remove_prefix state.remote_path remote_dir in
  state.local_path / basename

let remote_of_local_file state local_file =
  let basename = Filename.remove_prefix state.local_path local_file in
  state.remote_path // basename

let remote_of_local_dir state local_dir =
  let basename = Dirname.remove_prefix state.local_path local_dir in
  state.remote_path / basename

let download d =
  let cmd = Printf.sprintf "opam-%s-download" d.kind in
  let output = Run.read_command_output [
    cmd; Filename.to_string d.filename; string_of_bool d.force
  ] in
  match output with
  | None        -> None
  | Some []     -> failwith "download error"
  | Some (f::_) -> Some (Filename.of_string f)

let rec download_iter = function
  | []       -> None
  | (f,k)::t ->
      let d = {
        filename = f;
        force    = false;
        kind     = k;
      } in
      match download d with
      | None -> download_iter t
      | r    -> r

module Make (R : REPO) = struct

  let nv_set_of_files l =
    NV.Set.of_list (Utils.filter_map NV.of_filename (Filename.Set.elements l))

  let (++) = Filename.Set.union

  let get_updates t state =
    let new_files = R.sync t state in
    let updates local_dir =
      Filename.Set.filter (fun local_file ->
        Filename.starts_with (local_dir state.local_repo) local_file
      ) new_files in
    nv_set_of_files (
     (updates Path.R.archives_dir) ++ (updates Path.R.packages_dir)
    )

  let kind_of_file filename =
    if Filename.exists filename then "rsync" else "curl"

  let make_archive t state nv =
    Dirname.with_tmp_dir (fun tmp_download_dir ->
      Dirname.with_tmp_dir (fun tmp_extract_root ->
        let tmp_extract_dir = tmp_extract_root / NV.to_string nv in

        (* If the archive is on the server, download it directly *)
        let remote_archive = Path.R.archive state.remote_repo nv in
        let download_info = {
          kind     = kind_of_file remote_archive;
          filename = remote_archive;
          force    = false;
        } in
        let local_archive = Path.R.archive state.local_repo nv in
        match download download_info with
        | Some _ -> local_archive
        | None   ->
            log
              "%s is not on the server, need to build it"
              (Filename.to_string remote_archive);
            let url_f = Path.R.url state.local_repo nv in
            if Filename.exists url_f then begin
              (* download the archive upstream if the upstream address
                 is specified *)
              let urls = File.URL.read url_f in
              let urls_s =
                String.concat " "
                  (List.map
                     (fun (f,k) -> Printf.sprintf "%s:%s" (Filename.to_string f) k)
                     urls) in
              log "downloading %s" urls_s;
              match Dirname.in_dir tmp_download_dir (fun () -> download_iter urls) with
              | None -> Globals.error_and_exit "Cannot get %s" urls_s
              | Some local_archive ->
                  log "extracting %s to %s"
                    (Filename.to_string local_archive)
                    (Dirname.to_string tmp_extract_dir);
                  Filename.extract local_archive tmp_extract_dir;
            end;

            (* Eventually add the files/<package>/* to the extracted dir *)
            log "Adding the files to the archive";
            let files = Path.R.available_files state.local_repo nv in
            List.iter (fun f -> Filename.copy_in f tmp_extract_dir) files;

            (* And finally create the final archive *)
            (* XXX: we should add a suffix to the version to show that
               the archive has been repacked by opam *)
            log "Creating the archive files in %s" (Filename.to_string local_archive);
            log "Files in tmp_extract_root: %s"
              (Filename.Set.to_string (Filename.Set.of_list (Filename.list tmp_extract_root)));
            let err = Dirname.exec tmp_extract_root [
              [ "tar" ; "czf" ; Filename.to_string local_archive ; NV.to_string nv ]
            ] in
            if err <> 0 then
              Globals.error_and_exit "Cannot compress %s" (Dirname.to_string tmp_extract_dir);

            local_archive
      )
    )

  let upload t state =
    let local_repo = state.local_repo in
    let upload_path = Path.R.upload_dir local_repo in
    let upload_repo = Path.R.of_dirname upload_path in
    let state = { state with local_path = upload_path; local_repo = upload_repo } in
    let upload fn = R.upload t state (fn state.remote_repo) in
    let files =
         upload Path.R.packages_dir
      ++ upload Path.R.archives_dir
      ++ upload Path.R.compilers_dir in
    nv_set_of_files files

end

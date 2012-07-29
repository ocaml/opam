open Types

let log fmt = Globals.log "repo-helpers" fmt

type state = {
  local_path : dirname;
  local_repo : Path.R.t;
  remote_path: dirname;
  remote_repo: Path.R.t;
}

module type SYNC = sig
  type t
  val file: state -> t -> filename -> (filename * bool) option
  val dir: state -> t -> dirname -> Filename.Set.t
  val upload: state -> t -> dirname -> unit
  val same_digest: state -> t -> local_file:filename -> remote_file:filename -> bool
end

let make_state () =
  let local_path = Dirname.of_string (Run.cwd ()) in
  let local_repo = Path.R.of_dirname local_path in
  let remote_path = Dirname.raw Sys.argv.(1) in
  let remote_repo = Path.R.of_dirname remote_path in
  { local_path; local_repo; remote_path; remote_repo }

let local_of_remote_file state remote_file =
  let basename = Filename.remove_prefix state.remote_path remote_file in
  state.local_path // basename

let local_of_remote_dir state remote_dir =
  let basename = Dirname.remove_prefix state.remote_path remote_dir in
  state.local_path / basename

let remote_of_local_file state local_file =
  let basename = Filename.remove_prefix state.local_path local_file in
  state.remote_path // basename

let nv_set_of_files l =
  NV.Set.of_list (Utils.filter_map NV.of_filename (Filename.Set.elements l))

module Make (Sync : SYNC) = struct

  module Init = struct

    let make state =
      (* This folder should have already been created, but be sure they are here *)
      Dirname.mkdir (Path.R.packages_dir state.local_repo);
      Dirname.mkdir (Path.R.archives_dir state.local_repo);
      Dirname.mkdir (Path.R.compilers_dir state.local_repo);
      Dirname.mkdir (Path.R.upload_dir state.local_repo)

  end

  (* Build a package archive *)
  (* - convert to the right file hierarchy (ie. the root folder should be $package.$version/)
     - convert to the right name (ie. $package.$version.tar.gz)
     - add the eventual files (ie. files/$package.$version/) *)
  module Archives = struct

    let make state t nv =
      Dirname.with_tmp_dir (fun tmp_download_dir ->
        Dirname.with_tmp_dir (fun tmp_extract_root ->
          let tmp_extract_dir = tmp_extract_root / NV.to_string nv in

          (* If the archive is there, download it directly *)
          let remote_archive = Path.R.archive state.remote_repo nv in
          match Sync.file state t remote_archive with
          | Some _ -> ()
          | None   ->
              log
                "%s is not on the server, need to build it"
                (Filename.to_string remote_archive);
              let url_f = Path.R.url state.local_repo nv in
              if Filename.exists url_f then begin
                (* if the url file is there, download the archive upstream *)
                let urls = File.URL.read url_f in
                let urls_s = String.concat " " (List.map Filename.to_string urls) in
                log "downloading %s" urls_s;
                match Filename.download_iter urls tmp_download_dir with
                | None -> Globals.error_and_exit "Cannot get %s" urls_s
                | Some local_archive ->
                    log "extracting %s to %s"
                      (Filename.to_string local_archive)
                      (Dirname.to_string tmp_extract_dir);
                    Filename.extract local_archive tmp_extract_dir;
              end;

              (* Eventually add the files/<package>/* to the extracted dir *)
              log "Adding the files to the archive";
              let files =
                let _files = Sync.dir state t (Path.R.files state.remote_repo nv) in
                Path.R.available_files state.local_repo nv in
              List.iter (fun f -> Filename.copy_in f tmp_extract_dir) files;

              (* And finally create the final archive *)
              (* XXX: ww should add a suffix to the version to show that the archive has been repacked by opam *)
              let local_archive = Path.R.archive state.local_repo nv in
              log "Creating the archive files in %s" (Filename.to_string local_archive);
              log "Files in tmp_extract_root: %s" (Filename.Set.to_string (Filename.Set.of_list (Filename.list tmp_extract_root)));
              let err = Dirname.exec tmp_extract_root [
                [ "tar" ; "czf" ; Filename.to_string local_archive ; NV.to_string nv ]
              ] in
              if err <> 0 then
                Globals.error_and_exit "Cannot compress %s" (Dirname.to_string tmp_extract_dir)
        )
      )
  end

  module Updates = struct

    let (++) = NV.Set.union

    let get_nv_updates state t dir =
      let s = Sync.dir state t dir in
      nv_set_of_files s

    let get_archives_updates state t =
      let l =
        Filename.Set.filter (fun local_file ->
          let remote_file = remote_of_local_file state local_file in
          Filename.starts_with (Path.R.archives_dir state.local_repo) local_file
          && not (Sync.same_digest state t ~local_file ~remote_file)
        ) (Path.R.available_archives state.local_repo) in
      nv_set_of_files l

    let get state t =
      let packages = get_nv_updates state t (Path.R.packages_dir state.remote_repo) in
      let archives = get_archives_updates state t in
      let _comps = Sync.dir state t (Path.R.compilers_dir state.remote_repo) in
      packages ++ archives

  end

  module Upload = struct

    let (++) = Filename.Set.union

    let upload state t =
      let local_repo = state.local_repo in
      let upload_path = Path.R.upload_dir local_repo in
      let upload_repo = Path.R.of_dirname upload_path in
      let state = { state with local_path = upload_path; local_repo = upload_repo } in
      let set d = Filename.Set.of_list (if Dirname.exists d then Filename.list d else []) in
      let files =
           set (Path.R.packages_dir upload_repo)
        ++ set (Path.R.archives_dir upload_repo)
        ++ set (Path.R.compilers_dir upload_repo) in
      Sync.upload state t (Path.R.packages_dir state.remote_repo);
      Sync.upload state t (Path.R.archives_dir state.remote_repo);
      Sync.upload state t (Path.R.compilers_dir state.remote_repo);
      files

  end

end

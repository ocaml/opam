open Types

let log msg = Globals.log "CURL" msg

type state = {
  remote_repo         : Path.R.t;
  remote_path         : dirname;
  local_repo          : Path.R.t;
  local_path          : dirname;
  index_file          : filename;
  local_index_archive : filename;
  remote_index_archive: filename;
  local_files         : Filename.Set.t;
  remote_local        : filename Filename.Map.t;
  local_remote        : filename Filename.Map.t;
  file_permissions    : (filename * int) list;
  file_digests        : (filename * string) list;
}

let make_state remote_path =
  let remote_repo = Path.R.of_dirname remote_path in
  let local_repo = Path.R.of_dirname (Dirname.cwd ()) in
  let local_path = Path.R.root local_repo in
  let index_file = remote_path // "urls.txt" in
  let local_index_archive = local_path // "index.tar.gz" in
  let remote_index_archive = remote_path // "index.tar.gz" in
  let remote_local, local_remote, local_files, file_permissions, file_digests =
    match Filename.download index_file local_path with
    | None            -> Globals.error_and_exit "Cannot get urls.txt"
    | Some local_file ->
        let urls = File.Urls_txt.read local_file in
        let remote_local, local_remote, locals, perms, digests =
          Remote_file.Set.fold (fun r (rl, lr, locals, perms, digests) ->
            let base = Remote_file.base r in
            let perm = match Remote_file.perm r with
              | None  ->  0o640
              | Some p -> p in
            let digest = Remote_file.md5 r in
            let remote = Filename.create remote_path base in
            let local = Filename.create (Dirname.cwd()) base in
            Filename.Map.add remote local rl,
            Filename.Map.add local remote lr,
            Filename.Set.add local locals,
            (local, perm) :: perms,
            (local, digest) :: digests
          ) urls (Filename.Map.empty, Filename.Map.empty, Filename.Set.empty, [], []) in
        remote_local, local_remote, locals, perms, digests in
  {
    remote_repo; remote_path; local_repo; local_path;
    index_file; local_index_archive; remote_index_archive;
    local_files; remote_local; local_remote;
    file_permissions; file_digests;
  }


let is_up_to_date state local_file =
  List.mem_assoc local_file state.file_digests
  && Filename.exists local_file
  && List.assoc local_file state.file_digests = Filename.digest local_file

module B = struct

  let init address =
    let state = make_state address in
    let warning () =
      Globals.msg "Cannot find index.tar.gz on the OPAM repository.\n\
                 Initialisation might take some time ...\n" in

    (* Download index.tar.gz *)
    try match Filename.download state.remote_index_archive state.local_path with
    | None   -> warning ()
    | Some _ ->
        (* Untar the files *)
        Filename.extract_in state.local_index_archive state.local_path
    with _ -> warning ()

  let curl ~remote_file ~local_file =
    log "dowloading %s" (Filename.to_string remote_file);
    let local_dir = Filename.dirname local_file in
    Dirname.mkdir local_dir;
    Filename.download remote_file local_dir

  let update address =
    let state = make_state address in
    log "dir local_dir=%s remote_dir=%s"
      (Dirname.to_string state.local_path)
      (Dirname.to_string state.remote_path);
    if state.local_path <> state.remote_path then begin
      let (--) = Filename.Set.diff in
      let current = Filename.Set.of_list (Filename.list state.local_path) in
      let to_keep = Filename.Set.filter (is_up_to_date state) state.local_files in
      let to_delete = current -- to_keep in
      let new_files = state.local_files -- to_keep in
      log "current: %s" (Filename.Set.to_string current);
      log "to_keep: %s" (Filename.Set.to_string to_keep);
      log "to_delete: %s" (Filename.Set.to_string to_delete);
      log "new_files: %s" (Filename.Set.to_string new_files);
      Filename.Set.iter Filename.remove to_delete;
      if Filename.Set.cardinal new_files > 4 then
        init address
      else
        Filename.Set.iter (fun local_file ->
          let remote_file = Filename.Map.find local_file state.local_remote in
          ignore (curl ~remote_file ~local_file)
        ) new_files;
      new_files
    end else
      Filename.Set.empty

  let download_archive address nv =
    let remote_repo = Path.R.of_dirname address in
    let remote_file = Path.R.archive remote_repo nv in
    let state = make_state address in
    if not (Filename.Map.mem remote_file state.remote_local) then
      Not_available
    else begin
      let local_file = Filename.Map.find remote_file state.remote_local in
      if is_up_to_date state local_file then
        Up_to_date local_file
      else begin
        log "dowloading %s" (Filename.to_string remote_file);
        let local_dir = Filename.dirname local_file in
        Dirname.mkdir local_dir;
        Globals.msg "Downloading %s ...\n" (Filename.to_string remote_file); 
        match Filename.download remote_file local_dir with
        | None -> Globals.error_and_exit "Cannot download %s" (Filename.to_string remote_file);
        | Some local_file ->
            if not (Filename.exists local_file) then
              (* This may happen with empty files *)
              Filename.touch local_file;
            begin
              try
                let perm = List.assoc local_file state.file_permissions in
                Filename.chmod local_file perm
              with Not_found ->
                ()
            end;
            Result local_file
      end
    end

  (* XXX: use checksums *)
  let download_file nv remote_file =
    let local_repo = Path.R.cwd () in
    let dest_dir = Path.R.tmp_dir local_repo nv in
    Globals.msg "Downloading %s ...\n" (Filename.to_string remote_file);
    match Filename.download remote_file dest_dir with
    | None   -> Not_available
    | Some f -> Result f

  let not_supported action =
    failwith (action ^ ": not supported by CURL backend")

  let download_dir nv dir =
    not_supported ("Downloading " ^ Dirname.to_string dir)

  let upload_dir ~address remote_dir =
    not_supported ("Uploading to " ^ Dirname.to_string remote_dir)

end

let make_urls_txt local_repo =
  let local_index_file = Filename.of_string "urls.txt" in
  let index = Remote_file.Set.of_list (List.map (fun f ->
    let basename =
      Basename.of_string (Filename.remove_prefix ~prefix:(Dirname.cwd()) f) in
    let perm =
      let s = Unix.stat (Filename.to_string f) in
      s.Unix.st_perm in
    let digest =
      Digest.to_hex (Digest.file (Filename.to_string f)) in
    Remote_file.create basename digest perm
  ) (Filename.rec_list (Path.R.packages_dir local_repo)
   @ Filename.list (Path.R.archives_dir local_repo)
   @ Filename.list (Path.R.compilers_dir local_repo)
  )) in
  File.Urls_txt.write local_index_file index;
  index

let make_index_tar_gz local_repo =
  Dirname.in_dir (Path.R.root local_repo) (fun () ->
    let dirs = [ "compilers"; "packages" ] in
    let dirs = List.filter Sys.file_exists dirs in
    let err = Run.command [
      "sh"; "-c"; "tar cz " ^ (String.concat " " dirs) ^ "> index.tar.gz"
    ] in
    if err <> 0 then
      Globals.error_and_exit "Cannot create index.tar.gz";
  )
    
let () =
  Repositories.register_backend "curl" (module B : Repositories.BACKEND)

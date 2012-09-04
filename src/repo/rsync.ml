let (+) = Filename.concat
open Types

let log fmt = Globals.log "RSYNC" fmt

let rsync ?(delete=true) src dst =
  log "rsync: delete:%b src:%s dst:%s" delete src dst;
  if src <> dst then (
    Run.mkdir src;
    Run.mkdir dst;
    let delete = if delete then ["--delete"] else [] in
    match
      Run.read_command_output (["rsync" ; "-arv"; src; dst] @ delete)
    with
    | None   -> Not_available
    | Some l -> match Utils.rsync_trim l with
      | []    -> Up_to_date []
      | lines ->
          List.iter (fun f -> log "updated: %s %s" (Run.cwd ()) f) lines;
          Result lines
  ) else
    Up_to_date []

let rsync_dirs ?delete src dst =
  let src_s = Dirname.to_string src + "" in
  let dst_s = Dirname.to_string dst in
  let dst_files0 = Filename.rec_list dst in
  match rsync ?delete src_s dst_s with
  | Not_available -> Not_available
  | Up_to_date _  -> Up_to_date []
  | Result lines  ->
      let src_files = Filename.rec_list src in
      let dst_files = Filename.rec_list dst in
      if delete = Some true && List.length src_files <> List.length dst_files then (
        List.iter (fun f -> Globals.msg "src-file: %s\n" (Filename.to_string f)) src_files;
        List.iter (fun f -> Globals.msg "dst-file0: %s\n" (Filename.to_string f)) dst_files0;
        List.iter (fun f -> Globals.msg "dst-file: %s\n" (Filename.to_string f)) dst_files;
        Globals.error_and_exit "rsync_dir failed!"
      );
      Result lines

let rsync_file src dst =
  log "rsync_file src=%s dst=%s" (Filename.to_string src) (Filename.to_string dst);
  match
    Run.read_command_output [
      "rsync"; "-av"; Filename.to_string src; Filename.to_string dst;
    ]
  with
  | None   -> Not_available
  | Some l -> match Utils.rsync_trim l with
    | []  -> Up_to_date dst
    | [x] -> Result dst
    | l   ->
        Globals.error_and_exit
          "unknown rsync output: {%s}"
          (String.concat ", " l)

module B = struct

  let init r = ()

  let download_file nv remote_file =
    let local_repo = Path.R.of_dirname (Dirname.cwd ()) in
    let tmp_dir = Path.R.tmp_dir local_repo nv in
    let local_file = Filename.create tmp_dir (Filename.basename remote_file) in
    rsync_file remote_file local_file

  let download_dir nv remote_dir =
    let local_repo = Path.R.of_dirname (Dirname.cwd ()) in
    let tmp_dir = Path.R.tmp_dir local_repo nv in
    let local_dir = tmp_dir / Basename.to_string (Dirname.basename remote_dir) in
    match rsync_dirs ~delete:true remote_dir local_dir with
    | Up_to_date _  -> Up_to_date local_dir
    | Result _      -> Result local_dir
    | Not_available -> Not_available

  let download_archive address nv =
    let remote_repo = Path.R.of_dirname address in
    let remote_file = Path.R.archive remote_repo nv in
    let local_repo = Path.R.of_dirname (Dirname.cwd ()) in
    let local_file = Path.R.archive local_repo nv in
    rsync_file remote_file local_file

  let update address =
    let remote_repo = Path.R.of_dirname address in
    let local_repo = Path.R.of_dirname (Dirname.cwd ()) in
    let sync_dir fn =
      match rsync_dirs ~delete:true (fn remote_repo) (fn local_repo) with
      | Not_available
      | Up_to_date _ -> Filename.Set.empty
      | Result lines ->
          let files = List.map Filename.of_string lines in
          Filename.Set.of_list files in
    let archives =
      let available_packages = Path.R.available_packages local_repo in
      let updates = NV.Set.filter (fun nv ->
        let archive = Path.R.archive local_repo nv in
        if not (Filename.exists archive) then
          false
        else match download_archive address nv with
        | Not_available -> true
        | Up_to_date _  -> false
        | Result _      -> true
      ) available_packages in
      List.map (Path.R.archive local_repo) (NV.Set.elements updates) in
    let (++) = Filename.Set.union in
    let updates = Filename.Set.of_list archives
    ++ sync_dir Path.R.packages_dir
    ++ sync_dir Path.R.compilers_dir in
    updates

  let upload_dir ~address local_dir =
    let remote_repo = Path.R.of_dirname address in
    let remote_dir = Path.R.root remote_repo in
    (* we assume that rsync is only used locally *)
    if Dirname.exists (Dirname.dirname remote_dir)
    && not (Dirname.exists remote_dir) then
      Dirname.mkdir remote_dir;
    if Dirname.exists local_dir then
      match rsync_dirs ~delete:false local_dir remote_dir with
      | Not_available ->
          Globals.error_and_exit "Cannot upload %s to %s"
            (Dirname.to_string local_dir)
            (Dirname.to_string address)
      | Up_to_date _ -> Filename.Set.empty
      | Result lines ->
          let files = Filename.rec_list local_dir in
          Filename.Set.of_list files
    else
      Filename.Set.empty

end    

let () =
  Repositories.register_backend "rsync" (module B: Repositories.BACKEND)

open Types

let log fmt = Globals.log "GIT" fmt

let git_fetch local_path =
  Dirname.in_dir local_path (fun () ->
    let err = Run.command [ "git" ; "fetch" ; "origin" ] in
    if err <> 0 then
      Globals.error_and_exit
        "Cannot fetch git repository %s"
        (Dirname.to_string local_path)
  )

let git_merge local_path =
  Dirname.in_dir local_path (fun () ->
      let err = Run.command [ "git" ; "merge" ; "origin/master" ] in
      if err <> 0 then
        Globals.error_and_exit
          "Cannot update git repository %s"
          (Dirname.to_string local_path)
    )

(* Return the list of modified files of the git repository located
   at [dirname] *)
let git_diff local_path =
  Dirname.in_dir local_path (fun () ->
    match
      Run.read_command_output
        [ "git" ; "diff" ; "remotes/origin/master" ; "--name-only" ]
    with
    | Some fs -> Filename.Set.of_list (List.map Filename.of_string fs)
    | None    ->
        Globals.error_and_exit
          "Cannot diff git repository %s"
          (Dirname.to_string local_path)
  )

let git_init address =
  let repo = Dirname.to_string address in
  let err =
    Run.commands [
      [ "git" ; "init" ] ;
      [ "git" ; "remote" ; "add" ; "origin" ; repo ] ;
    ] in
  if err <> 0 then
  Globals.error_and_exit "Cannot clone %s" repo

let check_updates local_path =
  if Dirname.exists (local_path / ".git") then begin
    git_fetch local_path;
    let files = git_diff local_path in
    git_merge local_path;
    Some files
  end else
    None

module B = struct

  let updates r =
    Path.R.root r // "last-git-updates"

  let check_file file =
    let local_repo = Path.R.cwd () in
    let updates = File.Filenames.read (updates local_repo) in
    if Filename.Set.mem file updates then
      Result file
    else if Filename.exists file then
      Up_to_date file
    else
      Not_available

  let init address =
    let local_repo = Path.R.cwd () in
    git_init address;
    File.Filenames.write (updates local_repo) (Filename.Set.empty)

  let download_archive address nv =
    let local_repo = Path.R.cwd () in
    let archive = Path.R.archive local_repo nv in
    check_file archive

  let download_file nv filename =
    let local_repo = Path.R.cwd () in
    let basename = Filename.basename filename in
    let file = Path.R.tmp_dir local_repo nv // Basename.to_string basename in
    check_file file
      
  let rec download_dir nv dirname =
    let local_repo = Path.R.cwd () in
    let basename = Dirname.basename dirname in
    let dir = Path.R.tmp_dir local_repo nv / Basename.to_string basename in
    match check_updates dir with
    | None ->
        Dirname.mkdir dir;
        Dirname.in_dir dir (fun () -> git_init dirname);
        download_dir nv dirname
    | Some f ->
        if Filename.Set.empty = f then
          Up_to_date dir
        else
          Result dir
      
  let update address =
    let local_path = Dirname.cwd () in
    match check_updates local_path with
    | Some f -> f
    | None   ->
        Globals.error_and_exit
          "The repository %s is not initialized correctly"
          (Dirname.to_string local_path)

  let upload_dir ~address dirname =
    let files = Filename.rec_list dirname in
    let err = Run.commands [
      [ "git"; "add"; Dirname.to_string dirname; ];
      [ "git"; "commit"; "-a"; "-m"; "upload new files" ];
      [ "git"; "push"; "origin"; "master" ]
    ] in
    if err = 0 then
      Filename.Set.of_list files
    else
      Filename.Set.empty

end

let () =
  Repositories.register_backend "git" (module B: Repositories.BACKEND)

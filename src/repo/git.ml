open Types

let log fmt = Globals.log "git" fmt

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

let git_init local_path remote_path =
  Dirname.mkdir local_path;
  Dirname.in_dir local_path (fun () ->
    let repo = Dirname.to_string remote_path in
    let err =
      Run.commands [
        [ "git" ; "init" ] ;
        [ "git" ; "remote" ; "add" ; "origin" ; repo ] ;
      ] in
    if err <> 0 then
      Globals.error_and_exit "Cannot clone %s" repo
  )

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

  let init r =
    let local_repo = Path.R.create r in
    git_init (Path.R.root local_repo) (Repository.address r);
    File.Filenames.write (updates local_repo) (Filename.Set.empty)

  let check_file r file =
    let local_repo = Path.R.create r in
    let updates = File.Filenames.read (updates local_repo) in
    if Filename.Set.mem file updates then
      Result file
    else if Filename.exists file then
      Up_to_date file
    else
      Not_available

  let download_archive r nv =
    let local_repo = Path.R.create r in
    let archive = Path.R.archive local_repo nv in
    check_file r archive

  let download_file r nv filename =
    let local_repo = Path.R.create r in
    let basename = Filename.basename filename in
    let file = Path.R.tmp_dir local_repo nv // Basename.to_string basename in
    check_file r file
      
  let rec download_dir r nv dirname =
    let local_repo = Path.R.create r in
    let basename = Dirname.basename dirname in
    let dir = Path.R.tmp_dir local_repo nv / Basename.to_string basename in
    match check_updates dir with
    | None ->
        git_init dir dirname;
        download_dir r nv dirname
    | Some f ->
        if Filename.Set.empty = f then
          Up_to_date dir
        else
          Result dir
      
  let update r =
    let local_path = Path.R.root (Path.R.create r) in
    match check_updates local_path with
    | Some f -> f
    | None   ->
        Globals.error_and_exit
          "The repository %s is not initialized correctly"
          (Repository.to_string r)

  let upload_dir state dirname =
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

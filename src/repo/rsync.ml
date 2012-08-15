let (+) = Filename.concat
open Types

let log fmt = Globals.log "rsync" fmt

(* if rsync -arv return 4 lines, this means that no files have changed *)
let trim = function
  | [] -> []
  | _ :: t ->
      match List.rev t with
      | _ :: _ :: _ :: l -> l
      | _ -> []

let rsync ?(delete=true) src dst =
  log "rsync: delete:%b src:%s dst:%s" delete src dst;
  Run.mkdir src;
  Run.mkdir dst;
  let delete = if delete then ["--delete"] else [] in
  match
    Run.read_command_output (["rsync" ; "-arv"; src; dst] @ delete)
  with
  | None   -> Not_available
  | Some l -> match trim l with
    | []    -> Up_to_date []
    | lines ->
        List.iter (fun f -> log "updated: %s %s" (Run.cwd ()) f) lines;
        Result lines

let rsync_dirs ?delete src dst =
  let src_s = Dirname.to_string src + "" in
  let dst_s = Dirname.to_string dst in
  let dst_files0 = Filename.rec_list dst in
  match rsync ?delete src_s dst_s with
  | Not_available -> Not_available
  | Up_to_date _  -> Up_to_date dst
  | Result lines  ->
      let src_files = Filename.rec_list src in
      let dst_files = Filename.rec_list dst in
      if delete = Some true && List.length src_files <> List.length dst_files then (
        List.iter (fun f -> Globals.msg "src-file: %s\n" (Filename.to_string f)) src_files;
        List.iter (fun f -> Globals.msg "dst-file0: %s\n" (Filename.to_string f)) dst_files0;
        List.iter (fun f -> Globals.msg "dst-file: %s\n" (Filename.to_string f)) dst_files;
        Globals.error_and_exit "rsync_dir failed!"
      );
      Result dst

let rsync_file src dst =
  match
    Run.read_command_output [
      "rsync"; "-av"; Filename.to_string src; Filename.to_string dst;
    ]
  with
  | None   -> Not_available
  | Some l -> match trim l with
    | []  -> Up_to_date dst
    | [x] -> assert (Filename.to_string dst = x); Result dst
    | l   ->
        Globals.error_and_exit
          "unknown rsync output: {%s}"
          (String.concat ", " l)

module B = struct

  let init r = ()

  let download_file r nv remote_file =
    let local_repo = Path.R.create r in
    let tmp_dir = Path.R.tmp_dir local_repo nv in
    let local_file = Filename.create tmp_dir (Filename.basename remote_file) in
    rsync_file remote_file local_file

  let download_dir r nv remote_dir =
    let local_repo = Path.R.create r in
    let tmp_dir = Path.R.tmp_dir local_repo nv in
    let local_dir = tmp_dir / Basename.to_string (Dirname.basename remote_dir) in
    rsync_dirs ~delete:true remote_dir local_dir

  let download_archive r nv =
    let remote_repo = Path.R.of_dirname (Repository.address r) in
    let remote_file = Path.R.archive remote_repo nv in
    let local_repo = Path.R.create r in
    let local_file = Path.R.archive local_repo nv in
    rsync_file remote_file local_file


  let update r =
    let remote_repo = Path.R.of_dirname (Repository.address r) in
    let local_repo = Path.R.create r in
    let sync_dir fn =
      match rsync_dirs ~delete:true (fn remote_repo) (fn local_repo) with
      | Not_available
      | Up_to_date _ -> Filename.Set.empty
      | Result dir   ->
          let files = Filename.rec_list dir in
          Filename.Set.of_list files in
    let archives =
      let available_packages = Path.R.available_packages local_repo in
      let updates = NV.Set.filter (fun nv ->
        match download_archive r nv with
        | Not_available -> true
        | Up_to_date _  -> false
        | Result _      -> true
      ) available_packages in
      List.map (Path.R.archive local_repo) (NV.Set.elements updates) in
    let (++) = Filename.Set.union in
    Filename.Set.of_list archives
    ++ sync_dir Path.R.packages_dir
    ++ sync_dir Path.R.compilers_dir

  let upload_dir r local_dir =
    let remote_repo = Path.R.of_dirname (Repository.address r) in
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
            (Repository.to_string r)
      | Up_to_date _ -> Filename.Set.empty
      | Result dir   ->
          let files = Filename.rec_list dir in
          Filename.Set.of_list files
    else
      Filename.Set.empty

end    

let () =
  Repositories.register_backend "rsync" (module B: Repositories.BACKEND)

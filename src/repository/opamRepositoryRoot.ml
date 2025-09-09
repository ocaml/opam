module Dir = struct
  type t = OpamFilename.Dir.t

  let of_dir = Fun.id
  let to_dir = Fun.id
  let to_string = OpamFilename.Dir.to_string

  let quarantine repo_root = OpamFilename.raw_dir (to_string repo_root ^ ".new")
  let with_tmp = OpamFilename.with_tmp_dir
  let backup ~tmp_dir repo_root =
    OpamFilename.Op.(tmp_dir / OpamFilename.Base.to_string (OpamFilename.basename_dir repo_root))

  let in_dir = OpamFilename.in_dir
  let exists = OpamFilename.exists_dir
  let remove = OpamFilename.rmdir
  let clean = OpamFilename.cleandir
  let move = OpamFilename.move_dir
  let copy = OpamFilename.copy_dir
  let copy_except_vcs = OpamFilename.copy_dir_except_vcs
  let is_symlink = OpamFilename.is_symlink_dir
  let patch = OpamFilename.patch
  let make = OpamFilename.mkdir
  let dirs = OpamFilename.dirs
  let is_empty = OpamFilename.dir_is_empty
  let dirname = OpamFilename.dirname_dir

  let repo repo_root = OpamFilename.Op.(repo_root // "repo" |> OpamFile.make)
end

let make_tar_gz_job = OpamFilename.make_tar_gz_job
let extract_in_job = OpamFilename.extract_in_job

type t =
  | Dir of Dir.t

let quarantine = function
  | Dir dir -> Dir (Dir.quarantine dir)

let remove = function
  | Dir dir -> Dir.remove dir

let exists = function
  | Dir dir -> Dir.exists dir

let is_empty = function
  | Dir dir -> Dir.is_empty dir

let make = function
  | Dir dir -> Dir.make dir

let dirname = function
  | Dir dir -> OpamFilename.dirname_dir (Dir.to_dir dir)

let basename = function
  | Dir dir -> OpamFilename.basename_dir (Dir.to_dir dir)

let to_string = function
  | Dir dir -> Dir.to_string dir

let copy ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.copy ~src ~dst

let move ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.move ~src ~dst

let is_symlink = function
  | Dir dir -> Dir.is_symlink dir

let patch ?preprocess ~allow_unclean patch = function
  | Dir dir -> Dir.patch ?preprocess ~allow_unclean patch dir

let clean = function
  | Dir dir -> Dir.clean dir

let delayed_read_repo = function
  | Dir dir ->
    let repo_file_path = Dir.repo dir in
    let read () = OpamFile.Repo.safe_read repo_file_path in
    (OpamFile.exists repo_file_path, read)

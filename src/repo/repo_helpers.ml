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

module type REPO = sig
  val sync  : state -> Filename.Set.t
  val upload: state -> dirname -> Filename.Set.t
end

module Make (R : REPO) = struct

  let nv_set_of_files l =
    NV.Set.of_list (Utils.filter_map NV.of_filename (Filename.Set.elements l))

  let (++) = Filename.Set.union

  let get_updates state =
    let new_files = R.sync state in
    let updates local_dir =
      Filename.Set.filter (fun local_file ->
        Filename.starts_with (local_dir state.local_repo) local_file
      ) new_files in
    let files =
         updates Path.R.archives_dir
      ++ updates Path.R.packages_dir in
    nv_set_of_files files

  let upload state =
    let local_repo = state.local_repo in
    let upload_path = Path.R.upload_dir local_repo in
    let upload_repo = Path.R.of_dirname upload_path in
    let state = { state with local_path = upload_path; local_repo = upload_repo } in
    let upload fn = R.upload state (fn state.remote_repo) in
    let files =
         upload Path.R.packages_dir
      ++ upload Path.R.archives_dir
      ++ upload Path.R.compilers_dir in
    nv_set_of_files files

end

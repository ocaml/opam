open Types
open Repo_helpers

let log fmt = Globals.log "git" fmt

type state = {
  git_root: dirname;
  git_dir : NV.t -> dirname;
  diffs   : Filename.Set.t;
}

let git_fetch dirname =
  Dirname.in_dir dirname (fun () ->
    let err = Run.command [ "git" ; "fetch" ; "origin" ] in
    if err <> 0 then
      Globals.error_and_exit
        "Cannot fetch git repository %s"
        (Dirname.to_string dirname)
  )

let git_merge dirname =
  Dirname.in_dir dirname (fun () ->
      let err = Run.command [ "git" ; "merge" ; "origin/master" ] in
      if err <> 0 then
        Globals.error_and_exit
          "Cannot update git repository %s"
          (Dirname.to_string dirname)
    )

(* Return the list of modified files of the git repository located
   at [dirname] *)
let get_diff t dirname =
  Dirname.in_dir dirname (fun () ->
    match
      Run.read_command_output
        [ "git" ; "diff" ; "remotes/origin/master" ; "--name-only" ]
    with
    | Some fs -> Filename.Set.of_list (List.map ((//) t.remote_path) fs)
    | None    ->
        Globals.error_and_exit
          "Cannot diff git repository %s"
          (Dirname.to_string dirname)
  )

let make_state fetch t =
  log "make_state fetch=%b" fetch;
  let git_root = t.local_path / "git" in
  let git_dir nv = git_root / (NV.to_string nv) in
  let diffs =
    if Dirname.exists (t.local_path / ".git") then begin
      if fetch then git_fetch t.local_path;
      get_diff t t.local_path
    end else
      Filename.Set.empty in
  { git_root; git_dir; diffs }

module Sync = struct

  type t = state

  let update state =
    if not (Filename.Set.is_empty (get_diff state state.local_path)) then
      git_merge state.local_path

  let file state t filename =
    update state;
    let local_file = Repo_helpers.local_of_remote_file state filename in
    if Filename.exists local_file then
      Some local_file
    else
      None

  let dir state t dirname =
    update state;
    let local_dir = Repo_helpers.local_of_remote_dir state dirname in
    if Dirname.exists local_dir then
      Filename.Set.of_list (Filename.rec_list local_dir)
    else
      Filename.Set.empty

  let same_digest state t ~local_file ~remote_file =
    true

  let upload state t =
    assert false

end

module M = Repo_helpers.Make(Sync)
include M

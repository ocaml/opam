open Types
open Repo_helpers

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
    | Some fs -> fs
    | None    ->
        Globals.error_and_exit
          "Cannot diff git repository %s"
          (Dirname.to_string local_path)
  )

let remote_diff state =
  let fs = git_diff state.local_path in
  Filename.Set.of_list (List.map ((//) state.remote_path) fs)

module Repo = struct

  (* The list of modified files *)
  type t = Filename.Set.t

  let make state =
    log "make_state";
    if Dirname.exists (state.local_path / ".git") then begin
      git_fetch state.local_path;
      remote_diff state;
    end else
      Filename.Set.empty

  let sync state =
    let diff = make state in
    if not (Filename.Set.is_empty diff) then
      git_merge state.local_path;
    diff

  let upload state dirname =
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

module M = Repo_helpers.Make(Repo)
include M

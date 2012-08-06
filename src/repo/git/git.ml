open Types
open Repo_helpers

let log fmt = Globals.log "git" fmt

let git_fetch state =
  Dirname.in_dir state.local_path (fun () ->
    let err = Run.command [ "git" ; "fetch" ; "origin" ] in
    if err <> 0 then
      Globals.error_and_exit
        "Cannot fetch git repository %s"
        (Dirname.to_string state.local_path)
  )

let git_merge state =
  Dirname.in_dir state.local_path (fun () ->
      let err = Run.command [ "git" ; "merge" ; "origin/master" ] in
      if err <> 0 then
        Globals.error_and_exit
          "Cannot update git repository %s"
          (Dirname.to_string state.local_path)
    )

(* Return the list of modified files of the git repository located
   at [dirname] *)
let get_diff state =
  Dirname.in_dir state.local_path (fun () ->
    match
      Run.read_command_output
        [ "git" ; "diff" ; "remotes/origin/master" ; "--name-only" ]
    with
    | Some fs -> Filename.Set.of_list (List.map ((//) state.remote_path) fs)
    | None    ->
        Globals.error_and_exit
          "Cannot diff git repository %s"
          (Dirname.to_string state.local_path)
  )

module Repo = struct

  (* The list of modified files *)
  type t = Filename.Set.t

  let make state =
    log "make_state";
    if Dirname.exists (state.local_path / ".git") then begin
      git_fetch state;
      get_diff state;
    end else
      Filename.Set.empty

  let sync state =
    let diff = make state in
    if not (Filename.Set.is_empty diff) then
      git_merge state;
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

(* Update script for git repositories *)

(* The update script:
   - pull the main repo to see if some new packages are available
   - pull each git sub-repo to see if the package has been updated
*)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: opam-git-init <remote-address>";
    exit 1;
  )

let local_path = Run.cwd ()
let remote_address = Sys.argv.(1)
let repositories = Filename.concat local_path "repositories"

(* Return the list of modified files of the git repository located
   at [dirname] *)
let get_updates dirname =
  Run.in_dir dirname (fun () ->
    let err = Run.command "git fetch origin" in
    if err = 0 then
      Run.read_command_output "git diff remotes/origin/master --name-only"
    else
      Globals.error_and_exit "Cannot fetch git repository %s" dirname
  )

(* Update the git repository located at [dirname] *)
let update dirname =
  Run.in_dir dirname (fun () ->
    let err = Run.command "git pull origin master" in
    if err <> 0 then
      Globals.error_and_exit "Cannot update git repository %s" dirname
  )

let needs_update dirname =
  get_updates dirname <> []

open Types

let () =
  (* Look at new packages *)
  (* XXX: do something if a file related to an already cloned sub-tree is modified *)
  let repo_updates = get_updates local_path in
  let repo_updates =
    Utils.filter_map (fun f -> NV.of_filename (Filename.of_string f)) repo_updates in
  let repo_updates = List.fold_right NV.Set.add repo_updates NV.Set.empty in

  (* Look at already cloned packages *)
  let dirs = Run.directories repositories in
  let updates = List.filter needs_update dirs in
  let updates = Utils.filter_map (fun d -> NV.of_dirname (Dirname.of_string d)) updates in
  let updates = List.fold_right NV.Set.add updates NV.Set.empty in

  (* Write $opam/repo/$repo/updated *)
  File.Updated.write
    (Path.R.updated (Path.R.of_path (Dirname.of_string local_path)))
    (NV.Set.union repo_updates updates)

    
    

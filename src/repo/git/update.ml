(* Update script for git repositories *)

(* The update script:
   - pull the main repo to see if some new packages are available
   - pull each git sub-repo to see if the package has been updated
*)

open Types
open Repo_helpers

open Types

let () =
  let state = Repo_helpers.make_state () in
  let updates = Git.get_updates state in
  File.Updated.write (Path.R.updated state.local_repo) updates

(*
  (* Look at new packages *)
  (* re-clone the repository if the url has changed *)
  let url_updates =
    Filename.Set.filter (fun f ->
      Filename.basename f = Basename.of_string "url"
    ) repo_updates in
  let url_updates =
    Utils.filter_map (fun url ->
      if Sys.file_exists url then begin
        let package = Stdlib_filename.basename url in
        Run.remove_dir (Stdlib_filename.concat "git" package);
        let err = Run.command ["opam-git-download"; remote_address; package ] in
        if err <> 0 then begin
          Globals.error "Cannot download package %s" package;
          exit err
        end;
        Some (NV.of_string package)
      end else
        None
    ) url_updates in
  let url_updates = NV.Set.of_list url_updates in

  let repo_updates =
    Utils.filter_map (fun f -> NV.of_filename (Filename.of_string f)) repo_updates in
  let repo_updates = NV.Set.of_list repo_updates in
  update local_path;

  (* Look at already cloned packages *)
  let dirs = Run.directories_with_links repositories in
  let updates = List.filter needs_update dirs in
  let updates =
    Utils.filter_map (fun d ->
      match NV.of_dirname (Dirname.of_string d) with
      | None    -> None
      | Some nv -> update d; Some nv
    ) updates in
  let updates = List.fold_right NV.Set.add updates NV.Set.empty in

  (* Write $opam/repo/$repo/updated *)
  File.Updated.write
    (Path.R.updated (Path.R.of_dirname (Dirname.of_string local_path)))
    (repo_updates ++ url_updates ++ updates)
*)

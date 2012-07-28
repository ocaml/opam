let script_name = Filename.basename Sys.argv.(0)

open Types
open Repo_helpers

let log msg = Globals.log script_name msg

type state = {
  index_file          : filename;
  local_index_archive : filename;
  remote_index_archive: filename;
  remote_files        : Filename.Set.t;
  file_permissions    : (filename * int) list;
  file_digests        : (filename * string) list;
}

let make_state state =
  let index_file = state.remote_path // "urls.txt" in
  let local_index_archive = state.local_path // "index.tar.gz" in
  let remote_index_archive = state.remote_path // "index.tar.gz" in
  let remote_files, file_permissions, file_digests =
    match Filename.download index_file state.local_path with
    | None            -> Globals.error_and_exit "Cannot get urls.txt"
    | Some local_file ->
        let urls = File.Urls_txt.read local_file in
        let files, perms, digests =
          List.fold_left (fun (files, perms, digests) (base,perm,digest) ->
            let file = Filename.create (Dirname.cwd()) base in
            Filename.Set.add file files,
            (file,perm) :: perms,
            (file,digest) :: digests
          ) (Filename.Set.empty, [], []) urls in
        files, perms, digests in
  {
    index_file; local_index_archive; remote_index_archive;
    remote_files; file_permissions; file_digests;
  }

module Sync = struct

  type t = state

  (* all the local files which mirror a remote file *)
  let active_local_files state t =
    Filename.Set.map (local_of_remote_file state) t.remote_files

  let same_digest _ t ~local_file ~remote_file =
    List.mem_assoc remote_file t.file_digests
    && Filename.exists local_file
    && List.assoc remote_file t.file_digests = Filename.digest local_file

  let file state t remote_file =
    if not (Filename.Set.mem remote_file t.remote_files) then
    None
    else begin
      let local_file = local_of_remote_file state remote_file in
      if same_digest state t ~local_file ~remote_file then
        (* Do not overwrite the file if it is already there, with the right contents *)
        Some (local_file, false)
      else begin
        log "dowloading %s" (Filename.to_string remote_file);
        let local_dir = Filename.dirname local_file in
        Dirname.mkdir local_dir;
        match Filename.download remote_file local_dir with
        | None -> Globals.error_and_exit "Cannot download %s" (Filename.to_string remote_file);
        | Some local_file ->
            if not (Filename.exists local_file) then
              (* This may happen with empty files *)
            Filename.touch local_file;
            begin
              try
                let perm = List.assoc remote_file t.file_permissions in
                Filename.chmod local_file perm
              with Not_found ->
                ()
            end;
            Some (local_file, true)
    end
  end

  (* sync remote_dir with the corresponding local_dir*)
  let dir state t remote_dir =
    let local_dir = local_of_remote_dir state remote_dir in
    log "dir local_dir=%s remote_dir=%s"
      (Dirname.to_string local_dir)
      (Dirname.to_string remote_dir);
    if local_dir <> remote_dir then begin
      let current = Filename.Set.of_list (Filename.list local_dir) in
      log "current: %s" (Filename.Set.to_string current);
      let to_keep = Filename.Set.filter (fun local_file ->
        let remote_file = remote_of_local_file state local_file in
        Filename.starts_with local_dir local_file
        && same_digest state t ~local_file ~remote_file
      ) (active_local_files state t) in
      log "to_keep: %s" (Filename.Set.to_string to_keep);
      let to_delete = Filename.Set.diff current to_keep in
      log "to_delete: %s" (Filename.Set.to_string to_delete);
      Filename.Set.iter Filename.remove to_delete;
      Filename.Set.filter (fun f ->
        if Filename.starts_with remote_dir f then begin
          match file state t f with
          | Some _ -> true
          | None   -> false
        end else
        false
      ) t.remote_files
    end else
      Filename.Set.empty

  let upload state t remote_dir =
    Globals.error_and_exit "Upload is not available for CURL backends"

end

module M = Make(Sync)
include M

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStd.Op
open OpamStateTypes

let log fmt = OpamConsole.log "RSTATE" fmt
let slog = OpamConsole.slog

module Cache = struct
  type t = {
    cached_repofiles: (repository_name * OpamFile.Repo.t) list;
    cached_opams: (repository_name * OpamFile.OPAM.t OpamPackage.Map.t) list;
  }

  module C = OpamCached.Make (struct
      type nonrec t = t
      let name = "repository"
    end)

  let remove () =
    let root = OpamStateConfig.(!r.root_dir) in
    let cache_dir = OpamPath.state_cache_dir root in
    let remove_cache_file file =
      if OpamFilename.check_suffix file ".cache" then
        OpamFilename.remove file
    in
    List.iter remove_cache_file (OpamFilename.files cache_dir)

  let marshall rt =
    (* Repository without remote are not cached, they are intended to be
       manually edited *)
    let filter_out_nourl repos_map =
      OpamRepositoryName.Map.filter
        (fun name _ ->
           try
             (OpamRepositoryName.Map.find name rt.repositories).repo_url <>
             OpamUrl.empty
           with Not_found -> false)
        repos_map
    in
      { cached_repofiles =
          OpamRepositoryName.Map.bindings
            (filter_out_nourl rt.repos_definitions);
        cached_opams =
          OpamRepositoryName.Map.bindings
            (filter_out_nourl rt.repo_opams);
      }

  let file rt =
    OpamPath.state_cache rt.repos_global.root

  let save rt =
    remove ();
    C.save (file rt) (marshall rt)

  let save_new rt =
    C.save (file rt) (marshall rt)

  let load root =
    let file = OpamPath.state_cache root in
    match C.load file with
    | Some cache ->
      Some
        (OpamRepositoryName.Map.of_list cache.cached_repofiles,
         OpamRepositoryName.Map.of_list cache.cached_opams)
    | None -> None

end

module Thread_pool : sig
  type t
  val create : unit -> t
  val async : t -> (unit -> (OpamPackage.t * OpamFile.OPAM.t) option) -> unit
  val value : t -> OpamFile.OPAM.t OpamPackage.Map.t
end = struct
  type t = {
    kill : bool Atomic.t;
    threads : unit Domain.t list;
    tasks_mutex : Mutex.t;
    tasks : (unit -> (OpamPackage.t * OpamFile.OPAM.t) option) Queue.t;
    value_mutex : Mutex.t;
    value : OpamFile.OPAM.t OpamPackage.Map.t ref;
  }

  let create () =
    let kill = Atomic.make false in
    let tasks_mutex = Mutex.create () in
    let tasks = Queue.create () in
    let value_mutex = Mutex.create () in
    let value = ref OpamPackage.Map.empty in
    let aux () =
      Domain.spawn (fun () ->
          while not (Atomic.get kill && Mutex.protect tasks_mutex (fun () -> Queue.is_empty tasks)) do
            Mutex.protect tasks_mutex (fun () ->
                Queue.take_opt tasks
              ) |>
            Option.iter (fun task ->
                Option.iter (fun (k, v) ->
                    Mutex.protect value_mutex (fun () ->
                        value := OpamPackage.Map.add k v !value;
                      )
                  ) (task ())
              )
          done
        )
    in
    let max_jobs = Int.max (Domain.recommended_domain_count () - 1) 1 in
    log (fun fmt -> fmt "Spawning %d threads" max_jobs);
    let threads = List.init max_jobs (fun _ -> aux ()) in
    {kill; threads; tasks_mutex; tasks; value_mutex; value}

  let async {tasks_mutex; tasks; _} f =
    Mutex.protect tasks_mutex (fun () ->
        Queue.add f tasks;
      )

  let value {kill; threads; value_mutex; value; _} =
    log (fun fmt -> fmt "Getting all the values...");
    Atomic.set kill true;
    List.iter Domain.join threads;
    Mutex.protect value_mutex (fun () ->
        log (fun fmt -> fmt "Got %d values" (OpamPackage.Map.cardinal !value));
        !value
      )
end

let load_opams_from_dir repo_name repo_root =
  let thread_pool = Thread_pool.create () in
  (* FIXME: why is this different from OpamPackage.list ? *)
  let rec aux dir =
    if OpamFilename.exists_dir dir then
      let fnames = Sys.readdir (OpamFilename.Dir.to_string dir) in
      if Array.exists (fun f -> f = "opam") fnames then
        Thread_pool.async thread_pool @@ fun () ->
        match OpamFileTools.read_repo_opam ~repo_name ~repo_root dir with
        | Some opam ->
          (try
             let nv =
               OpamPackage.of_string
                 OpamFilename.(Base.to_string (basename_dir dir))
             in
             Some (nv, opam)
           with Failure _ ->
             log (fun fmt ->
                 fmt "ERR: directory name not a valid package: ignored %s"
                   OpamFilename.(to_string Op.(dir // "opam")));
             None)
        | None ->
          log (fun fmt ->
              fmt "ERR: Could not load %s, ignored"
                OpamFilename.(to_string Op.(dir // "opam")));
          None
      else
        Array.iter (fun name -> aux OpamFilename.Op.(dir / name)) fnames
    else ()
  in
  aux (OpamRepositoryPath.packages_dir repo_root);
  Thread_pool.value thread_pool

let load_repo repo repo_root =
  let t = OpamConsole.timer () in
  let repo_def =
    OpamFile.Repo.safe_read (OpamRepositoryPath.repo repo_root)
    |> OpamFile.Repo.with_root_url repo.repo_url
  in
  let opams = load_opams_from_dir repo.repo_name repo_root in
  log (fun fmt ->
      fmt "loaded opam files from repo %s in %.3fs"
        (OpamRepositoryName.to_string repo.repo_name)
        (t ()));
  repo_def, opams

(* Cleaning directories follows the repo path pattern:
   TMPDIR/opam-tmp-dir/repo-dir, defined in [load]. *)
let clean_repo_tmp tmp_dir =
  if Lazy.is_val tmp_dir then
    (let dir = Lazy.force tmp_dir in
     OpamFilename.rmdir dir;
     let parent = OpamFilename.dirname_dir dir in
     if OpamFilename.dir_is_empty parent then
       OpamFilename.rmdir parent)

let remove_from_repos_tmp rt name =
  try
    clean_repo_tmp (Hashtbl.find rt.repos_tmp name);
    Hashtbl.remove rt.repos_tmp name
  with Not_found -> ()

let cleanup rt =
  Hashtbl.iter (fun _ tmp_dir -> clean_repo_tmp tmp_dir) rt.repos_tmp;
  Hashtbl.clear rt.repos_tmp

let get_root_raw root repos_tmp name =
  match Hashtbl.find repos_tmp name with
  | lazy repo_root -> repo_root
  | exception Not_found -> OpamRepositoryPath.root root name

let get_root rt name =
  get_root_raw rt.repos_global.root rt.repos_tmp name

let get_repo_root rt repo =
  get_root_raw rt.repos_global.root rt.repos_tmp repo.repo_name

let load lock_kind gt =
  OpamFormatUpgrade.as_necessary_repo_switch_light_upgrade lock_kind `Repo gt;
  log (fun fmt -> fmt "LOAD-REPOSITORY-STATE %@ %a" (slog OpamFilename.Dir.to_string) gt.root);
  let lock = OpamFilename.flock lock_kind (OpamPath.repos_lock gt.root) in
  let repos_map = OpamStateConfig.Repos.safe_read ~lock_kind gt in
  if OpamStateConfig.is_newer_than_self gt then
    log (fun fmt ->
        fmt "root version (%s) is greater than running binary's (%s); \
             load with best-effort (read-only)"
          (OpamVersion.to_string (OpamFile.Config.opam_root_version gt.config))
          (OpamVersion.to_string (OpamFile.Config.root_version)));
  let mk_repo name url_opt = {
    repo_name = name;
    repo_url = OpamStd.Option.Op.((url_opt >>| fst) +! OpamUrl.empty);
    repo_trust = OpamStd.Option.Op.(url_opt >>= snd);
  } in
  let uncached =
    (* Don't cache repositories without remote, as they should be editable
       in-place *)
    OpamRepositoryName.Map.filter (fun _ url -> url = None) repos_map
  in
  let repositories = OpamRepositoryName.Map.mapi mk_repo repos_map in
  let repos_tmp_root = lazy (OpamFilename.mk_tmp_dir ()) in
  let repos_tmp = Hashtbl.create 23 in
  OpamRepositoryName.Map.iter (fun name repo ->
      let uncompressed_root = OpamRepositoryPath.root gt.root repo.repo_name in
      let tar = OpamRepositoryPath.tar gt.root repo.repo_name in
      if not (OpamFilename.exists_dir uncompressed_root) &&
         OpamFilename.exists tar
      then
        let tmp = lazy (
          let tmp_root = Lazy.force repos_tmp_root in
          try
            (* We rely on this path pattern to clean the repo.
               cf. [clean_repo_tmp] *)
            OpamFilename.extract_in tar tmp_root;
            OpamFilename.Op.(tmp_root / OpamRepositoryName.to_string name)
          with Failure s ->
            OpamFilename.remove tar;
            OpamConsole.error_and_exit `Aborted
              "%s.\nRun `opam update --repositories %s` to fix the issue"
              s (OpamRepositoryName.to_string name);
        ) in
        Hashtbl.add repos_tmp name tmp
    ) repositories;
  let make_rt repos_definitions opams =
    let rt = {
      repos_global = (gt :> unlocked global_state);
      repos_lock = lock;
      repos_tmp;
      repositories;
      repos_definitions;
      repo_opams = opams;
    } in
    OpamStd.Sys.at_exit (fun () -> cleanup rt);
    rt
  in
  match Cache.load gt.root with
  | Some (repofiles, opams) when OpamRepositoryName.Map.is_empty uncached ->
    log (fun fmt -> fmt "Cache found");
    make_rt repofiles opams
  | Some (repofiles, opams) ->
    log (fun fmt -> fmt "Cache found, loading repositories without remote only");
    OpamFilename.with_flock_upgrade `Lock_read lock @@ fun _ ->
    let repofiles, opams =
      OpamRepositoryName.Map.fold (fun name url (defs, opams) ->
          let repo = mk_repo name url in
          let repo_def, repo_opams =
            load_repo repo (get_root_raw gt.root repos_tmp name)
          in
          OpamRepositoryName.Map.add name repo_def defs,
          OpamRepositoryName.Map.add name repo_opams opams)
        uncached (repofiles, opams)
    in
    make_rt repofiles opams
  | None ->
    log (fun fmt -> fmt "No cache found");
    OpamFilename.with_flock_upgrade `Lock_read lock @@ fun _ ->
    let repofiles, opams =
      OpamRepositoryName.Map.fold (fun name url (defs, opams) ->
          let repo = mk_repo name url in
          let repo_def, repo_opams =
            load_repo repo (get_root_raw gt.root repos_tmp name)
          in
          OpamRepositoryName.Map.add name repo_def defs,
          OpamRepositoryName.Map.add name repo_opams opams)
        repos_map (OpamRepositoryName.Map.empty, OpamRepositoryName.Map.empty)
    in
    let rt = make_rt repofiles opams in
    Cache.save_new rt;
    rt

let find_package_opt rt repo_list nv =
  List.fold_left (function
      | None ->
        fun repo_name ->
          OpamStd.Option.Op.(
            OpamRepositoryName.Map.find_opt repo_name rt.repo_opams >>=
            OpamPackage.Map.find_opt nv >>| fun opam ->
            repo_name, opam
          )
      | some -> fun _ -> some)
    None repo_list

let build_index rt repo_list =
  List.fold_left (fun acc repo_name ->
      try
        let repo_opams = OpamRepositoryName.Map.find repo_name rt.repo_opams in
        OpamPackage.Map.union (fun a _ -> a) acc repo_opams
      with Not_found ->
        (* A repo is unavailable, error should have been already reported *)
        acc)
    OpamPackage.Map.empty
    repo_list

let get_repo rt name = OpamRepositoryName.Map.find name rt.repositories

let unlock ?cleanup:(cln=true) rt =
  if cln then cleanup rt;
  OpamSystem.funlock rt.repos_lock;
  (rt :> unlocked repos_state)

let drop ?cleanup rt =
  let _ = unlock ?cleanup rt in ()

let with_write_lock ?dontblock rt f =
  if OpamStateConfig.is_newer_than_self rt.repos_global then
    OpamConsole.error_and_exit `Locked
      "The opam root has been upgraded by a newer version of opam-state \
       and cannot be written to";
  let ret, rt =
    OpamFilename.with_flock_upgrade `Lock_write ?dontblock rt.repos_lock
    @@ fun _ -> f ({ rt with repos_lock = rt.repos_lock } : rw repos_state)
    (* We don't actually change the field value, but this makes restricting the
       phantom lock type possible *)
  in
  ret, { rt with repos_lock = rt.repos_lock }

let with_ lock gt f =
  let rt = load lock gt in
  OpamStd.Exn.finally (fun () -> drop rt) (fun () -> f rt)

let write_config rt =
  OpamFile.Repos_config.write (OpamPath.repos_config rt.repos_global.root)
    (OpamRepositoryName.Map.map (fun r ->
         if r.repo_url = OpamUrl.empty then None
         else Some (r.repo_url, r.repo_trust))
        rt.repositories)

let check_last_update () =
  if OpamCoreConfig.(!r.debug_level) < 0 then () else
  let last_update =
    OpamFilename.written_since
      (OpamPath.state_cache (OpamStateConfig.(!r.root_dir)))
  in
  if last_update > float_of_int (3600*24*21) then
    OpamConsole.note "It seems you have not updated your repositories \
                      for a while. Consider updating them with:\n%s\n"
      (OpamConsole.colorise `bold "opam update");

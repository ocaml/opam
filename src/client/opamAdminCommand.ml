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
open OpamProcess.Job.Op
open OpamStateTypes
open Cmdliner

type command = unit Cmdliner.Term.t * Cmdliner.Term.info

let checked_repo_root () =
  let repo_root = OpamFilename.cwd () in
  if not (OpamFilename.exists_dir (OpamRepositoryPath.packages_dir repo_root))
  then
    OpamConsole.error_and_exit `Bad_arguments
      "No repository found in current directory.\n\
       Please make sure there is a \"packages%s\" directory" OpamArg.dir_sep;
  repo_root

let global_options cli =
  let apply_cli options = { options with OpamArg.cli = options.OpamArg.cli} in
  Term.(const apply_cli $ OpamArg.global_options cli)

let admin_command_doc =
  "Tools for repository administrators"

let admin_command_man = [
  `S Manpage.s_description;
  `P (Printf.sprintf
       "This command can perform various actions on repositories in the opam \
        format. It is expected to be run from the root of a repository, i.e. a \
        directory containing a 'repo' file and a subdirectory 'packages%s' \
        holding package definition within subdirectories. A 'compilers%s' \
        subdirectory (opam repository format version < 2) will also be used by \
        the $(b,upgrade-format) subcommand."
       OpamArg.dir_sep OpamArg.dir_sep)
]

let index_command_doc =
  "Generate an inclusive index file for serving over HTTP."
let index_command cli =
  let command = "index" in
  let doc = index_command_doc in
  let man = [
    `S Manpage.s_description;
    `P "An opam repository can be served over HTTP or HTTPS using any web \
        server. To that purpose, an inclusive index needs to be generated \
        first: this command generates the files the opam client will expect \
        when fetching from an HTTP remote, and should be run after any changes \
        are done to the contents of the repository."
  ]
  in
  let urls_txt_arg cli =
    OpamArg.mk_vflag ~cli `minimal_urls_txt [
      OpamArg.cli_original, `no_urls_txt, ["no-urls-txt"],
      "Don't generate a 'urls.txt' file. That index file is no longer \
       needed from opam 2.0 on, but is still used by older versions.";
      OpamArg.cli_original, `full_urls_txt, ["full-urls-txt"],
      "Generate an inclusive 'urls.txt', for a repository that will be \
       used by opam versions earlier than 2.0.";
      OpamArg.cli_original, `minimal_urls_txt, ["minimal-urls-txt"],
      "Generate a minimal 'urls.txt' file, that only includes the 'repo' \
       file. This allows opam versions earlier than 2.0 to read that file, \
       and be properly redirected to a repository dedicated to their \
       version, assuming a suitable 'redirect:' field is defined, instead \
       of failing. This is the default.";
    ]
  in
  let cmd global_options urls_txt () =
    OpamArg.apply_global_options cli global_options;
    let repo_root = checked_repo_root ()  in
    let repo_file = OpamRepositoryPath.repo repo_root in
    let repo_def =
      match OpamFile.Repo.read_opt repo_file with
      | None ->
        OpamConsole.warning "No \"repo\" file found. Creating a minimal one.";
        OpamFile.Repo.create ()
      | Some r -> r
    in
    let repo_stamp =
      let date () =
        let t = Unix.gmtime (Unix.time ()) in
        Printf.sprintf "%04d-%02d-%02d %02d:%02d"
          (t.Unix.tm_year + 1900) (t.Unix.tm_mon +1) t.Unix.tm_mday
          t.Unix.tm_hour t.Unix.tm_min
      in
      match OpamUrl.guess_version_control (OpamFilename.Dir.to_string repo_root)
      with
      | None -> date ()
      | Some vcs ->
        let module VCS = (val OpamRepository.find_backend_by_kind vcs) in
        match OpamProcess.Job.run (VCS.revision repo_root) with
        | None -> date ()
        | Some hash -> OpamPackage.Version.to_string hash
    in
    let repo_def = OpamFile.Repo.with_stamp repo_stamp repo_def in
    OpamFile.Repo.write repo_file repo_def;
    if urls_txt <> `no_urls_txt then
      (OpamConsole.msg "Generating urls.txt...\n";
       OpamFilename.of_string "repo" ::
       (if urls_txt = `full_urls_txt then
          OpamFilename.rec_files OpamFilename.Op.(repo_root / "compilers") @
          OpamFilename.rec_files (OpamRepositoryPath.packages_dir repo_root)
        else []) |>
       List.fold_left (fun set f ->
           if not (OpamFilename.exists f) then set else
           let attr = OpamFilename.to_attribute repo_root f in
           OpamFilename.Attribute.Set.add attr set
         ) OpamFilename.Attribute.Set.empty |>
       OpamFile.File_attributes.write
         (OpamFile.make (OpamFilename.of_string "urls.txt")));
    OpamConsole.msg "Generating index.tar.gz...\n";
    OpamHTTP.make_index_tar_gz repo_root;
    OpamConsole.msg "Done.\n";
  in
  OpamArg.mk_command  ~cli OpamArg.cli_original command ~doc ~man
    Term.(const cmd $ global_options cli $ urls_txt_arg cli)

let cache_urls repo_root repo_def =
  let global_dl_cache =
    OpamStd.Option.Op.(OpamStateConfig.(load ~lock_kind:`Lock_read !r.root_dir) +!
                       OpamFile.Config.empty)
    |> OpamFile.Config.dl_cache
  in
  let repo_dl_cache =
    OpamStd.List.filter_map (fun rel ->
        if OpamStd.String.contains ~sub:"://" rel
        then OpamUrl.parse_opt ~handle_suffix:false rel
        else Some OpamUrl.Op.(OpamUrl.of_string
                                (OpamFilename.Dir.to_string repo_root) / rel))
      (OpamFile.Repo.dl_cache repo_def)
  in
  repo_dl_cache @ global_dl_cache

(* Downloads all urls of the given package to the given cache_dir *)
let package_files_to_cache repo_root cache_dir cache_urls
    ~recheck ?link (nv, prefix) =
  match
    OpamFileTools.read_opam
      (OpamRepositoryPath.packages repo_root prefix nv)
  with
  | None -> Done (OpamPackage.Map.empty)
  | Some opam ->
    let add_to_cache ?name urlf errors =
      let label =
        OpamPackage.to_string nv ^
        OpamStd.Option.to_string ((^) "/") name
      in
      let checksums =
        OpamHash.sort (OpamFile.URL.checksum urlf)
      in
      match checksums with
      | [] ->
        OpamConsole.warning "[%s] no checksum, not caching"
          (OpamConsole.colorise `green label);
        Done errors
      | best_chks :: _ ->
        let cache_file =
          OpamRepository.cache_file cache_dir best_chks
        in
        let error_opt =
          if not recheck && OpamFilename.exists cache_file then Done None
          else
            OpamRepository.pull_file_to_cache label
              ~cache_urls ~cache_dir
              checksums
              (OpamFile.URL.url urlf :: OpamFile.URL.mirrors urlf)
            @@| fun r -> match OpamRepository.report_fetch_result nv r with
            | Not_available (_,m) -> Some m
            | Up_to_date () | Result () -> None
        in
        error_opt @@| function
        | Some m ->
          OpamPackage.Map.update nv (fun l -> m::l) [] errors
        | None ->
          OpamStd.Option.iter (fun link_dir ->
              let name =
                OpamStd.Option.default
                  (OpamUrl.basename (OpamFile.URL.url urlf))
                  name
              in
              let link =
                OpamFilename.Op.(link_dir / OpamPackage.to_string nv // name)
              in
              OpamFilename.link ~relative:true ~target:cache_file ~link)
            link;
          errors
    in
    let urls =
      (match OpamFile.OPAM.url opam with
       | None -> []
       | Some urlf -> [add_to_cache urlf]) @
      (List.map (fun (name,urlf) ->
           add_to_cache ~name:(OpamFilename.Base.to_string name) urlf)
          (OpamFile.OPAM.extra_sources opam))
    in
    OpamProcess.Job.seq urls OpamPackage.Map.empty

let cache_command_doc = "Fills a local cache of package archives"
let cache_command cli =
  let command = "cache" in
  let doc = cache_command_doc in
  let man = [
    `S Manpage.s_description;
    `P "Downloads the archives for all packages to fill a local cache, that \
        can be used when serving the repository."
  ]
  in
  let cache_dir_arg =
    Arg.(value & pos 0 OpamArg.dirname (OpamFilename.Dir.of_string "./cache") &
         info [] ~docv:"DIR" ~doc:
           "Name of the cache directory to use.")
  in
  let no_repo_update_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["no-repo-update";"n"]
      "Don't check, create or update the 'repo' file to point to the \
       generated cache ('archive-mirrors:' field)."
  in
  let link_arg =
    OpamArg.mk_opt ~cli OpamArg.cli_original ["link"] "DIR"
      (Printf.sprintf
         "Create reverse symbolic links to the archives within $(i,DIR), in \
          the form $(b,DIR%sPKG.VERSION%sFILENAME)."
         OpamArg.dir_sep OpamArg.dir_sep)
      Arg.(some OpamArg.dirname) None
  in
  let jobs_arg =
    OpamArg.mk_opt ~cli OpamArg.cli_original ["jobs"; "j"]
      "JOBS" "Number of parallel downloads"
      OpamArg.positive_integer 8
  in
  let recheck_arg =
    OpamArg.mk_flag ~cli OpamArg.(cli_from cli2_2) ["check-all"; "c"]
      "Run a full integrity check on the existing cache. If this is not set, \
       only missing cache files are handled."
  in
  let cmd global_options cache_dir no_repo_update link jobs recheck () =
    OpamArg.apply_global_options cli global_options;
    (* this option was the default until 2.1 *)
    let recheck = recheck || OpamCLIVersion.Op.(cli @< OpamArg.cli2_2) in
    let repo_root = checked_repo_root () in
    let repo_file = OpamRepositoryPath.repo repo_root in
    let repo_def = OpamFile.Repo.safe_read repo_file in

    let pkg_prefixes = OpamRepository.packages_with_prefixes repo_root in
    let cache_urls = cache_urls repo_root repo_def in

    let errors =
      OpamParallel.reduce ~jobs
        ~nil:OpamPackage.Map.empty
        ~merge:(OpamPackage.Map.union (fun a _ -> a))
        ~command:(package_files_to_cache repo_root cache_dir cache_urls
                    ~recheck ?link)
        (List.sort (fun (nv1,_) (nv2,_) ->
             (* Some pseudo-randomisation to avoid downloading all files from
                the same host simultaneously *)
             match compare (Hashtbl.hash nv1) (Hashtbl.hash nv2) with
             | 0 -> compare nv1 nv2
             | n -> n)
            (OpamPackage.Map.bindings pkg_prefixes))
    in

    let cache_dir_url = OpamFilename.remove_prefix_dir repo_root cache_dir in
    if not no_repo_update then
      if not (List.mem cache_dir_url (OpamFile.Repo.dl_cache repo_def)) then
        (OpamConsole.msg "Adding %s to %s...\n"
           cache_dir_url (OpamFile.to_string repo_file);
         OpamFile.Repo.write repo_file
           (OpamFile.Repo.with_dl_cache
              (cache_dir_url :: OpamFile.Repo.dl_cache repo_def)
              repo_def));

    if not (OpamPackage.Map.is_empty errors) then (
      OpamConsole.error "Got some errors while processing: %s"
        (OpamStd.List.concat_map ", " OpamPackage.to_string
           (OpamPackage.Map.keys errors));
      OpamConsole.errmsg "%s"
        (OpamStd.Format.itemize (fun (nv,el) ->
             Printf.sprintf "[%s] %s" (OpamPackage.to_string nv)
               (String.concat "\n" el))
            (OpamPackage.Map.bindings errors))
    );

    OpamConsole.msg "Done.\n";
  in
  OpamArg.mk_command  ~cli OpamArg.cli_original command ~doc ~man
    Term.(const cmd $ global_options cli $
          cache_dir_arg $ no_repo_update_arg $ link_arg $ jobs_arg $
          recheck_arg)

let add_hashes_command_doc =
  "Add archive hashes to an opam repository."
let add_hashes_command cli =
  let command = "add-hashes" in
  let doc = add_hashes_command_doc in
  let cache_dir = OpamFilename.Dir.of_string "~/.cache/opam-hash-cache" in
  let man = [
    `S Manpage.s_description;
    `P (Printf.sprintf
          "This command scans through package definitions, and add hashes as \
           requested (fetching the archives if required). A cache is generated \
           in %s for subsequent runs."
          (OpamArg.escape_path (OpamFilename.Dir.to_string cache_dir)));
  ]
  in
  let hash_kinds = [`MD5; `SHA256; `SHA512] in
  let hash_types_arg =
    OpamArg.nonempty_arg_list "HASH_ALGO" "The hash, or hashes to be added"
      (Arg.enum
         (List.map (fun k -> OpamHash.string_of_kind k, k)
            hash_kinds))
  in
  let packages =
    OpamArg.mk_opt ~cli OpamArg.(cli_from cli2_1) ["p";"packages"]
      "PACKAGES" "Only add hashes for the given packages"
      Arg.(list OpamArg.package) []
  in
  let replace_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["replace"]
      "Replace the existing hashes rather than adding to them"
  in
  let hash_tables =
    let t = Hashtbl.create (List.length hash_kinds) in
    List.iter (fun k1 ->
        List.iter (fun k2 ->
            if k1 <> k2 then (
              let cache_file : string list list OpamFile.t =
                OpamFile.make @@ OpamFilename.Op.(
                    cache_dir //
                    (OpamHash.string_of_kind k1 ^ "_to_" ^
                     OpamHash.string_of_kind k2))
              in
              let t_mapping = Hashtbl.create 187 in
              (OpamStd.Option.default [] (OpamFile.Lines.read_opt cache_file)
               |> List.iter @@ function
                | [src; dst] ->
                  Hashtbl.add t_mapping
                    (OpamHash.of_string src) (OpamHash.of_string dst)
                | _ -> failwith ("Bad cache at "^OpamFile.to_string cache_file));
              Hashtbl.add t (k1,k2) (cache_file, t_mapping);
            ))
          hash_kinds
      )
      hash_kinds;
    t
  in
  let save_hashes () =
    Hashtbl.iter (fun _ (file, tbl) ->
        Hashtbl.fold
          (fun src dst l -> [OpamHash.to_string src; OpamHash.to_string dst]::l)
          tbl [] |> fun lines ->
        try OpamFile.Lines.write file lines with e ->
          OpamStd.Exn.fatal e;
          OpamConsole.log "ADMIN"
            "Could not write hash cache to %s, skipping (%s)"
            (OpamFile.to_string file)
            (Printexc.to_string e))
      hash_tables
  in
  let additions_count = ref 0 in
  let get_hash cache_urls kind known_hashes url =
    let found =
      List.fold_left (fun result hash ->
          match result with
          | None ->
            let known_kind = OpamHash.kind hash in
            let _, tbl = Hashtbl.find hash_tables (known_kind, kind) in
            (try Some (Hashtbl.find tbl hash) with Not_found -> None)
          | some -> some)
        None known_hashes
    in
    match found with
    | Some h -> Some h
    | None ->
      let h =
        OpamProcess.Job.run @@
        OpamFilename.with_tmp_dir_job @@ fun dir ->
        let f = OpamFilename.Op.(dir // OpamUrl.basename url) in
        OpamProcess.Job.ignore_errors ~default:None
          (fun () ->
             OpamRepository.pull_file (OpamUrl.to_string url)
               ~cache_dir:(OpamRepositoryPath.download_cache
                             OpamStateConfig.(!r.root_dir))
               ~cache_urls
               f known_hashes [url]
             @@| function
             | Result () | Up_to_date () ->
               OpamHash.compute ~kind (OpamFilename.to_string f)
               |> OpamStd.Option.some
             | Not_available _ -> None)
      in
      (match h with
       | Some h ->
         List.iter (fun h0 ->
             Hashtbl.replace
               (snd (Hashtbl.find hash_tables (OpamHash.kind h0, kind)))
               h0 h
           ) known_hashes;
         incr additions_count;
         if !additions_count mod 20 = 0 then save_hashes ()
       | None -> ());
      h
  in
  let cmd global_options hash_types replace packages () =
    OpamArg.apply_global_options cli global_options;
    let repo_root = checked_repo_root () in
    let cache_urls =
      cache_urls repo_root
        (OpamFile.Repo.safe_read (OpamRepositoryPath.repo repo_root))
    in
    let pkg_prefixes =
      let pkgs_map = OpamRepository.packages_with_prefixes repo_root in
      if packages = [] then pkgs_map
      else
        (let pkgs_map, missing_pkgs =
           List.fold_left (fun ((map: string option OpamPackage.Map.t),error)  (n,vo)->
               match vo with
               | Some v ->
                 let nv = OpamPackage.create n v in
                 (match OpamPackage.Map.find_opt nv pkgs_map with
                  | Some pre ->( OpamPackage.Map.add nv pre map), error
                  | None -> map, (n,vo)::error)
               | None ->
                 let n_map = OpamPackage.packages_of_name_map pkgs_map n in
                 if OpamPackage.Map.is_empty n_map then
                   map, (n,vo)::error
                 else
                   (OpamPackage.Map.union (fun _nv _nv' -> assert false) n_map map),
                   error
             ) (OpamPackage.Map.empty, []) packages
         in
         if missing_pkgs <> [] then
           OpamConsole.warning "Not found package%s %s. Ignoring them."
             (if List.length missing_pkgs = 1 then "" else "s")
             (OpamStd.List.concat_map ~left:"" ~right:"" ~last_sep:" and " ", "
                (fun (n,vo) ->
                   OpamConsole.colorise `underline
                     (match vo with
                      | Some v -> OpamPackage.to_string (OpamPackage.create n v)
                      | None -> OpamPackage.Name.to_string n)) missing_pkgs);
         pkgs_map)
    in
    let has_error =
      OpamPackage.Map.fold (fun nv prefix has_error ->
          let opam_file = OpamRepositoryPath.opam repo_root prefix nv in
          let opam = OpamFile.OPAM.read opam_file in
          let has_error =
            if OpamFile.exists (OpamRepositoryPath.url repo_root prefix nv) then
              (OpamConsole.warning "Not updating external URL file at %s"
                 (OpamFile.to_string (OpamRepositoryPath.url repo_root prefix nv));
               true)
            else has_error
          in
          let process_url has_error urlf =
              let hashes = OpamFile.URL.checksum urlf in
              let hashes =
                if replace then
                  List.filter (fun h -> List.mem (OpamHash.kind h) hash_types)
                    hashes
                else hashes
              in
              let has_error, hashes =
                List.fold_left (fun (has_error, hashes) kind ->
                    if List.exists (fun h -> OpamHash.kind h = kind) hashes
                    then has_error, hashes else
                    match get_hash cache_urls kind hashes
                            (OpamFile.URL.url urlf)
                    with
                    | Some h -> has_error, hashes @ [h]
                    | None ->
                      OpamConsole.error "Could not get hash for %s: %s"
                        (OpamPackage.to_string nv)
                        (OpamUrl.to_string (OpamFile.URL.url urlf));
                      true, hashes)
                  (has_error, hashes)
                  hash_types
              in
              has_error, OpamFile.URL.with_checksum hashes urlf
          in
          let has_error, url_opt =
            match OpamFile.OPAM.url opam with
            | None -> has_error, None
            | Some urlf ->
              let has_error, urlf = process_url has_error urlf in
              has_error, Some urlf
          in
          let has_error, extra_sources =
            List.fold_right (fun (basename, urlf) (has_error, acc) ->
                let has_error, urlf = process_url has_error urlf in
                has_error, (basename, urlf) :: acc)
              (OpamFile.OPAM.extra_sources opam)
              (has_error, [])
          in
          let opam1 = OpamFile.OPAM.with_url_opt url_opt opam in
          let opam1 = OpamFile.OPAM.with_extra_sources extra_sources opam1 in
          if opam1 <> opam then
            OpamFile.OPAM.write_with_preserved_format opam_file opam1;
          has_error
        )
        pkg_prefixes false
    in
    save_hashes ();
    if has_error then OpamStd.Sys.exit_because `Sync_error
    else OpamStd.Sys.exit_because `Success
  in
  OpamArg.mk_command  ~cli OpamArg.cli_original command ~doc ~man
    Term.(const cmd $ global_options cli $
          hash_types_arg $ replace_arg $ packages)

let upgrade_command_doc =
  "Upgrades repository from earlier opam versions."
let upgrade_command cli =
  let command = "upgrade" in
  let doc = upgrade_command_doc in
  let man = [
    `S Manpage.s_description;
    `P (Printf.sprintf
          "This command reads repositories from earlier opam versions, and \
           converts them to repositories suitable for the current opam version. \
           Packages might be created or renamed, and any compilers defined in the \
           old format ('compilers%s' directory) will be turned into packages, \
           using a pre-defined hierarchy that assumes OCaml compilers."
          OpamArg.dir_sep)
  ]
  in
  let clear_cache_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["clear-cache"]
      (Printf.sprintf
         "Instead of running the upgrade, clear the cache of archive hashes (held \
          in ~%s.cache), that is used to avoid re-downloading files to obtain \
          their hashes at every run." OpamArg.dir_sep)
  in
  let create_mirror_arg =
    OpamArg.mk_opt ~cli OpamArg.cli_original ["m"; "mirror"] "URL"
      "Don't overwrite the current repository, but put an upgraded mirror in \
       place in a subdirectory, with proper redirections. Needs the URL the \
       repository will be served from to put in the redirects (older versions \
       of opam don't understand relative redirects)."
      Arg.(some OpamArg.url) None
  in
  let cmd global_options clear_cache create_mirror () =
    OpamArg.apply_global_options cli global_options;
    if clear_cache then OpamAdminRepoUpgrade.clear_cache ()
    else match create_mirror with
      | None ->
        OpamAdminRepoUpgrade.do_upgrade (OpamFilename.cwd ());
        if OpamFilename.exists (OpamFilename.of_string "index.tar.gz") ||
           OpamFilename.exists (OpamFilename.of_string "urls.txt")
        then
          OpamConsole.note
            "Indexes need updating: you should now run:\n\
             \n\
            \  opam admin index"
      | Some m -> OpamAdminRepoUpgrade.do_upgrade_mirror (OpamFilename.cwd ()) m
  in
  OpamArg.mk_command  ~cli OpamArg.cli_original command ~doc ~man
    Term.(const cmd $ global_options cli $
          clear_cache_arg $ create_mirror_arg)

let lint_command_doc =
  "Runs 'opam lint' and reports on a whole repository"
let lint_command cli =
  let command = "lint" in
  let doc = lint_command_doc in
  let man = [
    `S Manpage.s_description;
    `P "This command gathers linting results on all files in a repository. The \
        warnings and errors to show or hide can be selected"
  ]
  in
  let short_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["s";"short"]
      "Print only packages and warning/error numbers, without explanations"
  in
  let list_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["list";"l"]
      "Only list package names, without warning details"
  in
  let include_arg =
    OpamArg.arg_list "INT" "Show only these warnings"
      OpamArg.positive_integer
  in
  let exclude_arg =
    OpamArg.mk_opt_all ~cli OpamArg.cli_original ["exclude";"x"] "INT"
      "Exclude the given warnings or errors"
      OpamArg.positive_integer
  in
  let ignore_arg =
    OpamArg.mk_opt_all ~cli OpamArg.cli_original ["ignore-packages";"i"] "INT"
      "Ignore any packages having one of these warnings or errors"
      OpamArg.positive_integer
  in
  let warn_error_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["warn-error";"W"]
      "Return failure on any warnings, not only on errors"
  in
  let cmd global_options short list incl excl ign warn_error () =
    OpamArg.apply_global_options cli global_options;
    let repo_root = OpamFilename.cwd () in
    if not (OpamFilename.exists_dir OpamFilename.Op.(repo_root / "packages"))
    then
        OpamConsole.error_and_exit `Bad_arguments
          "No repository found in current directory.\n\
           Please make sure there is a \"packages\" directory";
    let pkg_prefixes = OpamRepository.packages_with_prefixes repo_root in
    let ret =
      OpamPackage.Map.fold (fun nv prefix ret ->
          let opam_file = OpamRepositoryPath.opam repo_root prefix nv in
          let w, _ = OpamFileTools.lint_file ~handle_dirname:true opam_file in
          if List.exists (fun (n,_,_) -> List.mem n ign) w then ret else
          let w =
            List.filter (fun (n,_,_) ->
                (incl = [] || List.mem n incl) && not (List.mem n excl))
              w
          in
          if w <> [] then
            if list then
              OpamConsole.msg "%s\n" (OpamPackage.to_string nv)
            else if short then
              OpamConsole.msg "%s %s\n" (OpamPackage.to_string nv)
                (OpamStd.List.concat_map " " (fun (n,k,_) ->
                     OpamConsole.colorise
                       (match k with `Warning -> `yellow | `Error -> `red)
                       (string_of_int n))
                    w)
            else begin
              OpamConsole.carriage_delete ();
              OpamConsole.msg "In %s:\n%s\n"
                (OpamPackage.to_string nv)
                (OpamFileTools.warns_to_string w)
            end;
          ret && not (warn_error && w <> [] ||
                      List.exists (fun (_,k,_) -> k = `Error) w))
        pkg_prefixes
        true
    in
    OpamStd.Sys.exit_because (if ret then `Success else `False)
  in
  OpamArg.mk_command  ~cli OpamArg.cli_original command ~doc ~man
    Term.(const cmd $ global_options cli $
          short_arg $ list_arg $ include_arg $ exclude_arg $ ignore_arg $
          warn_error_arg)

let check_command_doc =
  "Runs some consistency checks on a repository"
let check_command cli =
  let command = "check" in
  let doc = check_command_doc in
  let man = [
    `S Manpage.s_description;
    `P "This command runs consistency checks on a repository, and prints a \
        report to stdout. Checks include packages that are not installable \
        (due e.g. to a missing dependency) and dependency cycles. The \
        'available' field is ignored for these checks, that is, all packages \
        are supposed to be available. By default, all checks are run."
  ]
  in
  let ignore_test_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["ignore-test-doc";"i"]
      "By default, $(b,{with-test}) and $(b,{with-doc}) dependencies are \
       included. This ignores them, and makes the test more tolerant."
  in
  let print_short_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["s";"short"]
      "Only output a list of uninstallable packages"
  in
  let installability_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["installability"]
      "Do the installability check (and disable the others by default)"
  in
  let cycles_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["cycles"]
      "Do the cycles check (and disable the others by default)"
  in
  let obsolete_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["obsolete"]
      "Analyse for obsolete packages"
  in
  let cmd global_options ignore_test print_short
      installability cycles obsolete () =
    OpamArg.apply_global_options cli global_options;
    let repo_root = checked_repo_root () in
    let installability, cycles, obsolete =
      if installability || cycles || obsolete
      then installability, cycles, obsolete
      else true, true, false
    in
    let pkgs, unav_roots, uninstallable, cycle_packages, obsolete =
      OpamAdminCheck.check
        ~quiet:print_short ~installability ~cycles ~obsolete ~ignore_test
        repo_root
    in
    let all_ok =
      OpamPackage.Set.is_empty uninstallable &&
      OpamPackage.Set.is_empty cycle_packages &&
      OpamPackage.Set.is_empty obsolete
    in
    let open OpamPackage.Set.Op in
    (if print_short then
       OpamConsole.msg "%s\n"
         (OpamStd.List.concat_map "\n" OpamPackage.to_string
            (OpamPackage.Set.elements
               (uninstallable ++ cycle_packages ++ obsolete)))
     else if all_ok then
       OpamConsole.msg "No issues detected on this repository's %d packages\n"
         (OpamPackage.Set.cardinal pkgs)
     else
     let pr set msg =
       if OpamPackage.Set.is_empty set then ""
       else Printf.sprintf "- %d %s\n" (OpamPackage.Set.cardinal set) msg
     in
     OpamConsole.msg "Summary: out of %d packages (%d distinct names)\n\
                      %s%s%s%s\n"
       (OpamPackage.Set.cardinal pkgs)
       (OpamPackage.Name.Set.cardinal (OpamPackage.names_of_packages pkgs))
       (pr unav_roots "uninstallable roots")
       (pr (uninstallable -- unav_roots) "uninstallable dependent packages")
       (pr (cycle_packages -- uninstallable)
          "packages part of dependency cycles")
       (pr obsolete "obsolete packages"));
    OpamStd.Sys.exit_because (if all_ok then `Success else `False)
  in
  OpamArg.mk_command  ~cli OpamArg.cli_original command ~doc ~man
  Term.(const cmd $ global_options cli $ ignore_test_arg $ print_short_arg
        $ installability_arg $ cycles_arg $ obsolete_arg)

let pattern_list_arg =
  OpamArg.arg_list "PATTERNS"
    "Package patterns with globs. matching against $(b,NAME) or \
     $(b,NAME.VERSION)"
    Arg.string

let env_arg cli =
  OpamArg.mk_opt ~cli OpamArg.cli_original ["environment"]
    "VAR=VALUE[,VAR=VALUE]"
    (Printf.sprintf
       "Use the given opam environment, in the form of a list of \
        comma-separated 'var=value' bindings, when resolving variables. This \
        is used e.g. when computing available packages: if undefined, \
        availability of packages will be assumed as soon as it can not be \
        resolved purely from globally defined variables. Note that, unless \
        overridden, variables like 'root' or 'opam-version' may be taken \
        from the current opam installation. What is defined in \
        $(i,~%s.opam%sconfig) is always ignored."
       OpamArg.dir_sep OpamArg.dir_sep)
    Arg.(list string) []

let state_selection_arg cli =
  OpamArg.mk_vflag ~cli ~section:OpamArg.package_selection_section
    OpamListCommand.Available [
    OpamArg.cli_original, OpamListCommand.Any, ["A";"all"],
    "Include all, even uninstalled or unavailable packages";
    OpamArg.cli_original, OpamListCommand.Available, ["a";"available"],
    "List only packages that are available according to the defined \
     $(b,environment). Without $(b,--environment), this will include \
     any packages for which availability is not resolvable at this \
     point.";
    OpamArg.cli_original, OpamListCommand.Installable, ["installable"],
    "List only packages that are installable according to the defined \
     $(b,environment) (this calls the solver and may be more costly; \
     a package depending on an unavailable one may be available, but \
     is never installable)";
  ]

let get_virtual_switch_state repo_root env =
  let env =
    List.map (fun s ->
        match OpamStd.String.cut_at s '=' with
        | Some (var,value) -> OpamVariable.of_string var, S value
        | None -> OpamVariable.of_string s, B true)
      env
  in
  let repo = {
    repo_name = OpamRepositoryName.of_string "local";
    repo_url = OpamUrl.empty;
    repo_trust = None;
  } in
  let repo_file = OpamRepositoryPath.repo repo_root in
  let repo_def = OpamFile.Repo.safe_read repo_file in
  let opams =
    OpamRepositoryState.load_opams_from_dir repo.repo_name repo_root
  in
  let gt = {
    global_lock = OpamSystem.lock_none;
    root = OpamStateConfig.(!r.root_dir);
    config = OpamStd.Option.Op.(OpamStateConfig.(
        load ~lock_kind:`Lock_read !r.root_dir) +!
                                OpamFile.Config.empty);
    global_variables = OpamVariable.Map.empty;
  } in
  let singl x = OpamRepositoryName.Map.singleton repo.repo_name x in
  let repos_tmp =
    let t = Hashtbl.create 1 in
    Hashtbl.add t repo.repo_name (lazy repo_root); t
  in
  let rt = {
    repos_global = gt;
    repos_lock = OpamSystem.lock_none;
    repositories = singl repo;
    repos_definitions = singl repo_def;
    repo_opams = singl opams;
    repos_tmp;
  } in
  let gt =
    {gt with global_variables =
               OpamVariable.Map.of_list @@
               List.map (fun (var, value) ->
                   var, (lazy (Some value), "Manually defined"))
                 env }
  in
  OpamSwitchState.load_virtual
    ~repos_list:[repo.repo_name]
    ~avail_default:(env = [])
    gt rt

let or_arg cli =
  OpamArg.mk_flag ~cli OpamArg.cli_original ~section:OpamArg.package_selection_section ["or"]
    "Instead of selecting packages that match $(i,all) the \
     criteria, select packages that match $(i,any) of them"

let list_command_doc = "Lists packages from a repository"
let list_command cli =
  let command = "list" in
  let doc = list_command_doc in
  let man = [
    `S Manpage.s_description;
    `P "This command is similar to 'opam list', but allows listing packages \
        directly from a repository instead of what is available in a given \
        opam installation.";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
    `S OpamArg.package_selection_section;
    `S OpamArg.package_listing_section;
  ]
  in
  let cmd
      global_options package_selection disjunction state_selection
      package_listing env packages () =
    OpamArg.apply_global_options cli global_options;
    let format =
      let force_all_versions =
        match packages with
        | [single] ->
          let nameglob =
            match OpamStd.String.cut_at single '.' with
            | None -> single
            | Some (n, _v) -> n
          in
          (try ignore (OpamPackage.Name.of_string nameglob); true
           with Failure _ -> false)
        | _ -> false
      in
      package_listing ~force_all_versions
    in
    let pattern_selector = OpamListCommand.pattern_selector packages in
    let join =
      if disjunction then OpamFormula.ors else OpamFormula.ands
    in
    let filter =
      OpamFormula.ands [
        Atom state_selection;
        join (pattern_selector ::
              List.map (fun x -> Atom x) package_selection);
      ]
    in
    let st = get_virtual_switch_state (OpamFilename.cwd ()) env in
    if not format.OpamListCommand.short && filter <> OpamFormula.Empty then
      OpamConsole.msg "# Packages matching: %s\n"
        (OpamListCommand.string_of_formula filter);
    let results =
      OpamListCommand.filter ~base:st.packages st filter
    in
    OpamListCommand.display st format results
  in
  OpamArg.mk_command  ~cli OpamArg.cli_original command ~doc ~man
  Term.(const cmd $ global_options cli $ OpamArg.package_selection cli $
        or_arg cli $ state_selection_arg cli $ OpamArg.package_listing cli $
        env_arg cli $ pattern_list_arg)

let filter_command_doc = "Filters a repository to only keep selected packages"
let filter_command cli =
  let command = "filter" in
  let doc = filter_command_doc in
  let man = [
    `S Manpage.s_description;
    `P "This command removes all package definitions that don't match the \
        search criteria (specified similarly to 'opam admin list') from a \
        repository.";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
    `S OpamArg.package_selection_section;
  ]
  in
  let remove_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["remove"]
      "Invert the behaviour and remove the matching packages, keeping the ones \
       that don't match."
  in
  let dryrun_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["dry-run"]
      "List the removal commands, without actually performing them"
  in
  let cmd
      global_options package_selection disjunction state_selection env
      remove dryrun packages () =
    OpamArg.apply_global_options cli global_options;
    let repo_root = OpamFilename.cwd () in
    let pattern_selector = OpamListCommand.pattern_selector packages in
    let join =
      if disjunction then OpamFormula.ors else OpamFormula.ands
    in
    let filter =
      OpamFormula.ands [
        Atom state_selection;
        join
          (pattern_selector ::
           List.map (fun x -> Atom x) package_selection)
      ]
    in
    let st = get_virtual_switch_state repo_root env in
    let packages = OpamListCommand.filter ~base:st.packages st filter in
    if OpamPackage.Set.is_empty packages then
      if remove then
        (OpamConsole.warning "No packages match the selection criteria";
         OpamStd.Sys.exit_because `Success)
      else
        OpamConsole.error_and_exit `Not_found
          "No packages match the selection criteria";
    let num_total = OpamPackage.Set.cardinal st.packages in
    let num_selected = OpamPackage.Set.cardinal packages in
    if remove then
      OpamConsole.formatted_msg
        "The following %d packages will be REMOVED from the repository (%d \
         packages will be kept):\n%s\n"
        num_selected (num_total - num_selected)
        (OpamStd.List.concat_map " " OpamPackage.to_string
           (OpamPackage.Set.elements packages))
    else
      OpamConsole.formatted_msg
        "The following %d packages will be kept in the repository (%d packages \
         will be REMOVED):\n%s\n"
        num_selected (num_total - num_selected)
        (OpamStd.List.concat_map " " OpamPackage.to_string
           (OpamPackage.Set.elements packages));
    let packages =
      if remove then packages else OpamPackage.Set.Op.(st.packages -- packages)
    in
    if not (dryrun || OpamConsole.confirm "Confirm?") then
      OpamStd.Sys.exit_because `Aborted
    else
    let pkg_prefixes = OpamRepository.packages_with_prefixes repo_root in
    OpamPackage.Map.iter (fun nv prefix ->
        if OpamPackage.Set.mem nv packages then
          let d = OpamRepositoryPath.packages repo_root prefix nv in
          if dryrun then
            OpamConsole.msg "rm -rf %s\n" (OpamFilename.Dir.to_string d)
          else
            (OpamFilename.cleandir d;
             OpamFilename.rmdir_cleanup d))
      pkg_prefixes
  in
  OpamArg.mk_command  ~cli OpamArg.cli_original command ~doc ~man
  Term.(const cmd $ global_options cli $ OpamArg.package_selection cli $
        or_arg cli $ state_selection_arg cli $ env_arg cli $ remove_arg $
        dryrun_arg $
        pattern_list_arg)

let add_constraint_command_doc =
  "Adds version constraints on all dependencies towards a given package"
let add_constraint_command cli =
  let command = "add-constraint" in
  let doc = add_constraint_command_doc in
  let man = [
    `S Manpage.s_description;
    `P "This command searches to all dependencies towards a given package, and \
        adds a version constraint to them. It is particularly useful to add \
        upper bounds to existing dependencies when a new, incompatible major \
        version of a library is added to a repository. The new version \
        constraint is merged with the existing one, and simplified if \
        possible (e.g. $(b,>=3 & >5) becomes $(b,>5)).";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
  ]
  in
  let atom_arg =
    Arg.(required & pos 0 (some OpamArg.atom) None
         & info [] ~docv:"PACKAGE" ~doc:
           "A package name with a version constraint, e.g. $(b,name>=version). \
            If no version constraint is specified, the command will just \
            simplify existing version constraints on dependencies to the named \
            package.")
  in
  let force_arg =
    OpamArg.mk_flag ~cli OpamArg.cli_original ["force"]
      "Force updating of constraints even if the resulting constraint is \
       unsatisfiable (e.g. when adding $(b,>3) to the constraint \
       $(b,<2)). The default in this case is to print a warning and keep \
       the existing constraint unchanged."
  in
  let cmd global_options force atom () =
    OpamArg.apply_global_options cli global_options;
    let repo_root = checked_repo_root () in
    let pkg_prefixes = OpamRepository.packages_with_prefixes repo_root in
    let name, cstr_opt = atom in
    let cstr = match cstr_opt with
      | Some (relop, v) ->
        OpamFormula.Atom
          (Constraint (relop, FString (OpamPackage.Version.to_string v)))
      | None ->
        OpamFormula.Empty
    in
    let add_cstr op cstr nv n c =
      let f = op [ cstr; c] in
      match OpamFilter.simplify_extended_version_formula f with
      | Some f -> f
      | None -> (* conflicting constraint *)
        if force then f
        else
          (OpamConsole.warning
             "In package %s, updated constraint %s cannot be satisfied, not \
              updating (use `--force' to update anyway)"
             (OpamPackage.to_string nv)
             (OpamConsole.colorise `bold
                (OpamFilter.string_of_filtered_formula
                   (Atom (n, f))));
           c)
    in
    OpamPackage.Map.iter (fun nv prefix ->
        let opam_file = OpamRepositoryPath.opam repo_root prefix nv in
        let opam = OpamFile.OPAM.read opam_file in
        let deps0 = OpamFile.OPAM.depends opam in
        let deps =
          OpamFormula.map (function
              | (n,c as atom) ->
                if n = name then Atom (n, (add_cstr OpamFormula.ands cstr nv n c))
                else Atom atom)
            deps0
        in
        let depopts0 = OpamFile.OPAM.depopts opam in
        let conflicts0 = OpamFile.OPAM.conflicts opam in
        let contains name =
          OpamFormula.fold_left (fun contains (n,_) ->
              contains || n = name) false
        in
        let conflicts =
          if contains name depopts0 then
            match cstr_opt with
            | Some (relop, v) ->
              let icstr =
                OpamFormula.Atom
                  (Constraint (OpamFormula.neg_relop relop,
                               FString (OpamPackage.Version.to_string v)))
              in
              if contains name conflicts0 then
                OpamFormula.map (function
                    | (n,c as atom) ->
                      if n = name then Atom (n, (add_cstr OpamFormula.ors icstr nv n c))
                      else Atom atom)
                  conflicts0
              else
                OpamFormula.ors [ conflicts0; Atom (name, icstr) ]
            | None -> conflicts0
          else conflicts0
        in
        if deps <> deps0 || conflicts <> conflicts0 then
          OpamFile.OPAM.write_with_preserved_format opam_file
            (OpamFile.OPAM.with_depends deps opam
             |> OpamFile.OPAM.with_conflicts conflicts))
      pkg_prefixes
  in
  OpamArg.mk_command  ~cli OpamArg.cli_original command ~doc ~man
  Term.(pure cmd $ global_options cli $ force_arg $ atom_arg)

(* HELP *)
let help =
  let doc = "Display help about opam admin and opam admin subcommands." in
  let man = [
    `S Manpage.s_description;
    `P "Prints help about opam admin commands.";
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ] in
  let topic =
    let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
    Arg.(value & pos 0 (some string) None & doc )
  in
  let help man_format cmds topic = match topic with
    | None       -> `Help (`Pager, None)
    | Some topic ->
      let topics = "topics" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" ->
          List.iter (OpamConsole.msg "%s\n") cmds; `Ok ()
      | `Ok t -> `Help (man_format, Some t) in

  Term.(ret (const help $Term.man_format $Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let admin_subcommands cli =
  let index_command = index_command cli in
  [
    index_command; OpamArg.make_command_alias ~cli index_command "make";
    cache_command cli;
    upgrade_command cli;
    lint_command cli;
    check_command cli;
    list_command cli;
    filter_command cli;
    add_constraint_command cli;
    add_hashes_command cli;
    help;
  ]

let default_subcommand cli =
  let man =
    admin_command_man @ [
      `S Manpage.s_commands;
      `S "COMMAND ALIASES";
    ] @ OpamArg.help_sections cli
  in
  let usage global_options =
    OpamArg.apply_global_options cli global_options;
    OpamConsole.formatted_msg
      "usage: opam admin [--version]\n\
      \                  [--help]\n\
      \                  <command> [<args>]\n\
       \n\
       The most commonly used opam admin commands are:\n\
      \    index          %s\n\
      \    cache          %s\n\
      \    upgrade-format %s\n\
       \n\
       See 'opam admin <command> --help' for more information on a specific \
       command.\n"
      index_command_doc
      cache_command_doc
      upgrade_command_doc
  in
  Term.(const usage $ global_options cli),
  Term.info "opam admin"
    ~version:(OpamVersion.to_string OpamVersion.current)
    ~sdocs:OpamArg.global_option_section
    ~doc:admin_command_doc
    ~man

let get_cmdliner_parser cli =
  default_subcommand cli, admin_subcommands cli

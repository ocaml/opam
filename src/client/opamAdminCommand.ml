(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2017 OCamlPro                                        *)
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

let admin_command_doc =
  "Tools for repository administrators"

let admin_command_man = [
  `S "DESCRIPTION";
  `P "This command can perform various actions on repositories in the opam \
      format. It is expected to be run from the root of a repository, i.e. a \
      directory containing a 'repo' file and a subdirectory 'packages/' \
      holding package definition within subdirectories. A 'compilers/' \
      subdirectory (opam repository format version < 2) will also be used by \
      the $(b,upgrade-format) subcommand."
]

let index_command_doc =
  "Generate an inclusive index file for serving over HTTP."
let index_command =
  let command = "index" in
  let doc = index_command_doc in
  let man = [
    `S "DESCRIPTION";
    `P "An opam repository can be served over HTTP or HTTPS using any web \
        server. To that purpose, an inclusive index needs to be generated \
        first: this command generates the files the opam client will expect \
        when fetching from an HTTP remote, and should be run after any changes \
        are done to the contents of the repository."
  ]
  in
  let urls_txt_arg =
    Arg.(value & vflag `minimal_urls_txt [
        `no_urls_txt, info ["no-urls-txt"] ~doc:
          "Don't generate a 'urls.txt' file. That index file is no longer \
           needed from opam 2.0 on, but is still used by older versions.";
        `full_urls_txt, info ["full-urls-txt"] ~doc:
          "Generate an inclusive 'urls.txt', for a repository that will be \
           used by opam versions earlier than 2.0.";
        `minimal_urls_txt, info ["minimal-urls-txt"] ~doc:
          "Generate a minimal 'urls.txt' file, that only includes the 'repo' \
           file. This allows opam versions earlier than 2.0 to read that file, \
           and be properly redirected to a repository dedicated to their \
           version, assuming a suitable 'redirect:' field is defined, instead \
           of failing. This is the default.";
      ])
  in
  let cmd global_options urls_txt =
    OpamArg.apply_global_options global_options;
    let repo_root = OpamFilename.cwd () in
    if not (OpamFilename.exists_dir OpamFilename.Op.(repo_root / "packages"))
    then
      OpamConsole.error_and_exit `Bad_arguments
        "No repository found in current directory.\n\
         Please make sure there is a \"packages/\" directory";
    let repo_file = OpamRepositoryPath.repo repo_root in
    let repo_def =
      match OpamFile.Repo.read_opt repo_file with
      | None ->
        OpamConsole.warning "No \"repo\" file found. Creating a minimal one.";
        OpamFile.Repo.create ~opam_version:OpamVersion.current_nopatch ()
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
  Term.(const cmd $ OpamArg.global_options $ urls_txt_arg),
  OpamArg.term_info command ~doc ~man


(* Downloads all urls of the given package to the given cache_dir *)
let package_files_to_cache repo_root cache_dir ?link (nv, prefix) =
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
      match OpamFile.URL.checksum urlf with
      | [] ->
        OpamConsole.warning "[%s] no checksum, not caching"
          (OpamConsole.colorise `green label);
        Done errors
      | (first_checksum :: _) as checksums ->
        OpamRepository.pull_file_to_cache label
          ~cache_dir
          checksums
          (OpamFile.URL.url urlf :: OpamFile.URL.mirrors urlf)
        @@| function
        | Not_available m ->
          OpamPackage.Map.update nv (fun l -> m::l) [] errors
        | Up_to_date () | Result () ->
          OpamStd.Option.iter (fun link_dir ->
              let target =
                OpamRepository.cache_file cache_dir first_checksum
              in
              let name =
                OpamStd.Option.default
                  (OpamUrl.basename (OpamFile.URL.url urlf))
                  name
              in
              let link =
                OpamFilename.Op.(link_dir / OpamPackage.to_string nv // name)
              in
              OpamFilename.link ~relative:true ~target ~link)
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
let cache_command =
  let command = "cache" in
  let doc = cache_command_doc in
  let man = [
    `S "DESCRIPTION";
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
    Arg.(value & flag & info ["no-repo-update";"n"] ~doc:
           "Don't check, create or update the 'repo' file to point to the \
            generated cache ('archive-mirrors:' field).")
  in
  let link_arg =
    Arg.(value & opt (some OpamArg.dirname) None &
         info ["link"] ~docv:"DIR" ~doc:
           "Create reverse symbolic links to the archives within $(i,DIR), in \
            the form $(b,DIR/PKG.VERSION/FILENAME).")
  in
  let jobs_arg =
    Arg.(value & opt OpamArg.positive_integer 8 &
         info ["jobs"; "j"] ~docv:"JOBS" ~doc:
           "Number of parallel downloads")
  in
  let cmd global_options cache_dir no_repo_update link jobs =
    OpamArg.apply_global_options global_options;
    let repo_root = OpamFilename.cwd () in
    if not (OpamFilename.exists_dir OpamFilename.Op.(repo_root / "packages"))
    then
        OpamConsole.error_and_exit `Bad_arguments
          "No repository found in current directory.\n\
           Please make sure there is a \"packages\" directory";
    let repo_file = OpamRepositoryPath.repo repo_root in
    let repo_def = OpamFile.Repo.safe_read repo_file in

    let repo = OpamRepositoryBackend.local repo_root in
    let pkg_prefixes = OpamRepository.packages_with_prefixes repo in

    let errors =
      OpamParallel.reduce ~jobs
        ~nil:OpamPackage.Map.empty
        ~merge:(OpamPackage.Map.union (fun a _ -> a))
        ~command:(package_files_to_cache repo_root cache_dir ?link)
        (List.sort (fun (nv1,_) (nv2,_) ->
             (* Some pseudo-randomisation to avoid downloading all files from
                the same host simultaneously *)
             match compare (Hashtbl.hash nv1) (Hashtbl.hash nv2) with
             | 0 -> compare nv1 nv2
             | n -> n)
            (OpamPackage.Map.bindings pkg_prefixes))
    in

    if not no_repo_update then
      let cache_dir_url = OpamFilename.remove_prefix_dir repo_root cache_dir in
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
  Term.(const cmd $ OpamArg.global_options $
        cache_dir_arg $ no_repo_update_arg $ link_arg $ jobs_arg),
  OpamArg.term_info command ~doc ~man


let upgrade_command_doc =
  "Upgrades repository from earlier opam versions."
let upgrade_command =
  let command = "upgrade" in
  let doc = upgrade_command_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command reads repositories from earlier opam versions, and \
        converts them to repositories suitable for the current opam version. \
        Packages might be created or renamed, and any compilers defined in the \
        old format ('compilers/' directory) will be turned into packages, \
        using a pre-defined hierarchy that assumes OCaml compilers."
  ]
  in
  let clear_cache_arg =
    let doc =
      "Instead of running the upgrade, clear the cache of archive hashes (held \
       in ~/.cache), that is used to avoid re-downloading files to obtain \
       their hashes at every run."
    in
    Arg.(value & flag & info ["clear-cache"] ~doc)
  in
  let create_mirror_arg =
    let doc =
      "Don't overwrite the current repository, but put an upgraded mirror in \
       place in a subdirectory, with proper redirections. Needs the URL the \
       repository will be served from to put in the redirects (older versions \
       of opam don't understand relative redirects)."
    in
    Arg.(value & opt (some OpamArg.url) None &
         info ~docv:"URL" ["m"; "mirror"] ~doc)
  in
  let cmd global_options clear_cache create_mirror =
    OpamArg.apply_global_options global_options;
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
  Term.(const cmd $ OpamArg.global_options $
        clear_cache_arg $ create_mirror_arg),
  OpamArg.term_info command ~doc ~man

let lint_command_doc =
  "Runs 'opam lint' and reports on a whole repository"
let lint_command =
  let command = "lint" in
  let doc = lint_command_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command gathers linting results on all files in a repository. The \
        warnings and errors to show or hide can be selected"
  ]
  in
  let short_arg =
    OpamArg.mk_flag ["s";"short"]
      "Print only packages and warning/error numbers, without explanations"
  in
  let list_arg =
    OpamArg.mk_flag ["list";"l"]
      "Only list package names, without warning details"
  in
  let include_arg =
    OpamArg.arg_list "INT" "Show only these warnings"
      OpamArg.positive_integer
  in
  let exclude_arg =
    OpamArg.mk_opt_all ["exclude";"x"] "INT"
      "Exclude the given warnings or errors"
      OpamArg.positive_integer
  in
  let ignore_arg =
    OpamArg.mk_opt_all ["ignore-packages";"i"] "INT"
      "Ignore any packages having one of these warnings or errors"
      OpamArg.positive_integer
  in
  let warn_error_arg =
    OpamArg.mk_flag ["warn-error";"W"]
      "Return failure on any warnings, not only on errors"
  in
  let cmd global_options short list incl excl ign warn_error =
    OpamArg.apply_global_options global_options;
    let repo_root = OpamFilename.cwd () in
    if not (OpamFilename.exists_dir OpamFilename.Op.(repo_root / "packages"))
    then
        OpamConsole.error_and_exit `Bad_arguments
          "No repository found in current directory.\n\
           Please make sure there is a \"packages\" directory";
    let repo = OpamRepositoryBackend.local repo_root in
    let pkg_prefixes = OpamRepository.packages_with_prefixes repo in
    let ret =
      OpamPackage.Map.fold (fun nv prefix ret ->
          let opam_file = OpamRepositoryPath.opam repo_root prefix nv in
          let w, _ = OpamFileTools.lint_file opam_file in
          if List.exists (fun (n,_,_) -> List.mem n ign) w then ret else
          let w =
            List.filter (fun (n,_,_) ->
                (incl = [] || List.mem n incl) && not (List.mem n excl))
              w
          in
          if w <> [] then
            if list then
              print_endline (OpamPackage.to_string nv)
            else if short then
              OpamConsole.msg "%s %s\n" (OpamPackage.to_string nv)
                (OpamStd.List.concat_map " " (fun (n,k,_) ->
                     OpamConsole.colorise
                       (match k with `Warning -> `yellow | `Error -> `red)
                       (string_of_int n))
                    w)
            else
              OpamConsole.msg "\r\027[KIn %s:\n%s\n"
                (OpamPackage.to_string nv)
                (OpamFileTools.warns_to_string w);
          ret && not (warn_error && w <> [] ||
                      List.exists (fun (_,k,_) -> k = `Error) w))
        pkg_prefixes
        true
    in
    OpamStd.Sys.exit_because (if ret then `Success else `False)
  in
  Term.(const cmd $ OpamArg.global_options $
        short_arg $ list_arg $ include_arg $ exclude_arg $ ignore_arg $
        warn_error_arg),
  OpamArg.term_info command ~doc ~man

let check_command_doc =
  "Runs some consistency checks on a repository"
let check_command =
  let command = "check" in
  let doc = check_command_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command runs consistency checks on a repository, and prints a \
        report to stdout. Checks include packages that are not installable \
        (due e.g. to a missing dependency) and dependency cycles. The \
        'available' field is ignored for these checks, that is, all packages \
        are supposed to be available. By default, all checks are run."
  ]
  in
  let ignore_test_arg =
    OpamArg.mk_flag ["ignore-test-doc";"i"]
      "By default, $(b,{with-test}) and $(b,{with-doc}) dependencies are \
       included. This ignores them, and makes the test more tolerant."
  in
  let print_short_arg =
    OpamArg.mk_flag ["s";"short"]
      "Only output a list of uninstallable packages"
  in
  let installability_arg =
    OpamArg.mk_flag ["installability"]
      "Do the installability check (and disable the others by default)"
  in
  let cycles_arg =
    OpamArg.mk_flag ["cycles"]
      "Do the cycles check (and disable the others by default)"
  in
  let cmd global_options ignore_test print_short installability cycles =
    OpamArg.apply_global_options global_options;
    let repo_root = OpamFilename.cwd () in
    let installability, cycles =
      if installability || cycles then installability, cycles
      else true, true
    in
    if not (OpamFilename.exists_dir OpamFilename.Op.(repo_root / "packages"))
    then
      OpamConsole.error_and_exit `Bad_arguments
        "No repository found in current directory.\n\
         Please make sure there is a \"packages\" directory";
    let repo = OpamRepositoryBackend.local repo_root in
    let pkg_prefixes = OpamRepository.packages_with_prefixes repo in
    let opams =
      OpamPackage.Map.fold (fun nv prefix acc ->
          let opam_file = OpamRepositoryPath.opam repo_root prefix nv in
          match OpamFile.OPAM.read_opt opam_file with
          | Some o -> OpamPackage.Map.add nv o acc
          | None ->
            OpamConsole.warning "Error while reading %s"
              (OpamFile.to_string opam_file);
            acc)
        pkg_prefixes
        OpamPackage.Map.empty
    in
    let packages = OpamPackage.keys opams in
    let env ~with_test ~with_doc ~dev nv v =
      match OpamVariable.Full.scope v,
            OpamVariable.(to_string (Full.variable v))
      with
      | (OpamVariable.Full.Global | OpamVariable.Full.Self), "name" ->
        Some (S (OpamPackage.Name.to_string nv.name))
      | (OpamVariable.Full.Global | OpamVariable.Full.Self), "version" ->
        Some (S (OpamPackage.Version.to_string nv.version))
      | OpamVariable.Full.Global, "opam-version" ->
        Some (S OpamVersion.(to_string current))
      | OpamVariable.Full.Global, "with-test" ->
        Some (B with_test)
      | OpamVariable.Full.Global, "dev" ->
        Some (B dev)
      | OpamVariable.Full.Global, "with-doc" ->
        Some (B with_doc)
      | _ -> None
    in
    let universe ~with_test ~with_doc ~dev =
      let env = env ~with_test ~with_doc ~dev in {
        u_packages = packages;
        u_action = Query;
        u_installed = OpamPackage.Set.empty;
        u_available = packages;
        u_depends =
          OpamPackage.Map.mapi
            (fun nv o ->
               OpamFile.OPAM.depends o |>
               OpamFilter.partial_filter_formula (env nv))
            opams;
        u_depopts =
          OpamPackage.Map.mapi
            (fun nv o ->
               OpamFile.OPAM.depopts o |>
               OpamFilter.partial_filter_formula (env nv))
            opams;
        u_conflicts =
          OpamPackage.Map.mapi
            (fun nv o ->
               OpamFile.OPAM.conflicts o |>
               OpamFilter.filter_formula ~default:false (env nv))
            opams;
        u_installed_roots = OpamPackage.Set.empty;
        u_pinned = OpamPackage.Set.empty;
        u_base = OpamPackage.Set.empty;
        u_attrs = [];
        u_reinstall = OpamPackage.Set.empty;
      }
    in
    let univ =
      universe
        ~with_test:(not ignore_test) ~with_doc:(not ignore_test) ~dev:false
    in
    let open OpamPackage.Set.Op in

    (* Installability check *)
    let uninstallable, unav_roots, unav_others =
      if installability then
        let graph =
          OpamCudf.Graph.of_universe @@
          OpamSolver.load_cudf_universe
            ~depopts:false ~build:true ~post:true univ packages
        in
        let filter_roots g packages =
          let has_pkg p = OpamPackage.Set.mem (OpamCudf.cudf2opam p) packages in
          OpamCudf.Graph.fold_vertex (fun p acc ->
              if has_pkg p &&
                 not (List.exists has_pkg (OpamCudf.Graph.succ g p))
              then OpamPackage.Set.add (OpamCudf.cudf2opam p) acc
              else acc)
            g OpamPackage.Set.empty
        in
        if not print_short then
          OpamConsole.msg "Checking installability of every package. This may \
                           take a few minutes...\n";
        let installable = OpamSolver.installable univ in
        let uninstallable = packages -- installable in
        let unav_roots = filter_roots graph uninstallable in
        let unav_others = uninstallable -- unav_roots in
        if print_short then () else
        if not (OpamPackage.Set.is_empty uninstallable) then
          OpamConsole.error "These packages are not installable (%d):\n%s%s"
            (OpamPackage.Set.cardinal unav_roots)
            (OpamStd.List.concat_map " " OpamPackage.to_string
               (OpamPackage.Set.elements unav_roots))
            (if OpamPackage.Set.is_empty unav_others then "" else
               "\n(the following depend on them and are also unavailable:\n"^
               (OpamStd.List.concat_map " " OpamPackage.to_string
                  (OpamPackage.Set.elements unav_others))^")");
        uninstallable, unav_roots, unav_others
      else
        OpamPackage.Set.empty, OpamPackage.Set.empty, OpamPackage.Set.empty
    in

    (* Cyclic dependency checks *)
    let cycle_packages =
      if not cycles then OpamPackage.Set.empty else
      let cudf_univ =
        OpamSolver.load_cudf_universe
          ~depopts:true ~build:true ~post:false univ packages
      in
      let graph =
        OpamCudf.Graph.of_universe cudf_univ |>
        OpamCudf.Graph.mirror
      in
      (* conflicts break cycles *)
      let conflicts =
        Algo.Defaultgraphs.PackageGraph.conflict_graph cudf_univ
      in
      let module CGraph = Algo.Defaultgraphs.PackageGraph.UG in
      CGraph.iter_edges (fun nv1 nv2 ->
          OpamCudf.Graph.remove_edge graph nv1 nv2;
          OpamCudf.Graph.remove_edge graph nv2 nv1)
        conflicts;
      let scc =
        let module Comp = Graph.Components.Make(OpamCudf.Graph) in
        Comp.scc_list graph |>
        List.filter (function [] | [_] -> false | _ -> true)
      in
      let node_map, cy =
        List.fold_left (fun (node_map, acc) pkgs ->
            let univ = Cudf.load_universe pkgs in
            let g = OpamCudf.Graph.of_universe univ in
            let conflicts =
              Algo.Defaultgraphs.PackageGraph.conflict_graph univ
            in
            (* Simplify the graph by merging all equivalent versions of each
               package *)
            (* (note: this is not completely accurate, as dependencies might be
               conjunctions or disjunctions, information which is lost in the
               dependency graph) *)
            (* let count = OpamCudf.Graph.nb_vertex g in *)
            let node_map =
              Cudf.fold_packages_by_name (fun node_map _ pkgs ->
                  let id p =
                    let f pl =
                      List.sort compare @@
                      List.map (Cudf.uid_by_package univ) pl
                    in
                    f (OpamCudf.Graph.pred g p),
                    f (OpamCudf.Graph.succ g p),
                    f (CGraph.succ conflicts p)
                  in
                  let ids =
                    List.fold_left (fun acc p ->
                        OpamCudf.Map.add p (id p) acc)
                      OpamCudf.Map.empty pkgs
                  in
                  let rec gather node_map = function
                    | [] -> node_map
                    | p::pkgs ->
                      let pid = OpamCudf.Map.find p ids in
                      let ps, pkgs =
                        List.partition
                          (fun p1 -> OpamCudf.Map.find p1 ids = pid)
                          pkgs
                      in
                      List.iter (OpamCudf.Graph.remove_vertex g) ps;
                      let node_map = OpamCudf.Map.add p (p::ps) node_map in
                      gather node_map pkgs
                  in
                  gather node_map pkgs)
                node_map univ
            in
            (* OpamConsole.msg
             *   "Number of vertices: before merge %d, after merge %d\n"
             *   count (OpamCudf.Graph.nb_vertex g); *)
            let it = ref 0 in
            let rec extract_cycles acc rpath v g =
              incr it;
              let rec find_pref acc v = function
                | [] -> None
                | v1::r ->
                  if Cudf.(=%) v v1 then Some (v1::acc)
                  else if CGraph.mem_edge conflicts v v1 then None
                  else find_pref (v1::acc) v r
              in
              match find_pref [] v rpath with
              | Some cy -> cy :: acc
              | None ->
                let rpath = v::rpath in
                (* split into sub-graphs for each successor *)
                List.fold_left
                  (fun acc s -> extract_cycles acc rpath s g)
                  acc (OpamCudf.Graph.succ g v)
            in
            let p0 = List.find (OpamCudf.Graph.mem_vertex g) pkgs in
            let r = extract_cycles acc [] p0 g in
            (* OpamConsole.msg "Iterations: %d\n" !it; *)
            node_map, r
          )
          (OpamCudf.Map.empty, []) scc
      in
      (* OpamConsole.msg "all cycles: %d\n" (List.length cy); *)
      let rec has_conflict = function
        | [] | [_] -> false
        | p::r ->
          List.exists
            (CGraph.mem_edge conflicts p)
            r
          || has_conflict r
      in
      let cy =
        List.rev cy |>
        List.filter (fun c -> not (has_conflict c))
      in
      let formula_of_pkglist = function
        | [] -> OpamFormula.Empty
        | [p] ->
          let nv = OpamCudf.cudf2opam p in
          Atom (nv.name, Atom (`Eq, nv.version))
        | p::ps ->
          let name = (OpamCudf.cudf2opam p).name in
          let nvs = List.map OpamCudf.cudf2opam (p::ps) in
          Atom
            (name,
             OpamFormula.formula_of_version_set
               (OpamPackage.versions_of_name packages name)
               (OpamPackage.versions_of_packages
                  (OpamPackage.Set.of_list nvs)))
      in
      (* OpamConsole.msg "non-conflicting cycles: %d\n" (List.length cy); *)
      if not print_short && cy <> [] then
        (let arrow =
           OpamConsole.colorise `yellow @@
           if OpamConsole.utf8 () then " \xe2\x86\x92 " (* U+2192 *)
           else " -> "
         in
         OpamConsole.error "Dependency cycles detected:";
         OpamConsole.errmsg "%s"
           (OpamStd.Format.itemize
              ~bullet:(OpamConsole.colorise `bold "  * ")
              (OpamStd.List.concat_map arrow
                 (fun p ->
                    OpamFormula.to_string
                      (formula_of_pkglist (OpamCudf.Map.find p node_map))))
              cy));
      List.fold_left
        (List.fold_left (fun acc p ->
             List.fold_left (fun acc p ->
                 OpamPackage.Set.add (OpamCudf.cudf2opam p) acc)
               acc (OpamCudf.Map.find p node_map)))
        OpamPackage.Set.empty cy
    in

    let all_ok =
      OpamPackage.Set.is_empty uninstallable &&
      OpamPackage.Set.is_empty cycle_packages
    in
    if print_short then
      OpamConsole.msg "%s\n"
        (OpamStd.List.concat_map "\n" OpamPackage.to_string
           (OpamPackage.Set.elements (uninstallable ++ cycle_packages)))
    else if all_ok then
      OpamConsole.msg "No issues detected on this repository\n"
    else if installability && cycles then
      OpamConsole.msg "Summary:\n\
                       - %d uninstallable roots\n\
                       - %d uninstallable dependent packages\n\
                       - %d packages are part of dependency cycles\n"
        (OpamPackage.Set.cardinal unav_roots)
        (OpamPackage.Set.cardinal unav_others)
        (OpamPackage.Set.cardinal (cycle_packages -- uninstallable));

    OpamStd.Sys.exit_because (if all_ok then `Success else `False)
  in
  Term.(const cmd $ OpamArg.global_options $ ignore_test_arg $ print_short_arg
        $ installability_arg $ cycles_arg),
  OpamArg.term_info command ~doc ~man

let pattern_list_arg =
  OpamArg.arg_list "PATTERNS"
    "Package patterns with globs. matching against $(b,NAME) or \
     $(b,NAME.VERSION)"
    Arg.string

let env_arg =
  Arg.(value & opt (list string) [] & info ["environment"] ~doc:
         "Use the given opam environment, in the form of a list \
          comma-separated 'var=value' bindings, when resolving variables. \
          This is used e.g. when computing available packages: if undefined, \
          availability of packages is not taken into account. Note that, \
          unless overriden, variables like 'root' or 'opam-version' may be \
          taken from the current opam installation. What is defined in \
          $(i,~/.opam/config) is always ignored.")

let state_selection_arg =
  let docs = OpamArg.package_selection_section in
  Arg.(value & vflag OpamListCommand.Available [
      OpamListCommand.Any, info ~docs ["A";"all"]
        ~doc:"Include all, even uninstalled or unavailable packages";
      OpamListCommand.Available, info ~docs ["a";"available"]
        ~doc:"List only packages that are available according to the defined \
              $(b,environment). Without $(b,--environment), equivalent to \
              $(b,--all).";
      OpamListCommand.Installable, info ~docs ["installable"]
        ~doc:"List only packages that are installable according to the \
              defined $(b,environment) (this calls the solver and may be \
              more costly; a package depending on an unavailable may be \
              available, but is never installable)";
    ])

let get_virtual_switch_state repo_root env =
  let env =
    List.map (fun s ->
        match OpamStd.String.cut_at s '=' with
        | Some (var,value) -> OpamVariable.of_string var, S value
        | None -> OpamVariable.of_string s, B true)
      env
  in
  let repo = OpamRepositoryBackend.local repo_root in
  let repo_file = OpamRepositoryPath.repo repo_root in
  let repo_def = OpamFile.Repo.safe_read repo_file in
  let opams = OpamRepositoryState.load_repo_opams repo in
  let gt = {
    global_lock = OpamSystem.lock_none;
    root = OpamStateConfig.(!r.root_dir);
    config = OpamStd.Option.Op.(OpamStateConfig.(load !r.root_dir) +!
                                OpamFile.Config.empty);
    global_variables = OpamVariable.Map.empty;
  } in
  let singl x = OpamRepositoryName.Map.singleton repo.repo_name x in
  let rt = {
    repos_global = gt;
    repos_lock = OpamSystem.lock_none;
    repositories = singl repo;
    repos_definitions = singl repo_def;
    repo_opams = singl opams;
  } in
  let st = OpamSwitchState.load_virtual ~repos_list:[repo.repo_name] gt rt in
  if env = [] then st else
  let gt =
    {gt with global_variables =
               OpamVariable.Map.of_list @@
               List.map (fun (var, value) ->
                   var, (lazy (Some value), "Manually defined"))
                 env }
  in
  {st with
   switch_global = gt;
   available_packages = lazy (
     OpamPackage.keys @@
     OpamPackage.Map.filter (fun package opam ->
         OpamFilter.eval_to_bool ~default:false
           (OpamPackageVar.resolve_switch_raw ~package gt
              OpamSwitch.unset OpamFile.Switch_config.empty)
           (OpamFile.OPAM.available opam))
       st.opams
   )}

let or_arg =
  Arg.(value & flag & info ~docs:OpamArg.package_selection_section ["or"]
         ~doc:"Instead of selecting packages that match $(i,all) the \
               criteria, select packages that match $(i,any) of them")

let list_command_doc = "Lists packages from a repository"
let list_command =
  let command = "list" in
  let doc = list_command_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command is similar to 'opam list', but allows listing packages \
        directly from a repository instead of what is available in a given \
        opam installation.";
    `S "ARGUMENTS";
    `S "OPTIONS";
    `S OpamArg.package_selection_section;
    `S OpamArg.package_listing_section;
  ]
  in
  let cmd
      global_options package_selection disjunction state_selection
      package_listing env packages =
    OpamArg.apply_global_options global_options;
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
  Term.(const cmd $ OpamArg.global_options $ OpamArg.package_selection $
        or_arg $ state_selection_arg $ OpamArg.package_listing $ env_arg $
        pattern_list_arg),
  OpamArg.term_info command ~doc ~man

let filter_command_doc = "Filters a repository to only keep selected packages"
let filter_command =
  let command = "filter" in
  let doc = filter_command_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command removes all package definitions that don't match the \
        search criteria (specified similarly to 'opam admin list') from a \
        repository.";
    `S "ARGUMENTS";
    `S "OPTIONS";
    `S OpamArg.package_selection_section;
  ]
  in
  let remove_arg =
    OpamArg.mk_flag ["remove"]
      "Invert the behaviour and remove the matching packages, keeping the ones \
       that don't match."
  in
  let dryrun_arg =
    OpamArg.mk_flag ["dry-run"]
      "List the removal commands, without actually performing them"
  in
  let cmd
      global_options package_selection disjunction state_selection env
      remove dryrun packages =
    OpamArg.apply_global_options global_options;
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
    if not (dryrun || OpamConsole.confirm "Confirm ?") then
      OpamStd.Sys.exit_because `Aborted
    else
    let repo = OpamRepositoryBackend.local repo_root in
    let pkg_prefixes = OpamRepository.packages_with_prefixes repo in
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
  Term.(const cmd $ OpamArg.global_options $ OpamArg.package_selection $ or_arg $
        state_selection_arg $ env_arg $ remove_arg $ dryrun_arg $
        pattern_list_arg),
  OpamArg.term_info command ~doc ~man

let add_constraint_command_doc =
  "Adds version constraints on all dependencies towards a given package"
let add_constraint_command =
  let command = "add-constraint" in
  let doc = add_constraint_command_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command searches to all dependencies towards a given package, and \
        adds a version constraint to them. It is particularly useful to add \
        upper bounds to existing dependencies when a new, incompatible major \
        version of a library is added to a repository. The new version \
        constraint is merged with the existing one, and simplified if \
        possible (e.g. $(b,>=3 & >5) becomes $(b,>5)).";
    `S "ARGUMENTS";
    `S "OPTIONS";
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
    Arg.(value & flag & info ["force"] ~doc:
           "Force updating of constraints even if the resulting constraint is \
            unsatisfiable (e.g. when adding $(b,>3) to the constraint \
            $(b,<2)). The default in this case is to print a warning and keep \
            the existing constraint unchanged.")
  in
  let cmd global_options force atom =
    OpamArg.apply_global_options global_options;
    let repo_root = OpamFilename.cwd () in
    if not (OpamFilename.exists_dir OpamFilename.Op.(repo_root / "packages"))
    then
      OpamConsole.error_and_exit `Not_found
        "No repository found in current directory.\n\
         Please make sure there is a \"packages\" directory";
    let repo = OpamRepositoryBackend.local repo_root in
    let pkg_prefixes = OpamRepository.packages_with_prefixes repo in
    let name, cstr = atom in
    let cstr = match cstr with
      | Some (relop, v) ->
        OpamFormula.Atom
          (Constraint (relop, FString (OpamPackage.Version.to_string v)))
      | None ->
        OpamFormula.Empty
    in
    let add_cstr nv n c =
      let f = OpamFormula.ands [c; cstr] in
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
                if n = name then Atom (n, (add_cstr nv n c))
                else Atom atom)
            deps0
        in
        if deps <> deps0 then
          OpamFile.OPAM.write_with_preserved_format opam_file
            (OpamFile.OPAM.with_depends deps opam))
      pkg_prefixes
  in
  Term.(pure cmd $ OpamArg.global_options $ force_arg $ atom_arg),
  OpamArg.term_info command ~doc ~man


let admin_subcommands = [
  index_command; OpamArg.make_command_alias index_command "make";
  cache_command;
  upgrade_command;
  lint_command;
  check_command;
  list_command;
  filter_command;
  add_constraint_command;
]

let default_subcommand =
  let man =
    admin_command_man @ [
      `S "COMMANDS";
      `S "COMMAND ALIASES";
    ] @ OpamArg.help_sections
  in
  let usage global_options =
    OpamArg.apply_global_options global_options;
    OpamConsole.formatted_msg
      "usage: opam admin [--version]\n\
      \                  [--help]\n\
      \                  <command> [<args>]\n\
       \n\
       The most commonly used opam commands are:\n\
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
  Term.(const usage $ OpamArg.global_options),
  Term.info "opam admin"
    ~version:(OpamVersion.to_string OpamVersion.current)
    ~sdocs:OpamArg.global_option_section
    ~doc:admin_command_doc
    ~man

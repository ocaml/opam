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
open Cmdliner

let admin_command_doc =
  "Tools for repository administrators"

let admin_command_man = [
  `S "DESCRIPTION";
  `P "This command can perform various actions on repositories in the opam \
      format. It is expected to be run from the root of a repository, i.e. a \
      directory containing a 'repo' file and a subdirectory 'packages/' \
      holding package definition within sub-subdirectories. A 'compilers/' \
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
           of failing.";
      ])
  in
  let cmd global_options urls_txt =
    OpamArg.apply_global_options global_options;
    let repo_root = OpamFilename.cwd () in
    if not (OpamFilename.exists_dir OpamFilename.Op.(repo_root / "packages"))
    then
        OpamConsole.error_and_exit
          "No repository found in current directory.\n\
           Please make sure there is a \"packages/\" directory";
    let repo_file =
      OpamFile.Repo.read_opt (OpamRepositoryPath.repo repo_root)
    in
    if repo_file = None then
      OpamConsole.warning "No \"repo\" file found.";
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
  Term.(pure cmd $ OpamArg.global_options $ urls_txt_arg),
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
                OpamFilename.create cache_dir
                  (OpamFilename.Base.of_string
                     (String.concat "/" (OpamHash.to_path first_checksum)))
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
        OpamConsole.error_and_exit
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
  Term.(pure cmd $ OpamArg.global_options $
        cache_dir_arg $ no_repo_update_arg $ link_arg $ jobs_arg),
  OpamArg.term_info command ~doc ~man


let upgrade_command_doc =
  "Upgrades a repository from an earlier version of opam to the current \
   format."
let upgrade_command =
  let command = "upgrade" in
  let doc = upgrade_command_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command is similar to 'opam upgrade', but allows upgrading packages \
        directly from a repository instead of what is available in a given \
        opam installation."
  ]
  in
  let cmd global_options =
    OpamArg.apply_global_options global_options;
  in
  Term.(pure cmd $ OpamArg.global_options $
        cache_dir_arg $ no_repo_update_arg $ link_arg $ jobs_arg),
  OpamArg.term_info command ~doc ~man

(*
let list_command_doc = "Lists packages from a repository"
let list_command =
  let command = "list" in
  let doc = list_command_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command is similar to 'opam list', but allows listing packages \
        directly from a repository instead of what is available in a given \
        opam installation."
  ]
  in
  let cmd global_options =
    OpamArg.apply_global_options global_options;
  in
  Term.(pure cmd $ OpamArg.global_options $
        cache_dir_arg $ no_repo_update_arg $ link_arg $ jobs_arg),
  OpamArg.term_info command ~doc ~man
*)

let admin_subcommands = [
  index_command; OpamArg.make_command_alias index_command "make";
  cache_command;
  upgrade_format;
  (* "list";
   * "upgrade-format";
   * "lint"; *)
]

let default_subcommand =
  let man =
    admin_command_man @ [
      `S "COMMANDS";
      `S "COMMAND ALIASES";
      `S "OPTIONS";
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
      \    index        %s\n\
      \    cache        %s\n\
       \n\
       See 'opam admin <command> --help' for more information on a specific \
       command.\n"
      index_command_doc
      cache_command_doc
  in
  Term.(pure usage $ OpamArg.global_options),
  Term.info "opam admin"
    ~version:(OpamVersion.to_string OpamVersion.current)
    ~sdocs:OpamArg.global_option_section
    ~doc:admin_command_doc
    ~man
(*
let admin_command =
  let admin_command_arg = Arg.(value (pos 0 string "admin" (info []))) in
  let subcommand_choice =
    Arg.pos 1
      (Arg.enum
         (List.map (fun ((name,cmd),info) -> name, (cmd,info))
            admin_subcommands))
      (Term.pure (), info)
      (Arg.info ~doc:admin_command_doc [])
  in
  (* let eval_choice t =
   *   (Arg.value t, Term.info "admin-sub") |>
   *   Term.eval |>
   *   Term.ret
   * in *)
  let lift _ t = ignore (Term.eval t) in
  Term.(pure lift $ admin_command_arg $ Arg.value subcommand_choice),
  (* Term.(pure (lift ((Arg.value subcommand_choice), Term.info "admin-sub"))
   *       $ ()), *)
  info
*)

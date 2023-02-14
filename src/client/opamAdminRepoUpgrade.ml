(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamProcess.Job.Op
open OpamStd.Option.Op

module O = OpamFile.OPAM

let upgradeto_version_string = "2.0"
let upgradeto_version = OpamVersion.of_string upgradeto_version_string

let ocaml_wrapper_pkgname = OpamPackage.Name.of_string "ocaml"
let ocaml_official_pkgname = OpamPackage.Name.of_string "ocaml-base-compiler"
let ocaml_variants_pkgname = OpamPackage.Name.of_string "ocaml-variants"
let ocaml_system_pkgname = OpamPackage.Name.of_string "ocaml-system"

let ocaml_conflict_class = OpamPackage.Name.of_string "ocaml-core-compiler"

let ocaml_package_names =
  [ocaml_wrapper_pkgname;
   ocaml_official_pkgname;
   ocaml_variants_pkgname;
   ocaml_system_pkgname]

(* OCaml script that generates the .config file for a given ocaml compiler *)
let wrapper_conf_script =
  "let () =\n\
  \  let ocaml_version =\n\
  \    let v = Sys.ocaml_version in\n\
  \    try String.sub v 0 (String.index v '+') with Not_found -> v\n\
  \  in\n\
  \  if ocaml_version <> \"%{_:version}%\" then\n\
  \    (Printf.eprintf\n\
  \       \"OCaml version mismatch: %%s, expected %{_:version}%\"\n\
  \       ocaml_version;\n\
  \     exit 1)\n\
  \  else\n\
  \  let oc = open_out \"%{_:name}%.config\" in\n\
  \  let exe = \".exe\" in\n\
  \  let (ocaml, suffix) =\n\
  \    let s = Sys.executable_name in\n\
  \    if Filename.check_suffix s exe then\n\
  \      (Filename.chop_suffix s exe, exe)\n\
  \    else\n\
  \      (s, \"\")\n\
  \  in\n\
  \  let ocamlc = ocaml^\"c\"^suffix in\n\
  \  let libdir =\n\
  \    let ic = Unix.open_process_in (ocamlc^\" -where\") in\n\
  \    set_binary_mode_in ic false;\n\
  \    let r = input_line ic in\n\
  \    if Unix.close_process_in ic <> Unix.WEXITED 0 then \n\
  \      failwith \"Bad return from 'ocamlc -where'\";\n\
  \    r\n\
  \  in\n\
  \  let stubsdir =\n\
  \    let ic = open_in (Filename.concat libdir \"ld.conf\") in\n\
  \    let rec r acc = try r (input_line ic::acc) with End_of_file -> acc in\n\
  \    let lines = List.rev (r []) in\n\
  \    close_in ic;\n\
  \    String.concat \":\" lines\n\
  \  in\n\
  \  let p fmt = Printf.fprintf oc (fmt ^^ \"\\n\") in\n\
  \  p \"opam-version: \\\"" ^ upgradeto_version_string ^
  "\\\"\";\n\
  \  p \"variables {\";\n\
  \  p \"  native: %%b\"\n\
  \    (Sys.file_exists (ocaml^\"opt\"^suffix));\n\
  \  p \"  native-tools: %%b\"\n\
  \    (Sys.file_exists (ocamlc^\".opt\"^suffix));\n\
  \  p \"  native-dynlink: %%b\"\n\
  \    (Sys.file_exists (Filename.concat libdir \"dynlink.cmxa\"));\n\
  \  p \"  stubsdir: %%S\"\n\
  \    stubsdir;\n\
  \  p \"  preinstalled: %{ocaml-system:installed}%\";\n\
  \  p \"  compiler: \\\"%{ocaml-system:installed?system:}%\
                       %{ocaml-base-compiler:version}%\
                       %{ocaml-variants:version}%\\\"\";\n\
  \  p \"}\";\n\
  \  close_out oc\n\
  "

let system_conf_script =
  "let () =\n\
  \  let exe = \".exe\" in\n\
  \  let ocamlc =\n\
  \    let (base, suffix) =\n\
  \      let s = Sys.executable_name in\n\
  \      if Filename.check_suffix s exe then\n\
  \        (Filename.chop_suffix s exe, exe)\n\
  \      else\n\
  \        (s, \"\") in\n\
  \    base ^ \"c\" ^ suffix in\n\
  \  if Sys.ocaml_version <> \"%{_:version}%\" then\n\
  \    (Printf.eprintf\n\
  \       \"ERROR: The compiler found at %%s has version %%s,\\n\\\n\
  \        and this package requires %{_:version}%.\\n\\\n\
  \        You should use e.g. 'opam switch create %{_:name}%.%%s' \\\n\
  \        instead.\"\n\
  \       ocamlc Sys.ocaml_version Sys.ocaml_version;\n\
  \     exit 1)\n\
  \  else\n\
  \  let ocamlc_digest = Digest.to_hex (Digest.file ocamlc) in\n\
  \  let libdir =\n\
  \    if Sys.command (ocamlc^\" -where > %{_:name}%.config\") = 0 then\n\
  \      let ic = open_in \"%{_:name}%.config\" in\n\
  \      let r = input_line ic in\n\
  \      close_in ic;\n\
  \      Sys.remove \"%{_:name}%.config\";\n\
  \      r\n\
  \    else\n\
  \      failwith \"Bad return from 'ocamlc -where'\"\n\
  \  in\n\
  \  let graphics = Filename.concat libdir \"graphics.cmi\" in\n\
  \  let graphics_digest =\n\
  \    if Sys.file_exists graphics then\n\
  \      Digest.to_hex (Digest.file graphics)\n\
  \    else\n\
  \      String.make 32 '0'\n\
  \  in\n\
  \  let oc = open_out \"%{_:name}%.config\" in\n\
  \  Printf.fprintf oc \"opam-version: \\\"" ^ upgradeto_version_string ^
  "\\\"\\n\\\n\
  \                     file-depends: [ [ %%S %%S ] [ %%S %%S ] ]\\n\\\n\
  \                     variables { path: %%S }\\n\"\n\
  \    ocamlc ocamlc_digest graphics graphics_digest (Filename.dirname ocamlc);\n\
  \  close_out oc\n\
  "

let conf_script_name = "gen_ocaml_config.ml"

let all_base_packages =
  OpamPackage.Name.Set.of_list (List.map OpamPackage.Name.of_string [
      "base-bigarray";
      "base-threads";
      "base-unix";
    ])

let cache_file : string list list OpamFile.t =
  OpamFile.make @@
  OpamFilename.of_string "~/.cache/opam-compilers-to-packages/url-hashes"

let do_upgrade repo_root =
  let write_opam ?(add_files=[]) opam =
    let nv = O.package opam in
    let pfx = Some (OpamPackage.name_to_string nv) in
    let files_dir = OpamRepositoryPath.files repo_root pfx nv in
    O.write (OpamRepositoryPath.opam repo_root pfx nv) opam;
    List.iter (fun (base,contents) ->
        OpamFilename.(write Op.(files_dir // base) contents))
      add_files
  in

  let compilers =
    let compilers_dir = OpamFilename.Op.(repo_root / "compilers") in
    if OpamFilename.exists_dir compilers_dir then (
      List.fold_left (fun map f ->
          if OpamFilename.check_suffix f ".comp" then
            let c = OpamFilename.(Base.to_string (basename (chop_extension f))) in
            OpamStd.String.Map.add c f map
          else
            map)
        OpamStd.String.Map.empty (OpamFilename.rec_files compilers_dir)
    ) else
      OpamStd.String.Map.empty
  in

  let get_url_md5, save_cache =
    let url_md5 = Hashtbl.create 187 in
    let () =
      OpamFile.Lines.read_opt cache_file +! [] |> List.iter @@ function
      | [url; md5] ->
        OpamStd.Option.iter
          (fun url -> Hashtbl.add url_md5 url (OpamHash.of_string md5))
          (OpamUrl.parse_opt ~handle_suffix:false url)
      | _ -> failwith "Bad cache, run 'opam admin upgrade --clear-cache'"
    in
    (fun url ->
       try Done (Some (Hashtbl.find url_md5 url))
       with Not_found ->
         OpamFilename.with_tmp_dir_job @@ fun dir ->
         OpamProcess.Job.ignore_errors ~default:None
           (fun () ->
                (* Download to package.patch, rather than allowing the name to be
                   guessed since, on Windows, some of the characters which are
                   valid in URLs are not valid in filenames *)
              let f =
                let base = OpamFilename.Base.of_string "package.patch" in
                OpamFilename.create dir base
              in
              OpamDownload.download_as ~overwrite:false url f @@| fun () ->
              let hash = OpamHash.compute (OpamFilename.to_string f) in
              Hashtbl.add url_md5 url hash;
              Some hash)),
    (fun () ->
       Hashtbl.fold
         (fun url hash l -> [OpamUrl.to_string url; OpamHash.to_string hash]::l)
         url_md5 [] |> fun lines ->
       try OpamFile.Lines.write cache_file lines with e ->
         OpamStd.Exn.fatal e;
         OpamConsole.log "REPO_UPGRADE"
           "Could not write archive hash cache to %s, skipping (%s)"
           (OpamFile.to_string cache_file)
           (Printexc.to_string e))
  in
  let ocaml_versions =
    OpamStd.String.Map.fold (fun c comp_file ocaml_versions ->
      let comp = OpamFile.Comp.read (OpamFile.make comp_file) in
      let descr_file =
        OpamFilename.(opt_file (add_extension (chop_extension comp_file) "descr"))
      in
      let descr = descr_file >>| fun f -> OpamFile.Descr.read (OpamFile.make f) in
      let nv, ocaml_version, variant =
        match OpamStd.String.cut_at c '+' with
        | None ->
          OpamPackage.create ocaml_official_pkgname
            (OpamPackage.Version.of_string c),
          c, None
        | Some (version,variant) ->
          OpamPackage.create ocaml_variants_pkgname
            (OpamPackage.Version.of_string (version^"+"^variant)),
          version, Some variant
      in

      (* (Some exotic compiler variants have e.g. 'lwt' as base package, which
         won't work in our current setup. They'll need to be rewritten, but
         break the following detection of all base packages, which isn't
         idempotent anyway...)

         List.iter (fun (name, _) ->
             all_base_packages := OpamPackage.Name.Set.add name !all_base_packages)
           (OpamFormula.atoms (OpamFile.Comp.packages comp));
      *)

      let opam = OpamFormatUpgrade.comp_file ~package:nv ?descr comp in
      let opam = O.with_conflict_class [ocaml_conflict_class] opam in
      let opam =
        match OpamFile.OPAM.url opam with
        | Some urlf when OpamFile.URL.checksum urlf = [] ->
          let url = OpamFile.URL.url urlf in
          (match url.OpamUrl.backend with
           | #OpamUrl.version_control -> Some opam
           | `rsync when OpamUrl.local_dir url <> None -> Some opam
           | _ ->
             (match OpamProcess.Job.run (get_url_md5 url) with
              | None -> None
              | Some hash ->
                Some
                  (OpamFile.OPAM.with_url (OpamFile.URL.with_checksum [hash] urlf)
                     opam)))
        | _ -> Some opam
      in
      match opam with
      | None ->
        OpamConsole.error
          "Could not get the archive of %s, skipping"
          (OpamPackage.to_string nv);
        ocaml_versions
      | Some opam ->
      let patches = OpamFile.Comp.patches comp in
      if patches <> [] then
        OpamConsole.msg "Fetching patches of %s to check their hashes...\n"
          (OpamPackage.to_string nv);
      let extra_sources =
        (* Download them just to get their MD5 *)
        OpamParallel.map
          ~jobs:3
          ~command:(fun url ->
              get_url_md5 url @@| function
              | Some md5 -> Some (url, md5, None)
              | None ->
                OpamConsole.error
                  "Could not get patch file for %s from %s, skipping"
                  (OpamPackage.to_string nv) (OpamUrl.to_string url);
                None)
          (OpamFile.Comp.patches comp)
      in
      if List.mem None extra_sources then ocaml_versions
      else
      let opam =
        opam |>
        OpamFile.OPAM.with_extra_sources
          (List.map (fun (url, hash, _) ->
               OpamFilename.Base.of_string (OpamUrl.basename url),
               OpamFile.URL.create ~checksum:[hash] url)
              (OpamStd.List.filter_some extra_sources))
      in
      write_opam opam;

      if variant = None then begin
        (* "official" compiler release: generate a system compiler package *)
        let sys_nv = OpamPackage.create ocaml_system_pkgname nv.version in
        let rev_dep_flag =
          Filter (FIdent ([], OpamVariable.of_string "post", None))
        in
        let system_opam =
          O.create sys_nv |>
          O.with_substs [OpamFilename.Base.of_string conf_script_name] |>
          O.with_build [
            List.map (fun s -> CString s, None)
              [ "ocaml"; conf_script_name ],
            None
          ] |>
          O.with_conflict_class [ocaml_conflict_class] |>
          O.with_depends (OpamFormula.ands (
              List.map (fun name ->
                  Atom (OpamPackage.Name.of_string name, Atom (rev_dep_flag)))
                ["ocaml"; "base-unix"; "base-threads"; "base-bigarray"]
            )) |>
          O.with_maintainer [ "platform@lists.ocaml.org" ] |>
          O.with_flags [Pkgflag_Compiler] |>
          O.with_descr
            (OpamFile.Descr.create
                 "The OCaml compiler (system version, from outside of opam)") |>
          O.with_available
            (FOp (FIdent ([],OpamVariable.of_string "sys-ocaml-version",None),
                  `Eq,
                  FString (OpamPackage.Version.to_string nv.version)))
            (* add depext towards an 'ocaml' package? *)
        in
        write_opam ~add_files:[conf_script_name^".in", system_conf_script]
          system_opam
      end;

      (* cleanup *)
      OpamFilename.remove comp_file;
      OpamStd.Option.iter OpamFilename.remove descr_file;
      OpamFilename.rmdir_cleanup (OpamFilename.dirname comp_file);
      OpamConsole.status_line
        "Compiler %s successfully converted to package %s"
        c (OpamPackage.to_string nv);
      OpamStd.String.Set.add ocaml_version ocaml_versions
      )
    compilers OpamStd.String.Set.empty
  in
  OpamConsole.clear_status ();
  save_cache ();

  (* Generate "ocaml" package wrappers depending on one of the implementations
     at the appropriate version *)
  let gen_ocaml_wrapper str_version =
    let version = OpamPackage.Version.of_string str_version in
    let wrapper_nv = OpamPackage.create ocaml_wrapper_pkgname version in
    let upper_bound_v =
      try
        let g = Re.(exec @@ compile @@ seq [rep1 digit; eos]) str_version in
        let sn = Re.Group.get g 0 in
        String.sub str_version 0 (fst (Re.Group.offset g 0)) ^
        (string_of_int (1 + int_of_string sn)) ^ "~"
      with Not_found -> str_version ^ "a"
    in
    let wrapper_opam =
      O.create wrapper_nv |>
      O.with_substs [OpamFilename.Base.of_string conf_script_name] |>
      O.with_build [
        List.map (fun s -> CString s, None)
          [ "ocaml"; "-I"; "+unix"; "unix.cma"; conf_script_name ],
        None
      ] |>
      O.with_maintainer [ "platform@lists.ocaml.org" ] |>
      O.with_build_env ["CAML_LD_LIBRARY_PATH", Eq, "", None] |>
      O.with_env [
        "CAML_LD_LIBRARY_PATH", Eq, "%{_:stubsdir}%", None;
        "CAML_LD_LIBRARY_PATH", PlusEq, "%{lib}%/stublibs", None;
        "OCAML_TOPLEVEL_PATH", Eq, "%{toplevel}%", None;
      ] |>
      (* leave the Compiler flag to the implementations (since the user
         needs to select one)
         O.with_flags [Pkgflag_Compiler] |> *)
      O.with_descr
        (OpamFile.Descr.create
           "The OCaml compiler (virtual package)\n\
            This package requires a matching implementation of OCaml,\n\
            and polls it to initialise specific variables like \
            `ocaml:native-dynlink`") |>
      O.with_depends
        (OpamFormula.ors [
            Atom (
              ocaml_official_pkgname,
              Atom (Constraint (`Eq, FString str_version))
            );
            Atom (
              ocaml_variants_pkgname,
              OpamFormula.ands [
                Atom (Constraint (`Geq, FString str_version));
                Atom (Constraint (`Lt, FString upper_bound_v));
              ]
            );
            Atom (
              ocaml_system_pkgname,
              Atom (Constraint (`Eq, FString str_version))
            )
          ])
    in
    write_opam ~add_files:[conf_script_name^".in", wrapper_conf_script]
      wrapper_opam
  in
  OpamStd.String.Set.iter gen_ocaml_wrapper ocaml_versions;

  let packages = OpamRepository.packages_with_prefixes repo_root in

  OpamConsole.log "REPO_UPGRADE"
    "Will not update base packages: %s\n"
    (OpamPackage.Name.Set.to_string all_base_packages);

  OpamPackage.Map.iter (fun package prefix ->
      let opam_file = OpamRepositoryPath.opam repo_root prefix package in
      let opam0 = OpamFile.OPAM.read opam_file in
      OpamFile.OPAM.print_errors ~file:opam_file opam0;
      let nv = OpamFile.OPAM.package opam0 in
      if not (List.mem nv.name ocaml_package_names) &&
         not (OpamPackage.Name.Set.mem nv.name all_base_packages) then
        let opam = OpamFileTools.add_aux_files ~files_subdir_hashes:true opam0 in
        let opam =
          OpamFormatUpgrade.opam_file_from_1_2_to_2_0 ~filename:opam_file opam
        in
        if opam <> opam0 then
          (OpamFile.OPAM.write_with_preserved_format opam_file opam;
           List.iter OpamFilename.remove [
             OpamFile.filename
               (OpamRepositoryPath.descr repo_root prefix package);
             OpamFile.filename
               (OpamRepositoryPath.url repo_root prefix package);
           ];
           OpamConsole.status_line "Updated %s" (OpamFile.to_string opam_file))
    )
    packages;
  OpamConsole.clear_status ();

  let repo_file = OpamRepositoryPath.repo repo_root in
  OpamFile.Repo.write repo_file
    (OpamFile.Repo.with_opam_version upgradeto_version
       (OpamFile.Repo.safe_read repo_file))

let clear_cache () =
  OpamFilename.remove (OpamFile.filename cache_file)

let do_upgrade_mirror repo_root base_url =
  OpamFilename.with_tmp_dir @@ fun tmp_mirror_dir ->
  let open OpamFilename.Op in
  let copy_dir d =
    let src = repo_root / d in
    if OpamFilename.exists_dir src then
      OpamFilename.copy_dir ~src ~dst:(tmp_mirror_dir / d)
  in
  let copy_file f =
    let src = repo_root // f in
    if OpamFilename.exists src then
      OpamFilename.copy ~src ~dst:(tmp_mirror_dir // f)
  in
  copy_dir "packages";
  copy_dir "compilers";
  copy_file "repo";
  do_upgrade tmp_mirror_dir;
  let repo_file = OpamFile.make (OpamFilename.of_string "repo") in
  let repo0 = OpamFile.Repo.safe_read repo_file in
  let opam_version_fid =
    FIdent ([], OpamVariable.of_string "opam-version", None)
  in
  let redir =
    OpamUrl.to_string OpamUrl.Op.(base_url / upgradeto_version_string),
    Some (FOp (opam_version_fid, `Geq,
               FString (upgradeto_version_string ^ "~")))
  in
  let repo0 =
    if OpamFile.Repo.opam_version repo0 = None
    then OpamFile.Repo.with_opam_version (OpamVersion.of_string "1.2") repo0
    else repo0
  in
  let repo0 =
    OpamFile.Repo.with_redirect
      (List.filter (fun r -> r <> redir) (OpamFile.Repo.redirect repo0))
      repo0
  in
  let repo_12 =
    OpamFile.Repo.with_redirect (redir :: OpamFile.Repo.redirect repo0)
      repo0
  in
  let repo_20 =
    let redir = (OpamUrl.to_string base_url,
                 Some (FOp (opam_version_fid, `Lt,
                            FString (upgradeto_version_string ^ "~"))))
    in
    repo0 |>
    OpamFile.Repo.with_opam_version OpamFile.Repo.format_version |>
    OpamFile.Repo.with_redirect (redir :: OpamFile.Repo.redirect repo0)
  in
  OpamFile.Repo.write repo_file repo_12;
  OpamFile.Repo.write
    (OpamFile.make OpamFilename.Op.(tmp_mirror_dir // "repo"))
    repo_20;
  let dir20 = OpamFilename.Dir.of_string upgradeto_version_string in
  OpamFilename.rmdir dir20;
  OpamFilename.move_dir ~src:tmp_mirror_dir ~dst:dir20;
  OpamConsole.note
    "Indexes need updating: you should now run\n\
     \n%s\
    \  cd %s && opam admin index"
    (if repo_12 <> repo0 &&
        OpamFilename.exists (OpamFilename.of_string "urls.txt")
     then
       "  opam admin index --full-urls-txt\n"
     else "")
    (OpamFilename.remove_prefix_dir repo_root dir20)

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

let descr_template =
  OpamFile.Descr.of_string "Short description\n\nLong\ndescription\n"

let () =
  OpamHTTP.register ()

let mkwarn () =
  let warnings = ref ([]: string list) in
  (fun s -> warnings := s::!warnings),
  (fun file -> match !warnings with
     | [] -> true
     | w ->
       OpamGlobals.error "In %s:\n  - %s\n"
         (OpamFilename.to_string file)
         (String.concat "\n  - " (List.rev w));
       false)

let check_opam file =
  let module OF = OpamFile.OPAM in
  try
    let _opam = OF.read file in
    (* let warn, warnings = mkwarn () in *)
    (* Add factored-out todo "opam-lint" here *)
    (* warnings file *)
    true
  with e ->
    OpamMisc.fatal e;
    OpamGlobals.error "Couldn't read %s" (OpamFilename.to_string file);
    false

let check_descr file =
  let module OF = OpamFile.Descr in
  try
    let descr = OF.read file in
    let warn, warnings = mkwarn () in
    if OF.synopsis descr = OF.synopsis descr_template ||
       OpamMisc.strip (OF.synopsis descr) = "" then
      warn "short description unspecified";
    if OF.body descr = OF.body descr_template ||
       OpamMisc.strip (OF.body descr) = "" then
      warn "long description unspecified";
    warnings file
  with e ->
    OpamMisc.fatal e;
    OpamGlobals.error "Couldn't read %s" (OpamFilename.to_string file);
    false

let check_url file =
  let module OF = OpamFile.URL in
  try
    let url = OF.read file in
    let warn, warnings = mkwarn () in
    let checksum = OF.checksum url in
    if checksum = None then warn "no checksum supplied";
    let check_url address =
      let addr,kind = OpamTypesBase.parse_url address in
      if snd address <> None || kind <> `http then
        warn (Printf.sprintf "%s is not a regular http or ftp address"
                (OpamTypesBase.string_of_address addr))
      else
        OpamFilename.with_tmp_dir @@ fun tmpdir ->
        let name =
          OpamPackage.of_string
            (Filename.basename (OpamTypesBase.string_of_address address))
        in
        let archive = OpamRepository.pull_url kind name tmpdir None [address] in
        match archive with
        | Not_available s ->
          warn (Printf.sprintf "%s couldn't be fetched (%s)"
                  (OpamTypesBase.string_of_address address)
                  s)
        | Result (F f) ->
          if checksum <> None && Some (OpamFilename.digest f) <> checksum then
            warn (Printf.sprintf "bad checksum for %s"
                    (OpamTypesBase.string_of_address address))
        | _ -> assert false
    in
    List.iter check_url (OF.url url :: OF.mirrors url);
    warnings file
  with e ->
    OpamMisc.fatal e;
    OpamGlobals.error "Couldn't read %s" (OpamFilename.to_string file);
    false


(* -- Prepare command -- *)

let prepare ?name ?version http_url =
  let open OpamFilename.OP in
  OpamFilename.with_tmp_dir (fun tmpdir ->
      (* Fetch the archive *)
      let url = (http_url,None) in
      let f =
        OpamRepository.pull_url `http
          (OpamPackage.of_string (Filename.basename http_url)) tmpdir None
          [url]
      in
      let archive = match f with
        | Not_available s ->
          OpamGlobals.error_and_exit "%s is not available: %s" http_url s
        | Result (F file) -> file
        | _ -> assert false
      in
      let checksum = List.hd (OpamFilename.checksum archive) in
      let srcdir = tmpdir / "src" in
      OpamFilename.extract archive srcdir;
      (* Gather metadata *)
      let meta_dir =
        if OpamFilename.exists_dir (srcdir / "opam")
        then srcdir / "opam"
        else srcdir
      in
      let src_opam =
        if OpamFilename.exists (meta_dir // "opam")
        then OpamFile.OPAM.read (meta_dir // "opam")
        else OpamGlobals.error_and_exit "No metadata found"
      in
      let name = match name, OpamFile.OPAM.name_opt src_opam with
        | None, None ->
          OpamGlobals.error_and_exit "Package name unspecified"
        | Some n1, Some n2 when n1 <> n2 ->
          OpamGlobals.warning
            "Publishing as package %s, while it refers to itself as %s"
            (OpamPackage.Name.to_string n1) (OpamPackage.Name.to_string n2);
          n1
        | Some n, _ | None, Some n -> n
      in
      let version = match version, OpamFile.OPAM.version_opt src_opam with
        | Some v, _ | None, Some v -> v
        | _ ->
          OpamGlobals.error_and_exit "Package version unspecified"
      in
      let package = OpamPackage.create name version in
      let src_descr =
        if OpamFilename.exists (meta_dir // "descr")
        then OpamFile.Descr.read (meta_dir // "descr")
        else descr_template
      in
      (* TODO: add data from the repo if found, take the best of the two.
         Use data from existing prepare_dir if specified instead of URL ?
         Just update url file in prepare_dir if both specified ? *)
      (* Fix and generate missing metadata *)
      let prep_url =
        OpamFile.URL.with_checksum (OpamFile.URL.create `http url) checksum
      in
      let prep_opam = OpamFile.OPAM.with_name_opt src_opam None in
      let prep_opam = OpamFile.OPAM.with_version_opt prep_opam None in
      (* Generate prepare dir *)
      let prepare_dir =
        OpamFilename.cwd () / OpamPackage.to_string package
      in
      if OpamFilename.exists_dir prepare_dir &&
         not (OpamGlobals.confirm "%s exists. Override contents ?"
                (OpamFilename.Dir.to_string prepare_dir))
      then () else
      OpamFile.OPAM.write (prepare_dir // "opam") prep_opam;
      OpamFile.Descr.write (prepare_dir // "descr") src_descr;
      OpamFile.URL.write (prepare_dir // "url") prep_url;
      if OpamFilename.exists_dir (meta_dir / "files") then
        OpamFilename.copy_dir ~src:(meta_dir / "files") ~dst:prepare_dir;

      OpamGlobals.msg
        "Template metadata for %s generated in %s.\n\
        \  * Check the 'opam' file\n\
        \  * Fill in or check the description of your package in 'descr'\n\
        \  * Check that there are no unneeded files under 'files/'\n\
        \  * Run 'opam publish submit %s' to submit your package\n"
        (OpamPackage.to_string package)
        (OpamFilename.prettify_dir prepare_dir)
        (OpamFilename.prettify_dir prepare_dir)
    )

(* -- Submit command -- *)

let git cmds = OpamSystem.command ("git" :: cmds)
let hub cmds = OpamSystem.command ("hub" :: cmds)

let github_root = "git@github.com:"
let github_api = "https://api.github.com/repos"
let owner = "ocaml"
let repo = "opam-repository"

let (/) a b = String.concat "/" [a;b]

let init_mirror dir user =
  OpamFilename.mkdir dir;
  git ["clone"; github_root/owner/repo^".git";
       OpamFilename.Dir.to_string dir];
  OpamSystem.command ~verbose:true (* FORK ! (WARN: there may be a delay !) *)
    ["curl"; "-u"; user; "-d"; "";
     github_api/owner/repo/"forks"];
  OpamFilename.in_dir dir (fun () ->
      git ["remote"; "add"; "user"; github_root/user/repo]
    )

let update_mirror dir =
  OpamFilename.in_dir dir (fun () ->
      git ["fetch"; "--multiple"; "origin"; "user"];
      git ["reset"; "origin/master"; "--hard"];
    )

let add_metadata mirror user user_meta_dir package =
  let meta_dir =
    let (/) = Filename.concat in
    "packages" /
    OpamPackage.Name.to_string (OpamPackage.name package) /
    OpamPackage.to_string package
  in
  OpamFilename.in_dir mirror (fun () ->
      (try git ["rm"; "-r"; meta_dir] with e -> OpamMisc.fatal e);
      OpamFilename.copy_dir
        ~src:user_meta_dir
        ~dst:(OpamFilename.Dir.of_string meta_dir);
      git ["add"; meta_dir];
      git ["commit"; "-m";
           Printf.sprintf "[opam-publish] description for %s"
             (OpamPackage.to_string package)];
      git ["push"; "user"; "+HEAD:opam-publish/"^OpamPackage.to_string package]);
  OpamSystem.command ~verbose:true (* PULL-REQUEST *)
    ["curl"; "-u"; user; "-d";
     Printf.sprintf "\
{\n\
       \"title\" : %S,\n\
       \"body\" : %S,\n\
       \"head\" : %S,\n\
       \"base\" : %S\n\
}"
       ("[opam-publish] TESTING "^OpamPackage.to_string package)
       "PLEASE IGNORE this is from a test version of opam-publish"
       (user^":opam-publish"/OpamPackage.to_string package)
       "master";
     github_api/owner/repo/"pulls"];
  let url = "https://github.com"/owner/repo/"pulls" in (*TODO: extract real pull*)
  OpamGlobals.msg "Pull-requested: %s\n" url;
  OpamSystem.command ["xdg-open";url]

let sanity_checks meta_dir =
  let files = OpamFilename.files meta_dir in
  let dirs = OpamFilename.dirs meta_dir in
  let warns =
    files |> List.fold_left (fun warns f ->
        match OpamFilename.Base.to_string (OpamFilename.basename f) with
        | "opam" | "descr" | "url" -> warns
        | f -> Printf.sprintf "extra file %S" f :: warns
      ) []
  in
  let warns =
    dirs |> List.fold_left (fun warns d ->
        match OpamFilename.Base.to_string (OpamFilename.basename_dir d) with
        | "files" -> warns
        | d -> Printf.sprintf "extra dir %S" d :: warns
      ) warns
  in
  if warns <> [] then
    OpamGlobals.error "Bad contents in %s:\n  - %s\n"
      (OpamFilename.Dir.to_string meta_dir)
      (String.concat "\n  - " warns);
  let ok = warns = [] in
  let ok = check_opam OpamFilename.OP.(meta_dir // "opam") && ok in
  let ok = check_url OpamFilename.OP.(meta_dir // "url") && ok in
  let ok = check_descr OpamFilename.OP.(meta_dir // "descr") && ok in
  ok

let submit package meta_dir user =
  if not (sanity_checks meta_dir) then
    OpamGlobals.error "Please correct the above errors and retry"
  else
  (* pull-request processing *)
  let mirror_dir =
    OpamFilename.OP.(
      OpamFilename.Dir.of_string OpamGlobals.default_opam_dir /
      "plugins" / "opam-publish" / "default"
    ) in
  if not (OpamFilename.exists_dir mirror_dir) then
    init_mirror mirror_dir user;
  update_mirror mirror_dir;
  add_metadata mirror_dir user meta_dir package


(* -- Command-line handling -- *)

open Cmdliner

(* name * version option *)
let package =
  let parse str =
    let name, version_opt =
      match OpamMisc.cut_at str '.' with
      | None -> str, None
      | Some (n,v) -> n, Some v
    in
    try
      `Ok
        (OpamPackage.Name.of_string name,
         OpamMisc.Option.map OpamPackage.Version.of_string version_opt)
    with Failure _ -> `Error (Printf.sprintf "bad package name %s" name)
  in
  let print ppf (name, version_opt) =
    match version_opt with
    | None -> Format.pp_print_string ppf (OpamPackage.Name.to_string name)
    | Some v -> Format.fprintf ppf "%s.%s"
                  (OpamPackage.Name.to_string name)
                  (OpamPackage.Version.to_string v)
  in
  parse, print

let prepare_cmd =
  let doc = "Gets a local metadatada directory from a given remote archive URL, \
             to let you edit locally before submitting." in
  let url = Arg.(required & pos ~rev:true 0 (some string) None & info
                   ~doc:"Public URL hosting the package source archive"
                   ~docv:"URL" [])
  in
  let pkg_opt = Arg.(value & pos ~rev:true 1 (some package) None & info
                   ~doc:"Package to release" [])
  in
  let prepare url pkg_opt =
    OpamMisc.Option.Op.(
      prepare ?name:(pkg_opt >>| fst) ?version:(pkg_opt >>= snd) url
    )
  in
  Term.(pure prepare $ url $ pkg_opt),
  Term.info "prepare" ~doc

let submit_cmd =
  let doc = "submits or updates a pull-request to an OPAM repo." in
  let user = Arg.(required & opt (some string) None & info
                    ~doc:"github user name" ["n";"name"]) in
  let dir = Arg.(required & pos ~rev:true 0 (some string) None & info
                   ~doc:"Path to the metadata from opam-publish prepare" []) in
  let submit user dir =
    submit (OpamPackage.of_string (Filename.basename dir))
      (OpamFilename.Dir.of_string dir) user
  in
  Term.(pure submit $ user $ dir),
  Term.info "submit" ~doc

let cmds = [prepare_cmd; submit_cmd]

let help_cmd =
  let usage () =
    OpamGlobals.msg "\
Sub-commands:\n\
\      prepare URL   Prepares a local package definition directory from a\n\
\                    public URL pointing to a source archive.\n\
\      submit DIR    Submits or updates the request for integration of\n\
\                    the package defined by metadata at DIR.\n\
\n\
See '%s COMMAND --help' for details on each command.\n\
"
    Sys.argv.(0);
    `Help (`Pager, None)
  in
  Term.(ret (pure usage $ pure ())),
  Term.info "opam-publish"
    ~version:(OpamVersion.to_string OpamVersion.current)

let () =
  Sys.catch_break true;
  let _ = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  try match Term.eval_choice ~catch:false help_cmd cmds with
    | `Error _ -> exit 1
    | _ -> exit 0
  with
  | OpamGlobals.Exit i as e ->
    if !OpamGlobals.debug && i <> 0 then
      Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
    exit i
  | OpamSystem.Internal_error _
  | OpamSystem.Process_error _ as e ->
    Printf.eprintf "%s\n" (Printexc.to_string e);
    Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
  | Sys.Break ->
    exit 130
  | Failure msg as e ->
    Printf.eprintf "Fatal error: %s\n" msg;
    Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
    exit 1
  | e ->
    Printf.eprintf "Fatal error:\n%s\n" (Printexc.to_string e);
    Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
    exit 1


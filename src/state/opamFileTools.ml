(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
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
open OpamTypesBase

let log fmt = OpamConsole.log "opam-file" fmt

(* Templating & linting *)

let template nv =
  let open OpamFile.OPAM in
  let maintainer =
    let from_git = try
        match
          OpamSystem.read_command_output
            ["git"; "config"; "--get"; "user.name"],
          OpamSystem.read_command_output
            ["git"; "config"; "--get"; "user.email"]
        with
        | [name], [email] ->
          Some [Printf.sprintf "%s <%s>" name email]
        | _ -> raise Not_found
      with e -> OpamStd.Exn.fatal e; None
    in
    match from_git with
    | Some u -> u
    | None ->
      let email =
        try Some (Sys.getenv "EMAIL") with Not_found -> None in
      try
        let open Unix in
        let pw = getpwuid (getuid ()) in
        let email = match email with
          | Some e -> e
          | None -> pw.pw_name^"@"^gethostname () in
        match OpamStd.String.split pw.pw_gecos ',' with
        | name::_ -> [Printf.sprintf "%s <%s>" name email]
        | _ -> [email]
      with Not_found -> match email with
        | Some e -> [e]
        | None -> []
  in
  create nv
  |> with_maintainer maintainer
  |> with_build
    [[CString "./configure", None;
      CString "--prefix=%{prefix}%", None], None;
     [CIdent "make", None], None]
  |> with_install
    [[CIdent "make", None; CString "install", None], None]
  |> with_remove
    [[CString "ocamlfind", None; CString "remove", None;
      CString
        (OpamPackage.Name.to_string (nv.OpamPackage.name)),
      None],
     None]
  |> with_depends
    (Atom (OpamPackage.Name.of_string "ocamlfind",
           (Atom (OpamFilter.(Filter (FIdent (ident_of_string "build")))))))
  |> with_author maintainer
  |> with_homepage [""]
  |> with_license [""]
  |> with_dev_repo (OpamUrl.of_string "git+https://")
  |> with_bug_reports [""]

let lint t =
  let open OpamFile.OPAM in
  let cond num level msg ?detail cd =
    if cd then
      let msg = match detail with
        | Some d ->
          Printf.sprintf "%s: \"%s\"" msg (String.concat "\", \"" d)
        | None -> msg
      in
      Some (num, level, msg)
    else None
  in
  let names_of_formula flag f =
    OpamPackageVar.filter_depends_formula
      ~build:true ~dev:true ~test:flag ~doc:flag ~default:false
      ~env:OpamStd.Option.none f
    |> OpamFormula.atoms
    |> List.map fst
    |> OpamPackage.Name.Set.of_list
  in
  let all_commands =
    t.build @ t.install @ t.remove @ t.build_test @ t.build_doc
  in
  let all_filters =
    OpamStd.List.filter_map snd t.patches @
    OpamStd.List.filter_map snd t.messages @
    OpamStd.List.filter_map snd t.post_messages @
    [t.available] @
    OpamFormula.fold_left (fun acc (_, f) ->
        OpamFormula.fold_left (fun acc -> function
            | Constraint _ -> acc
            | Filter f -> f :: acc)
          acc f)
      [] (OpamFormula.ands [t.depends; t.depopts]) @
    List.map (fun (_,_,f) -> f) t.features
  in
  let all_variables =
    OpamFilter.commands_variables all_commands @
    List.fold_left (fun acc f -> OpamFilter.variables f @ acc)
      [] all_filters
  in
  let all_expanded_strings =
    List.map fst t.messages @
    List.map fst t.post_messages @
    List.fold_left (fun acc (args, _) ->
        List.fold_left
          (fun acc -> function CString s, _ -> s :: acc | _ -> acc)
          acc args)
      [] all_commands @
    List.fold_left
      (OpamFilter.fold_down_left
         (fun acc -> function FString s -> s :: acc | _ -> acc))
      [] all_filters
  in
  let all_depends =
    OpamPackage.Name.Set.union
      (names_of_formula true t.depends)
      (names_of_formula true t.depopts)
  in
  let warnings = [
    cond 20 `Warning
      "Field 'opam-version' refers to the patch version of opam, it \
       should be of the form MAJOR.MINOR"
      ~detail:[OpamVersion.to_string t.opam_version]
      (OpamVersion.nopatch t.opam_version <> t.opam_version);
    cond 21 `Error
      "Field 'opam-version' doesn't match the current version, \
       validation may not be accurate"
      ~detail:[OpamVersion.to_string t.opam_version]
      (OpamVersion.compare t.opam_version OpamVersion.current_nopatch <> 0
       && OpamVersion.compare t.opam_version (OpamVersion.of_string "1.2")
          <> 0);
(*
          cond (t.name = None)
            "Missing field 'name' or directory in the form 'name.version'";
          cond (t.version = None)
            "Missing field 'version' or directory in the form 'name.version'";
*)
    (let empty_fields =
       OpamStd.List.filter_map (function n,[""] -> Some n | _ -> None)
         ["maintainer", t.maintainer; "homepage", t.homepage;
          "author", t.author; "license", t.license; "doc", t.doc;
          "tags", t.tags; "bug_reports", t.bug_reports]
     in
     cond 22 `Error
       "Some fields are present but empty; remove or fill them"
       ~detail:empty_fields
       (empty_fields <> []));
    cond 23 `Error
      "Missing field 'maintainer'"
      (t.maintainer = []);
    cond 24 `Error
      "Field 'maintainer' has the old default value"
      (List.mem "contact@ocamlpro.com" t.maintainer &&
       not (List.mem "org:ocamlpro" t.tags));
    cond 25 `Error
      "Missing field 'authors'"
      (t.author = []);
    cond 26 `Warning
      "No field 'install', but a field 'remove': install instructions \
       probably part of 'build'. Use the 'install' field or a .install \
       file"
      (t.install = [] && t.build <> [] && t.remove <> []);
    cond 27 `Warning
      "No field 'remove' while a field 'install' is present, uncomplete \
       uninstallation suspected"
      (t.install <> [] && t.remove = []);
    (let unk_flags =
       OpamStd.List.filter_map (function
           | Pkgflag_Unknown s -> Some s
           | _ -> None)
         t.flags
     in
     cond 28 `Error
       "Unknown package flags found"
       ~detail:unk_flags
       (unk_flags <> []));
    (let filtered_vars =
       OpamFilter.variables_of_filtered_formula t.depends @
       OpamFilter.variables_of_filtered_formula t.depopts
       |> List.filter (fun v -> not (OpamVariable.Full.is_global v))
       |> List.map OpamVariable.Full.to_string
     in
     cond 29 `Error
       "Package dependencies mention package variables"
       ~detail:filtered_vars
       (filtered_vars <> []));
    cond 30 `Error
      "Field 'depopts' is not a pure disjunction"
      (List.exists (function
           | OpamFormula.Atom _ -> false
           | _ -> true)
          (OpamFormula.ors_to_list t.depopts));
    (let dup_depends =
       OpamPackage.Name.Set.inter
         (names_of_formula false t.depends)
         (names_of_formula true t.depopts)
     in
     cond 31 `Error
       "Fields 'depends' and 'depopts' refer to the same package names"
       ~detail:OpamPackage.Name.
                 (List.map to_string (Set.elements dup_depends))
       (not (OpamPackage.Name.Set.is_empty dup_depends)));
    cond 32 `Error
      "Field 'ocaml-version' is deprecated, use 'available' and the \
       'ocaml-version' variable instead"
      (t.ocaml_version <> None);
    cond 33 `Error
      "Field 'os' is deprecated, use 'available' and the 'os' variable \
       instead"
      (t.os <> Empty);
    (let pkg_vars =
       List.filter (fun v -> not (OpamVariable.Full.is_global v))
         (OpamFilter.variables t.available)
     in
     cond 34 `Error
       "Field 'available' contains references to package-local variables. \
        It should only be determined from global configuration variables"
       ~detail:(List.map OpamVariable.Full.to_string pkg_vars)
       (pkg_vars <> []));
    cond 35 `Error
      "Missing field 'homepage'"
      (t.homepage = []);
    (* cond (t.doc = []) *)
    (*   "Missing field 'doc'"; *)
    cond 36 `Warning
      "Missing field 'bug-reports'"
      (t.bug_reports = []);
    cond 37 `Warning
      "Missing field 'dev-repo'"
      (t.dev_repo = None);
(*
        cond 38 `Warning
          "Package declares 'depexts', but has no 'post-messages' to help \
           the user out when they are missing"
          (t.depexts <> None && t.post_messages = []);
*)
    cond 39 `Error
      "Command 'make' called directly, use the built-in variable \
       instead"
      (List.exists (function
           | (CString "make", _)::_, _ -> true
           | _ -> false
         ) all_commands);
    cond 40 `Warning
      "Field 'features' is still experimental and not yet to be used on \
       the official repo"
      (t.features <> []);
    (let alpha_flags =
       OpamStd.List.filter_map (function
           | Pkgflag_LightUninstall | Pkgflag_Unknown _ -> None
           | f ->
             if List.exists (fun tag -> flag_of_tag tag = Some f) t.tags
             then None
             else Some (string_of_pkg_flag f))
         t.flags
     in
     cond 40 `Warning
       "Package uses flags that aren't recognised by earlier versions in \
        OPAM 1.2 branch. At the moment, you should use a tag \"flags:foo\" \
        instead for compatibility"
       ~detail:alpha_flags
       (alpha_flags <> []));
    (let undep_pkgs =
       List.fold_left
         (fun acc v ->
            match OpamVariable.Full.package v with
            | Some n when
                t.OpamFile.OPAM.name <> Some n &&
                not (OpamPackage.Name.Set.mem n all_depends) &&
                OpamVariable.(Full.variable v <> of_string "installed")
              ->
              OpamPackage.Name.Set.add n acc
            | _ -> acc)
         OpamPackage.Name.Set.empty all_variables
     in
     cond 41 `Warning
       "Some packages are mentionned in package scripts of features, but \
        there is no dependency or depopt toward them"
       ~detail:OpamPackage.Name.
                 (List.map to_string (Set.elements undep_pkgs))
       (not (OpamPackage.Name.Set.is_empty undep_pkgs)));
    cond 42 `Error
      "The 'dev-repo:' field doesn't use version control. You should use \
       URLs of the form \"git://\", \"git+https://\", \"hg+https://\"..."
      (match t.dev_repo with
       | None -> false
       | Some { OpamUrl.backend = #OpamUrl.version_control; _ } -> false
       | Some _ -> true);
    cond 43 `Error
      "Conjunction used in 'conflicts:' field. Only '|' is allowed"
      (OpamVersion.compare t.opam_version (OpamVersion.of_string "1.3") >= 0 &&
       List.exists (function Atom _ -> false | _ -> true) @@
       OpamFormula.(ors_to_list (to_atom_formula t.conflicts)));
    cond 44 `Warning
      "The 'plugin' package flag is set but the package name doesn't \
       begin with 'opam-'"
      (OpamVersion.compare t.opam_version (OpamVersion.of_string "1.3") >= 0 &&
       List.mem Pkgflag_Plugin t.flags &&
       match t.OpamFile.OPAM.name with
       | None -> false
       | Some name ->
         OpamStd.String.starts_with ~prefix:"opam-"
           (OpamPackage.Name.to_string name));
    (let unclosed =
       List.fold_left (fun acc s ->
           List.rev_append (OpamFilter.unclosed_expansions s) acc)
         [] all_expanded_strings
     in
     cond 45 `Error
       "Unclosed variable interpolations in strings"
       ~detail:(List.map snd unclosed)
       (unclosed <> []));
  ]
  in
  OpamStd.List.filter_map (fun x -> x) warnings

let lint_gen reader filename =
  let open OpamFile.OPAM in
  let module Pp = OpamFormat.Pp in
  let warnings, t =
    try
      let f = reader filename in
      let _, _, good_items, invalid_items =
        Pp.parse ~pos:(pos_file (OpamFile.filename filename))
          (Pp.I.good_fields ~name:"opam-file" ~allow_extensions:true
             ~sections fields)
          f.file_contents
      in
      let warnings =
        List.map (function
            | Section (pos, s) ->
              3, `Error,
              Printf.sprintf "Invalid or duplicate section: '%s' at %s"
                s.section_kind (string_of_pos pos)
            | Variable (pos, f, _) ->
              3, `Error,
              Printf.sprintf "Invalid or duplicate field: '%s:' at %s"
                f (string_of_pos pos))
          invalid_items
      in
      let t, warnings =
        let warn_of_bad_format (pos, msg) =
          2, `Error, Printf.sprintf "File format error%s: %s"
            (match pos with
             | Some (_,li,col) when li >= 0 && col >= 0 ->
               Printf.sprintf " at line %d, column %d" li col
             | _ -> "")
            msg
        in
        try
          Some (Pp.parse ~pos:(pos_file (OpamFile.filename filename))
                  pp_raw_fields good_items),
          warnings
        with
        | OpamFormat.Bad_format bf -> None, warnings @ [warn_of_bad_format bf]
        | OpamFormat.Bad_format_list bfl ->
          None, warnings @ List.map warn_of_bad_format bfl
      in
      let warnings =
        match OpamPackage.of_filename (OpamFile.filename filename), t with
        | None, _ | _, None -> warnings
        | Some nv, Some t ->
          let name = nv.OpamPackage.name in
          let version = nv.OpamPackage.version in
          warnings @
          (match t.OpamFile.OPAM.name with
           | Some tname when tname <> name ->
             [ 4, `Warning,
               Printf.sprintf
                 "Field 'name: %S' while the directory name or pinning \
                  implied %S"
                 (OpamPackage.Name.to_string tname)
                 (OpamPackage.Name.to_string name) ]
           | _ -> []) @
          (match t.OpamFile.OPAM.version with
           | Some tversion when tversion <> version ->
             [ 4, `Warning,
               Printf.sprintf
                 "Field 'version: %S' while the directory name or pinning \
                  implied %S"
                 (OpamPackage.Version.to_string tversion)
                 (OpamPackage.Version.to_string version) ]
           | _ -> [])
      in
      warnings, t
    with
    | OpamSystem.File_not_found _ ->
      OpamConsole.error "%s not found" (OpamFile.to_string filename);
      [0, `Error, "File does not exist"], None
    | Lexer_error _ | Parsing.Parse_error ->
      [1, `Error, "File does not parse"], None
  in
  warnings @ (match t with Some t -> lint t | None -> []),
  t

let lint_file filename =
  let reader filename =
    let ic = OpamFilename.open_in (OpamFile.filename filename) in
    try
      let f = OpamFile.Syntax.of_channel filename ic in
      close_in ic; f
    with e -> close_in ic; raise e
  in
  lint_gen reader filename

let lint_string filename string =
  let reader filename = OpamFile.Syntax.of_string filename string in
  lint_gen reader filename

let warns_to_string ws =
  OpamStd.List.concat_map "\n"
    (fun (n, w, s) ->
       let ws = match w with
         | `Warning -> OpamConsole.colorise `yellow "warning"
         | `Error -> OpamConsole.colorise `red "error"
       in
       OpamStd.Format.reformat ~indent:14
         (Printf.sprintf "  %15s %2d: %s" ws n s))
    ws

(* Package definition loading *)

open OpamFilename.Op
open OpamStd.Option.Op


let try_read rd f =
  try rd f with
  | (OpamSystem.Internal_error _ | Not_found) as exc ->
    (if OpamFormatConfig.(!r.strict) then
       OpamConsole.error_and_exit
         "Could not read file %s: %s.\nAborting (strict mode)."
     else
       OpamConsole.warning
         "Could not read file %s: %s. Skipping.")
      (OpamFile.to_string f) (Printexc.to_string exc);
    None
  | OpamFormat.Bad_format _ as exc ->
    (if OpamFormatConfig.(!r.strict) then
       OpamConsole.error_and_exit
         "Errors while parsing %s: %s.\nAborting (strict mode)."
     else
       OpamConsole.warning
         "Errors while parsing  %s: %s. Skipping.")
      (OpamFile.to_string f) (Printexc.to_string exc);
    None

let add_aux_files ?dir opam =
  let dir = match dir with
    | None -> OpamFile.OPAM.metadata_dir opam
    | some -> some
  in
  match dir with
  | None -> opam
  | Some dir ->
    let (url_file: OpamFile.URL.t OpamFile.t) =
      OpamFile.make (dir // "url")
    in
    let (descr_file: OpamFile.Descr.t OpamFile.t)  =
      OpamFile.make (dir // "descr")
    in
    let files_dir =
      OpamFilename.Op.(dir / "files")
    in
    let opam =
      match try_read OpamFile.URL.read_opt url_file with
      | Some url ->
        if OpamFile.OPAM.url opam <> None then
          log "Overriding url of %s through external url file at %s"
            (OpamPackage.to_string (OpamFile.OPAM.package opam))
            (OpamFilename.Dir.to_string dir);
        OpamFile.OPAM.with_url url opam
      | None -> opam
    in
    let opam =
      match try_read OpamFile.Descr.read_opt descr_file with
      | Some descr ->
        if OpamFile.OPAM.descr opam <> None then
          log "Overriding descr of %s through external descr fileat %s"
            (OpamPackage.to_string (OpamFile.OPAM.package opam))
            (OpamFilename.Dir.to_string dir);
        OpamFile.OPAM.with_descr descr opam
      | None -> opam
    in
    let extra_files =
      OpamFilename.opt_dir files_dir >>| fun dir ->
      List.map
        (fun f ->
           OpamFilename.Base.of_string (OpamFilename.remove_prefix dir f),
           OpamFilename.digest f)
        (OpamFilename.rec_files dir)
    in
    let opam =
      match OpamFile.OPAM.extra_files opam, extra_files with
      | None, None -> opam
      | None, Some ef -> OpamFile.OPAM.with_extra_files ef opam
      | Some ef, None ->
        log "Missing expected extra files %s at %s/files"
          (OpamStd.List.concat_map ", "
             (fun (f,_) -> OpamFilename.Base.to_string f) ef)
          (OpamFilename.Dir.to_string dir);
        opam
      | Some oef, Some ef ->
        if oef <> ef then
          log "Mismatching extra-files at %s"
            (OpamFilename.Dir.to_string dir);
        opam
    in
    opam

let read_opam dir =
  let (opam_file: OpamFile.OPAM.t OpamFile.t) =
    OpamFile.make (dir // "opam")
  in
  try_read OpamFile.OPAM.read_opt opam_file >>| fun opam ->
  add_aux_files ~dir opam

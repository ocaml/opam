(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamTypesBase

let log fmt = OpamConsole.log "opam-file" fmt

open OpamFile.OPAM

(** manipulation utilities *)

let names_of_formula flag f =
  OpamPackageVar.filter_depends_formula
    ~build:true ~post:true ~dev:true ~test:flag ~doc:flag ~default:false
    ~env:OpamStd.Option.none f
  |> OpamFormula.atoms
  |> List.map fst
  |> OpamPackage.Name.Set.of_list

let all_commands t =
  t.build @ t.install @ t.remove @ t.run_test @ t.deprecated_build_doc

let all_urls t =
  let urlf_urls uf = OpamFile.URL.url uf :: OpamFile.URL.mirrors uf in
  (match t.url with Some uf -> urlf_urls uf | None -> []) @
  (match t.dev_repo with Some u -> [u] | None -> []) @
  List.fold_left (fun acc (_, uf) -> urlf_urls uf @ acc) [] t.extra_sources

let filters_of_formula f =
  OpamFormula.fold_left (fun acc (_, f) ->
      OpamFormula.fold_left (fun acc -> function
          | Constraint (_,f) -> f :: acc
          | Filter f -> f :: acc)
        acc f)
    [] f

(* Doesn't include filters in commands *)
let all_filters ?(exclude_post=false) t =
  OpamStd.List.filter_map snd t.patches @
  OpamStd.List.filter_map snd t.messages @
  (if exclude_post then [] else OpamStd.List.filter_map snd t.post_messages) @
  List.map snd t.depexts @
  OpamStd.List.filter_map snd t.libraries @
  OpamStd.List.filter_map snd t.syntax @
  [t.available] @
  filters_of_formula
    (OpamFormula.ands
       (t.depends ::
        t.depopts ::
        t.conflicts ::
        List.map (fun (_,f,_) -> f) t.features))

let map_all_filters f t =
  let mapsnd x =
    List.map (fun (x, ft) -> x, f ft) x
  in
  let mapsndopt x =
    List.map (function
        | (x, Some ft) -> x, Some (f ft)
        | nf -> nf)
      x
  in
  let map_commands =
    List.map
      (fun (args, filter) ->
         List.map (function
             | s, Some ft -> s, Some (f ft)
             | nf -> nf)
           args,
         OpamStd.Option.map f filter)
  in
  let map_filtered_formula =
    OpamFormula.map (fun (name, fc) ->
        let fc =
          OpamFormula.map (function
              | Filter flt -> Atom (Filter (f flt))
              | Constraint (relop, flt) -> Atom (Constraint (relop, (f flt))))
            fc
        in
        Atom (name, fc))
  in
  let map_features =
    List.map (fun (var, fformula, doc) ->
        var, map_filtered_formula fformula, doc)
  in
  t |>
  with_patches (mapsndopt t.patches) |>
  with_messages (mapsndopt t.messages) |>
  with_post_messages (mapsndopt t.post_messages) |>
  with_depexts (mapsnd t.depexts) |>
  with_libraries (mapsndopt t.libraries) |>
  with_syntax (mapsndopt t.syntax) |>
  with_available (f t.available) |>
  with_depends (map_filtered_formula t.depends) |>
  with_depopts (map_filtered_formula t.depopts) |>
  with_conflicts (map_filtered_formula t.conflicts) |>
  with_features (map_features t.features) |>
  with_build (map_commands t.build) |>
  with_run_test (map_commands t.run_test) |>
  with_install (map_commands t.install) |>
  with_remove (map_commands t.remove) |>
  with_deprecated_build_test (map_commands t.deprecated_build_test) |>
  with_deprecated_build_doc (map_commands t.deprecated_build_doc)

let all_variables ?exclude_post t =
  OpamFilter.commands_variables (all_commands t) @
  List.fold_left (fun acc f -> OpamFilter.variables f @ acc)
    [] (all_filters ?exclude_post t)

let map_all_variables f t =
  let map_fld (x, flt) = x, OpamFilter.map_variables f flt in
  let map_optfld = function
    | x, Some flt -> x, Some (OpamFilter.map_variables f flt)
    | _, None as optfld -> optfld
  in
  let map_commands =
    let map_args =
      List.map
        (fun (s, filter) ->
           (match s with
            | CString s -> CString (OpamFilter.map_variables_in_string f s)
            | CIdent id ->
              let id =
                try filter_ident_of_string id |>
                    OpamFilter.map_variables_in_fident f |>
                    string_of_filter_ident
                with Failure _ -> id
              in
              CIdent id),
           OpamStd.Option.Op.(filter >>| OpamFilter.map_variables f))
    in
    List.map
      (fun (args, filter) ->
         map_args args,
         OpamStd.Option.Op.(filter >>| OpamFilter.map_variables f))
  in
  let map_filtered_formula =
    OpamFormula.map (fun (name, fc) ->
        let fc =
          OpamFormula.map (function
              | Filter flt ->
                Atom (Filter (OpamFilter.map_variables f flt))
              | Constraint (relop, flt) ->
                Atom (Constraint (relop, (OpamFilter.map_variables f flt))))
            fc
        in
        Atom (name, fc)
      )
  in
  let map_features =
    List.map (fun (var, fformula, doc) ->
        var, map_filtered_formula fformula, doc)
  in
  t |>
  with_patches (List.map map_optfld t.patches) |>
  with_messages (List.map map_optfld t.messages) |>
  with_post_messages (List.map map_optfld t.post_messages) |>
  with_depexts (List.map map_fld t.depexts) |>
  with_libraries (List.map map_optfld t.libraries) |>
  with_syntax (List.map map_optfld t.syntax) |>
  with_build (map_commands t.build) |>
  with_run_test (map_commands t.run_test) |>
  with_install (map_commands t.install) |>
  with_remove (map_commands t.remove) |>
  with_depends (map_filtered_formula t.depends) |>
  with_depopts (map_filtered_formula t.depopts) |>
  with_conflicts (map_filtered_formula t.conflicts) |>
  with_available (OpamFilter.map_variables f t.available) |>
  with_features (map_features t.features) |>
  with_deprecated_build_test (map_commands t.deprecated_build_test) |>
  with_deprecated_build_doc (map_commands t.deprecated_build_doc)

let all_expanded_strings t =
  List.map fst t.messages @
  List.map fst t.post_messages @
  List.fold_left (fun acc (args, _) ->
      List.fold_left
        (fun acc -> function CString s, _ -> s :: acc | _ -> acc)
        acc args)
    [] (all_commands t) @
  List.fold_left
    (OpamFilter.fold_down_left
       (fun acc -> function FString s -> s :: acc | _ -> acc))
    [] (all_filters t)

let all_depends t =
  OpamPackage.Name.Set.union
    (names_of_formula true t.depends)
    (names_of_formula true t.depopts)

(* Templating & linting *)

let template nv =
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
  |> with_name_opt None
  |> with_maintainer maintainer
  |> with_build
    [[CString "./configure", None;
      CString "--prefix=%{prefix}%", None], None;
     [CIdent "make", None], None]
  |> with_install
    [[CIdent "make", None; CString "install", None], None]
  |> with_depends
    (Atom (OpamPackage.Name.of_string "specify-dependencies-here",
           (Atom (Constraint (`Geq, FString "optional-version")))))
  |> with_author maintainer
  |> with_homepage [""]
  |> with_license [""]
  |> with_dev_repo (OpamUrl.of_string "git+https://")
  |> with_bug_reports [""]
  |> with_synopsis ""

let lint t =
  let format_errors =
    List.map (fun (field, (pos, msg)) ->
        3, `Error,
        Printf.sprintf "File format error in '%s'%s: %s"
          field
          (match pos with
           | Some (_,li,col) when li >= 0 && col >= 0 ->
             Printf.sprintf " at line %d, column %d" li col
           | _ -> "")
          msg)
      (OpamFile.OPAM.format_errors t)
  in
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
  let all_commands = all_commands t in
  let all_expanded_strings = all_expanded_strings t in
  let all_depends = all_depends t in
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
      (OpamVersion.compare t.opam_version OpamVersion.current_nopatch <> 0);
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
    cond 25 `Warning
      "Missing field 'authors'"
      (t.author = []);
    cond 26 `Warning
      "No field 'install', but a field 'remove': install instructions \
       probably part of 'build'. Use the 'install' field or a .install \
       file"
      (t.install = [] && t.build <> [] && t.remove <> []);
(*
    cond 27 `Warning
      "No field 'remove' while a field 'install' is present, uncomplete \
       uninstallation suspected"
      (t.install <> [] && t.remove = []);
*)
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
(*
    cond 30 `Error
      "Field 'depopts' is not a pure disjunction"
      (List.exists (function
           | OpamFormula.Atom _ -> false
           | _ -> true)
          (OpamFormula.ors_to_list t.depopts));
*)
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
      "Field 'ocaml-version:' and variable 'ocaml-version' are deprecated, use \
       a dependency towards the 'ocaml' package instead for availability, and \
       the 'ocaml:version' package variable for scripts"
      (t.ocaml_version <> None ||
       List.mem (OpamVariable.Full.of_string "ocaml-version")
         (all_variables t));
    cond 33 `Error
      "Field 'os' is deprecated, use 'available' and the 'os' variable \
       instead"
      (t.os <> Empty);
    (let pkg_vars =
       List.filter (fun v -> not (OpamVariable.Full.is_global v))
         (OpamFilter.variables t.available)
     in
     cond 34 `Error
       "Field 'available:' contains references to package-local variables. \
        It should only be determined from global configuration variables"
       ~detail:(List.map OpamVariable.Full.to_string pkg_vars)
       (pkg_vars <> []));
    cond 35 `Warning
      "Missing field 'homepage'"
      (t.homepage = []);
    (* cond (t.doc = []) *)
    (*   "Missing field 'doc'"; *)
    cond 36 `Warning
      "Missing field 'bug-reports'"
      (t.bug_reports = []);
    cond 37 `Warning
      "Missing field 'dev-repo'"
      (t.dev_repo = None && t.url <> None);
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
(*
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
*)
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
         OpamPackage.Name.Set.empty (all_variables ~exclude_post:true t)
     in
     cond 41 `Warning
       "Some packages are mentioned in package scripts or features, but \
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
       let rec ors_only_constraint = function
         | Atom _ | Empty -> true
         | Or (a, b) -> ors_only_constraint a && ors_only_constraint b
         | And (a, Atom (Filter _)) | And (Atom (Filter _), a) | Block a ->
           ors_only_constraint a
         | And _ -> false
       in
       let rec check = function
         | Atom (_, c) -> ors_only_constraint c
         | Empty -> true
         | Or (a, b) -> check a && check b
         | Block a -> check a
         | And _ -> false
       in
       not (check t.conflicts));
    cond 44 `Warning
      "The 'plugin' package flag is set but the package name doesn't \
       begin with 'opam-'"
      (OpamVersion.compare t.opam_version (OpamVersion.of_string "1.3") >= 0 &&
       List.mem Pkgflag_Plugin t.flags &&
       match t.OpamFile.OPAM.name with
       | None -> false
       | Some name ->
         not (OpamStd.String.starts_with ~prefix:"opam-"
                (OpamPackage.Name.to_string name)));
    (let unclosed =
       List.fold_left (fun acc s ->
           List.rev_append (OpamFilter.unclosed_expansions s) acc)
         [] all_expanded_strings
     in
     cond 45 `Error
       "Unclosed variable interpolations in strings"
       ~detail:(List.map snd unclosed)
       (unclosed <> []));
    cond 46 `Error
      "Package is flagged \"conf\" but has source, install or remove \
       instructions"
      (has_flag Pkgflag_Conf t &&
       (t.install <> [] || t.remove <> [] || t.url <> None ||
        t.extra_sources <> []));
    cond 47 `Warning
      "Synopsis should start with a capital and not end with a dot"
      (let valid_re =
         Re.(compile (seq [bos; diff any (alt [blank; lower]); rep any;
                           diff any (alt [blank; char '.']); eos]))
       in
       match t.descr with None -> false | Some d ->
         not (Re.execp valid_re (OpamFile.Descr.synopsis d)));
    cond 48 `Warning
      "The fields 'build-test:' and 'build-doc:' are deprecated, and should be \
       replaced by uses of the 'with-test' and 'with-doc' filter variables in \
       the 'build:' and 'install:' fields, and by the newer 'run-test:' \
       field"
      (t.deprecated_build_test <> [] || t.deprecated_build_doc <> []);
    (let suspicious_urls =
       List.filter (fun u ->
           OpamUrl.parse ~handle_suffix:true (OpamUrl.to_string u) <> u)
         (all_urls t)
     in
     cond 49 `Warning
       "The following URLs don't use version control but look like version \
        control URLs"
       ~detail:(List.map OpamUrl.to_string suspicious_urls)
       (suspicious_urls <> []));
    cond 50 `Warning
      "The 'post' flag doesn't make sense with build or optional \
       dependencies"
      (List.mem (OpamVariable.Full.of_string "post")
         (List.flatten
            (List.map OpamFilter.variables
               (filters_of_formula t.depopts))) ||
       OpamFormula.fold_left (fun acc (_, f) ->
           acc ||
           let vars =
             OpamFormula.fold_left (fun vars f ->
                 match f with
                 | Constraint _ -> vars
                 | Filter fi -> OpamFilter.variables fi @ vars)
               [] f
           in
           List.mem (OpamVariable.Full.of_string "build") vars &&
           List.mem (OpamVariable.Full.of_string "post") vars)
         false
         t.depends);
    cond 51 `Error
      "The behaviour for negated dependency flags 'build' or 'post' is \
       unspecified"
      (OpamFormula.fold_left (fun acc (_, f) ->
           acc || OpamFormula.fold_left (fun acc f ->
               acc || match f with
               | Filter fi ->
                 OpamFilter.fold_down_left (fun acc fi ->
                     acc || match fi with
                     | FNot (FIdent ([], var, None)) ->
                       (match OpamVariable.to_string var with
                        | "build" | "post" -> true
                        | _ -> false)
                     | _ -> false)
                   false (OpamFilter.distribute_negations fi)
               | _ -> false)
             false f)
          false
          (OpamFormula.ands [t.depends; t.depopts]));
    cond 46 `Error
      "Package is needlessly flagged \"light-uninstall\", since it has no \
       remove instructions"
      (has_flag Pkgflag_Conf t && t.remove = []);
  ]
  in
  format_errors @
  OpamStd.List.filter_map (fun x -> x) warnings

let lint_gen reader filename =
  let warnings, t =
    let warn_of_bad_format (pos, msg) =
      2, `Error, Printf.sprintf "File format error%s: %s"
        (match pos with
         | Some (_,li,col) when li >= 0 && col >= 0 ->
           Printf.sprintf " at line %d, column %d" li col
         | _ -> "")
        msg
    in
    try
      let f = reader filename in

      let _, t =
        OpamPp.parse ~pos:(pos_file (OpamFile.filename filename))
          (OpamFormat.I.map_file OpamFile.OPAM.pp_raw_fields) f
      in
      let t, warnings =
        match OpamPackage.of_filename (OpamFile.filename filename) with
        | None -> t, []
        | Some nv ->
          let fname = nv.OpamPackage.name in
          let fversion = nv.OpamPackage.version in
          let t, name_warn =
            match t.OpamFile.OPAM.name with
            | Some tname ->
              if tname = fname then t, [] else
                t,
                [ 4, `Warning,
                  Printf.sprintf
                    "Field 'name: %S' while the directory name or pinning \
                     implied %S"
                    (OpamPackage.Name.to_string tname)
                    (OpamPackage.Name.to_string fname) ]
            | None ->
              OpamFile.OPAM.with_name fname t, []
          in
          let t, version_warn =
            match t.OpamFile.OPAM.version with
            | Some tversion ->
              if tversion = fversion then t, [] else
                t,
                [ 4, `Warning,
                  Printf.sprintf
                    "Field 'version: %S' while the directory name or pinning \
                     implied %S"
                    (OpamPackage.Version.to_string tversion)
                    (OpamPackage.Version.to_string fversion) ]
            | None -> OpamFile.OPAM.with_version fversion t, []
          in
          t, name_warn @ version_warn
      in
      warnings, Some t
    with
    | OpamSystem.File_not_found _ ->
      OpamConsole.error "%s not found" (OpamFile.to_string filename);
      [0, `Error, "File does not exist"], None
    | OpamLexer.Error _ | Parsing.Parse_error ->
      [1, `Error, "File does not parse"], None
    | OpamPp.Bad_format bf -> [warn_of_bad_format bf], None
    | OpamPp.Bad_format_list bfl -> List.map warn_of_bad_format bfl, None
  in
  warnings @ (match t with Some t -> lint t | None -> []),
  t

let lint_file filename =
  let reader filename =
    try
      let ic = OpamFilename.open_in (OpamFile.filename filename) in
      try
        let f = OpamFile.Syntax.of_channel filename ic in
        close_in ic; f
      with e -> close_in ic; raise e
    with OpamSystem.File_not_found _ ->
      OpamConsole.error_and_exit `Bad_arguments "File %s not found"
        (OpamFile.to_string filename)
  in
  lint_gen reader filename

let lint_channel filename ic =
  let reader filename = OpamFile.Syntax.of_channel filename ic in
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
  try rd f, None with
  | (OpamSystem.Internal_error _ | Not_found) as exc ->
    if OpamFormatConfig.(!r.strict) then
      OpamConsole.error_and_exit `File_error
        "Could not read file %s: %s.\nAborting (strict mode)."
        (OpamFile.to_string f) (Printexc.to_string exc);
    None,
    let f = OpamFile.filename f in
    Some (OpamFilename.(Base.to_string (basename f)),
          (Some (pos_file f), Printexc.to_string exc))
  | OpamPp.Bad_format bf as exc ->
    if OpamFormatConfig.(!r.strict) then
      OpamConsole.error_and_exit `File_error
        "Errors while parsing %s: %s.\nAborting (strict mode)."
        (OpamFile.to_string f) (Printexc.to_string exc);
    None,
    let f = OpamFile.filename f in
    Some (OpamFilename.(Base.to_string (basename f)), bf)

let add_aux_files ?dir ~files_subdir_hashes opam =
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
      match OpamFile.OPAM.url opam, try_read OpamFile.URL.read_opt url_file with
      | None, (Some url, None) -> OpamFile.OPAM.with_url url opam
      | Some opam_url, (Some url, errs) ->
        if url = opam_url && errs = None then
          log "Duplicate definition of url in '%s' and opam file"
            (OpamFile.to_string url_file)
        else
          OpamConsole.warning
            "File '%s' ignored (conflicting url already specified in the \
             'opam' file)"
            (OpamFile.to_string url_file);
        opam
      | _, (_, Some err) ->
        OpamFile.OPAM.with_format_errors (err :: opam.format_errors) opam
      | _, (None, None) -> opam
    in
    let opam =
      match OpamFile.OPAM.descr opam,
            try_read OpamFile.Descr.read_opt descr_file with
      | None, (Some descr, None) -> OpamFile.OPAM.with_descr descr opam
      | Some _, (Some _, _) ->
        log "Duplicate descr in '%s' and opam file"
          (OpamFile.to_string descr_file);
        opam
      | _, (_, Some err) ->
        OpamFile.OPAM.with_format_errors (err :: opam.format_errors) opam
      | _, (None, None)  -> opam
    in
    let opam =
      if not files_subdir_hashes then opam else
      let extra_files =
        OpamFilename.opt_dir files_dir >>| fun dir ->
        List.map
          (fun f ->
             OpamFilename.Base.of_string (OpamFilename.remove_prefix dir f),
             OpamHash.compute (OpamFilename.to_string f))
          (OpamFilename.rec_files dir)
      in
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
  match try_read OpamFile.OPAM.read_opt opam_file with
  | Some opam, None -> Some (add_aux_files ~dir ~files_subdir_hashes:true opam)
  | _, Some err ->
    OpamConsole.warning
      "Could not read file %s. skipping:\n%s"
      (OpamFile.to_string opam_file)
      (OpamPp.string_of_bad_format (OpamPp.Bad_format (snd err)));
    None
  | None, None -> None

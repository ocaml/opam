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

open OpamParserTypes.FullPos
open OpamTypes
open OpamTypesBase

let log ?level fmt = OpamConsole.log "opam-file" ?level fmt

open OpamFile.OPAM

let is_valid_license_id s =
  match Spdx_licenses.parse s with
  | Ok _ -> true
  | Error _ -> false

(** manipulation utilities *)

let names_of_formula flag f =
  OpamPackageVar.filter_depends_formula
    ~build:true ~post:true ~dev:true ~test:flag ~doc:flag ~dev_setup:flag
    ~default:false ~env:OpamStd.Option.none f
  |> OpamFormula.atoms
  |> List.map fst
  |> OpamPackage.Name.Set.of_list

let all_commands t =
  t.build @ t.install @ t.remove @ t.run_test @ t.deprecated_build_doc

let all_urls t =
  let urlf_urls uf = OpamFile.URL.url uf :: OpamFile.URL.mirrors uf in
  (match t.url with Some uf -> urlf_urls uf | None -> []) @
  (match t.dev_repo with Some u -> [u] | None -> []) @
  List.fold_left (fun acc (_, uf) -> urlf_urls uf @ acc) [] t.extra_sources @
  List.map snd t.pin_depends

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

(* Returns all variables from all commands (or on given [command]) and all filters *)
let all_variables ?exclude_post ?command t =
  let commands =
    match command with
    | Some cmd -> cmd
    | None -> all_commands t
  in
  OpamFilter.commands_variables commands @
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

let t_lint ?check_extra_files ?(check_upstream=false) ?(all=false) t =
  let format_errors =
    List.map (fun (field, (pos, msg)) ->
        3, `Error,
        Printf.sprintf "File format error in '%s'%s: %s"
          field
          (match pos with
           | Some {start=li,col; _} when li >= 0 && col >= 0 ->
             Printf.sprintf " at line %d, column %d" li col
           | _ -> "")
          msg)
      (OpamFile.OPAM.format_errors t)
  in
  let cond num level msg ?detail cd =
    if all then Some (num, level, msg)
    else if cd then
      let msg = match detail with
        | None | Some [] -> msg
        | Some d ->
          Printf.sprintf "%s: \"%s\"" msg (String.concat "\", \"" d)
      in
      Some (num, level, msg)
    else None
  in
  let all_commands = all_commands t in
  let all_expanded_strings = all_expanded_strings t in
  let all_depends = all_depends t in
  (* Upstream is checked only if it is an archive and non vcs backend *)
  let url_vcs =
    let open OpamStd.Option.Op in
    t.url >>| OpamFile.URL.url >>| (fun u ->
        match u.OpamUrl.backend with
        | #OpamUrl.version_control -> true
        | _ -> false)
  in
  let check_upstream =
    check_upstream &&
    not (OpamFile.OPAM.has_flag Pkgflag_Conf t) &&
    url_vcs = Some false
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
      (OpamVersion.compare t.opam_version OpamFile.OPAM.format_version <> 0);
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
         not (OpamStd.String.starts_with ~prefix:OpamPath.plugin_prefix
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
      "Synopsis (or description first line) should start with a capital and \
       not end with a dot"
      (let valid_re =
         Re.(compile (seq [bos; diff any (alt [blank; lower]); rep any;
                           diff any (alt [blank; char '.']); eos]))
       in
       match t.descr with None -> false | Some d ->
         let synopsis = OpamFile.Descr.synopsis d in
         synopsis <> "" && not (Re.execp valid_re synopsis));
    cond 48 `Warning
      "The fields 'build-test:' and 'build-doc:' are deprecated, and should be \
       replaced by uses of the 'with-test' and 'with-doc' filter variables in \
       the 'build:' and 'install:' fields, and by the newer 'run-test:' \
       field"
      (t.deprecated_build_test <> [] || t.deprecated_build_doc <> []);
    (let suspicious_urls =
       List.filter (fun u ->
           OpamUrl.parse_opt ~handle_suffix:true (OpamUrl.to_string u) <> Some u)
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
    cond 52 `Error
      "Package is needlessly flagged \"light-uninstall\", since it has no \
       remove instructions"
      (has_flag Pkgflag_LightUninstall t && t.remove = []);
    (let mismatching_extra_files =
       match t.extra_files, check_extra_files with
       | None, _ | _, None -> []
       | Some fs, Some [] -> List.map fst fs
       | Some efiles, Some ffiles ->
         OpamStd.List.filter_map (fun (n, _) ->
             if List.mem_assoc n ffiles then None else Some n)
           efiles @
         OpamStd.List.filter_map (fun (n, check_f) ->
             try if check_f (List.assoc n efiles) then None else Some n
             with Not_found -> Some n)
           ffiles
     in
     cond 53 `Error
       "Mismatching 'extra-files:' field"
       ~detail:(List.map OpamFilename.Base.to_string mismatching_extra_files)
       (mismatching_extra_files <> []));
    (let spaced_depexts =
       List.concat (List.map (fun (dl,_) ->
           OpamStd.List.filter_map
             (fun s ->
                let d = OpamSysPkg.to_string s in
                if String.contains d ' ' || String.length d = 0 then
                  Some d
                else None)
             (OpamSysPkg.Set.elements dl))
           t.depexts) in
     cond 54 `Warning
       "External dependencies should not contain spaces nor empty string"
       ~detail:spaced_depexts
       (spaced_depexts <> []));
    (let bad_os_arch_values =
       List.fold_left
         (OpamFilter.fold_down_left (fun acc -> function
              | FOp (FIdent ([],vname,None), _, FString value)
              | FOp (FString value, _, FIdent ([],vname,None)) ->
                (match OpamVariable.to_string vname with
                 | "os" ->
                   let norm = OpamSysPoll.normalise_os value in
                   if value <> norm then (value, norm)::acc else acc
                 | "arch" ->
                   let norm = OpamSysPoll.normalise_arch value in
                   if value <> norm then (value, norm)::acc else acc
                 | _ -> acc)
              | _ -> acc))
         [] (all_filters t)
     in
     cond 55 `Error
       "Non-normalised OS or arch string being tested"
       ~detail:(List.map
                  (fun (used,norm) -> Printf.sprintf "%s (use %s instead)"
                      used norm)
                  bad_os_arch_values)
       (bad_os_arch_values <> []));
    cond 56 `Warning
      "It is discouraged for non-compiler packages to use 'setenv:'"
      (t.env <> [] && not (has_flag Pkgflag_Compiler t));
    cond 57 `Error
      "Synopsis and description must not be both empty"
      (t.descr = None || t.descr = Some OpamFile.Descr.empty);
    (let vars = all_variables ~exclude_post:false ~command:[] t in
     let exists svar =
       List.exists (fun v -> v = OpamVariable.Full.of_string svar) vars
     in
     let rem_test = exists "test" in
     let rem_doc = exists "doc" in
     cond 58 `Warning
       (let var, s_, nvar =
          match rem_test, rem_doc with
          | true, true -> "`test` and `doc`", "s", "s are `with-test` and `with-doc`"
          | true, false -> "`test`", "", " is `with-test`"
          | false, true -> "`doc`", "", " is `with-doc`"
          | _ -> "","",""
        in
        Printf.sprintf "Found %s variable%s, predefined one%s" var s_ nvar)
       (rem_test || rem_doc));
    cond 59 `Warning "url doesn't contain a checksum"
      (check_upstream &&
       OpamStd.Option.map OpamFile.URL.checksum t.url = Some []);
    (let upstream_error =
       if not check_upstream then None else
       match t.url with
       | None -> Some "No url defined"
       | Some urlf ->
         let open OpamProcess.Job.Op in
         let check_checksum f =
           match OpamFile.URL.checksum urlf with
           | [] -> None
           | chks ->
             let not_corresponding =
               OpamStd.List.filter_map (fun chk ->
                   match OpamHash.mismatch (OpamFilename.to_string f) chk with
                   | Some m -> Some (m, chk)
                   | None -> None)
                 chks
             in
             if not_corresponding = [] then None
             else
             let msg =
               let is_singular = function [_] -> true | _ -> false in
               Printf.sprintf "The archive doesn't match checksum%s:\n%s."
                 (if is_singular not_corresponding then "" else "s")
                 (OpamStd.Format.itemize (fun (good, bad) ->
                      Printf.sprintf "archive: %s, in opam file: %s"
                        (OpamHash.to_string good) (OpamHash.to_string bad))
                     not_corresponding)
             in
             Some msg
         in
         let url = OpamFile.URL.url urlf in
         OpamProcess.Job.run @@
         OpamFilename.with_tmp_dir_job @@ fun dir ->
         match url.backend with
         | #OpamUrl.version_control -> Done None (* shouldn't happen *)
         | `http ->
           OpamProcess.Job.catch (function
               | Failure msg -> Done (Some msg)
               | OpamDownload.Download_fail (s,l) ->
                 Done (Some (OpamStd.Option.default l s))
               | e -> Done (Some (Printexc.to_string e)))
           @@ fun () ->
           OpamDownload.download ~overwrite:false url dir
           @@| check_checksum
         | `rsync ->
           let filename =
             let open OpamStd.Option.Op in
             (OpamFile.OPAM.name_opt t
              >>| OpamPackage.Name.to_string)
             +! "lint-check-upstream"
             |> OpamFilename.Base.of_string
             |> OpamFilename.create dir
           in
           OpamLocal.rsync_file url filename
           @@| function
           | Up_to_date f | Result f -> check_checksum f
           | Not_available (_,src) ->
             Some ("Source not found: "^src)
     in
     cond 60 `Error "Upstream check failed"
       ~detail:(OpamStd.Option.to_list upstream_error)
       (upstream_error <> None));
    (let with_test =
       List.exists ((=) (OpamVariable.Full.of_string "with-test"))
         (OpamFilter.commands_variables t.run_test)
     in
     cond 61 `Warning
       "`with-test` variable in `run-test` is out of scope, it will be ignored"
       with_test);
    (let bad_licenses =
       List.filter (fun s -> not (is_valid_license_id s)) t.license
     in
     cond 62 `Warning
       "License doesn't adhere to the SPDX standard, see https://spdx.org/licenses/"
       ~detail:bad_licenses
       (bad_licenses <> []));
(*
    (let subpath =
       match OpamStd.String.Map.find_opt "x-subpath" (extensions t) with
       | Some {pelem = String _; _} -> true
       | _ -> false
     in
     let opam_restriction =
       OpamFilter.fold_down_left (fun acc filter ->
           acc ||
           match filter with
           | FOp (FIdent (_, var, _), op, FString version)
             when OpamVariable.to_string var = "opam-version" ->
             OpamFormula.simplify_version_formula
               (OpamFormula.ands
                  [ Atom (`Lt, OpamPackage.Version.of_string "2.1");
                    Atom (op, OpamPackage.Version.of_string version) ])
             = None
           | _ -> false) false t.available
     in
     cond 63 `Error
       "`subpath` field need `opam-version = 2.1` restriction"
       (subpath && not opam_restriction));
    (let subpath_string =
       match OpamStd.String.Map.find_opt "x-subpath" (extensions t) with
       | Some {pelem = String _; _} | None -> false
       | _ -> true
     in
     cond 64 `Warning
       "`x-subpath` must be a simple string to be considered as a subpath`"
       subpath_string);
*)
    (let relative =
       let open OpamUrl in
       List.filter (fun u ->
           (* OpamUrl.local_dir is not used because it checks the existence of
              the directory *)
           (match u.backend, u.transport with
            | (#version_control | `rsync),
              ("file" | "path" | "local" | "rsync") -> true
            | _, _ -> false)
           && (Filename.is_relative u.path
               || OpamStd.String.contains ~sub:".." u.path))
         (all_urls t)
     in
     cond 65 `Error
       "URLs must be absolute"
       ~detail:(List.map (fun u -> u.OpamUrl.path) relative)
       (relative <> []));
    (let maybe_bool =
       (* Regexp from [OpamFilter.string_interp_regexp] *)
       let re =
         let open Re in
         let notclose =
           rep @@ alt [
             diff notnl @@ set "}";
             seq [char '}'; alt [diff notnl @@ set "%"; stop] ]
           ]
         in
         compile @@ seq [
           bos; alt [
             str "true"; str "false"; str "%%";
             seq [str "%{"; greedy notclose; opt @@ str "}%"];
           ]; eos]
       in
       fun s ->
         try let _ = Re.exec re s in true with Not_found -> false
     in
     let check_strings =
       let rec aux acc oped = function
         | FString s -> if oped || maybe_bool s then acc else s::acc
         | FIdent _ | FBool _ -> acc
         | FOp (fl,_,fr) -> (aux acc true fl) @ aux acc true fr
         | FAnd (fl, fr) | FOr (fl, fr)  ->
           (aux acc false fl) @ aux acc false fr
         | FNot f | FDefined f | FUndef f -> aux acc false f
       in
       aux [] false
     in
     let check_formula =
       OpamFormula.fold_left (fun acc (_, form as ff) ->
           match
             OpamFormula.fold_left (fun acc fc ->
                 match fc with
                 | Filter f -> check_strings f @ acc
                 | Constraint _ -> acc) [] form
           with
           | [] -> acc
           | strs -> (ff, List.rev strs)::acc
         )
     in
     let not_bool_strings =
       List.fold_left check_formula []
         (t.depends :: t.depopts :: t.conflicts
          :: List.map (fun (_,f,_) -> f) t.features)
     in
     cond 66 `Warning
       "String that can't be resolved to bool in filtered package formula"
       ~detail:(List.map (fun (f, strs) ->
           Printf.sprintf "%s in '%s'"
             (OpamStd.Format.pretty_list (List.map (Printf.sprintf "%S") strs))
             (OpamFilter.string_of_filtered_formula (Atom f)))
           not_bool_strings)
       (not_bool_strings <> []));
    cond 67 `Error
      "Checksum specified with a non archive url"
      ?detail:(OpamStd.Option.map (fun url ->
          [Printf.sprintf "%s - %s"
             (OpamFile.URL.url url |> OpamUrl.to_string)
             (OpamFile.URL.checksum url
              |> List.map OpamHash.to_string
              |> OpamStd.Format.pretty_list)])
          t.url)
      (url_vcs = Some true
       && OpamStd.Option.Op.(t.url >>| fun u -> OpamFile.URL.checksum u <> [])
          = Some true);
    cond 68 `Warning
      "Missing field 'license'"
      (t.license = []);
  ]
  in
  format_errors @
  OpamStd.List.filter_map (fun x -> x) warnings

let lint = t_lint ~all:false

let extra_files_default filename =
  let dir =
    OpamFilename.Op.(OpamFilename.dirname
                       (OpamFile.filename filename) / "files")
  in
  List.map
    (fun f ->
       OpamFilename.Base.of_string (OpamFilename.remove_prefix dir f),
       OpamHash.check_file (OpamFilename.to_string f))
    (OpamFilename.rec_files dir)

let lint_gen ?check_extra_files ?check_upstream ?(handle_dirname=false)
    reader filename =
  let warnings, t =
    let warn_of_bad_format (pos, msg) =
      2, `Error, Printf.sprintf "File format error%s: %s"
        (match pos with
         | Some {start=li,col; _} when li >= 0 && col >= 0 ->
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
        if handle_dirname = false then t, [] else
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
      warnings,
      Some (OpamFile.OPAM.with_metadata_dir
              (Some (None,
                     OpamFilename.Dir.to_string
                       (OpamFilename.dirname
                          (OpamFile.filename filename)))) t)
    with
    | OpamSystem.File_not_found _ ->
      OpamConsole.error "%s not found" (OpamFile.to_string filename);
      [0, `Error, "File does not exist"], None
    | OpamLexer.Error _ | Parsing.Parse_error ->
      [1, `Error, "File does not parse"], None
    | OpamPp.Bad_version bf | OpamPp.Bad_format bf -> [warn_of_bad_format bf], None
    | OpamPp.Bad_format_list bfl -> List.map warn_of_bad_format bfl, None
  in
  let check_extra_files = match check_extra_files with
    | None -> extra_files_default filename
    | Some f -> f
  in
  warnings @ (match t with Some t -> lint ~check_extra_files ?check_upstream t | None -> []),
  t

let lint_file ?check_extra_files ?check_upstream ?handle_dirname filename =
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
  lint_gen ?check_extra_files ?check_upstream ?handle_dirname reader filename

let lint_channel ?check_extra_files ?check_upstream ?handle_dirname
    filename ic =
  let reader filename = OpamFile.Syntax.of_channel filename ic in
  lint_gen ?check_extra_files ?check_upstream ?handle_dirname reader filename

let lint_string ?check_extra_files ?check_upstream ?handle_dirname
    filename string =
  let reader filename = OpamFile.Syntax.of_string filename string in
  lint_gen ?check_extra_files ?check_upstream ?handle_dirname reader filename

let all_lint_warnings () =
  t_lint ~all:true OpamFile.OPAM.empty

let warns_to_string ws =
  OpamStd.List.concat_map "\n"
    (fun (n, w, s) ->
       let ws = match w with
         | `Warning -> OpamConsole.colorise `yellow "warning"
         | `Error -> OpamConsole.colorise `red "error"
       in
       OpamStd.Format.reformat ~indent:14
         (Printf.sprintf "  %16s %2d: %s" ws n s))
    ws

let warns_to_json ?filename ws =
  let filename =
  match filename with
  | Some f -> f
  | None -> "stdout"
  in
  let warn, err =
    List.fold_left (fun (w,e) (n,we,s) ->
        let arr =
          `O [ "id", `Float (float_of_int n);
               "message", `String s]
        in
        match we with
        | `Warning -> arr::w, e
        | `Error -> w, arr::e) ([],[]) ws
  in
  let result =
  match warn,err with
  | [],[] -> "passed"
  | _, _::_ -> "error"
  | _::_, [] -> "warning"
  in
  `O [
    "file", `String filename;
    "result", `String result;
     "warnings", `A warn;
     "errors", `A err
  ]

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
    | None ->
      OpamFile.OPAM.get_metadata_dir ~repos_roots:(fun r ->
          failwith ("Repository "^OpamRepositoryName.to_string r^
                    " not registered for add_aux_files!"))
        opam
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
        OpamFilename.rec_files dir
        |> List.map (fun file ->
            file,
            OpamFilename.Base.of_string (OpamFilename.remove_prefix dir file))
      in
      match OpamFile.OPAM.extra_files opam, extra_files with
      | None, None -> opam
      | None, Some ef ->
        log ~level:2 "Missing extra-files field for %s, adding them."
          (OpamStd.List.concat_map ", "
             (fun (_,f) -> OpamFilename.Base.to_string f) ef);
        let ef =
          List.map
            (fun (file, basename) ->
               basename,
               OpamHash.compute (OpamFilename.to_string file))
            ef
        in
        OpamFile.OPAM.with_extra_files ef opam
      | Some ef, None ->
        log "Missing expected extra files %s at %s/files"
          (OpamStd.List.concat_map ", "
             (fun (f,_) -> OpamFilename.Base.to_string f) ef)
          (OpamFilename.Dir.to_string dir);
        opam
      | Some oef, Some ef ->
        let wr_check, nf_opam, rest =
          List.fold_left (fun (wr_check, nf_opam, rest) (file, basename) ->
              match OpamStd.List.pick_assoc basename rest with
              | None, rest ->
                wr_check, (basename::nf_opam), rest
              | Some ohash, rest ->
                (if OpamHash.check_file (OpamFilename.to_string file) ohash then
                   wr_check
                 else
                   basename::wr_check),
                nf_opam, rest
            ) ([], [], oef) ef
        in
        let nf_file = List.map fst rest in
        if nf_file <> [] || wr_check <> [] || nf_opam <> [] then
          log "Mismatching extra-files at %s: %s"
            (OpamFilename.Dir.to_string dir)
            ((if nf_file = [] then None else
                Some (Printf.sprintf "missing from 'files' directory (%d)"
                        (List.length nf_file)))
             :: (if nf_opam = [] then None else
                   Some (Printf.sprintf "missing from opam file (%d)"
                           (List.length nf_opam)))
             :: (if wr_check = [] then None else
                   Some (Printf.sprintf "wrong checksum (%d)"
                           (List.length wr_check)))
             :: []
             |> OpamStd.List.filter_some
             |> OpamStd.Format.pretty_list);
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

let read_repo_opam ~repo_name ~repo_root dir =
  let open OpamStd.Option.Op in
  read_opam dir >>|
  OpamFile.OPAM.with_metadata_dir
    (Some (Some repo_name, OpamFilename.remove_prefix_dir repo_root dir))

let dep_formula_to_string f =
  let pp =
    OpamFormat.V.(package_formula `Conj (constraints version))
  in
  OpamPrinter.FullPos.value (OpamPp.print pp f)

let sort_opam opam =
  log "sorting %s" (OpamPackage.to_string (package opam));
  let sort_ff =
    let compare_filters filter filter' =
      let get_vars = function
        | Constraint _ -> []
        | Filter filter ->
          List.sort compare (OpamFilter.variables filter)
      in
      match get_vars filter, get_vars filter' with
      | v::_, v'::_ -> compare v v'
      | [], _::_ -> 1
      | _::_, [] -> -1
      | [],[] -> 0
    in
    OpamFilter.sort_filtered_formula
      (fun (n,filter) (n',filter') ->
         let cmp = OpamFormula.compare_formula compare_filters filter filter' in
         if cmp <> 0 then cmp else
           OpamPackage.Name.compare n n')
  in
  let fst_sort ?comp =
    let comp = OpamStd.Option.default compare comp in
    fun l -> List.sort (fun (e,_) (e',_) -> comp e e') l
  in
  opam
  |> with_author @@ List.sort compare opam.author
  |> with_tags @@ List.sort compare opam.tags
  |> with_depends @@ sort_ff opam.depends
  |> with_depopts @@ sort_ff opam.depopts
  |> with_depexts @@ fst_sort opam.depexts
  |> with_conflicts @@ sort_ff opam.conflicts
  |> with_pin_depends @@ fst_sort ~comp:OpamPackage.compare opam.pin_depends
  |> with_extra_files_opt @@ OpamStd.Option.map fst_sort opam.extra_files
  |> with_extra_sources @@ fst_sort opam.extra_sources

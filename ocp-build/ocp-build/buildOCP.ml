(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)


(* open BuildBase *)
(* open Stdlib2 *)
open BuildOCPTree
open BuildOCPTypes
open SimpleConfig
open BuildOCPVariable

let verbose = DebugVerbosity.verbose ["B";"BP"] "BuildOCP"

type temp_package = {
  tpk_pk : package;
  mutable tpk_need_validation : int;
  mutable tpk_tags : temp_tag list;
}

and temp_tag = {
  tag_name : string;
  tag_package_name : string;
  tag_tpk : temp_package;
  mutable tag_validated : bool;
  mutable tag_missing_deps : int;
}

type temp_state = {
  validated : (string * string, temp_tag) Hashtbl.t;
  missing : (string * string, temp_tag list ref) Hashtbl.t;
  conflicts :  (package * package *  package) list ref;
}

let init_packages () =
  let packages = BuildOCPInterp.initial_state () in
  packages

let empty_config () = BuildOCPInterp.empty_config (* !BuildOCPVariable.options *)
let generated_config () =
  BuildOCPInterp.generated_config (* !BuildOCPVariable.options *)

let print_loaded_ocp_files = ref false
let print_dot_packages = ref (Some "_obuild/packages.dot")
let print_package_deps = ref false

let load_ocp_files global_config packages files =

  let nerrors = ref 0 in
  let rec iter parents files =
    match files with
      [] -> ()
    | file :: next_files ->
      match parents with
	[] -> assert false
      | (parent, filename, config) :: next_parents ->
        let file = File.to_string file in
	if OcpString.starts_with file parent then
	  let dirname = Filename.dirname file in
	  if verbose 5 || !print_loaded_ocp_files then
	    Printf.eprintf "Reading %s with context from %s\n%!" file filename;
(*
   begin try
     let requires = BuildOCPInterp.config_get config "requires" in
     Printf.eprintf "REQUIRES SET\n%!";
   with Not_found ->
     Printf.eprintf "REQUIRES NOT SET\n%!";
   end;
*)
	  let config =
	    try
	      BuildOCPInterp.read_ocamlconf packages config file
	    with BuildMisc.ParseError ->
	      incr nerrors;
	      config
	  in
	  iter ( (dirname, file, config) :: parents ) next_files
	else
	  iter next_parents files
  in
  iter [ "", "<root>", global_config ] files;
  !nerrors

let print_conflicts pj verbose_conflicts =
  if pj.project_conflicts <> [] then
  if verbose_conflicts then
    List.iter (fun (pk, pk2, pk3) ->
      Printf.eprintf "Warning: two projects called %S\n" pk.package_name;
      let print_package msg pk =
        let msg_len = String.length msg in
        let dirname_len = String.length pk.package_dirname in
        let filename_len = String.length pk.package_filename in
        if msg_len + dirname_len + filename_len > 70 then
          Printf.eprintf "  %s %s\n     (%s)\n" msg
            pk.package_dirname pk.package_filename
        else
          Printf.eprintf "  %s %s (%s)\n" msg
            pk.package_dirname pk.package_filename;
      in
      print_package "In" pk;
      print_package "In" pk2;
      print_package "Keeping" pk3;
    ) pj.project_conflicts
  else
    Printf.eprintf
      "Warning: %d package conflicts solved (use -print-conflicts)\n%!"
      ( List.length pj.project_conflicts )

(*
let dump_dot_packages filename pj =
  let graph = Ocamldot.create "Packages" [] in
  let nodes = ref StringMap.empty in
  Array.iter (fun pk ->
    let node = Ocamldot.node graph pk.package_name [] in
    nodes := StringMap.add pk.package_name node !nodes;
    List.iter (fun name ->
      nodes := StringMap.add pk.package_name node !nodes;
    )
      ( get_strings_with_default [pk.package_options] "provides"  [] )


  ) pj.project_sorted;
  Array.iter (fun pk0 ->
    let node0 = StringMap.find pk0.package_name !nodes in
    List.iter (fun dep ->
      let pk1 = dep.dep_project in
      let node1 = StringMap.find pk1.package_name !nodes in
      let (_ : Ocamldot.edge) = Ocamldot.edge node0 node1 [] in
      Printf.eprintf "%s -> %s\n%!" pk0.package_name pk1.package_name;
      ()
    ) pk0.package_requires
  ) pj.project_sorted;
  Ocamldot.save graph filename
*)

type package_comparison =
  PackageEquality
| PackageConflict
| PackageUpgrade of bool

let normalized_dir dir =
  File.to_string (File.of_string dir)

let is_enabled options =
  get_bool_with_default options "enabled" true


module PackageLinkSorter = LinearToposort.Make(struct
  type t = package  package_dependency
  let node pd = pd.dep_project.package_node
  let iter_edges f pd1 =
    IntMap.iter (fun _ pd2 ->
      if pd2.dep_link then f pd2) pd1.dep_project.package_requires_map
  let name pd = pd.dep_project.package_name
  let debug = ref false
end)

(*
let compare_packages pk1 pk2 =
  let o1 = pk1.package_options in
  let o2 = pk2.package_options in
  let pk1_generated = get_bool_with_default [o1] "generated" false in
  let pk2_generated = get_bool_with_default [o2] "generated" false in
  match pk1_generated, pk2_generated with
    false, false ->
      PackageConflict
  | true, true ->
    if normalized_dir pk1.package_dirname = normalized_dir pk2.package_dirname &&
      pk1.package_type = pk2.package_type &&
      pk1.package_version = pk2.package_version
    (* TODO: We should also test for asm/byte... *)
    then begin
      if verbose 5 then
        Printf.eprintf "Discarding duplicated package %S\n%!" pk1.package_name;
      PackageEquality
    end
    else
      begin
        if verbose 5 then begin
          Printf.eprintf "Conflict over %S\n" pk1.package_name;
          Printf.eprintf "dirname: %S\n" pk1.package_dirname;
          Printf.eprintf "type: %S\n" (string_of_package_type pk1.package_type);
          Printf.eprintf "version: %S\n" pk1.package_version;
          Printf.eprintf "dirname: %S\n" pk2.package_dirname;
          Printf.eprintf "type: %S\n" (string_of_package_type pk2.package_type);
          Printf.eprintf "version: %S\n" pk2.package_version;
        end;
        PackageConflict
      end

  (* TODO: if we have the sources of a new version, we should probably
     accept the new version, but not accept any installed version
     depending on the old version.

     TODO: implement this in a different pass : we should filter out
     packages that are duplicated, especially to discard installed
     packages. But what if we have the sources of A, { A + B }
     installed, and not the sources of B ?

     In the meantime, the solution is to desinstall the installed
     versions when they conflict.  *)

  | true, false -> PackageConflict
  | false, true -> PackageConflict

let rec check_validate_tag s tag =
  let tpk = tag.tag_tpk in
  let pk = tpk.tpk_pk in
  if verbose 5 then
    Printf.eprintf "validate_project: %s, tag=%s, id=%d\n"
      tag.tag_package_name tag.tag_name pk.package_id;
  if tag.tag_missing_deps = 0 then begin
    validate_tag s tag;
    if tag.tag_validated then
    List.iter (fun provide ->
      let tag = { tag with tag_package_name = provide } in
      validate_tag s tag
    )
      ( get_strings_with_default [pk.package_options] "provides"  [] )
  end

and validate_tag s tag =
  let tpk = tag.tag_tpk in
  let pk = tpk.tpk_pk in

    let key =  (tag.tag_package_name, tag.tag_name) in

    let should_add =
      try
        let tag2 = Hashtbl.find s.validated key in
        let tpk2 = tag2.tag_tpk in
        let pk2 = tpk2.tpk_pk in
        Printf.eprintf "Compare: %s and %s over %s/%s\n%!"
          pk.package_name pk2.package_name
          tag.tag_package_name tag.tag_name;
        let should_add =
        match compare_packages pk pk2 with
        | PackageEquality ->
          (* same package, no need to add *)
          tpk.tpk_need_validation <- -1000;
          false

        | PackageUpgrade _ -> (* NOT YET DONE *)
          Hashtbl.remove s.validated key;
          tag2.tag_validated <- false;
          tpk2.tpk_need_validation <- -2000;
          true

        | PackageConflict ->
            if pk.package_id > pk2.package_id then begin
              Hashtbl.remove s.validated key;
              tag2.tag_validated <- false;
              tpk2.tpk_need_validation <- -2000;
              true
            end else begin
              tpk.tpk_need_validation <- -1000;
              false
            end
        in
          s.conflicts := (pk, pk2,
            if should_add then pk else pk2
          ) :: !(s.conflicts);

        should_add
      with Not_found -> true
    in

    if should_add then begin
      Hashtbl.add s.validated key tag;
      tag.tag_validated <- true;
      tpk.tpk_need_validation <- tpk.tpk_need_validation - 1;
      try
        let list_ref = Hashtbl.find s.missing key in
        Hashtbl.remove s.missing key;
        List.iter (fun tag2 ->
	  tag2.tag_missing_deps <- tag2.tag_missing_deps - 1;
	  check_validate_tag s tag2
        ) !list_ref;
      with Not_found -> ()
    end

let check_tag s tpk tag_name =
  let pk = tpk.tpk_pk in
  let tag = {
    tag_name = tag_name;
    tag_package_name = pk.package_name;
    tag_tpk = tpk;
    tag_missing_deps = 0;
    tag_validated = false;
  } in
  tpk.tpk_tags <- tag :: tpk.tpk_tags;
  tpk.tpk_need_validation <- tpk.tpk_need_validation + 1;

  StringMap.iter (fun name pkdep ->
    if not pkdep.dep_optional then

      let key = (name, tag_name) in
      (* TODO: we should use a datastructure that can handle
         dependencies by tag and by version *)
      if not (Hashtbl.mem s.validated key) then
	let list_ref =
	  try
	    Hashtbl.find s.missing key
	  with Not_found ->
	    let list_ref = ref [] in
	    Hashtbl.add s.missing key list_ref;
	    list_ref
	in
	list_ref := tag :: !list_ref;
	tag.tag_missing_deps <- tag.tag_missing_deps + 1
  ) tpk.tpk_pk.package_deps_map;
  check_validate_tag s tag

let check_package s pk =
  Printf.eprintf "check_package %s_%d of %s\n%!"
    pk.package_name pk.package_id pk.package_dirname;
  StringMap.iter (fun name pkdep ->
    Printf.eprintf "\t%s depends on %s\n%!"
      pk.package_name pkdep.dep_project
  ) pk.package_deps_map;
  let tpk = {
    tpk_need_validation = 0;
    tpk_tags = [];
    tpk_pk = pk;
  } in
  if is_enabled [pk.package_options] then begin
(* TODO: think about "rules" packages. What does these tags mean for them ?
   We might need to be smarter here, for example to set which tags should
   be set and which tags should be searched. *)
    match
      get_bool_with_default [pk.package_options] "has_byte" true,
      get_bool_with_default [pk.package_options] "has_asm" true with
      true, true
    | false, false (* both disabled ? probably a mistake *)
    ->
      check_tag s tpk "byte";
      check_tag s tpk "native";
    | true, false ->
      check_tag s tpk "byte";
    | false, true ->
      check_tag s tpk "native";
  end else
    (* this package should not be validated as it is disabled *)
    tpk.tpk_need_validation <- -10;
  tpk



module PackageDepSorter = LinearToposort.Make(struct
  type t = package
  let node pd = pd.package_node
  let iter_edges f pd =
    IntMap.iter (fun _ dep -> f dep.dep_project) pd.package_requires_map
  let name pd =
    Printf.sprintf "%s_%d" pd.package_name pd.package_id
  let debug = ref false
end)

*)

let print_deps msg pk =
  Printf.eprintf "%s: Project %s depends on:\n%!" msg pk.package_name;
  List.iter (fun dep ->
    let pd = dep.dep_project in
    Printf.eprintf "\t%s %s%s%s%s\n%!"
      (string_of_package_type pd.package_type)
      pd.package_name
      (if dep.dep_link then "(link)" else "")
      (if dep.dep_syntax then "(syntax)" else "")
      (if dep.dep_optional then "(optional)" else "")
  ) pk.package_requires


let new_dep pk pk2 options =
  try
    IntMap.find pk2.package_id pk.package_requires_map
  with Not_found ->
    let dep =
      {
        dep_project = pk2;
        dep_link = false;
        dep_syntax = false;
        dep_optional = false;
        dep_options = options;
      } in
    pk.package_requires_map <-
      IntMap.add pk2.package_id dep pk.package_requires_map;
    pk.package_requires <- dep :: pk.package_requires;
(*    Printf.eprintf "New dep %s <- %s\n%!"
      pk2.package_name pk.package_name; *)
    dep

(*
Do a closure of all dependencies for this project. Called only on
validated_projects. The closure is useful in two cases:
 - for libraries, we need to include directories (-I) and link.
 - for syntaxes, we need also to include directories (-I) and link them,
    when calling the preprocessor.
 - we don't need more than that.
 *)

let update_deps pj =

  if !print_package_deps || verbose 5 then
    print_deps "BEFORE update_deps" pj;

  (*
    This computation depends on what we are dealing with:
    - For a syntax:
    - we must include all transitive link dependencies
    - for a Library dependency, we want to include all link dependencies
    - for a Syntax dependency, we want to also include syntax dependencies

    - For a library, all syntax dependencies should remain syntax
    dependencies. Link dependencies of syntaxes should become syntax
    dependencies.

    For now, we have three kinds of dependencies:
    1) 'link' dependencies: we must copy all 'link' transitive dependencies
    as new 'link' dependencies.
    2) 'syntax' dependencies: we must copy all 'link' transitive dependencies
    as new 'syntax' dependencies.
    3) neither 'link' nor 'syntax': we should not copy transitive dependencies.

    We cannot do it in one pass: we should first compute strong dependencies, and
    remove packages not meeting strong dependencies. Then, we can redo the
    computation, this time knowing which optional packages are available.

  *)

  List.iter (fun dep ->
    match dep.dep_project.package_type with
    | SyntaxPackage ->
      dep.dep_syntax <- true;
      dep.dep_link <- false;
    | RulesPackage
    | ProgramPackage ->
      dep.dep_syntax <- false;
      dep.dep_link <- false;
    | LibraryPackage
    | ObjectsPackage
      -> ()
    | TestPackage ->
      Printf.eprintf "Error: Test %S appears in requirements of %S\n%!"
        dep.dep_project.package_name
        pj.package_name;
      exit 2;
  ) pj.package_requires;

  (* add all link dependencies, transitively *)
  let rec add_link_deps pj1 =
    List.iter (fun dep ->
      let pj2 = dep.dep_project in
      let dep2 = new_dep pj pj2 dep.dep_options in
      if verbose 5 then
        Printf.eprintf "%S -> %S\n" pj.package_name pj2.package_name;
      if dep.dep_link &&
         (not dep2.dep_link || pj1 == pj) then begin
        dep2.dep_link <- true;
        if verbose 5 then
          Printf.eprintf "%S -> link %S\n" pj.package_name pj2.package_name;
        add_link_deps pj2
      end
    ) pj1.package_requires
  in
  add_link_deps pj;

  (* add syntax dependencies, and their link dependencies
     transformed into syntax dependencies *)
  let rec add_link_as_syntax_deps pj1 =
    List.iter (fun dep ->
      if dep.dep_link then
        let pj2 = dep.dep_project in
        let dep2 = new_dep pj pj2 dep.dep_options in
        if not dep2.dep_syntax then begin
          if verbose 5 then
            Printf.eprintf "%S -> syntax %S\n" pj.package_name pj2.package_name;
          dep2.dep_syntax <- true;
          add_link_as_syntax_deps pj2
        end
    ) pj1.package_requires
  in

  let add_syntax_deps pj1 =
    List.iter (fun dep ->
      if dep.dep_syntax then
        let pj2 = dep.dep_project in
        let dep2 = new_dep pj pj2 dep.dep_options in
        if not dep2.dep_syntax || pj1 == pj then begin
          dep2.dep_syntax <- true;
          if verbose 5 then
            Printf.eprintf "%S -> syntax %S\n" pj.package_name pj2.package_name;
          add_link_as_syntax_deps pj2;
        end
    ) pj1.package_requires
  in
  add_syntax_deps pj;
  if !print_package_deps ||  verbose 5 then
    print_deps "AFTER update_deps SORT" pj;
  ()


let reset_package_ids array =
  for i = 0 to Array.length array - 1 do
    array.(i).package_id <- i
  done

let requires_keep_order_option = new_bool_option "requires_keep_order" false

(*
let verify_packages packages =
  let packages = BuildOCPInterp.final_state packages in

  let state = {
    missing = Hashtbl.create 111;
    validated = Hashtbl.create 111;
    conflicts = ref [];
  }
  in

  let tpks = Array.map (check_package state) packages in

  let project_incomplete = ref [] in
  let project_disabled = ref [] in
  let project_validated = ref [] in

  Array.iter (fun tpk ->
    let pk = tpk.tpk_pk in
    Printf.eprintf "tpk %s need validation %d\n%!"
      pk.package_name tpk.tpk_need_validation;
    if tpk.tpk_need_validation > 0 then begin
(*    if is_enabled [pk.package_options] then begin
      if pk.package_missing_deps > 0 then *)
      project_incomplete := pk :: !project_incomplete;
      List.iter (fun tag ->
        Printf.eprintf "\tTag %s validate = %b\n" tag.tag_name tag.tag_validated;
      ) tpk.tpk_tags
    end
    else
    if tpk.tpk_need_validation < 0 then
      project_disabled := pk :: !project_disabled
    else begin
      project_validated := pk :: !project_validated;

      let is_after = get_strings_with_default [pk.package_options]
          "is_after"  []
      in
      let is_before = get_strings_with_default [pk.package_options]
          "is_before" []
      in

      List.iter (fun tag ->
        StringMap.iter (fun dep_name dep ->
          try
            let tag2 = Hashtbl.find state.validated (dep_name, tag.tag_name) in
            let tpk2 = tag2.tag_tpk in
            let pk2 = tpk2.tpk_pk in
            let dep2 = new_dep pk pk2 in
            dep2.dep_link <- dep.dep_link;
            dep2.dep_syntax <- dep.dep_syntax;
          with Not_found -> () (* probably an optional dependency *)
        ) pk.package_deps_map;


        begin
    (* a list of packages that should appear before this package *)
          List.iter (fun name ->
            try
              let tag2 = Hashtbl.find state.validated (name, tag.tag_name) in
              ignore (new_dep pk tag2.tag_tpk.tpk_pk :
                  package package_dependency
              );
            with Not_found -> ()
          ) is_after;
        end;

        begin
          (* a list of packages that should appear after this package *)
          List.iter (fun name ->
            try
              let tag2 = Hashtbl.find state.validated (name, tag.tag_name) in
              ignore (new_dep tag2.tag_tpk.tpk_pk pk :
                  package package_dependency);
            with Not_found -> ()
          ) is_before;
        end;


      ) tpk.tpk_tags;



    end
  ) tpks;

  let project_missing = ref [] in

  begin
    let map = ref StringMap.empty in
    Hashtbl.iter (fun (name, tag_name) list_ref ->
      let missing =
        try
          StringMap.find name !map
        with Not_found ->
          let set = ref IntMap.empty in
          map := StringMap.add name set !map;
          set
      in
      List.iter (fun tag ->
        let pk = tag.tag_tpk.tpk_pk in
        missing := IntMap.add pk.package_id pk !missing
      ) !list_ref
    )
    state.missing;
    StringMap.iter (fun name map ->
      let list = ref [] in
      IntMap.iter (fun _ pk ->
        list := pk :: !list
      ) !map;
      project_missing := (name, !list) :: !project_missing
    ) !map;
  end;

(* Note that the result of this function can contain more elements
  as the initial list, as new dependencies are automatically added. *)

  let (project_sorted, cycle, other) =
    PackageDepSorter.sort !project_validated
  in
  let _list = () in
  List.iter update_deps project_sorted;

  let npackages = Array.length packages in

  let pj = {
(*    project_files = files; *)
    project_sorted = Array.of_list project_sorted;
    project_missing = !project_missing;
    project_disabled = Array.of_list !project_disabled;
    project_incomplete = Array.of_list !project_incomplete;
    project_conflicts = ! (state.conflicts);
  } in
(* TODO: fix this assertion. The equality stands only if we count
also duplicated packages. *)
  assert (npackages >=
      Array.length pj.project_sorted +
        Array.length pj.project_incomplete +
        Array.length pj.project_disabled);

  (* Change the package IDs: the package_requires_map is not correct anymore ! *)
  reset_package_ids pj.project_sorted;
  (* TODO: The impact of this is that all dependencies are sorted in
     the same order in all packages. This might, however, not be what
     someone wants, because you might want to have a different link
     order than the one globally inferred.  *)
  Array.iter (fun pk ->
    if requires_keep_order_option.get [pk.package_options]  then
      let (sorted, cycle, _ ) = PackageLinkSorter.sort pk.package_requires in
      assert (cycle = []);
      pk.package_requires <- sorted
    else
      pk.package_requires <- List.sort (fun dep1 dep2 ->
        compare
          dep1.dep_project.package_id
          dep2.dep_project.package_id) pk.package_requires;

    if !print_package_deps || verbose 9 then begin
      Printf.eprintf "Package %S[%d]\n" pk.package_name pk.package_id;
      List.iter (fun dp ->
        Printf.eprintf "\t%S[%d]%s%s\n"
          dp.dep_project.package_name
          dp.dep_project.package_id
          (if dp.dep_link then " (link)" else "")
          (if dp.dep_syntax then " (syntax)" else "");
      ) pk.package_requires
    end;
  ) pj.project_sorted;

  reset_package_ids pj.project_incomplete;
  reset_package_ids pj.project_disabled;

(*
  begin match !print_dot_packages with
    None -> ()
    | Some filename ->
      dump_dot_packages filename pj
  end;
*)

  pj
*)

type tpk = {
  tpk_id : int;
  tpk_name : string;
  tpk_pk : package;
  tpk_names : string list;
  tpk_tags : string list;
  tpk_raw_requires : string package_dependency StringMap.t;
  mutable tpk_enabled : bool;
  mutable tpk_keys : (string * string) list;
  mutable tpk_requires : tpk IntMap.t;
  mutable tpk_required_by : tpk IntMap.t;
}

(*
and tag = {
  tag_key : (string * string);
  tag_id  : int;
  tag_name : string; (* "", "byte", "native" *)
  tag_tpk : tpk;
}
*)

let verify_packages packages =
  let packages = BuildOCPInterp.final_state packages in

(* Verify that package directories really exist, and add 'requires'
   to pk.package_deps_map *)
  Array.iter BuildOCPInterp.check_package packages;


  let disabled_packages = ref [] in
  let tpk_packages = ref [] in
  let conflicts = ref [] in
  let tpk_id = ref 0 in

  (* (1) Verify that a given package is only provided by one
      package, discarding older versions and so on. *)
  let h = Hashtbl.create 113 in

  let remove_tpk tpk =
    List.iter (Hashtbl.remove h) tpk.tpk_keys;
    tpk.tpk_keys <- [];
    tpk.tpk_enabled <- false;
  in

  let compare_tpks tpk1 tpk2 =
    let pk1 = tpk1.tpk_pk in
    let pk2 = tpk2.tpk_pk in
    let o1 = pk1.package_options in
    let o2 = pk2.package_options in
    let pk1_generated = get_bool_with_default [o1] "generated" false in
    let pk2_generated = get_bool_with_default [o2] "generated" false in
    match pk1_generated, pk2_generated with
      false, false ->
      PackageConflict
    | true, true ->
      if normalized_dir pk1.package_dirname =
          normalized_dir pk2.package_dirname &&
         pk1.package_type = pk2.package_type &&
         pk1.package_version = pk2.package_version
           (* TODO: We should also test for asm/byte... *)
      then begin
        if verbose 5 then
          Printf.eprintf "Discarding duplicated package %S\n%!" pk1.package_name;
        PackageEquality
      end
      else
        begin
          if verbose 5 then begin
            Printf.eprintf "Conflict over %S\n" pk1.package_name;
            Printf.eprintf "dirname: %S\n" pk1.package_dirname;
            Printf.eprintf "type: %S\n" (string_of_package_type pk1.package_type);
            Printf.eprintf "version: %S\n" pk1.package_version;
            Printf.eprintf "dirname: %S\n" pk2.package_dirname;
            Printf.eprintf "type: %S\n" (string_of_package_type pk2.package_type);
            Printf.eprintf "version: %S\n" pk2.package_version;
          end;
          PackageConflict
        end

    (* TODO: if we have the sources of a new version, we should probably
       accept the new version, but not accept any installed version
       depending on the old version.

       TODO: implement this in a different pass : we should filter out
       packages that are duplicated, especially to discard installed
       packages. But what if we have the sources of A, { A + B }
       installed, and not the sources of B ?

       In the meantime, the solution is to desinstall the installed
       versions when they conflict.  *)

    | _, tpk2_installed -> PackageUpgrade tpk2_installed
  in
  (* A package with a given tag can only be provided by one package.
     This will change in the future, but cannot be handled currently. *)

  let verify_unicity name tag_name tpk =
    let key =  (name, tag_name) in
    try
      let tpk2 = Hashtbl.find h key in
      match compare_tpks tpk tpk2 with

      | PackageEquality ->
        (* TODO: we should keep the best *)
        remove_tpk tpk2;
        raise Not_found

      | PackageConflict ->
        conflicts := (tpk.tpk_pk, tpk2.tpk_pk, tpk.tpk_pk) :: !conflicts;
        remove_tpk tpk2;
        raise Not_found

      | PackageUpgrade add_new ->
        if add_new then begin
          remove_tpk tpk2;
          raise Not_found
        end else begin
          remove_tpk tpk;
          raise Exit
        end

    with Not_found ->
      Hashtbl.add h key tpk;
      tpk.tpk_keys <- key :: tpk.tpk_keys
  in


  let check_package_unicity pk =
    if verbose 5 || !print_package_deps then
    Printf.eprintf "check_package_unicity %s_%d of %s\n%!"
      pk.package_name pk.package_id pk.package_dirname;
(*
    StringMap.iter (fun name pkdep ->
      Printf.eprintf "\t%s depends on %s\n%!"
        pk.package_name pkdep.dep_project
    ) pk.package_deps_map;
*)
    let envs = [pk.package_options] in
    if is_enabled envs then try
      let tpk_name = pk.package_name in
      let tpk_tags =
        match pk.package_type with
        | ProgramPackage
        | LibraryPackage
        | ObjectsPackage
        | SyntaxPackage -> begin
            match
              get_bool_with_default envs "has_byte" true,
              get_bool_with_default envs "has_asm" true with
              true, true
            | false, false (* both disabled ? probably a mistake *)
              -> [ "byte"; "native" ]
            | true, false -> [ "byte" ]
            | false, true -> [ "native" ]
          end
        | RulesPackage -> []
        | TestPackage -> []
      in
      let tpk_names = get_strings_with_default envs "provides" [] in
      incr tpk_id;
      let tpk = {
        tpk_id = !tpk_id;
        tpk_requires = IntMap.empty; tpk_required_by = IntMap.empty;
        tpk_raw_requires = StringMap.empty;
        tpk_pk = pk; tpk_enabled = true; tpk_keys = [];
        tpk_name; tpk_tags; tpk_names } in
      tpk_packages := tpk :: !tpk_packages;
      verify_unicity tpk_name "" tpk;
      List.iter (fun tag_name ->
        List.iter (fun package_name ->
          verify_unicity package_name tag_name tpk
        ) (tpk_name :: tpk_names)
      ) tpk_tags
    with Exit ->
      (* Happens if verify_unicity discards this package because
         it knows a better one. *)
      ()
    else
      disabled_packages := pk :: !disabled_packages
  in
  Array.iter check_package_unicity packages;



  let superseded_packages = ref [] in
  let enabled_packages = ref [] in
  List.iter (fun tpk ->
    if tpk.tpk_enabled then
      enabled_packages := tpk :: !enabled_packages
    else
      superseded_packages := tpk.tpk_pk :: !superseded_packages
  ) !tpk_packages;

  (* 2. Ok, we have only one package providing every key, so we can start
     associating dependencies. However, there are also two cases:
     - some dependencies are not at all provided. This can lead to either
       completely disabling a package, or just removing either the "byte"
       or "native" part.

     If we depend on either a program or a syntax, it _always_ means that
      we depend on the bytecode part, unless the program/syntax is only
      provided in bytecode.
  *)

  let ready_queue = ref [] in
  let waiting_queue = ref IntMap.empty in

  let add_requires tpk tpk2 =
    tpk.tpk_requires <- IntMap.add tpk2.tpk_id tpk2 tpk.tpk_requires;
    tpk2.tpk_required_by <- IntMap.add tpk.tpk_id tpk tpk2.tpk_required_by
  in

  let add_requires tpk tag_name =
    let pk = tpk.tpk_pk in
    StringMap.iter (fun _ dep ->
      try
        let tpk2 = Hashtbl.find h (dep.dep_project, tag_name) in
        add_requires tpk tpk2
      with Not_found ->
(*        Printf.eprintf "Warning: missing dependency, %S requires %S/%s\n%!"
          tpk.tpk_name dep.dep_project tag_name; *)
        ()
    ) pk.package_deps_map
  in
  List.iter (fun tpk ->
    let pk = tpk.tpk_pk in
    match pk.package_type with
    | RulesPackage
    | TestPackage ->
      add_requires tpk ""

    | ProgramPackage
    | LibraryPackage
    | ObjectsPackage
    | SyntaxPackage ->

      let envs = [pk.package_options] in
      add_requires tpk "";
      let tag_names =
        match
          get_bool_with_default envs "has_byte" true,
          get_bool_with_default envs "has_asm" true with
          true, true | false, false -> [ "byte"; "native"]
        | true, false -> [ "byte" ]
        | false, true -> [ "native" ]
      in
      List.iter (add_requires tpk) tag_names
  ) !enabled_packages;

  List.iter (fun tpk ->
    if verbose 5 || !print_package_deps then

    Printf.eprintf "Taking %S from enabled_packages\n%!" tpk.tpk_pk.package_name;
    if IntMap.cardinal tpk.tpk_requires = 0 then
      ready_queue := tpk :: !ready_queue
    else
      waiting_queue := IntMap.add tpk.tpk_id tpk !waiting_queue
  ) !enabled_packages;

  let missing_packages = ref [] in
  let incomplete_packages = ref [] in
  let sorted_packages = ref [] in
  let h2 = Hashtbl.create 113 in
  let add_require pk (dep, pk2) =
    if (get_bool_with_default [pk.package_options] "generated" false)
      && not (get_bool_with_default [pk2.package_options] "generated" false)
    then begin
      match pk.package_type with
      | ProgramPackage
      | RulesPackage
      | SyntaxPackage -> ()
      | LibraryPackage
      | TestPackage
      | ObjectsPackage ->
        Printf.eprintf
          "Warning: installed package %s depends on source package %s\n%!"
          pk.package_name pk2.package_name;
        raise Exit
    end;

    let dep2 = new_dep pk pk2 dep.dep_options in
    dep2.dep_link <- dep.dep_link;
    dep2.dep_syntax <- dep.dep_syntax;
  in
  let add_missing pk dep =
    missing_packages := (dep.dep_project, [pk]) :: !missing_packages;
  in
  while !ready_queue <> [] do
    let queue = !ready_queue in
    ready_queue := [];
    List.iter (fun tpk ->
      if verbose 5 || !print_package_deps then
        Printf.eprintf "Taking %S from ready_queue\n%!" tpk.tpk_pk.package_name;
      IntMap.iter (fun _ tpk2 ->
        tpk2.tpk_requires <- IntMap.remove tpk.tpk_id tpk2.tpk_requires;
        if IntMap.cardinal tpk2.tpk_requires = 0 then begin
          if verbose 5 || !print_package_deps then
            Printf.eprintf "Adding %S to ready_queue!\n%!" tpk2.tpk_pk.package_name;
          ready_queue := tpk2 :: !ready_queue;
          waiting_queue := IntMap.remove tpk2.tpk_id !waiting_queue
        end
      ) tpk.tpk_required_by;
      let pk = tpk.tpk_pk in
      if verbose 5 || !print_package_deps then
        Printf.eprintf "Examining package %S\n" pk.package_name;
      let envs = [pk.package_options] in

      try
        match pk.package_type with
        | RulesPackage ->

          StringMap.iter (fun _ dep ->
            try
              let pk2 = Hashtbl.find h2 (dep.dep_project, "") in
              add_require pk (dep, pk2)
            with Not_found ->
              Printf.eprintf "Warning: missing dependency, %S requires %S\n%!"
                tpk.tpk_name dep.dep_project;
              add_missing pk dep;
              raise Exit
          ) pk.package_deps_map;
          Hashtbl.add h2 (pk.package_name, "") pk;
          List.iter (fun package_name ->
            Hashtbl.add h2 (package_name, "") pk;
          ) (get_strings_with_default envs "provides" []);
          sorted_packages := pk :: !sorted_packages

        | TestPackage
        | ProgramPackage
        | LibraryPackage
        | ObjectsPackage
        | SyntaxPackage ->

          let envs = [pk.package_options] in
          let had_byte = get_bool_with_default envs "has_byte" true in
          let had_asm = get_bool_with_default envs "has_asm" true in
          let has_byte = ref had_byte in
          let has_asm = ref had_asm in

          let byte_requires = ref [] in
          let asm_requires = ref [] in




          (* bytecode version *)
          if !has_byte then begin try
          StringMap.iter (fun _ dep ->
            try
              let pk2 = Hashtbl.find h2 (dep.dep_project, "byte") in
              match pk2.package_type with
              | RulesPackage -> assert false
              | TestPackage (* TODO: why would we depend on a test package ?? *)
              | LibraryPackage
              | ProgramPackage
              | ObjectsPackage
              | SyntaxPackage
                ->
                byte_requires := (dep, pk2) :: !byte_requires
            with Not_found ->
              try
                let pk2 = Hashtbl.find h2 (dep.dep_project, "native") in
                match pk2.package_type with
                | RulesPackage -> assert false
                | TestPackage ->
                  (* TODO: why would we depend on a test package ?? *)
                  byte_requires := (dep, pk2) :: !byte_requires;
                | LibraryPackage
                | ObjectsPackage
                  ->
                  Printf.eprintf
                    "Warning: %S depends on %S, that only exists in native code\n%!" pk.package_name pk2.package_name;
                  raise Exit
                | ProgramPackage
                | SyntaxPackage ->
                  byte_requires := (dep, pk2) :: !byte_requires

              with Not_found ->
              try
                let pk2 = Hashtbl.find h2 (dep.dep_project, "") in
                match pk2.package_type with
                | RulesPackage ->
                  byte_requires := (dep, pk2) :: !byte_requires;
                | TestPackage ->
                  (* TODO: why would we depend on a test package ?? *)
                  byte_requires := (dep, pk2) :: !byte_requires;
                | LibraryPackage
                | ProgramPackage
                | ObjectsPackage
                | SyntaxPackage
                  -> assert false
              with Not_found ->
                Printf.eprintf "Warning: missing dependency, bytecode %S requires %S bytecode\n%!"
                  tpk.tpk_name dep.dep_project;
                add_missing pk dep;
                raise Exit
          ) pk.package_deps_map;
            with Exit ->
              Printf.eprintf "\tDisabling bytecode version of %S\n%!"
                  tpk.tpk_name;
                has_byte := false;
          end;




          (* native version *)
          if !has_asm then begin try
          StringMap.iter (fun _ dep ->
            try
              let pk2 = Hashtbl.find h2 (dep.dep_project, "native") in
              match pk2.package_type with
              | RulesPackage -> assert false
              | TestPackage (* TODO: why would we depend on a test package ?? *)
              | LibraryPackage
              | ProgramPackage
              | ObjectsPackage
              | SyntaxPackage
                ->
                asm_requires := (dep, pk2) :: !asm_requires
            with Not_found ->
              try
                let pk2 = Hashtbl.find h2 (dep.dep_project, "byte") in
                match pk2.package_type with
                | RulesPackage -> assert false
                | TestPackage ->
                  (* TODO: why would we depend on a test package ?? *)
                  asm_requires := (dep, pk2) :: !asm_requires;
                | LibraryPackage
                | ObjectsPackage
                  ->
                  Printf.eprintf
                    "Warning: %S depends on %S, that only exists in byte code\n%!" pk.package_name pk2.package_name;
                  raise Exit
                | ProgramPackage
                | SyntaxPackage ->
                  asm_requires := (dep, pk2) :: !asm_requires

              with Not_found ->
              try
                let pk2 = Hashtbl.find h2 (dep.dep_project, "") in
                match pk2.package_type with
                | RulesPackage ->
                  asm_requires := (dep, pk2) :: !asm_requires;
                | TestPackage ->
                  (* TODO: why would we depend on a test package ?? *)
                  asm_requires := (dep, pk2) :: !asm_requires;
                | LibraryPackage
                | ProgramPackage
                | ObjectsPackage
                | SyntaxPackage
                  -> assert false
              with Not_found ->
                Printf.eprintf "Warning: missing dependency, native %S requires %S native\n%!"
                  tpk.tpk_name dep.dep_project;
                add_missing pk dep;
                raise Exit
          ) pk.package_deps_map;
      with Exit ->
                Printf.eprintf "\tDisabling native version of %S\n%!"
                  tpk.tpk_name;
                has_asm := false;
          end;

          if !has_byte || !has_asm then begin
            List.iter (add_require pk) !byte_requires;
            List.iter (add_require pk) !asm_requires;
            sorted_packages := pk :: !sorted_packages;
            Hashtbl.add h2 (pk.package_name, "") pk;
            if !has_byte then begin
              Hashtbl.add h2 (pk.package_name, "byte") pk;
              List.iter (fun package_name ->
                Hashtbl.add h2 (package_name, "byte") pk;
              ) (get_strings_with_default envs "provides" []);
            end else
            if had_byte then
              pk.package_options <- set_bool pk.package_options
                  "has_byte" false;
            if !has_asm then begin
              Hashtbl.add h2 (pk.package_name, "native") pk;
              List.iter (fun package_name ->
                Hashtbl.add h2 (package_name, "native") pk;
              ) (get_strings_with_default envs "provides" []);
            end else
            if had_asm then
              pk.package_options <- set_bool pk.package_options
                  "has_asm" false;
          end else raise Exit
      with Exit ->
        Printf.eprintf "Package %S disabled\n%!" tpk.tpk_name;
        tpk.tpk_enabled <- false;
        incomplete_packages := pk :: !incomplete_packages
    ) queue
  done;

  if IntMap.cardinal !waiting_queue > 0 then begin
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "%d packages waiting in waiting queue !\n%!"
      (IntMap.cardinal !waiting_queue);
    IntMap.iter (fun _ tpk ->
      Printf.eprintf "  %S still requires:\n%!" tpk.tpk_pk.package_name;
      IntMap.iter (fun _ tpk ->
        Printf.eprintf "     %S\n%!" tpk.tpk_pk.package_name;
      ) tpk.tpk_requires
    ) !waiting_queue;
  end;

  let sorted_packages = List.rev !sorted_packages in

(* Already performed ?

  (* N. We have to check that installed packages only depend on
        installed packages ! *)
  List.iter (fun pk ->
    if get_bool_with_default [pk.package_options] "generated" false then
      List.iter (fun dep2 ->
        let pk2 = dep2.dep_project in
        if not ( get_bool_with_default [pk2.package_options] "generated" false )
        then begin
          Printf.eprintf "ERROR ERROR !!!\n%!";
          Printf.eprintf "Installed %s depends on non installed %s\n%!"
            pk.package_name pk2.package_name;
          exit 2
        end;
      ) pk.package_requires

  ) sorted_packages;
*)

  List.iter update_deps sorted_packages;

  let npackages = Array.length packages in

  let pj = {
(*    project_files = files; *)
    project_sorted = Array.of_list sorted_packages;
    project_missing = !missing_packages;
    project_disabled = Array.of_list !disabled_packages;
    project_incomplete = Array.of_list !incomplete_packages;
    project_conflicts = ! conflicts;
  } in
(* TODO: fix this assertion. The equality stands only if we count
also duplicated packages. *)
  assert (npackages >=
      Array.length pj.project_sorted +
        Array.length pj.project_incomplete +
        Array.length pj.project_disabled);

  (* Change the package IDs: the package_requires_map is not correct anymore ! *)
  reset_package_ids pj.project_sorted;
  (* TODO: The impact of this is that all dependencies are sorted in
     the same order in all packages. This might, however, not be what
     someone wants, because you might want to have a different link
     order than the one globally inferred.  *)
  Array.iter (fun pk ->
    if requires_keep_order_option.get [pk.package_options]  then
      let (sorted, cycle, _ ) = PackageLinkSorter.sort pk.package_requires in
      assert (cycle = []);
      pk.package_requires <- sorted
    else
      pk.package_requires <- List.sort (fun dep1 dep2 ->
        compare
          dep1.dep_project.package_id
          dep2.dep_project.package_id) pk.package_requires;

    if !print_package_deps || verbose 9 then begin
      Printf.eprintf "Package %S[%d]\n" pk.package_name pk.package_id;
      List.iter (fun dp ->
        Printf.eprintf "\t%S[%d]%s%s\n"
          dp.dep_project.package_name
          dp.dep_project.package_id
          (if dp.dep_link then " (link)" else "")
          (if dp.dep_syntax then " (syntax)" else "");
      ) pk.package_requires
    end;
  ) pj.project_sorted;

  reset_package_ids pj.project_incomplete;
  reset_package_ids pj.project_disabled;

(*
  begin match !print_dot_packages with
    None -> ()
    | Some filename ->
      dump_dot_packages filename pj
  end;
*)

  pj


let scan_root root_dir =
  let files = ref [] in
  BuildScanner.scan_directory
     (fun _ _ filename ->
    if Sys.is_directory filename then begin
      if Sys.file_exists (Filename.concat filename ".ocpstop") then
        BuildScanner.ignore_file_or_directory ()
    end else begin
      begin
        if Filename.check_suffix filename ".ocp" then
          match (Filename.basename filename).[0] with
            'a'..'z' | 'A'..'Z' | '0'..'9' ->
            files := File.of_string filename :: !files
          | _ -> ()
      end;
      BuildScanner.ignore_file_or_directory ()
    end;
  ) (File.to_string root_dir);
  List.rev !files
(* files are reverted, so that the first in breadth are used first
(this is expected from [load_project] *)

let magic_head = "OCP-"
let magic_head_len = String.length magic_head
let magic_kind = "PROJ"
let magic_kind_len = String.length magic_kind
let magic_version = "20120928"
let magic_version_len = String.length magic_version
let magic = magic_head ^ magic_kind ^ magic_version
let magic_len = String.length magic

let save_project_state state filename =
  let oc = File.X.open_out_bin filename in
  output_string oc magic;
  output_value oc (state : project);
  close_out oc

let load_project_state filename =
  let ic = File.X.open_in_bin filename in
  let possible_magic = String.create magic_len in
  begin try
          really_input ic possible_magic 0 magic_len;
    with e ->
      close_in ic;
      failwith "load_project_state: truncated file"
  end;
  if possible_magic <> magic then begin
    close_in ic;
    if String.sub possible_magic 0 magic_head_len <> magic_head then
      failwith "load_project_state: not an OCP file";
    if String.sub possible_magic magic_head_len magic_kind_len
      <> magic_kind then
      failwith "load_project_state: not an OCP PROJECT file";
    if String.sub possible_magic (magic_head_len + magic_kind_len)
      magic_version_len <> magic_version then
      failwith "load_project_state: bad OCP PROJECT version";
  end;
  try
    let v = (input_value ic : project) in
    close_in ic;
    v
  with e ->
    close_in ic;
    raise e

(*
val scan_project : (project -> unit)
let scan_project pj =
  let files = ref [] in
  BuildScanner.scan_directory_for_suffix
    (File.to_string pj.project_dir) ".ocp" (fun filename ->
    files := File.of_string filename :: !files);
  pj.project_files =:= List.rev !files;
  save_project pj.project_file pj;
  ()
*)

(*

  if !list_ocp_files || !verbosity_arg > 1 then begin
    Printf.eprintf "Found %d project files:\n%!" (List.length !files);
    List.iter (fun file ->
      Printf.eprintf "\t%s\n%!" file) !files;
  end;

*)

let find_package pj file =
  let list = ref [] in

  let st = File.X.lstat file in
(*
  let dir_t = pj.project_dir in
  let _dir = File.to_string dir_t in
*)
  let check_file pk filename =
    let file = File.of_string (Filename.concat pk.package_dirname filename) in
    try
      let st2 = File.X.lstat file in
      if
        st.MinUnix.st_ino = st2.MinUnix.st_ino &&
        st.MinUnix.st_dev = st2.MinUnix.st_dev then
        list := pk :: !list
    with _ -> ()
  in
  Array.iter (fun pk ->
    List.iter (fun (filename, _) ->
      check_file pk filename;
      let (kernel, extension) = File.cut_last_extension filename in
      match extension with
        | "ml" -> check_file pk (filename ^ ".mli")
        | "mli" -> ()
        | "mll" -> check_file pk (filename ^ ".ml")
        | "mly" ->
          check_file pk (filename ^ ".ml");
          check_file pk (filename ^ ".mli")
        | _ -> ()
    ) (BuildOCPVariable.get [pk.package_options] "files")
  ) pj.project_sorted;

  !list


let rec find_obuild f dir =
  let possible_dir = Filename.concat dir "_obuild" in
  if Sys.file_exists possible_dir then
    f possible_dir
  else
    let new_dir = Filename.dirname dir in
    if dir <> new_dir then find_obuild f new_dir

(*
val find_project : (File.t -> File.t)
*)
let find_root root_dir basenames =
  let rec find dirname (basenames : string list) =
    let file = File.add_basenames dirname basenames in
    if File.X.exists file then dirname else
      let new_dirname = File.dirname dirname in
      if new_dirname == dirname then raise Not_found;
      find new_dirname basenames
  in
  let root_dir = if File.is_absolute root_dir then root_dir else
      File.concat (File.X.getcwd ()) root_dir
  in
  find root_dir basenames

let rec eprint_project msg pj =
  Printf.eprintf "%s = {\n" msg;
  Printf.eprintf "  sorted = [\n";
  print_package_array "    " pj.project_sorted;
  Printf.eprintf "  ]\n";
  Printf.eprintf "}\n";

and print_package_array indent array =
  let indent2 = indent ^ "  " in
  Array.iter (fun pk ->
    Printf.eprintf "%s{\n" indent;
    Printf.eprintf "%spackage_name = %S\n" indent2 pk.package_name;
    Printf.eprintf "%spackage_dirname = %S\n" indent2 pk.package_dirname;
    BuildOCPInterp.eprint_env indent2 pk.package_options;
    Printf.eprintf "%s}\n" indent;
  ) array

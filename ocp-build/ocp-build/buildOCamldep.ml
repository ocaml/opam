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
(* open OcpLang *)
(* open OcpSystem *)
open BuildOCamlTypes
open BuildOCamlVariables

let verbose = DebugVerbosity.verbose [ "B" ] "BuildOCamldep"

let parse_dependencies b =
  let s = Buffer.contents b in
  let len = String.length s in
  let dependencies = ref [] in

  let rec parse_name pos pos0 =
(*    Printf.eprintf "parse_name %d %d\n%!" pos pos0; *)
    if pos+1 < len && s.[pos] = ':' && s.[pos+1] = ' ' then
      let target = String.sub s pos0 (pos-pos0) in
      skip_spaces target [] (pos+1)
    else
      if pos+1 < len && s.[pos] = ':' && s.[pos+1] = '\n' then begin
	let target = String.sub s pos0 (pos-pos0) in
	dependencies := (target, []) :: !dependencies;
	parse_name (pos+2) (pos+2)
      end else
	if pos = len then
	  !dependencies
	else
	  parse_name (pos+1) pos0

  and skip_spaces target deps pos =
(*    Printf.eprintf "skip_spaces %d\n%!" pos; *)
    if pos = len then
      (target, deps) :: !dependencies
    else
      match s.[pos] with
	  '\\' when pos+1 < len && s.[pos+1] = '\n' ->
	    skip_spaces target deps (pos+2)
	| ' ' ->
	  skip_spaces target deps (pos+1)
	| '\n' ->
	  dependencies := (target, deps) :: !dependencies;
	  parse_name (pos+1) (pos+1)
	| '\\' when pos + 1 < len && s.[pos+1] = ' ' ->
	  Buffer.clear b;
	  Buffer.add_char b ' ';
	  parse_dependency target deps (pos+2)
	| c ->
	  Buffer.clear b;
	  Buffer.add_char b c;
	  parse_dependency target deps (pos+1)

  and parse_dependency target deps pos =
(*    Printf.eprintf "parse_dependency %d\n%!" pos; *)
    if pos = len then
      (target, (Buffer.contents b) :: deps) :: !dependencies
    else
      match s.[pos] with
	| '\\' when pos + 1 < len && s.[pos+1] = ' ' ->
	  Buffer.add_char b ' ';
	  parse_dependency target deps (pos+2)
	| ' ' ->
	  skip_spaces target ((Buffer.contents b) :: deps) (pos+1)
	| '\n' ->
	  dependencies := (target,
     (Buffer.contents b) :: deps) :: !dependencies;
	  parse_name (pos+1) (pos+1)
	| c ->
	  Buffer.add_char b c;
	  parse_dependency target deps (pos+1)
  in
  parse_name 0 0


	    (* ocamldep generates dependencies towards .cmo files for
	       .cmi files, even in the case where we are only
	       interested in .cmx files !  Problem: if we add two
	       dependencies, one to .cmo and one to .cmx, then
	       rebuilding any of them will trigger regenerating the
	       .cmi, while in fact, only rebuilding both should
	       trigger the rebuilding.

	       In a general case, what should we do if there is a
	       dependency towards a bytecode file (in particular for
	       camlp4) when specifying native building ?

	       In fact, probably, we want to add the first active
	       dependency among a set of dependencies. So, the
	       dependency would not be a filename by a list of
	       filenames.
	    *)

let expanse_dependencies list =
  List.map (fun (target, deps) ->
    if Filename.check_suffix target  ".cmi" then
      (target, List.map (fun dep ->

	if Filename.check_suffix target ".cmo" then
	  let cmx = String.copy dep in
	  cmx.[String.length cmx - 1 ] <- 'x';
	  [ dep; cmx ]
	else
	  [dep]
       ) deps)
    else
      (target, List.map (fun dep -> [dep]) deps)
  ) list

(* load_dependencies: the old way, i.e. path to files in the load_path *)

let parse_dep_buf = Buffer.create 10000
let load_make_dependencies filename =
  Buffer.clear parse_dep_buf;
  let ic = open_in filename in
  begin
    try
      while true do
	let line = input_line ic in
	Printf.bprintf parse_dep_buf "%s\n%!" line
      done
    with End_of_file -> ()
  end;
  close_in ic;
  parse_dependencies parse_dep_buf


let print_make_dependencies deps =
  List.iter (fun (dep, deps) ->
    Printf.eprintf "%s: " dep;
    List.iter (fun x -> Printf.eprintf "%s " x ) deps;
    Printf.eprintf "\n%!";
  ) deps

let load_make_dependencies filename =
  try
    let deps = load_make_dependencies filename in
(*    print_make_dependencies deps; *)
    deps
  with e ->
    Printf.eprintf "Warning: exception %s in load_make_dependencies\n%!"
      (Printexc.to_string e);
    raise e


let load_dependencies filename =
  expanse_dependencies (load_make_dependencies filename)


(* Another solution:

Use ocamldep -modules toto.ml

When reading, we must keep track of what project this file belongs to.
Then, we can infer from which projects the dependencies are
*)

open BuildEngineTypes
open BuildTypes
open BuildGlobals
open BuildOCPTypes
open BuildOCPVariable

let print_dependencies deps =
  List.iter (fun (dep, deps) ->
    Printf.eprintf "%s: " dep;
    List.iter (fun list ->
      match list with
	  [] -> ()
	| [ x ] -> Printf.eprintf "%s " x
	| [ x; y ] -> Printf.eprintf " (%s|%s) " x y
	| _ -> assert false
    ) deps;
    Printf.eprintf "\n%!";
  ) deps


let load_ocamldep_modules filename =
  let ic = open_in filename in
  let s = input_line ic in
  close_in ic;
  let (source, modules) = OcpString.cut_at s ':' in
  let modules = OcpString.split modules ' ' in
  source, modules

(* With packing, tracking dependencies is trickier. From a given module,
we can only access internal modules of the current project, and external
modules of the other projects.
*)

let modname_of_file options force filename =
  let filename = Filename.basename filename in
  let is_ml =
    Filename.check_suffix filename ".ml"
      || force = Force_IMPL
    || get_bool_with_default options "ml" false
  in
  let basename = Filename.chop_extension filename in
  let modname = String.capitalize basename in
  is_ml, modname, basename

let filter_deps options option modules =
  let nodeps =
    let nodeps = ref StringSet.empty in
    List.iter (fun modname ->
      nodeps := StringSet.add modname !nodeps)
      (option.get options);
    !nodeps
  in
  List.filter (fun modname ->
    not (StringSet.mem modname nodeps)) modules

let load_modules_dependencies lib options force dst_dir pack_for needs_odoc filename =
  let envs = [options; lib.lib_options] in
  let has_asm = asm_option.get envs in
  let has_byte = byte_option.get envs in

  if verbose 5 then
    Printf.eprintf "load_modules_dependencies %s\n" filename;
  let source, modules = load_ocamldep_modules filename in
  let modules =
    if nopervasives.get envs then modules
    else "Pervasives" :: modules
  in

  let (is_ml, modname, basename) = modname_of_file envs force source in

  let modules =
    if not is_ml || nointernaldeps.get envs then modules
    else
      "CamlinterlLazy" ::
        "CamlinterlOO" ::
        "CamlinterlMod" ::
        modules
  in

  let modules = filter_deps envs nodeps_option modules in
  let cmx_modules = filter_deps envs nocmxdeps_option modules in

  (*  let is_ml = Filename.check_suffix source ".ml" in *)
  (*  let full_basename = Filename.chop_extension source in *)
  (*  let basename = Filename.basename full_basename in *)
  (*  let modname = String.capitalize basename in *)

  let deps = lib.lib_requires in
  let deps = List.concat (List.map (fun dep ->
        let lib = dep.dep_project in
        if dep.dep_link ||
           (get_bool_with_default [dep.dep_options] "externals_only" false) then
          [(lib.lib_dst_dir, lib.lib_modules)]
        else []
      ) (List.rev deps)) in
  let rec add_internal_deps pack_for deps =
    match pack_for with
    | [] ->
      let deps = (lib.lib_dst_dir, lib.lib_modules) :: deps in
      deps
    | _ :: tail ->
      let deps = add_internal_deps tail deps in
      let (dst_dir, map) =
        StringsMap.find pack_for lib.lib_internal_modules in
      let deps = (dst_dir, map) :: deps in
      deps
  in
  let deps = add_internal_deps (List.rev pack_for) deps in

  if verbose 6  then begin
    Printf.eprintf "load_modules_dependencies %s\n" filename;
    List.iter (fun (dst_dir, map) ->
      Printf.eprintf "   IN %s :\n\t" dst_dir.dir_fullname;
      StringMap.iter (fun modname _ ->
	Printf.eprintf "%s " modname
      ) !map;
      Printf.eprintf "\n"
    ) deps;

  end;

  let depends_only_on_cmi exts =
    let dependencies = ref [] in
    let rec find_module deps depname =
      (*      Printf.eprintf "find_module CMI %s\n" depname; *)
      match deps with
	[] ->
	if verbose 5 then
	  Printf.eprintf "Warning: could not solve dependency %s for %s\n" depname filename;
	()
      | (dst_dir, lib_modules) :: deps ->
	try
	  let (kind, basename) = StringMap.find depname !lib_modules in
	  let dst_dir = dst_dir.dir_fullname in
	  let full_basename = Filename.concat dst_dir basename in
	  match kind with
	  | ML ->
	    dependencies := [ full_basename ^ ".cmo"; full_basename ^ ".cmx" ] :: !dependencies
	  | MLI ->
	    dependencies := [ full_basename ^ ".cmi" ] :: !dependencies
	  | MLandMLI ->
	    dependencies := [ full_basename ^ ".cmi" ] :: !dependencies

	with Not_found ->
	  find_module deps depname
    in
    List.iter (find_module deps) modules;
    List.map (fun ext ->
      let target = Filename.concat dst_dir.dir_fullname (basename ^ ext) in
      target, !dependencies) exts
  in

  let dependencies =
    if is_ml then

      let byte_dependencies =
        if has_byte then

          let cmo_target = Filename.concat dst_dir.dir_fullname (basename ^ ".cmo") in

          let cmo_dependencies =
            let cmo_dependencies = ref [] in
            let rec find_module deps depname =
              (*      Printf.eprintf "find_module CMO %s\n" depname; *)
              match deps with
	        [] ->
	        if verbose 5 then
	          Printf.eprintf
                    "Warning: could not solve dependency %s for %s\n"
                    depname filename;
	        ()
	      | (dst_dir, lib_modules) :: deps ->
	        try
	          let (kind, basename) = StringMap.find depname !lib_modules in
	          let dst_dir = dst_dir.dir_fullname in
	          let full_basename = Filename.concat dst_dir basename in
	          let deps =
		    match kind with
		    | ML ->
		      [ full_basename ^ ".cmo" ]
		    | MLI ->
		      [ full_basename ^ ".cmi" ]
		    | MLandMLI ->
		      [ full_basename ^ ".cmi" ]
	          in
	          cmo_dependencies := deps :: !cmo_dependencies
	        with Not_found ->
	          find_module deps depname
            in
            List.iter (find_module deps) modules;
            !cmo_dependencies
          in
          [ cmo_target, cmo_dependencies ]
        else []
      in
      let asm_dependencies =
        if has_asm then
          let cmx_target = Filename.concat dst_dir.dir_fullname (basename ^ ".cmx") in

          let cmx_dependencies =
            let cmx_dependencies = ref [] in
            let rec find_module deps depname =
              (*      Printf.eprintf "find_module CMX %s\n" depname; *)
              match deps with
	        [] ->
	        if verbose 5 then
	          Printf.eprintf
                    "Warning: could not solve dependency %s for %s\n"
                    depname filename;
	        ()
	      | (dst_dir, lib_modules) :: deps ->
	        try
	          let (kind, basename) = StringMap.find depname !lib_modules in
	          let src_dir = dst_dir.dir_fullname in
	          let full_basename = Filename.concat src_dir basename in
	          let deps =
		    match kind with
		    | ML ->
		      [ full_basename ^ ".cmx" ]
		    | MLI ->
		      [ full_basename ^ ".cmi" ]
		    | MLandMLI ->
		      [ full_basename ^ ".cmx" ]
	          in
	          cmx_dependencies := deps :: !cmx_dependencies
	        with Not_found ->
	          find_module deps depname
            in
            List.iter (find_module deps) cmx_modules;
            !cmx_dependencies
          in
          [ cmx_target, cmx_dependencies ]
        else
          []
      in
      let dependencies =
        byte_dependencies @ asm_dependencies
      in
      if needs_odoc then
        (depends_only_on_cmi [ ".odoc"]) @ dependencies
      else dependencies
    else
      depends_only_on_cmi (if needs_odoc then [".odoc"; ".cmi"] else [".cmi"])
  in

  if verbose 5 then
    Printf.eprintf "load_modules_dependencies %s DONE\n" filename;
  if verbose 3 then
    print_dependencies dependencies;
  dependencies



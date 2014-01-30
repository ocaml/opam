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
open MetaTypes

let verbose = DebugVerbosity.verbose ["B"] "BuildOCamlMeta"

open BuildTypes
open BuildEngineTypes
open BuildEngineGlobals

open BuildOCPTypes
open BuildOCPVariable
open BuildOCamlConfig.TYPES

let load_META_files pj ocamllib top_dirname =
  let add_META meta_dirname basename meta_filename =
    (*
      Printf.eprintf "dirname=%S\n%!" dirname;
      Printf.eprintf "basename=%S\n%!" basename;
      Printf.eprintf "filename=%S\n%!" filename;
    *)
    try
      let meta = MetaParser.parse_file meta_filename in
      if verbose 4 then
        Printf.eprintf "Loaded %S\n%!" meta_filename;

      let rec add_meta meta_dirname pj path name meta =
        if verbose 4 then
          Printf.eprintf "add_meta %S %S\n%!" path name;

        let dirname = match meta.meta_directory with
          | Some dirname when dirname <> "" ->
            if dirname.[0] = '^' || dirname.[0] = '+' then
              Filename.concat ocamllib
                (String.sub dirname 1 (String.length dirname-1))
            else
            if Filename.is_relative dirname then
              Filename.concat meta_dirname dirname
            else
              dirname
          | _ -> meta_dirname
        in

        if verbose 4 then
          Printf.eprintf "dirname=%S\n%!" dirname;
        let exists =
          match meta.meta_exists_if with
            [] -> true
          | list ->
            List.for_all (fun filename ->
              let proof_filename = Filename.concat dirname filename in
              if not (Sys.file_exists proof_filename) then begin
                if verbose 4 then
                  Printf.eprintf
                    "Warning: proof of package %S does not exist\n%!"
                    proof_filename;
                false
              end else true) list
        in
        if exists then
          (*
            let name =
            match meta.meta_name with
            TODO: meta_name is the archive name !!
            None -> name
            | Some name ->  (* lowercase to handle 'camlimages' *)
            String.lowercase name
            in  *)
          let fullname = path ^ name in
          let has_asm = ref None in
          let has_byte = ref None in
          let has_syntax = ref None in

          StringMap.iter (fun _ var ->
            match var.metavar_preds, var.metavar_value with
            (* TODO: handle multiple files (objects) *)

            | [ "byte", true ], [ archive ]
              when Filename.check_suffix archive ".cma"
              ->
              has_byte := Some (Filename.chop_suffix archive ".cma")

            | [ "syntax", true; "preprocessor", true ], [ archive ]
              when Filename.check_suffix archive ".cma"
              ->
              has_syntax := Some (Filename.chop_suffix archive ".cma")

            | [ "native", true ], [ archive ]
              when Filename.check_suffix archive ".cmxa"
              ->
              has_asm := Some (Filename.chop_suffix archive ".cmxa")

            | _ -> ()
          ) meta.meta_archive;

          let archive = match !has_asm, !has_byte with
              None, None -> None
            | Some asm_archive, Some byte_archive ->
              if asm_archive = byte_archive then
                Some byte_archive
              else begin
                Printf.eprintf "Warning: no common name for asm and byte in %S\n%!" fullname;
                None
              end
            | archive, None
            | None , archive -> archive
          in

          let requires = ref [] in
          StringMap.iter (fun _ var ->
            match var.metavar_preds with
            | [] ->
              requires := List.map (fun s ->
                  match s with
                    "camlp4" -> "camlp4lib"
                  | s -> s
                ) var.metavar_value

            (*
                  | [ "byte", true ] ->
                  has_byte := Some var.metavar_value
                  | [ "native", true ] ->
                  has_asm := Some var.metavar_value
                *)
            | _ -> ()
          ) meta.meta_requires;

          (* for objects, we should set   pk.package_sources <- source_files; *)

          let create_package fullname kind requires archive =

            let options = empty_env in

            let options = set options
                "requires" (List.map (fun (s, link) ->
                  let link =
                    if Filename.check_suffix s ".syntax" then false else link in
                  s, (set_bool empty_env "tolink" link)
                ) requires) in
            let options = set_bool options "generated" true in

            let pk = BuildOCPInterp.new_package pj fullname dirname
                meta_filename [meta_filename, None (* matters only for non-installed packages *)
                              ] kind options in
            pk.package_source_kind <- "meta";
(*
          List.iter (fun (s, link) ->
            let ( dep :  'a package_dependency) =
              BuildOCPInterp.new_package_dep pk s in
            dep.dep_link <- link
          ) requires;
*)

            (* this package has already been generated *)

            begin
              match meta.meta_version with
                None -> ()
              | Some version -> pk.package_version <- version
            end;

            begin
              match archive with
                None ->
                pk.package_options <- set_bool pk.package_options "meta"  true ;
                if verbose 4 then
                  Printf.eprintf "Warning: package %S is meta\n%!" fullname
              | Some archive ->
                pk.package_options <- set_string pk.package_options "archive" archive;
            end;

            BuildOCPInterp.check_package pk;

          in


          (* For syntaxes, we need to do some black magic, since we
             need to create two to three different packages.  - 2
             packages if (archive = None, syntax = Some _) || (archive
             = syntax = Some _) - 3 packages if archive = Some x,
             syntax = Some y, x <> y TODO: we should do a pass, before
             verify_packages, to fix problems introduced by this
             heuristic. In particular, since one META package can
             generate several OCP packages, we must discriminate the
             dependencies of other META packages to choose between the
             OCP packages.

             TODO: I am not completely happy with this behavior. We
              might want to have a more aggressive behavior, based on
              using a combination of META and ocamlobjinfo, to fix
              information from META.  *)
          begin match !has_syntax with
            | None ->
              create_package fullname BuildOCPTree.LibraryPackage
                (List.map (fun l -> (l,true)) !requires) archive;
            | Some syntax_archive ->
              match archive with
              | None ->
                create_package (fullname ^ ".ocp-syntax-library")
                  BuildOCPTree.LibraryPackage
                  (List.map (fun l -> (l,true)) !requires)
                  (Some syntax_archive);
                create_package fullname BuildOCPTree.SyntaxPackage
                  [fullname ^ ".ocp-syntax-library", true] None;
              | Some archive ->
                create_package fullname BuildOCPTree.LibraryPackage
                  (List.map (fun l -> (l,true)) !requires) (Some archive);
                create_package (fullname ^ ".ocp-syntax-library")
                  BuildOCPTree.LibraryPackage
                  (List.map (fun l -> (l,true)) !requires)
                  (Some syntax_archive);
                create_package (fullname ^ ".ocp-syntax")
                  BuildOCPTree.SyntaxPackage
                  [fullname ^ ".ocp-syntax-library", true] None;
          end;
          List.iter (fun (name, meta) ->
            add_meta dirname pj (fullname ^ ".") name meta) meta.meta_package;

      in
      let name = MetaParser.name_of_META meta_filename in
      add_meta meta_dirname pj "" name meta

    with e ->
      Printf.eprintf "Warning: exception %S while loading %S\n%!"
        (Printexc.to_string e) meta_filename


  in
  if verbose 4 then
    Printf.eprintf "Loading METAs from %S\n%!" top_dirname;
  let files = Sys.readdir top_dirname in
  Array.iter (fun basename ->
    let filename = Filename.concat top_dirname basename in
    if OcpString.starts_with basename "META." then
      add_META top_dirname basename filename
    else
    if Sys.is_directory filename then
      let meta_filename = Filename.concat filename "META" in
      if Sys.file_exists meta_filename then
        add_META filename "META" meta_filename
  ) files


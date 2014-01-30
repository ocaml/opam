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
open BuildOCPVariable

let find_indent line =
  let rec find_indent line i len =
    if i < len then
      match line.[i] with
          ' ' | '\t' -> find_indent line (i+1) len
        | '#' -> (i, true)
        | _ -> (i, false)
    else (i, false)
  in
  let len = String.length line in
  let (indent, comment) = find_indent line 0 len in
  (indent, comment, String.sub line indent (len - indent))

(* From oasis-0.2.1~alpha1%  less src/oasis/OASISRecDescParser.ml LGPL *)
let oasis_lexer = Genlex.make_lexer
  [
        (* Statement *)
    "+:"; "$:"; ":"; "if"; "{"; "}"; "else";
        (* Section *)
    "Flag"; "Library"; "Executable";
    "SourceRepository"; "Test";
    "Document";
        (* Expression *)
    "!"; "&&"; "||"; "("; ")"; "true"; "false"
  ]

type oasis_line =
    Line of string * oasis_line list ref

let read_oasis filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    let rec read_line stack ic =
      let line = input_line ic in
      let (indent, comment, line) = find_indent line in
      if comment then
        read_line stack ic
      else
        push_line stack ic indent line

    and push_line stack ic indent line =
      match stack with
          [] -> assert false
        | (current_indent, lines) :: previous_stack ->
            if indent = current_indent then begin
              lines := Line (line, ref []) :: !lines;
              read_line stack ic
            end else
              if indent < current_indent then
                push_line previous_stack ic indent line
              else (* indent > current_indent *)
                match !lines with
                    [] -> assert false
                  | Line (previous_line, new_lines) :: _ ->
                    new_lines := Line (line, ref []) :: !new_lines;
                    let stack = (indent, new_lines) :: stack in
                    read_line stack ic
    in
    read_line [(0, lines)] ic
  with End_of_file ->
    close_in ic;
    !lines

let print_oasis lines =
  let rec print indent lines =
    List.iter (fun line ->
      let Line (s, lines) = line in
      Printf.fprintf stderr "%s%s\n" indent s;
      print (indent ^ "___") !lines
    ) (List.rev lines)
  in
  print "" lines;
  Printf.fprintf stderr "%!"

let merge_line content lines =
  let lines = content :: (List.map (function Line (s, _) -> s) (List.rev !lines)) in
  String.concat " " lines


let split_words s =
  let orig = String.copy s in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\r' | '\n' | '\t' | ';' | ',' ->
      s.[i] <- ' '
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '/' | '-'
    | ' ' | '.'
      ->
      ()
    | _ ->
      Printf.kprintf failwith "Illegal word in %S\n%!" orig
  done;
  OcpString.split_simplify s ' '

let split_words_lowercase s =
  let orig = String.copy s in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\r' | '\n' | '\t' | ';' | ',' ->
      s.[i] <- ' '
    | 'A'..'Z' -> s.[i] <- Char.lowercase s.[i]
    | 'a'..'z' | '0'..'9' | '_' | '/' | '-'
    | ' ' | '.'
      ->
      ()
    | _ ->
      Printf.kprintf failwith "Illegal word in %S\n%!" orig
  done;
  OcpString.split_simplify s ' '

(*
OASISFormat: OASIS format version used to write file _oasis. (mandatory)
Name: Name of the package. (mandatory)
Version: Version of the package. (mandatory)
Synopsis: Short description of the purpose of this package. (mandatory)
Description: Long description of the package purpose.
Authors: Real people that had contributed to the package. (mandatory)
Copyrights: Copyright owners.
Maintainers: Current maintainers of the package.
LicenseFile: File containing the license.
License: DEP-5 license of the package (See DEP-5). (mandatory)
OCamlVersion: Version constraint on OCaml.
FindlibVersion: Version constraint on Finblib.
ConfType: Configuration system.
PreConfCommand: Command to run before configuration.
PostConfCommand: Command to run after configuration.
BuildType: Build system.
PreBuildCommand: Command to run before build.
PostBuildCommand: Command to run after build.
InstallType: Install/uninstall system.
PreInstallCommand: Command to run before install.
PostInstallCommand: Command to run after install.
PreUninstallCommand: Command to run before uninstall.
PostUninstallCommand: Command to run after uninstall.
PreCleanCommand: Command to run before clean.
PostCleanCommand: Command to run after clean.
PreDistcleanCommand: Command to run before distclean.
PostDistcleanCommand: Command to run after distclean.
Homepage: URL of the package homepage.
Categories: URL(s) describing categories of the package.
FilesAB: Files to generate using environment variable substitution.
Plugins: Extra plugins to use.
BuildDepends: Dependencies on findlib packages,
   including internal findlib packages.
BuildTools: Tools required to compile, including internal executables.

Library:
Build: Set if the section should be built.
Install: Set if the section should be distributed.
DataFiles: Comma separated list of files to be installed for run-time.
BuildTools: Tools required to compile, including internal executables.
CSources: C source files.
CCOpt: -ccopt arguments to use when building.
CCLib: -cclib arguments to use when building.
DllLib: -dlllib arguments to use when building.
DllPath: -dllpath arguments to use when building.
ByteOpt: ocamlc arguments to use when building.
NativeOpt: ocamlopt arguments to use when building.

Executable:
Custom: Create custom bytecode executable.

*)

type oasis_package = {
  opk_filename : string;
  opk_archive : string;
  opk_type : BuildOCPTree.package_type;
  opk_dirname : string;
  opk_modules : string list;
  opk_internal_modules : string list;
  opk_build_depends : string list;
  opk_main_is : string list;
  opk_install : bool;
  opk_asm : bool;
  opk_byte : bool;
  opk_findlib_name : string option;
  opk_findlib_parent : string option;
}

type oasis_project = {
  opj_name : string;
  opj_packages : oasis_package list;
  opj_build_depends : string list;
}

let parse_package opk lines =
  Printf.fprintf stderr "parse_package %s\n%!" opk.opk_archive;
  let open Genlex in
  try
    let opk_modules = ref [] in
    let opk_internal_modules = ref [] in
    let opk_build_depends = ref [] in
    let opk_dirname = ref opk.opk_dirname in
    let opk_main_is = ref [] in
    let opk_install = ref true in
    let opk_byte = ref true in
    let opk_asm = ref true in
    let opk_findlib_name = ref None in
    let opk_findlib_parent = ref None in

    List.iter (fun line ->
      let Line (s, lines) = line in
      let (header, content) = OcpString.cut_at s ':' in
      match String.lowercase header with

      | "mainis" ->
        let line = merge_line content lines in
        Printf.eprintf  "[%s] MainIs = %S\n%!" opk.opk_archive line;
        let modules = split_words line in
        List.iter (fun s -> Printf.eprintf "MODULE %S\n" s) modules;
        Printf.eprintf "\n%!";
        opk_main_is := !opk_main_is @ modules

      | "modules" ->
        let line = merge_line content lines in
        Printf.eprintf  "[%s] modules = %S\n%!" opk.opk_archive line;
        let modules = split_words line in
        List.iter (fun s -> Printf.eprintf "MODULE %S\n" s) modules;
        Printf.eprintf "\n%!";
        opk_modules := !opk_modules @ modules

      | "internalmodules" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] internalmodules = %S\n%!" opk.opk_archive line;
        let modules = split_words line in
        opk_internal_modules := !opk_internal_modules @ modules

      | "path" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] path = %S\n%!" opk.opk_archive line;
        begin match split_words line with
          [ subdir ] -> opk_dirname := Filename.concat !opk_dirname subdir
          | _ ->
            failwith "Error 'path'\n%!";
        end

      | "findlibname" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] findlibname = %S\n%!" opk.opk_archive line;
        begin match split_words line with
          [ name ] -> opk_findlib_name := Some name
          | _ ->
            failwith "Error 'findlibname'\n%!";
        end

      | "findlibparent" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] findlibparent = %S\n%!" opk.opk_archive line;
        begin match split_words line with
          [ name ] -> opk_findlib_parent := Some name
          | _ ->
            failwith "Error 'findlibparent'\n%!";
        end

      | "install" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] install = %S\n%!" opk.opk_archive line;
        begin match split_words_lowercase line with
          [ "true" ] -> opk_install := true
          | [ "false" ] -> opk_install := false
          | _ ->
            failwith "Error 'install'\n%!";
        end

      | "compiledobject" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] compiledobject = %S\n%!" opk.opk_archive line;
        begin match split_words_lowercase line with
            [ "best" ] -> ()
          | [ "byte" ] -> opk_asm := false
          | [ "native" ] -> opk_byte := false
          | _ ->
            failwith "Error compiledobject\n%!";
        end

      | "builddepends" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] REQUIRES = %s\n%!" opk.opk_archive line;
        opk_build_depends := !opk_build_depends @ split_words line

      | _ ->
        Printf.eprintf "[%s]Discarding line [%s]\n%!" opk.opk_archive s
    ) (List.rev lines);
    List.iter (fun s -> Printf.eprintf "MODULE1 %S\n" s) !opk_modules;
    let opk = {
      opk with
      opk_modules = !opk_modules;
      opk_internal_modules = !opk_internal_modules;
      opk_build_depends = !opk_build_depends;
      opk_dirname = !opk_dirname;
      opk_main_is = !opk_main_is;
      opk_install = !opk_install;
      opk_byte = !opk_byte;
      opk_asm = !opk_asm;
      opk_findlib_parent = !opk_findlib_parent;
      opk_findlib_name = !opk_findlib_name;

    }
    in
    List.iter (fun s -> Printf.eprintf "MODULE2 %S\n" s) opk.opk_modules;
    Some opk
  with Failure s ->
    Printf.eprintf "Warning: in package %S, error:\n" opk.opk_archive;
    Printf.eprintf "  %s\n%!" s;
    None

let empty_opk = {
  opk_filename = "";
  opk_archive = "";
  opk_dirname = "";
  opk_type = LibraryPackage;
  opk_modules = [];
  opk_internal_modules = [];
  opk_build_depends = [];
  opk_main_is = [];
  opk_install = true;
  opk_byte = true;
  opk_asm = true;
  opk_findlib_name = None;
  opk_findlib_parent = None;
}

let parse_oasis opk_filename lines =
  let open Genlex in
  let empty_opk = {
    empty_opk with
    opk_filename;
    opk_dirname = Filename.dirname opk_filename;
  } in

  let opj_name = ref "" in
  let opj_packages = ref [] in
  let opj_build_depends = ref [] in

  List.iter (fun line ->
    let Line (s, lines) = line in
    try
      let tokens = OcpGenlex.tokens_of_string oasis_lexer s in
      let opk =
        match tokens with
          [ Ident "Name" ; Kwd ":" ; (String name | Ident name) ] ->
          opj_name := name;
          None

(*
      | Ident "BuildDepends"; Kwd ":"; tail ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] REQUIRES = %s\n%!" opk.opk_archive line;
        opk_build_depends := !opk_build_depends @ split_words line
*)


        | [ Kwd "Library"; (String opk_archive | Ident opk_archive) ] ->
          let opk = {
            empty_opk with
            opk_archive;
            opk_type = LibraryPackage;
          } in
          parse_package opk !lines
        | [ Kwd "Executable"; (String opk_archive | Ident opk_archive) ] ->
          let opk = {
            empty_opk with
            opk_archive;
            opk_type = ProgramPackage;
          } in
          parse_package opk !lines

        | _ -> None
      in
      match opk with
        None -> ()
      | Some opk ->
        opj_packages := opk :: !opj_packages
    with _ ->
      Printf.fprintf stderr "Discarding line [%s]\n%!" s
  ) (List.rev lines);

  {
    opj_name = !opj_name;
    opj_packages = !opj_packages;
    opj_build_depends = !opj_build_depends;
  }

open BuildOCPTypes

let load_project pj filename =
  let lines = read_oasis filename in
  print_oasis lines;
  let opj = parse_oasis filename lines in

  let po = empty_env in
  let po = set_bool po "sort" true in

(*  let _local_packages = ref StringMap.empty in *)
(*
  List.iter (fun opk ->
    if not opk.opk_install then
      let name = opk_name opj opk in
      let uniq_name = Printf.eprintf
  ) opj.opj_packages;
*)

  List.iter (fun opk ->
    Printf.eprintf "opk_archive = %S\n%!" opk.opk_archive;

    if opk.opk_modules <> [] || opk.opk_internal_modules <> [] ||
       opk.opk_main_is <> [] then

      let name =
        if opk.opk_install then
          match opk.opk_findlib_parent, opk.opk_findlib_name with
            None, None -> opk.opk_archive
          | None, Some name -> name
          | Some parent, Some name -> Printf.sprintf "%s.%s" parent name
          | Some _, None -> assert false
        else
          match opk.opk_type with
          | LibraryPackage ->
            Printf.sprintf "%s-library-%s" opj.opj_name opk.opk_archive
          | ProgramPackage ->
            Printf.sprintf "%s-program-%s" opj.opj_name opk.opk_archive
          | TestPackage -> assert false
          | ObjectsPackage -> assert false
          | SyntaxPackage -> assert false
          | RulesPackage -> assert false
      in
      Printf.eprintf "  name = %S\n%!" name;

      let pk = BuildOCPInterp.new_package pj name opk.opk_dirname
          opk.opk_filename [opk.opk_filename, None (* matters only for non-installed packages *)
                           ] opk.opk_type po in
      pk.package_source_kind <- "oasis";
      List.iter (fun s ->
        let ( dep :  'a package_dependency) =
          BuildOCPInterp.new_package_dep pk s empty_env in
        dep.dep_link <- true
      ) opk.opk_build_depends;
      List.iter (fun s ->
        let ( dep :  'a package_dependency) =
          BuildOCPInterp.new_package_dep pk s empty_env in
        dep.dep_link <- true
      ) opj.opj_build_depends;

(* TODO
      let external_options = [] in
      let internal_options = set
        (OptionBoolSet ("install", false)) ::
          external_options in
      pk.package_raw_files <-
        List.map (fun s -> (s, external_options)) opk.opk_modules @
          List.map (fun s -> (s, internal_options)) opk.opk_internal_modules @
          List.map (fun s -> (s, external_options)) opk.opk_main_is;
      assert false;
      let po = StringMap.add "install" (OptionBool opk.opk_install) po in
      let po = StringMap.add "has_byte" (OptionBool opk.opk_byte) po in
      let po = StringMap.add "has_asm" (OptionBool opk.opk_asm) po in
      let po = StringMap.add "archive" (OptionList [ opk.opk_archive]) po in
*)

  ) opj.opj_packages;
  0



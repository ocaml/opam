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

(* ocp-build install [OPTIONS]

  Set the options of the user preference file.

*)

(* open BuildBase *)
open BuildArgs
open BuildOptions
open BuildGlobals
open BuildActions
open BuildTypes
open BuildOCPTree

let arg_list =
  BuildOptions.merge
    [
      [

      ];
      BuildActionBuild.arg_list
    ]



let do_test b ncores projects =
  time_step "Executing tests";
  let stats = BuildOCamlTest.init () in
  List.iter (fun lib ->
    match lib.lib_type with
    | ProgramPackage
    | TestPackage ->
      BuildOCamlTest.test_package b stats lib !benchmarks_arg
    | LibraryPackage
    | ObjectsPackage
    | SyntaxPackage
    | RulesPackage
      -> ()
  ) projects;
  BuildOCamlTest.finish stats ncores;
  time_step "   Done executing tests"


let action () =
  let p = BuildActions.load_project () in
  let (bc, projects) = BuildActionBuild.do_build p in
  do_test bc.build_context (BuildActionBuild.get_ncores p.cin) projects;

  ()

let subcommand = {
  sub_name = "tests";
  sub_help =  "Run project tests.";
  sub_arg_list = arg_list;
  sub_arg_anon = Some arg_anon;
  sub_arg_usage = [ "Run project tests."; ];
  sub_action = action;
}


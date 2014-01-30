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

(* TODO: we should save the version of ocaml used to build a project,
   so that we can detect changes and ask for a clean before building.
   Can we access the magic used by every compiler ? (we can compile an
   empty file in bytecode and native code) We could cache this
   information using the uniq identifier of the executable (would not
   work with wrappers).
*)

(* TODO
   We could force packages with missing dependencies to still be compiaboutled,
   since it is still possible that these missing dependencies arbue not used
   in a particular compilation scheme.
*)


(* open BuildBase *)
(* open Stdlib2 *)
open SimpleConfig

open BuildActions
open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions
open BuildArgs
open BuildActionBuild

let _ = DebugVerbosity.add_submodules "B" [ "BuildMain" ]


let finally () =
  List.iter (fun action -> action ()) !finally_do;
  time_step "End of execution";

  if !time_arg then begin
    Printf.printf "Time schedule:\n";
    List.iter (fun (msg, t1) ->
      Printf.printf "  %.2fs\t%s\n%!" (t1 -. t0) msg
    ) (List.rev !time_steps);
    Printf.printf "%!";
  end;
  ()

let sub_map = ref StringMap.empty

let subcommands =  [
    BuildActionRoot.subcommand;
    BuildActionConfigure.subcommand;
    BuildActionBuild.subcommand;
    BuildActionTests.subcommand;
    BuildActionInstall.subcommand;
    BuildActionUninstall.subcommand;
    BuildActionClean.subcommand;
    BuildActionBuild.old_subcommand;

    BuildActionPrefs.subcommand;
    BuildActionQuery.subcommand;
    BuildActionHelp.subcommand;
  ]

(* The default command is the 'build' one *)
let default_subcommand = {
  BuildActionBuild.subcommand with
  sub_name = "[SUBCOMMAND]";
  sub_arg_usage = [
    "Build command for OCaml projects";
    "";
    "Available subcommands are:";
  ] @ List.map (fun s ->
                    Printf.sprintf "   %-15s %s" s.sub_name s.sub_help;
                  ) subcommands;
}

let make_arg_usage s =
  String.concat "\n"
    (
      [ "Command:";
        Printf.sprintf "  ocp-build %s [OPTIONS] %s"
          s.sub_name (match s.sub_arg_anon with
          None -> "" | Some _ -> "[ARGS]");
        "";
      ] @
      s.sub_arg_usage @ [ ""; "Available options in this mode:";])

let _ =
  List.iter (fun s ->
    sub_map := StringMap.add s.sub_name s !sub_map
  ) subcommands;

  Printexc.record_backtrace true;

  begin match initial_verbosity with None -> () | Some v ->
    DebugVerbosity.increase_verbosity "B"  v end;

  let s =
    try
      if Array.length Sys.argv < 2 then raise Not_found;
      let arg1 = Sys.argv.(1) in
      let s = StringMap.find arg1 !sub_map in
      Sys.argv.(1) <- Sys.argv.(0) ^ " " ^ arg1;
      incr Arg.current;
      s
    with Not_found ->
      Sys.argv.(0) <- Sys.argv.(0) ^ " build";
      default_subcommand
  in
  let arg_list = arg_align s.sub_arg_list in
  let arg_usage = make_arg_usage s in
  try
    Arg.parse arg_list (match s.sub_arg_anon with
        None -> arg_anon_none
      | Some f -> f) arg_usage;
    s.sub_action ();
    finally ()
  with
(*  | PrintShortArgList ->
    Arg.usage short_arg_list arg_usage; exit 0
  | PrintLongArgList ->
    Arg.usage arg_list arg_usage; exit 0 *)
  | BuildMisc.ExitStatus n ->
    finally ();
    Pervasives.exit n
  | e ->
    let backtrace = Printexc.get_backtrace () in
    Printf.fprintf stderr "ocp-build: Fatal Exception %s\n%s\n%!"
      (Printexc.to_string e) backtrace;
    raise e

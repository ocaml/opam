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

(* open OcpLang *)


(* open BuildBase *)
(* open Stdlib2 *)
open BuildEngineTypes
open BuildEngineGlobals

let verbose =
  DebugVerbosity.verbose [ "B" ] "BuildRules"

(* Rule Misc Functions *)

let new_rule b loc main_target commands =
  let r = {
    rule_id = new_rule_id b;
    rule_main_target = main_target;
    rule_commands = commands;
    rule_loc = (* lib.lib_loc*) loc;
    rule_forced = false;
    rule_sources = IntMap.empty;
    rule_time_dependencies = IntMap.empty;
    rule_temporaries = [];
    rule_targets = IntMap.empty;
    rule_missing_sources = 0;
    rule_state = RULE_INACTIVE;

    rule_context = b;
  } in
  Hashtbl.add b.build_rules r.rule_id r;
  main_target.file_target_of <- r :: main_target.file_target_of;
  r.rule_targets <- IntMap.add main_target.file_id main_target r.rule_targets;
  r

let add_rule_source r file =
  if not (IntMap.mem file.file_id r.rule_sources) then begin
    r.rule_sources <- IntMap.add file.file_id file r.rule_sources;
    file.file_source_for <- r :: file.file_source_for
  end

let add_rule_time_dependency r file =
  if not (IntMap.mem file.file_id r.rule_time_dependencies) then begin
    r.rule_time_dependencies <- IntMap.add file.file_id file r.rule_time_dependencies;
    file.file_source_for <- r :: file.file_source_for
  end

let add_rule_sources r files =
  List.iter (add_rule_source r) files

let add_rule_target r file =
  if not (IntMap.mem file.file_id r.rule_targets) then begin
    r.rule_targets <- IntMap.add file.file_id file r.rule_targets;
    if verbose 4 && file.file_target_of <> [] then
    Printf.eprintf "Warning: file %s targetted by multiple rules\n" (file_filename file);
    file.file_target_of <- r :: file.file_target_of
  end

let add_rule_targets r files =
  List.iter (add_rule_target r) files

let add_rule_temporary r file =
  match file.file_kind with
      FILE_TEMPORARY ->
	r.rule_temporaries <- file :: r.rule_temporaries
    | FILE_VIRTUAL | FILE_REAL ->
      Printf.ksprintf failwith "Temporary file %s is also real" (file_filename file)

let add_rule_command r cmd =
  r.rule_commands <- r.rule_commands @ [cmd]

let add_rule_commands r cmds =
  r.rule_commands <- r.rule_commands @ cmds

let add_rule_temporaries r files =
  List.iter (add_rule_temporary r) files

(* Commands Misc Functions *)

let new_command cmd args = {
  cmd_command = cmd;
  cmd_args = args;
  cmd_stdin_pipe = None;
  cmd_stdout_pipe = None;
  cmd_stderr_pipe = None;
  cmd_move_to_dir = None;
}

let string_of_argument arg =
  match arg with
      S s -> BuildSubst.subst_global s
    | T s -> "${temp}/" ^ BuildSubst.subst_global s
    | F f -> File.to_string f
    | BF f -> File.to_string f.file_file
    | BD d -> d.dir_fullname

let rule_temp_dir r =
  File.add_basename r.rule_context.build_dir (string_of_int r.rule_id)

let file_of_argument r arg =
  match arg with
      S s -> File.of_string (BuildSubst.subst_global s)
    | T s -> File.add_basename (rule_temp_dir r) (BuildSubst.subst_global s)
    | F f -> f
    | BF f -> f.file_file
    | BD d -> d.dir_file

let argument_of_argument r arg =
  match arg with
      S s -> BuildSubst.subst_global s
    | T s -> File.to_string (
                 File.add_basename (rule_temp_dir r) (BuildSubst.subst_global s))
    | F f -> File.to_string f
    | BF f -> File.to_string f.file_file
    | BD d -> d.dir_fullname


let command_of_command cmd =
  List.map BuildSubst.subst_global cmd.cmd_command

let argument_of_string s = S s

let add_command_string cmd arg =
  cmd.cmd_args <- cmd.cmd_args @ [S arg]

let add_command_arg cmd arg =
  cmd.cmd_args <- cmd.cmd_args @ [arg]

let add_command_strings cmd args =
  cmd.cmd_args <- cmd.cmd_args @ (List.map argument_of_string args)

let add_command_args cmd args =
  cmd.cmd_args <- cmd.cmd_args @ args

let add_command_pipe cmd filename =
  cmd.cmd_stdout_pipe <- Some filename


let print_indented_command cmd =
  match cmd with
  | Execute cmd ->
    begin match cmd.cmd_move_to_dir with
      None -> ()
      | Some chdir ->
        Printf.eprintf "\tcd %S\n" chdir;
    end;
	Printf.eprintf "\t%s %s"  (String.concat " " cmd.cmd_command) (String.concat " " (List.map string_of_argument cmd.cmd_args));
	begin
	  match cmd.cmd_stdout_pipe with
	      None -> Printf.eprintf "\n"
	    | Some filename ->
	      Printf.eprintf " > %s\n" filename
	end
    | LoadDeps (_, file, r) -> Printf.eprintf "\tLoad dependencies from %s for %d\n"
      (file_filename file) r.rule_id
    | Copy (f1, f2) ->
      Printf.eprintf "\tCopy %s to %s\n" (string_of_argument f1) (string_of_argument f2)
    | Move (f1, f2) ->
      Printf.eprintf "\tRename %s to %s\n" (string_of_argument f1) (string_of_argument f2)
  | MoveIfExists (f1, f2, f3) ->
    if verbose 4 then
      Printf.eprintf "\tRename? %s to %s\n" (string_of_argument f1) (string_of_argument f2)
    | DynamicAction (s,_) ->
      Printf.eprintf "\tDynamicAction %s\n" s
    | NeedTempDir ->
      Printf.eprintf "\tNeedTempDir\n"
    | Function (name, _, _) ->
      Printf.eprintf "\tFunction %s\n" name

let string_of_rule_state r =
match r.rule_state with
	RULE_INACTIVE -> "inactive"
      | RULE_ACTIVE -> "active"
      | RULE_WAITING -> "waiting"
      | RULE_EXECUTING -> "executing"
      | RULE_EXECUTED -> "executed"

let print_rule r =
  Printf.eprintf "RULE %d (state %s)" r.rule_id
    (match r.rule_state with
	RULE_INACTIVE -> "inactive"
      | RULE_ACTIVE -> "active"
      | RULE_WAITING -> "waiting"
      | RULE_EXECUTING -> "executing"
      | RULE_EXECUTED -> "executed");

  if r.rule_missing_sources <> 0 then Printf.eprintf "(%d missing)" r.rule_missing_sources;
  Printf.eprintf "\n";
  IntMap.iter (fun _ file ->
    Printf.eprintf "\t\tSOURCE %s%s\n" (file_filename file)
      (if file.file_exists then "(exists)" else "(not available)")
  ) r.rule_sources;
  List.iter print_indented_command r.rule_commands;
  IntMap.iter (fun _ file ->
    Printf.eprintf "\t\tTARGET %s\n" (file_filename file)
  ) r.rule_targets;
  ()


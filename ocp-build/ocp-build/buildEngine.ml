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

(* TODO: we should check the results of rules, so that we can make
   sure that the two rules building the same temporary files are not
   executed concurrently. *)

(* open OcpLang *)


(* open BuildBase *)
(* open Stdlib2 *)
open BuildEngineTypes
open BuildEngineGlobals

let verbose =
  DebugVerbosity.add_submodules "B" [ "BE" ];
  DebugVerbosity.verbose [ "BE" ] "BuildEngine"

let sigint_received = ref false
let _ =
  BuildMisc.at_sigint "BuildEngine" (fun _ ->
    Printf.eprintf "BuildEngine: sigint received\n%!";
    sigint_received := true)


let exit2 () = BuildMisc.clean_exit 2

(*
(* open BuildGlobals *)

(* refactor: replace "open X" by "let v = X.v, etc." *)
let verbosity_arg = BuildGlobals.verbosity_arg
let stop_on_error_arg = BuildGlobals.stop_on_error_arg
let cross_arg = BuildGlobals.cross_arg

let build_rules = BuildGlobals.build_rules
let build_files = BuildGlobals.build_files

let build_dir_basename = BuildGlobals.build_dir_basename
let build_dir_filename = BuildGlobals.build_dir_filename

let new_id_generator = BuildGlobals.new_id_generator (* should be in misc: new_counter_from_0 *)
*)

(* TODO: what about files that are in the distribution. We actually have
dependencies towards them, so we should take care of them ! *)

(* TODO: we should have a -cmi option to ocamlc/ocamlopt, to generate
   the .cmi file and stop. That could be useful to completely typecheck a
   project, without actually compiling it. *)

exception MissingSourceWithNoBuildingRule of build_rule * string

(*
let queue_inactive = ref ([] : build_rule list)
let queue_ready = ref (IntMap.empty : build_rule IntMap.t)
let queue_waiting = ref (IntMap.empty : build_rule IntMap.t)
let queue_not_waiting = ref (IntMap.empty : build_rule IntMap.t)
*)


let rule_need_execution =
  let cmdbuf = Buffer.create 10000 in
  fun b r ->
  if r.rule_forced then true else
    let missing_target = ref false in
    Buffer.clear cmdbuf;
    (*    let oldest_target = ref max_float in *)
    let targets = ref [] in
    IntMap.iter (fun _ f ->
      targets := file_filename f :: !targets;
      if f.file_mtime = BuildEngineMtime.zero then
	missing_target := true
        (*      else
	        oldest_target := min !oldest_target f.file_mtime *)
    ) r.rule_targets;
    let targets = List.sort compare !targets in
    List.iter (fun target ->
      Printf.bprintf cmdbuf "#target:%s\n" target;
    ) targets;

    let missing_sources = ref 0 in
    (*    let newest_source = ref (-1.0) in *)
    IntMap.iter (fun _ f ->
      if f.file_exists then begin
	Printf.bprintf cmdbuf "#source:%s %s\n" (file_filename f) (BuildEngineMtime.to_string f.file_mtime);
        (*	newest_source := max !newest_source f.file_mtime *)
      end else begin
	incr missing_sources;
	match f.file_target_of with
	  [] -> assert false; (* raise (MissingSourceWithNoBuildingRule (r, file_filename f)) *)
	| _ -> ()
      end
    ) r.rule_sources;

    let command_need_execution =
      !missing_target || !missing_sources > 0 (* || !oldest_target < !newest_source *)
    in
    (* Generates the commands to be executed, in order to compare with the
       one stored in the cache. If the command needs to be executed, the old
       command is removed from the cache, and the new command is put in a
       waiting slot, and will be copied in the cache if the command succeeds.
    *)
    let rec iter cmdbuf commands =
      match commands with
	[] -> Buffer.contents cmdbuf
      | cmd :: commands ->
	let commands =
	  match cmd with
	  | Execute cmd ->
	    Printf.bprintf cmdbuf "#command: '%s' '%s'\n"
	      (String.concat "' '" (BuildEngineRules.command_of_command cmd))
	      (String.concat "' '" (List.map BuildEngineRules.string_of_argument cmd.cmd_args));
	    commands
	  | Move (f1, f2) ->
	    Printf.bprintf cmdbuf "#move: %s %s\n"
	      (BuildEngineRules.string_of_argument f1) (BuildEngineRules.string_of_argument f2);
	    commands
	  | Copy (f1, f2) ->
	    Printf.bprintf cmdbuf "#copy: %s %s\n"
	      (BuildEngineRules.string_of_argument f1) (BuildEngineRules.string_of_argument f2);
	    commands
	  | MoveIfExists _ ->
            (* MoveIfExists() is only for non-targets ! Thus, it is not taken into account
               in the compilation results.
               TODO: we need to think more that. Maybe there are cases where we just want
               to rebuild to get compilation information ?

	       Printf.bprintf cmdbuf "#move? %s %s\n"
	       (File.to_string f1) (File.to_string f2);

            *)
	    commands
	  | LoadDeps ( _, file, _) ->
	    Printf.bprintf cmdbuf "#loaddeps %s\n"
	      (file_filename file); commands
          | NeedTempDir -> commands
	  | DynamicAction (msg, f) ->
	    let actions =
	      try
		Lazy.force f
	      with e ->
         (* TODO: this should be some error *)
		Printf.eprintf "Error: exception %s with DynamicAction %s\n%!"
		  (Printexc.to_string e) msg ;
		BuildMisc.clean_exit 2
	    in
	    actions @ commands
          | Function (name, printer, actor) ->
            printer cmdbuf;
            commands
	in
	iter cmdbuf commands
    in
    let cmd = iter cmdbuf  r.rule_commands in
    let main_target_digest = Digest.string (file_filename r.rule_main_target) in
    let command_digest = Digest.string cmd in
    Printf.fprintf b.build_cache_log "#RULE %d : %s -> %s if OK\n%s"
      r.rule_id
      (OcpDigest.to_hex main_target_digest)
      (OcpDigest.to_hex command_digest) cmd;
    let command_need_execution =
      try
	let old_digest = DigestMap.find main_target_digest b.build_cache_input
	in
	Printf.fprintf b.build_cache_log "#CACHE was %s\n"
	  (OcpDigest.to_hex old_digest);
	if command_need_execution then begin
	  Printf.fprintf b.build_cache_log
	    "#COMMAND ALREADY NEEDS EXECUTION\n";
	  command_need_execution
	end else
	if old_digest = command_digest then begin
	  Printf.fprintf b.build_cache_log "#NO NEED FOR EXECUTION CONFIRMED\n";
	  command_need_execution
	end else begin
	  Printf.fprintf b.build_cache_log "#EXECUTION WOULD BE NEEDED\n";
	  true
	end
      with Not_found -> (* command_need_execution *)
	Printf.fprintf b.build_cache_log "#NO CACHE: must be executed\n";
	true
    in
    if command_need_execution then begin
      (* if command needs execution, we should prepare to save the
	 command digest in case of success *)
      b.build_cache_entries <- IntMap.add r.rule_id
	  (main_target_digest, command_digest) b.build_cache_entries;
      (* if command needs execution, we can get rid
	 of the former command used *)
      b.build_cache_input <- DigestMap.remove main_target_digest
	  b.build_cache_input;
    end;
    assert (!missing_sources = 0);
    command_need_execution

let experimental =
  try
    ignore (Sys.getenv "OCPBUILD_SAFE_MODE");
    false
  with Not_found -> true

let init b targets =
  if verbose 5 then
    Printf.eprintf "BuildEngine.init, phase 1: init\n";
  (* Phase 1: clean everything *)
  (* reset and initialize *)
  Hashtbl.iter (fun _ r ->
    r.rule_missing_sources <- 0;
    r.rule_state <- RULE_INACTIVE;
  ) b.build_rules;

  (* Phase 2: check existence and modification times *)
  if verbose 5 then
    Printf.eprintf "BuildEngine.init, phase 2: loading times\n";

  Hashtbl.iter (fun _ f ->

    if verbose 7 then Printf.eprintf "Filename %s " (file_filename f);
    begin
      begin
	match f.file_kind with
	    FILE_VIRTUAL ->
	      if verbose 7 then Printf.eprintf " virtual";
	      f.file_exists <- false;
	      f.file_mtime <- BuildEngineMtime.zero;
	  | FILE_TEMPORARY ->
	    if verbose 7 then Printf.eprintf " temp";
	  | FILE_REAL ->
	    try
              let filename = file_filename f in
	      if verbose 7 then Printf.eprintf " exists";
	      f.file_exists <- true;
	      f.file_mtime <- BuildEngineMtime.compute filename
	    with _ ->
	      if verbose 7 then Printf.eprintf " does not exist";
	      f.file_exists <- false;
	      f.file_mtime <- BuildEngineMtime.zero
      end
    end;
      if  verbose 7 then begin
	begin
	  match f.file_target_of with
	      [] -> Printf.eprintf "(source)"
	    | _ -> Printf.eprintf "(to build [%d rules])" (List.length f.file_target_of)
	end;
	Printf.eprintf "\n%!"
      end;
    ) b.build_files;

(* Phase 3: activate only needed rules to build the targets *)
  (* Keep only rules that are useful to build the current targets *)

  let delayed_rules = ref [] in
  let rec activate_rule r =
    match r.rule_state with
	RULE_INACTIVE ->
	  if verbose 7 then begin
	    Printf.eprintf "ACTIVATING RULE\n%!";
	    BuildEngineRules.print_rule r;
	  end;
	  r.rule_state <- RULE_ACTIVE;
	  if verbose 9 then
	    Printf.eprintf "rule %d <- STATE ACTIVE\n" r.rule_id;
	  IntMap.iter (fun _ f ->
        if not f.file_exists && f.file_target_of = [] && experimental then
          raise (MissingSourceWithNoBuildingRule (r, file_filename f));
        activate_source f) r.rule_sources
      | RULE_ACTIVE -> () (* already active *)
      | _ -> assert false

  and activate_source f =
    if verbose 9 then
      Printf.eprintf "activate_source [%s]\n" (file_filename f);
    match f.file_target_of with
	[] ->
      if verbose 9 then
	Printf.eprintf "no rule to build target [%s]\n" (file_filename f);
      | [r] -> activate_rule r
      | _ -> delayed_rules := f.file_target_of :: !delayed_rules
  in
  if verbose 5 then
    Printf.eprintf "BuildEngine.init, phase 3: activating rules\n";

  List.iter (fun f ->
    if verbose 5 then
      Printf.eprintf "BuildEngine.init: activate main target %S\n"
        (file_filename f);
    activate_source f) targets;
  if verbose 3 then
    Printf.eprintf "%d targets waiting for active rule\n" (List.length !delayed_rules);

  let rec iter_delayed_rules () =
    match !delayed_rules with
	[] -> ()
      | rules :: tail ->
	delayed_rules := tail;
	if List.for_all (fun r -> r.rule_state = RULE_INACTIVE) rules then begin
	  match rules with
	      []  -> assert false
	    | r :: _ ->

	      (* TODO: what shall we do ???? We have several rules
		 leading to the same file, and none of them has been
		 activated yet. Usually, it corresponds to rules
		 generating source files that are used in different
		 projects. Since source files are generated in the
		 source directory, there is no easy way to detect
		 early that all these rules are similar. Either we
		 activate all of them, or we only activate one.  *)
	      if verbose 5 then begin
		Printf.eprintf "Picking rule %d among other ones\n" r.rule_id;
		Printf.eprintf "All rules:";
		List.iter (fun r -> BuildEngineRules.print_rule r) rules;
		Printf.eprintf "\n";
	      end;
	      activate_rule r
	end;
	iter_delayed_rules ()
  in
  iter_delayed_rules ();
  if verbose 5 then
    Printf.eprintf "BuildEngine.init, phase 3: activating delayed rules\n";

  (* Phase 4: update the readiness of all active rules *)

  if verbose 7 then Printf.eprintf "Rules activated\n%!";
  (* A rule needs to be replayed if one of the targets is either non existing
     or older than one of the sources. Even if it exists, a file might be set as
     non existing if it has to be rebuilt.

     We should save the Digests of all files, so that we might not call a
     command if we know that all sources have the same digests as before,
     so we only need to update the timestamps.*)

  let rec check_rule r =
    if match r.rule_state with
	 RULE_ACTIVE | RULE_WAITING -> true
      | _ -> false
	then  (* r.rule_active *)
(*    if not r.rule_waiting then *)
      let missing_target = ref false in
(*      let oldest_target = ref max_float in *)
      IntMap.iter (fun _ f ->
	if f.file_exists then begin
(*	  oldest_target := min !oldest_target f.file_mtime *)
          ()
	end else
	  missing_target := true
      ) r.rule_targets;

      let missing_sources = ref 0 in
(*      let newest_source = ref (-1.0) in *)
      IntMap.iter (fun _ f ->
	if f.file_exists then begin
(*	  newest_source := max !newest_source f.file_mtime *)
	end else begin
	  incr missing_sources;
	  if verbose 7 then Printf.eprintf "Missing %s\n%!" (file_filename f);
	  match f.file_target_of with
	      [] ->
       if not experimental then
         raise (MissingSourceWithNoBuildingRule (r, file_filename f))
	    | _ -> ()
	end
      ) r.rule_sources;

      IntMap.iter (fun _ f ->
	if f.file_exists then
	(* Time dependencies do not introduce rebuilding rules *)
(*	  newest_source := max !newest_source f.file_mtime *)
	  ()
	else
	  let active_rule = ref false in
	  List.iter (fun r ->
	    if (* r.rule_active *) r.rule_state <> RULE_INACTIVE then begin
	      active_rule := true;
	      incr missing_sources;
	      if verbose 7 then Printf.eprintf "Waiting for %s\n%!" (file_filename f);
	    end) f.file_target_of;
(*	  if not !active_rule then
	    raise (MissingSourceWithNoBuildingRule (r, file_filename f)) *)
      ) r.rule_time_dependencies;

      r.rule_missing_sources <- !missing_sources;
      if verbose 7 then Printf.eprintf "Initializing rule %d missing sources to %d\n%!" r.rule_id r.rule_missing_sources;
      if r.rule_state <> RULE_WAITING &&
	(!missing_target ||
	  !missing_sources > 0 || true
         (* || !oldest_target < !newest_source *) )
      then begin
	if verbose 9 then
	  Printf.eprintf "rule %d <- STATE WAITING\n" r.rule_id;
	r.rule_state  <- RULE_WAITING;
	if verbose 7 then Printf.eprintf "Killing targets from rule %d\n%!" r.rule_id;

	IntMap.iter (fun _ file ->  kill_target file) r.rule_targets
      end

  (* We have a problem with .cmi files: They are actually modified by
     both ocamlc and ocamlopt from the same file. To solve this
     problem, we should never put a dependency on a .cmi file if there
     is a corresponding .cmo or .cmx file that should carry the
     dependency instead.  *)

  (* [kill_target file] if [file] already exists at the beginning of the
     build process, it should be recomputed. *)
  and kill_target f =
    if f.file_exists then begin
      if verbose 7 then Printf.eprintf "Killing target %s\n%!" (file_filename f);
      f.file_exists <- false;
      List.iter kill_rule f.file_target_of;
      List.iter (fun r ->
	if r.rule_state <> RULE_INACTIVE then begin
   r.rule_missing_sources <- r.rule_missing_sources + 1;
   (*	Printf.eprintf "Killed %s =>\n%!" (file_filename f); *)
   if verbose 7 then
	Printf.eprintf "Setting rule %d missing sources to %d\n%!" r.rule_id r.rule_missing_sources;
 end;
	kill_rule r
      ) f.file_source_for;
    end

  and kill_rule r =
    if r.rule_state = RULE_ACTIVE then begin
      if verbose 7 then Printf.eprintf "Killing rule %d\n%!" r.rule_id;
      if verbose 9 then
	Printf.eprintf "rule %d <- STATE WAITING\n" r.rule_id;
      r.rule_state <- RULE_WAITING;
      IntMap.iter (fun _ file -> kill_target file) r.rule_targets
    end
  in
  if verbose 5 then
    Printf.eprintf "BuildEngine.init, phase 4: checking active rules\n";
  Hashtbl.iter (fun _ r ->
     check_rule r) b.build_rules;

(* Phase 5: update the different queues *)
(* Now, fill the queues ! *)

  if verbose 5 then
    Printf.eprintf "BuildEngine.init, phase 5: filling queues\n";

  Hashtbl.iter (fun _ r ->
    match r.rule_state with
	RULE_ACTIVE ->
	  b.queue_not_waiting <- IntMap.add r.rule_id r b.queue_not_waiting;
      | RULE_WAITING ->
	if r.rule_missing_sources > 0 then
	  b.queue_waiting <- IntMap.add r.rule_id r b.queue_waiting
	else
	  b.queue_ready <- IntMap.add r.rule_id r b.queue_ready
      | RULE_INACTIVE ->
	b.queue_inactive <- r :: b.queue_inactive
      | RULE_EXECUTING
      | RULE_EXECUTED -> assert false
  ) b.build_rules;

  b.build_stats_to_execute <-
    IntMap.cardinal b.queue_ready +
    IntMap.cardinal b.queue_waiting;

  if verbose 5 then
    Printf.eprintf "BuildEngine.init, phase 5: done\n";
  ()


let print_waiting_queue b max_print =
  let max_print = ref max_print in
  try
    Printf.eprintf "WAITING QUEUES:\n";
    begin
      IntMap.iter (fun _ r ->
        Printf.eprintf "Rule %d ready\n%!" r.rule_id;
        IntMap.iter (fun _ file ->
	  Printf.eprintf "\t\tTARGET %s\n%!" (file_filename file))
	  r.rule_targets;
        List.iter BuildEngineRules.print_indented_command r.rule_commands;
        IntMap.iter (fun _ f ->
	  if not f.file_exists then
	    Printf.eprintf "\t\tSOURCE %s missing\n%!" (file_filename f)
	  else
	    Printf.eprintf "\t\tSOURCE %s done\n%!" (file_filename f)
        ) r.rule_sources;
        decr max_print; if !max_print <= 0 then raise Exit;
      ) b.queue_ready
    end;
    begin
      IntMap.iter (fun _ r ->
        Printf.eprintf "Rule %d waiting for %d sources\n%!" r.rule_id r.rule_missing_sources;
        IntMap.iter (fun _ file ->
	  Printf.eprintf "\t\tTARGET %s\n%!" (file_filename file))
	  r.rule_targets;
        List.iter BuildEngineRules.print_indented_command r.rule_commands;
        IntMap.iter (fun _ f ->
	  if not f.file_exists then
	    Printf.eprintf "\t\tSOURCE %s missing\n%!" (file_filename f)
	  else
	    Printf.eprintf "\t\tSOURCE %s ok\n%!" (file_filename f)
        ) r.rule_sources;
        decr max_print; if !max_print <= 0 then raise Exit;
      ) b.queue_waiting
    end;
    begin
      List.iter (fun r ->
        Printf.eprintf "Rule %d inactive\n%!" r.rule_id;
        IntMap.iter (fun _ file ->
	  Printf.eprintf "\tTARGET %s\n%!" (file_filename file))
	  r.rule_targets;
        List.iter BuildEngineRules.print_indented_command r.rule_commands;
        IntMap.iter (fun _ f ->
	  if not f.file_exists then
	    Printf.eprintf "\t\t%s missing\n%!" (file_filename f)
	  else
	    Printf.eprintf "\t\t%s done\n%!" (file_filename f)
        ) r.rule_sources;
        decr max_print; if !max_print <= 0 then raise Exit;
      ) b.queue_inactive
    end
  with Exit -> ()

(* Beware not to use add_temp_file on a file that has already been
   initialized as a target, it would cause an assertion failure. *)

let check_temporary b r file =
  try
    let (r',list) = IntMap.find file.file_id b.temp_files in
    if r.rule_missing_sources = 0 && verbose 3 then
      Printf.eprintf "rule %d postponed to avoid conflict with rule %d\n" r.rule_id r'.rule_id;
    list := r :: !list;
    r.rule_missing_sources <- r.rule_missing_sources + 1;
  with Not_found -> ()

let lock_temporary b r file =
  if IntMap.mem file.file_id b.temp_files then begin
    Printf.eprintf "Error in lock_temporary: file %S is already locked\n%!"
      (File.to_string file.file_file);
    BuildMisc.clean_exit 2
  end;
(*    Printf.eprintf "LOCKING FILE %d %S\n%!" file.file_id
      (File.to_string file.file_file);
*)
  b.temp_files <- IntMap.add file.file_id (r, ref []) b.temp_files

let check_temporaries b r =
(*  Printf.eprintf "check_temporaries %d\n%!" r.rule_id; *)
  if (r.rule_missing_sources <> 0) then begin
    Printf.eprintf "check_temporaries: missing_sources = %d\n" r.rule_missing_sources;
    BuildMisc.clean_exit 2
  end;
  let check_temporary = check_temporary b r in
  List.iter check_temporary r.rule_temporaries;
  IntMap.iter (fun _ file -> check_temporary file) r.rule_targets;
  if r.rule_missing_sources > 0 then begin
    b.queue_waiting <- IntMap.add r.rule_id r b.queue_waiting;
    true
  end else begin
    let lock_temporary = lock_temporary b r in
(*    Printf.eprintf "lock_temporaries\n%!"; *)
    List.iter lock_temporary r.rule_temporaries;
(*    Printf.eprintf "lock_targets\n%!"; *)
    IntMap.iter (fun _ file -> lock_temporary file) r.rule_targets;
    false
  end

let release_temporaries b r =
  let release_temporary file =
(*    Printf.eprintf "UNLOCKING FILE %d %S\n%!" file.file_id
      (File.to_string file.file_file); *)
    let _, rules = IntMap.find file.file_id b.temp_files in
    b.temp_files <- IntMap.remove file.file_id b.temp_files;
    List.iter (fun r ->

      if verbose 5 then
	Printf.eprintf "\t\t\trule %d: missing %d -> %d (release_temporary)\n" r.rule_id r.rule_missing_sources (r.rule_missing_sources - 1);
      r.rule_missing_sources <- r.rule_missing_sources - 1;
      assert (r.rule_missing_sources >= 0);
      if r.rule_missing_sources = 0 then begin
	b.queue_waiting <- IntMap.remove r.rule_id b.queue_waiting;
	b.queue_ready <- IntMap.add r.rule_id r b.queue_ready
      end
    ) !rules
  in
  List.iter release_temporary r.rule_temporaries;
  IntMap.iter (fun _ file -> release_temporary file) r.rule_targets;
  ()


let rec next_rule b =
  if verbose 7 then Printf.eprintf "next_rule: %d targets ready\n%!" (IntMap.cardinal b.queue_ready);
  match IntMap.min_elt b.queue_ready with
      None ->
	None
    | Some (id, r) ->
      if verbose 7 then Printf.eprintf "next_rule: testing rule %d\n%!" r.rule_id;
      b.queue_ready <- IntMap.remove id b.queue_ready;
      b.build_stats_executed <- b.build_stats_executed + 1;
      if r.rule_missing_sources > 0 || r.rule_state = RULE_EXECUTED || check_temporaries b r then
	next_rule b
      else begin
      if verbose 7 then
	begin
	  Printf.eprintf "NEXT RULE\n%!";
	  BuildEngineRules.print_rule r;
	  IntMap.iter (fun _ f ->
	    if not f.file_exists then
	      Printf.eprintf "ERROR: missing source %s\n%!" (file_filename f)
	  ) r.rule_sources;
	end;
	if verbose 9 then
	  Printf.eprintf "rule %d <- STATE EXECUTING\n" r.rule_id;
	r.rule_state <- RULE_EXECUTING;
	Some r
      end

(* let errors = ref [] *)


type execution_status =
    EXECUTION_SUCCESS
  | EXECUTION_FAILURE
  | EXECUTION_AVOIDED

let rule_executed b r execution_status =
  if verbose 5 then
    Printf.eprintf "rule_executed %d\n" r.rule_id;
  if verbose 9 then
    Printf.eprintf "rule %d <- STATE EXECUTED\n" r.rule_id;
  r.rule_state <- RULE_EXECUTED;
  let temp_dir = BuildEngineRules.rule_temp_dir r in
  if File.X.exists temp_dir then File.Dir.remove_all temp_dir;
  begin
    match execution_status with
	EXECUTION_SUCCESS ->
	  begin try
		  let (target_digest, command_digest) =
		    IntMap.find r.rule_id b.build_cache_entries
		  in
		  b.build_cache_input <- DigestMap.add
		    target_digest command_digest
		    b.build_cache_input
	    with Not_found -> ()
	  end
      | EXECUTION_FAILURE ->
	b.build_cache_entries <- IntMap.remove r.rule_id b.build_cache_entries
      | EXECUTION_AVOIDED -> ()
  end;
  release_temporaries b r;
  IntMap.iter (fun _ f ->
    if not f.file_exists then begin
      begin
	match f.file_kind with
	    FILE_VIRTUAL ->
	      f.file_exists <- true;
	      f.file_mtime <- BuildEngineMtime.zero;
	(* a virtual dependency should not cause re-running the rule commands *)
	  | FILE_TEMPORARY -> assert false
	  | FILE_REAL ->
	  try
            let filename = file_filename f in
	    ignore ( MinUnix.stat filename : MinUnix.stats);
	    f.file_exists <- true;
	    f.file_mtime <- BuildEngineMtime.compute filename
	  with e ->
	    BuildEngineDisplay.add_error b
	      [Printf.sprintf "Target %s not built" (file_filename f);]
      end;
      if f.file_exists then
	List.iter (fun r2 ->
	  if r2.rule_state <> RULE_INACTIVE then begin
	  if verbose 5 then
	    Printf.eprintf "\t\t\trule %d: missing %d -> %d (rule %d executed)\n" r2.rule_id r2.rule_missing_sources (r2.rule_missing_sources - 1) r.rule_id;
	  r2.rule_missing_sources <- r2.rule_missing_sources - 1;
	  if (r2.rule_missing_sources < 0) then begin
	    BuildEngineRules.print_rule r;
	    BuildEngineRules.print_rule r2;
     BuildMisc.clean_exit 2
	  end;
(*	  Printf.eprintf "Generated %s =>\n%!" (file_filename f); *)
(*	  Printf.eprintf "Setting rule %d missing sources to %d\n%!" r.rule_id r.rule_missing_sources; *)

	  if r2.rule_state = RULE_WAITING && r2.rule_missing_sources = 0 then begin
	    b.queue_waiting <- IntMap.remove r2.rule_id b.queue_waiting;
	    b.queue_ready <- IntMap.add r2.rule_id r2 b.queue_ready
	  end
	  end
	) f.file_source_for
    end else begin
(* TODO: find something to do in that case. .cmi files can be generated as
sub-targets of both .cmo and .cmx files. As such, they are not actually targets
of the rules, but side-effects. *)
      (* We should remove it if it was generated again !!! *)
      if verbose 7 then Printf.eprintf "Target file was already generated\n%!";
(*
      if !cross_arg && Sys.file_exists (file_filename f) then
	MinUnix.unlink (file_filename f)
*)
    end
  ) r.rule_targets



let cross_dirname b dirname =
  match b.cross_arg with
      None -> dirname
    | Some arch ->
      let cross_dirname = Filename.concat dirname b.build_dir_basename in
      if not (Sys.file_exists cross_dirname) then dirname else cross_dirname



(* TODO: file_target_of should be a list of rules. These rules should
   have a weight.  When several rules are available, take the active one
   with the highest weight.

   This would allow to move the .cmo/.cmi -> .cmx/.cmi dependency modification
   from here to buildOCamlRules.ml
*)

exception EmptyListOfDependencies

let rec add_dependency b r target_file filenames =
  match filenames with
    [] -> raise EmptyListOfDependencies
  | filename :: other_filenames ->
    if verbose 7 then Printf.eprintf "\tDEP %s\n%!" filename;
    let dirname = Filename.dirname filename in
    (*      let pj = r.rule_lib.lib_project in *)
    let dirname = cross_dirname b dirname in
    let dir = BuildEngineContext.add_directory b dirname in
    let basename = Filename.basename filename in
    let src_file =
      try Some (BuildEngineContext.find_file dir basename)
      with Not_found ->
	b.unmanaged_dependencies <- filename :: b.unmanaged_dependencies;
	None
    in
    match src_file with
      None -> ()
    | Some src_file ->
      if
        src_file.file_target_of <> [] &&
        List.for_all (fun r -> r.rule_state = RULE_INACTIVE)
          src_file.file_target_of then begin
	if verbose 7 then
	  Printf.eprintf "\t\tDisabled. Trying next.\n";
	add_dependency b r target_file other_filenames
      end else
      if not (IntMap.mem src_file.file_id r.rule_sources) then
	begin
	  if verbose 7 then
	    Printf.eprintf "\t\tAdding dependency\n";
	  r.rule_sources <-
	    IntMap.add src_file.file_id src_file r.rule_sources;
	  src_file.file_source_for <- r :: src_file.file_source_for;
	  if not src_file.file_exists then begin
	    (*			Printf.eprintf "Missing new dep %s =>\n%!" (file_filename src_file); *)
	    (*			Printf.eprintf "Setting rule %d missing sources to %d\n%!" r.rule_id r.rule_missing_sources; *)

	    if verbose 7 then
	      Printf.eprintf "\t\t\trule %d: missing %d -> %d\n" r.rule_id r.rule_missing_sources (r.rule_missing_sources + 1);
	    r.rule_missing_sources <- r.rule_missing_sources + 1;
	    if r.rule_missing_sources = 1 then
	      b.queue_waiting <- IntMap.add r.rule_id r b.queue_waiting
	  end else begin
	    if verbose 7 then Printf.eprintf "Adding useless dependency to %s\n%!"
		(file_filename src_file);
	  end
	end

let add_dependency b r target_file filenames =
  try
    add_dependency b r target_file filenames
  with EmptyListOfDependencies ->
      let (rule_filename, rule_loc, rule_project) = r.rule_loc in
      BuildMisc.print_loc rule_filename rule_loc;
    Printf.eprintf "Error, unexpected situation:\n";
    Printf.eprintf "  Dependencies needed by %s\n"
      (file_filename target_file);
    Printf.eprintf "  cannot be obtained by any active compilation rule:\n%!";
    List.iter (fun filename ->
      Printf.eprintf "\t%s\n%!" filename
    ) filenames;
    BuildMisc.clean_exit 2


(* TODO: replace BuildOcamldep.load_dependencies by a generic function inside LoadDeps *)
let load_dependency_file b loader file r_ok =
  if verbose 7 then Printf.eprintf "Loading dependencies from %s\n%!" (file_filename file);
  begin try
    let dependencies = (* BuildOcamldep.load_dependencies *) loader (file_filename file) in

    List.iter (fun (filename, deps) ->
      if verbose 7 then Printf.eprintf "FILE %s\n%!" filename;
      let dirname = Filename.dirname filename in
      let (rule_filename, rule_loc, rule_name) = r_ok.rule_loc in
      let dirname = cross_dirname b dirname in
      let dir = BuildEngineContext.add_directory b dirname in
      let target_file = try
	Some (BuildEngineContext.find_file dir (Filename.basename filename))
      with Not_found ->
        Printf.eprintf "Warning: dependencies for unknown file %S\n%!"
          filename;
        None
      in
      match target_file with
      None -> ()
      | Some target_file ->
      List.iter (fun r ->
	if r.rule_state <> RULE_INACTIVE then begin
	  if verbose 7 then begin
	    Printf.eprintf "Adding deps to rule %d \n%!" r.rule_id;
	    BuildEngineRules.print_rule r;
	  end;
	  begin
            match r.rule_state with
              RULE_INACTIVE | RULE_WAITING -> ()
            | _ ->
              failwith (Printf.sprintf "Rule %d failure with state %s" r.rule_id (BuildEngineRules.string_of_rule_state r))
          end;
	  begin
	    b.unmanaged_dependencies <- [];
	    List.iter (add_dependency b r target_file) deps;
	    List.iter (fun filename ->
	      BuildMisc.print_loc rule_filename rule_loc;
	      Printf.eprintf "Warning: file \"%s\" of project \"%s\" depends on\n" (file_filename target_file)  rule_name;
	      Printf.eprintf "  file \"%s\", that is not generated by any project\n%!" filename
	    ) b.unmanaged_dependencies;
	    if b.unmanaged_dependencies <> [] then begin
(*	      MinUnix.unlink (file_filename file);*)
              BuildEngineDisplay.add_error b
                [Printf.sprintf "Dependency file %s contains unmanaged dependencies. You might want to remove it and rebuild." (file_filename file)]
	    end;
	    b.unmanaged_dependencies <- []
	  end
	end
      )
	target_file.file_target_of
    ) dependencies
  with e ->
    (* An exception while reading dependencies. Probably there was a
       problem with the generated file. We should generate it again.
       TODO: add an option to not remove the file for debugging purpose !
    *)
    (*      MinUnix.unlink (file_filename file); TODO: do it, or not ? *)
    BuildEngineDisplay.add_error b [Printf.sprintf "Incorrect dependency file %s (Exception %s). You should clean and rebuild." (file_filename file) (Printexc.to_string e)]
  end


let temp_stdout b r =
  Filename.concat b.build_dir_filename (Printf.sprintf "rule_%d.stdout" r.rule_id)

let temp_stderr b r =
  Filename.concat b.build_dir_filename (Printf.sprintf "rule_%d.stderr" r.rule_id)

let execute_command b proc =
  let r = proc.proc_rule in
  let cmd = match proc.proc_last with
      None -> assert false
    | Some cmd -> cmd
  in
  let cmd_args =
    (BuildEngineRules.command_of_command cmd) @ List.map (BuildEngineRules.argument_of_argument r) cmd.cmd_args
  in
  b.build_stats_running_rules <-
    (r.rule_id, MinUnix.gettimeofday()) :: b.build_stats_running_rules;
  BuildEngineDisplay.begin_command b proc;
  Printf.fprintf b.build_log "'%s' %s %s\n"
    (String.concat "' '" cmd_args)
    (match cmd.cmd_stdin_pipe with
	None -> ""
      | Some filename -> Printf.sprintf "< %s" filename)
    (match cmd.cmd_stdout_pipe with
	None -> ""
      | Some filename -> Printf.sprintf "> %s" filename);
  let pid = BuildMisc.create_process cmd_args  cmd.cmd_move_to_dir
       cmd.cmd_stdin_pipe
    (Some (temp_stdout b r)) (Some (temp_stderr b r))
  in
  b.stats_command_executed <- b.stats_command_executed + 1;
  if verbose 7 then Printf.eprintf "EXEC started\n%!";
  pid

let new_proc r =
  let proc = {
    proc_step = 0;
    proc_rule = r;
    proc_commands = r.rule_commands;
    proc_last = None;
  } in
  proc

let print_file message filename =
  let ic = open_in filename in
  let message_printed = ref false in
  begin
    try
      while true do
	let line = input_line ic in
	if not !message_printed then begin
	  message_printed := true;
	  Printf.eprintf "%s\n%!" message
	end;
	Printf.eprintf "%s\n%!" line
      done
    with _ -> ()
  end;
  close_in ic

let s = String.create 32768

(* TODO: Use File.copy Copy line-oriented file *)
let copy_file b src dst =
  if verbose 7 then Printf.eprintf "copy_file from %s to %s\n%!" src dst;
  try
    File.RawIO.copy_file src dst;
    0
  with e ->
    b.fatal_errors <- [
      Printf.sprintf "Error while copying %s to %s:" src dst;
      Printf.sprintf "exception %s" (Printexc.to_string e);
    ] :: b.fatal_errors;
    2

let command_executed b proc status =
  if verbose 7 then Printf.eprintf "command_executed...\n%!";
  match proc.proc_last with
  | None -> assert false
  | Some cmd ->
    let r = proc.proc_rule in
    let t =
      MinUnix.gettimeofday() -. List.assoc r.rule_id b.build_stats_running_rules
    in
    b.build_stats_running_rules <-
      List.filter (fun (id,_) -> id <> r.rule_id) b.build_stats_running_rules;
    b.stats_total_time <- b.stats_total_time +. t;
    BuildEngineDisplay.end_command b proc t status;
    let copy_stdout =
      match cmd.cmd_stdout_pipe with
	None -> 0
      | Some file ->
	let src = temp_stdout b r in
	copy_file b src file;
    in
    let copy_stderr =
      match cmd.cmd_stderr_pipe with
	None -> 0
      | Some file ->
	let src = temp_stderr b r in
	copy_file b src file;
    in
      (*      if force_verbose then
	      print_file  "Command stderr:" (temp_stderr b r); *)
      (*      if status <> 0 then begin
	      add_error
	      [
	      Printf.sprintf "[%d.%d] '%s'" r.rule_id proc.proc_step
	      (BuildEngineDisplay.term_escape (String.concat "' '" cmd_args));
	      File.string_of_file (temp_stderr b r)
	      ];
              end; *)
    MinUnix.unlink (temp_stdout b r);
    MinUnix.unlink (temp_stderr b r);
    b.stats_files_generated <- (IntMap.cardinal r.rule_targets) + b.stats_files_generated;
    if status <> 0 then status else copy_stdout + copy_stderr

(*
let print_project_location pj =
  let pos = pj.project_loc in
  Printf.eprintf "File \"%s\", line 0, characters %d-%d:\n%!"
    pj.project_filename pos pos
*)

let rule_executed b r s =
  try
    rule_executed b r s
  with e ->
    Printf.eprintf "rule_executed: exception %s\n%!"
      (Printexc.get_backtrace ());
    raise e

let parallel_loop b ncores =
  let max_nslots = ref 0 in
  let slots = ref IntMap.empty in

  (* [iter freeslots] tries to associate a waiting action to a free slot, if one
     is available. *)
  let rec iter nslots =
    if nslots > 0 then begin
      if !sigint_received ||
	 b.fatal_errors <> [] ||
	 (b.stop_on_error_arg &&
          BuildEngineDisplay.has_error b) then wait_for_end nslots else
	match next_rule b with
	  None -> wait_for_end nslots
	| Some r ->
	  if verbose 3 then Printf.eprintf "[%d.0] Examining rule\n%!" r.rule_id;
	  if rule_need_execution b r then begin
	    let proc = new_proc r in
	    iter (execute_proc proc nslots)
	  end else begin
	    if verbose 3 then Printf.eprintf "[%d.0] Execution not needed !!!!\n%!" r.rule_id;
	    rule_executed b r EXECUTION_AVOIDED;
	    iter nslots
	  end
    end else
      wait nslots

  and wait_for_end nslots =
    if !slots = IntMap.empty then
      ()
    else
      wait nslots

  (* [wait freeslots] waits for one process to terminate, and call [iter] with the new number
     of available slots. *)
  and wait nslots =
    assert (nslots >= 0);
    assert (ncores - nslots = IntMap.cardinal !slots);
    let nslots =
      try
        if verbose 3 then Printf.eprintf "Wait for %d processes\n%!" (IntMap.cardinal !slots);
        let (pid, status) =
          if MinUnix.os_type = MinUnix.WINDOWS then
            let list = ref [] in
            IntMap.iter (fun pid _ -> list := pid :: !list) !slots;
            let pids = Array.of_list !list in
            BuildMisc.waitpids (Array.length pids) pids
          else BuildMisc.uninterrupted_wait () in
        let status =
	  match status with
	  | MinUnix.WEXITED status -> Some status
	  | MinUnix.WSIGNALED signal -> Some (-100-signal)
	  | MinUnix.WSTOPPED _ -> None
        in
        match status with
	  None -> nslots
        | Some status ->
	  begin
	    try
	      let proc =
                try
                  IntMap.find pid !slots
                with Not_found ->
                  Printf.eprintf "SLOT NOT FOUND...\n%!";
                  raise Not_found
              in
	      slots := IntMap.remove pid !slots;
	      let status =
	        match proc.proc_last with
		  None -> status
	        | Some cmd ->
		  let status = command_executed b proc status in
		  if verbose 3 then
		    Printf.eprintf "[%d.%d] Just finished executing\n%!"
		      proc.proc_rule.rule_id proc.proc_step;
		  (* if verbose 7 then print_indented_command cmd; *)
		  status
	      in
	      proc.proc_last <- None;
	      if status <> 0 then begin
                (*
  let (rule_filename, rule_loc, _rule_name) = proc.proc_rule.rule_loc in
  print_project_location pj;
  Printf.eprintf "[%d.%d] ERROR in project %s\n%!"
  proc.proc_rule.rule_id proc.proc_step rule_name;
*)
	        rule_executed b proc.proc_rule EXECUTION_FAILURE;
	        nslots + 1
	      end else
	        execute_proc proc (nslots + 1) (* we just freeed a slot *)
	    with Not_found ->
              Printf.eprintf "WARNING: Could not find returned pid %d in slots\n%!"
                pid;
              nslots
	  end
      with
      | BuildMisc.ExitStatus _ as e -> raise e
      | e ->
        (*	if verbose 7 then *)
        Printf.eprintf "Error in waiting loop: exception %s\n%!" (Printexc.to_string e);
        (* nslots *)
        BuildMisc.clean_exit 2
    in
    iter nslots

  and execute_proc proc nslots =
    if nslots > !max_nslots then max_nslots := nslots;
    match proc.proc_commands with
      [] ->
      if verbose 3 then
	Printf.eprintf "[%d.%d] rule finished\n%!" proc.proc_rule.rule_id proc.proc_step;
      rule_executed b proc.proc_rule EXECUTION_SUCCESS;
      nslots
    | cmd :: tail ->
      if verbose 3 then
	Printf.eprintf "[%d.%d] command executed\n%!" proc.proc_rule.rule_id proc.proc_step;
      proc.proc_step <- proc.proc_step + 1;
      proc.proc_commands <- tail;
      let r = proc.proc_rule in
      match cmd with
      | Function (name, _, actor) ->
        let error =
          try
            actor ();
            None
          with e ->
            Some e
        in
        begin match error with
            None -> execute_proc proc nslots
          | Some e ->
            b.fatal_errors <- [
              Printf.sprintf "Error while doing action %s:" name;
              Printf.sprintf "\tException %s" (Printexc.to_string e);
            ] :: b.fatal_errors;

            rule_executed b proc.proc_rule EXECUTION_FAILURE;
            nslots
        end

      | NeedTempDir ->
        let temp_dir = BuildEngineRules.rule_temp_dir r in
        if not (File.X.exists temp_dir) then
          File.Dir.make_all temp_dir;
        execute_proc proc nslots

      | Execute cmd ->
        let temp_dir = BuildEngineRules.rule_temp_dir r in
        if not (File.X.exists temp_dir) then
          File.Dir.make_all temp_dir;
	proc.proc_last <- Some cmd;
	if verbose 3 then
	  Printf.eprintf "[%d.%d] new exec\n%!" proc.proc_rule.rule_id proc.proc_step;
	let pid = execute_command b proc in
	slots := IntMap.add pid proc !slots;
	nslots - 1

      | LoadDeps (loader, file, r) ->
	if verbose 7 then
	  Printf.eprintf "[%d.%d] load deps\n%!" proc.proc_rule.rule_id proc.proc_step;
	(*	    let loader =
	  try
	  find_dependency_loader loader
	  with Not_found ->
	  Printf.eprintf "Error: Unable to load dependencies of type '%s'\n" loader;
	  exit 2
	  in *)
	load_dependency_file b loader file r;
	execute_proc proc nslots

      | Copy (f1, f2) ->
	let fa1 = BuildEngineRules.argument_of_argument r f1 in
	let fa2 = BuildEngineRules.argument_of_argument r f2 in
	let ff1 = BuildEngineRules.file_of_argument r f1 in
	let ff2 = BuildEngineRules.file_of_argument r f2 in

	if verbose 2 then
	  Printf.eprintf "[%d.%d] cp %s %s\n%!"
	    proc.proc_rule.rule_id proc.proc_step
	    fa1 fa2;
	Printf.fprintf b.build_log "cp %s %s\n" fa1 fa2;
	begin try
	  File.X.copy_file ff1 ff2
	with e ->
	  Printf.eprintf "Error copying %s to %s: %s\n%!" fa1 fa2
	    (Printexc.to_string e);
	  if not (File.X.exists ff1) then
	    Printf.eprintf "\tSource file %s does not exist\n%!" fa1;
	  if not (File.X.exists (File.dirname ff2)) then
	    Printf.eprintf
	      "\tDestination directory of %s does not exist\n%!"
              fa2;
	  BuildMisc.clean_exit 2
	end;
	execute_proc proc nslots

      | Move (f1, f2) ->
	let fa1 = BuildEngineRules.argument_of_argument r f1 in
	let fa2 = BuildEngineRules.argument_of_argument r f2 in
	let ff1 = BuildEngineRules.file_of_argument r f1 in
	let ff2 = BuildEngineRules.file_of_argument r f2 in

	if verbose 2 then
	  Printf.eprintf "[%d.%d] mv %s %s\n%!"
	    proc.proc_rule.rule_id proc.proc_step
	    fa1 fa2;

	Printf.fprintf b.build_log "mv %s %s\n" fa1 fa2;
	begin try
	  BuildMisc.rename fa1 fa2;
	with e ->
	  Printf.eprintf "Error moving %s to %s: %s\n%!"
            (fa1)
            (fa2) (Printexc.to_string e);
	  if not (File.X.exists (ff1)) then
	    Printf.eprintf "\tSource file %s does not exist\n%!"
              (fa1);
	  if not (File.X.exists (File.dirname (ff2))) then
	    Printf.eprintf "\tDestination directory %s does not exist\n%!"
              (fa2);
	  BuildMisc.clean_exit 2
	end;
	execute_proc proc nslots

      | MoveIfExists (f1, f2, link) ->
	let fa1 = BuildEngineRules.argument_of_argument r f1 in
	let fa2 = BuildEngineRules.argument_of_argument r f2 in
	let ff1 = BuildEngineRules.file_of_argument r f1 in
	let ff2 = BuildEngineRules.file_of_argument r f2 in

        if File.X.exists ff1 then
	  begin try
	    if verbose 2 then
	      Printf.eprintf "[%d.%d] mv? %s %s\n%!"
		proc.proc_rule.rule_id proc.proc_step
		fa1 fa2;

	    Printf.fprintf b.build_log "mv? %s %s\n" fa1 fa2;

	    BuildMisc.rename fa1 fa2;
            match link with
            | None -> ()
	    | Some f3 ->
              let src_file = File.to_string ff2 in
              let dst_file =
                File.to_string (BuildEngineRules.file_of_argument r f3) in
              try
                if Sys.file_exists dst_file then begin
                  let ic = open_in dst_file in
                  try
		    let line = input_line ic in
		    close_in ic;
		    if line <> src_file then raise Not_found
                  with e ->
		    close_in ic;
		    raise e
		end
		else raise Not_found
              with _ ->
		let oc = open_out dst_file in
		output_string oc src_file;
		close_out oc
	  with e ->
	    Printf.eprintf "Error moving %s to %s: %s\n%!"
	      fa1 fa2 (Printexc.to_string e);
	    if not (File.X.exists (ff1)) then
	      Printf.eprintf "\tSource file %s does not exist\n%!"
		(fa1);
	    if not (File.X.exists (File.dirname (ff2))) then
	      Printf.eprintf "\tDestination directory %s does not exist\n%!"
		(File.to_string (File.dirname (ff2)));
	    exit2 ();
	  end;
	execute_proc proc nslots
      | DynamicAction (msg, f) ->
	let actions =
	  try
	    Lazy.force f
	  with e ->
	    Printf.eprintf "Error: exception %s with DynamicAction %s\n%!"
	      (Printexc.to_string e) msg ;
	    exit2 ()
	in
	proc.proc_commands <- actions @ proc.proc_commands;
	execute_proc proc nslots
  in
  iter ncores

let save_cache b =
  let oc = open_out b.build_cache_filename in
  DigestMap.iter (fun d1 d2 ->
    Printf.fprintf oc "%s %s\n" (OcpDigest.to_hex d1) (OcpDigest.to_hex d2)
  ) b.build_cache_input;
  close_out oc;
  close_out b.build_cache_log;
  close_out b.build_log

let parallel_loop b ncores =
  let raised_exn = try
    parallel_loop b ncores; None
  with e -> Some e
  in
  save_cache b;
  if !sigint_received then begin
    Printf.eprintf "Error: compilation aborted by user\n%!";
    BuildMisc.clean_exit 2
  end;
  let waiting_len = IntMap.cardinal b.queue_waiting in
  if not (BuildEngineDisplay.has_error b
          || b.build_should_restart) && waiting_len > 0
  then begin
      Printf.eprintf "Error: %d rules waiting in queue !\n%!" waiting_len;
      Printf.eprintf "  This usually means that you have a cycle in module\n";
      Printf.eprintf "  dependencies.\n";
      if verbose 3 then
	print_waiting_queue b max_int
      else begin
        Printf.eprintf "  Use '-v 3' to display waiting rules. \nDisplaying only the first 3  rules in waiting queue:\n%!";
        print_waiting_queue b 3;
      end;
      BuildMisc.clean_exit 2
  end;
  BuildEngineDisplay.finish ();
  match raised_exn with
    None -> ()
  | Some e -> raise e


        (*let errors () = !errors *)
let fatal_errors b = b.fatal_errors

let sanitize b delete_orphans is_ok =
  let has_orphan_directories = ref false in
(*  Printf.fprintf stderr "BuildEngine.sanitize %s\n%!" b.build_dir_filename; *)
  let dir = File.of_string b.build_dir_filename in
  let cdir = BuildEngineContext.add_directory b b.build_dir_filename in
  let orphan_files = ref 0 in
  let rec iter dir cdir =
    File.Dir.iter (fun basename ->
      let filename = File.add_basename dir basename in
      if File.X.is_directory filename then
        try
          let cdir = BuildEngineContext.find_dir cdir basename in
          iter filename cdir
        with Not_found ->
          if verbose 3 then
            Printf.eprintf "Warning: orphan directory %s/%s\n%!"
              cdir.dir_fullname basename;
          has_orphan_directories := true;
          match delete_orphans with
              KeepOrphans
            | DeleteOrphanFiles -> ()
            | DeleteOrphanFilesAndDirectories ->
              File.Dir.remove_all filename
      else
        try
          ignore (BuildEngineContext.find_file cdir basename)
        with Not_found ->
          Printf.eprintf "Warning: orphan file %s/%s\n%!" cdir.dir_fullname basename;
          match delete_orphans with
              KeepOrphans -> incr orphan_files
            | DeleteOrphanFiles
            | DeleteOrphanFilesAndDirectories ->
              decr orphan_files;
              File.X.remove filename
    ) dir

  in
  File.Dir.iter (fun basename ->
    let filename = File.add_basename dir basename in
    if File.X.is_directory filename then
      try
        let cdir = BuildEngineContext.find_dir cdir basename in
        iter filename cdir
      with Not_found ->
        if not (is_ok basename) then begin
          Printf.eprintf "Warning: orphan directory %s/%s\n%!"
            cdir.dir_fullname basename;
          has_orphan_directories := true;
          match delete_orphans with
            KeepOrphans
          | DeleteOrphanFiles -> ()
          | DeleteOrphanFilesAndDirectories ->
            File.Dir.remove_all filename
        end
  ) dir;
  if !has_orphan_directories then begin
    Printf.eprintf "\tYou can use -sanitize-dirs to remove \
                    orphan directories\n%!";
  end;
  !orphan_files




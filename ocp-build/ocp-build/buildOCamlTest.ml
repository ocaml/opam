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
open BuildOCPVariable
open BuildEngineTypes
open BuildTypes
open BuildOCamlVariables
open BuildOCPTree
open BuildOCPTypes

let verbose = DebugVerbosity.verbose ["B"] "BuildOCamlTest"

type stats = {
  mutable tests_nsuccesses : int;
  mutable tests_nfailures : int;
  mutable tests_failures : (string * string) list;
  mutable tests_timings : (string * float) list;
}

let init () =
  {
    tests_nsuccesses = 0;
    tests_nfailures = 0;
    tests_failures = [];
    tests_timings = [];
  }

let check_output options subst output  option_name output_name =
  let output_check = get_strings_with_default options
      option_name []
  in
  if output_check <> [] then
    let cmd_output = try
      File.string_of_file output
    with _ -> failwith
                (Printf.sprintf "Missing output on %s" output_name)
    in
    let expected_output_file =
      (String.concat "/" output_check) in
    let expected_output_file =
      BuildSubst.subst subst expected_output_file in
    let expected_output =
      try File.string_of_file expected_output_file
      with _ ->
        Printf.eprintf "Warning: missing expected output file %S\n%!"
          expected_output_file;
        failwith "Missing expected output"
    in
    let output_len = String.length cmd_output in
    let expected_len = String.length expected_output in
    if output_len < expected_len then
      failwith
        (Printf.sprintf "output %s too short (%d < %d)"
           output_name
           output_len  expected_len);
    if output_len > expected_len then
      failwith
        (Printf.sprintf "output %s too long (%d > %d)"
           output_name
           output_len  expected_len);
    let rec iter pos =
      if pos < String.length cmd_output then begin
        if cmd_output.[pos] <> expected_output.[pos] then
          failwith (Printf.sprintf "output %s differ at char %d"
              output_name pos);
        iter (pos+1)
      end
    in
    iter 0

let parallel_workqueue = Queue.create ()
let serial_workqueue = Queue.create ()



let find_binaries b cwd lib =
  let has_asm = get_bool_with_default [lib.lib_options] "asm" true in
  let has_byte = get_bool_with_default [lib.lib_options] "byte" true in
  let get_binary ext =
    let binary_basename =
      lib.lib_name ^ ext
    in
    let binary =
      File.to_string (File.add_basenames b.build_dir
          [lib.lib_name; binary_basename])
    in
    Filename.concat cwd binary
  in
  match has_asm, has_byte with
  | true, true ->
    [ "asm", get_binary ".asm"; "byte", get_binary ".byte" ]
  | false, true ->
    [ "byte", get_binary ".byte" ]
  | true, false ->
    [ "asm", get_binary ".asm" ]
  | false, false ->
    []

let test_package b stats lib only_benchmarks =
  let cwd = MinUnix.getcwd () in
  let binaries, tests =
    match lib.lib_type, lib.lib_tests with
    | TestPackage, _ ->
      let binaries =
        if lib.lib_sources = [] then
          let program = ref None in
          List.iter (fun dep ->
            let pro = dep.dep_project in
            match pro.lib_type, !program with
            | ProgramPackage, Some pro1 ->
              Printf.eprintf "Error: test %S depends on two programs %S and %S\n%!"
                lib.lib_name pro1.lib_name pro.lib_name;
              BuildMisc.clean_exit 2
            | ProgramPackage, None ->
              program := Some pro
            | _ -> () (* TODO: raise an error ? *)
          ) lib.lib_requires;
          begin
            match !program with
              None ->
              let cmd = get_strings_with_default [lib.lib_options]
                  "test_cmd" [] in
              if cmd = [] then begin
                Printf.eprintf "Error: test %S has no source files.\n%!"
                  lib.lib_name;
                Printf.eprintf "  It should depend on the program it is testing.";
                BuildMisc.clean_exit 2
              end;
              [ "cmd", String.concat "/" cmd ]
            | Some pro -> find_binaries b cwd pro
          end
        else
          find_binaries b cwd lib
      in
      let tests =
        match lib.lib_tests with
          [] -> [ lib.lib_name, empty_env ]
        | tests -> tests
      in
      binaries, tests

    | ProgramPackage, tests ->
      find_binaries b cwd lib, tests
    | (ObjectsPackage
      | LibraryPackage
      | SyntaxPackage
      | RulesPackage), _
      -> assert false
  in
  if tests <> [] then begin
  let ntests = ref 0 in

    List.iter (fun (kind, binary) ->
      let tests_dir = File.add_basenames b.build_dir
          [ "_tests"; lib.lib_name; "tests" ]
      in
      let tests_result_dir = Filename.concat cwd (File.to_string tests_dir) in
      List.iter (fun (test, options) ->
        let options = [ options; lib.lib_options ] in
        let test_asm = get_bool_with_default options "test_asm" true in
        let test_byte = get_bool_with_default options "test_byte" true in
        let do_test =
          match kind with
          | "asm" -> test_asm
          | "byte" -> test_byte
          | _ -> true
        in
        if do_test then
          List.iter (fun variant ->
            let test_basename =
              Printf.sprintf "%s-%s%s" test kind variant
            in
            let test_result_dir = Filename.concat tests_result_dir
                test_basename in
            let test_info = Filename.concat test_result_dir "test_info.txt" in


        let test_name = Printf.sprintf "%s/tests/%s" lib.lib_name test_basename
        in
        let subst = BuildSubst.global_subst in
        let subst = StringSubst.add_to_copy subst "%{variant}%" variant in
        let subst = StringSubst.add_to_copy subst "%{kind}%" kind in
        let subst = StringSubst.add_to_copy subst "%{test}%" test in
        let subst = StringSubst.add_to_copy subst "%{binary}%" binary in
        let subst = StringSubst.add_to_copy subst "%{tests}%"
            (File.to_string
               (File.add_basename lib.lib_src_dir.dir_file "tests"))
        in
        let subst = StringSubst.add_to_copy subst "%{sources}%"
            (File.to_string lib.lib_src_dir.dir_file) in
        let subst = StringSubst.add_to_copy subst "%{results}%"
            test_result_dir in
        let cmd = get_strings_with_default options
            "test_cmd" [ "%{binary}%" ]
        in
        let cmd_args = get_strings_with_default options
            "test_args" []
        in
        let cmd_args = cmd @ cmd_args in
        let cmd_args = List.map (BuildSubst.subst subst) cmd_args in
        if verbose 2 then
          Printf.eprintf "Starting test '%s'\n%!"
            (String.concat "' '" cmd_args);
        let result_out = Filename.concat test_result_dir "result.out" in
        let result_err = Filename.concat test_result_dir "result.err" in

        let test_stdin = get_strings_with_default options "test_stdin" [] in
        let test_stdin = match test_stdin with
            [] -> None
          | stdin ->
            Some ( BuildSubst.subst subst (String.concat "/" stdin) ) in

        let test_rundir = get_strings_with_default options "test_dir" [ "." ] in
        let test_rundir = String.concat "/" test_rundir in
        let test_rundir = BuildSubst.subst subst test_rundir in
        let expected_status = get_string_with_default options
            "test_exit" "0"
        in
        let expected_status = try
          int_of_string expected_status
        with _ ->
          Printf.eprintf "Error: test %s.%s: bad number for expected status %S\n%!"
            lib.lib_name test expected_status;
          BuildMisc.clean_exit 2
        in
        let benchmark =
          try
            let v = get options "test_benchmark" in
            if v = true_value then true else
            if v = false_value then false else
              let list = strings_of_plist v in
              let test_name =
               get_string_with_default options "test_benchmarked"
                  test_name
              in
              let test_name = BuildSubst.subst subst test_name in
              List.mem test_name list
          with Not_found -> false
        in
        let serialized = get_bool_with_default options
            "test_serialized" false in


        let start_test () =
          BuildMisc.safe_mkdir test_result_dir;
          let oc = open_out test_info in
          Printf.fprintf oc "test=%S\n" test;
          Printf.fprintf oc "test_binary=%S\n" binary;
          Printf.fprintf oc "variant=%S\n" variant;
          Printf.fprintf oc "test_dir=%S\n" test_rundir;
          Printf.fprintf oc "test_kind=%S\n" kind;
          Printf.fprintf oc "test_benchmark=%b\n" benchmark;
          Printf.fprintf oc "test_serialized=%b\n" serialized;
          Printf.fprintf oc "test_exit=%d\n" expected_status;
          Printf.fprintf oc "test_cmd=\"%s\"\n"
            (String.concat "\" \"" cmd_args);
          close_out oc;

          let pid = BuildMisc.create_process cmd_args (Some test_rundir)
              test_stdin (Some result_out) (Some result_err)
          in
          if verbose 2 then
            Printf.eprintf "Test started. Waiting...\n%!";
          pid
        in

        let check_test time status =
          if verbose 2 then
            Printf.eprintf "Test finished\n%!";

          if status = expected_status then begin
            try
              check_output options subst result_out "test_output" "stdout";
              check_output options subst result_out "test_stdout" "stdout";
              check_output options subst result_err "test_stderr" "stderr";

              if benchmark then
                stats.tests_timings <- (
                  test_name, time) :: stats.tests_timings;
              stats.tests_nsuccesses <- stats.tests_nsuccesses + 1
            with Failure s ->
              stats.tests_nfailures <- stats.tests_nfailures + 1;
              stats.tests_failures <- (test_name, s)
                :: stats.tests_failures
          end
          else begin
            stats.tests_nfailures <- stats.tests_nfailures + 1;
            stats.tests_failures <- (test_name,
              Printf.sprintf "status=%d%s" status
                (if expected_status <> 0 then
                   Printf.sprintf " instead of %d expected" expected_status
                 else "")
            ) :: stats.tests_failures
          end
            in
            if not only_benchmarks || benchmark then begin
              incr ntests;
              Queue.push (test_name, start_test, check_test)
                (if benchmark || serialized then serial_workqueue
                 else parallel_workqueue)
            end
          )
            (get_strings_with_default options
            "test_variants" [ "" ])
      ) tests
    ) binaries;
    if verbose 1 && !ntests > 0 then
      Printf.eprintf "%d tests to run for test %S...\n%!"
        !ntests lib.lib_name;
  end


let parallel_job_engine njobs f =

  let rec iter nstarted max_njobs jobs =
    if nstarted < njobs then
      match f () with
      | None ->
        wait_for_slot nstarted max_njobs jobs
      | Some (start_command, exit_handler) ->
        let t0 = MinUnix.gettimeofday () in
        let pid = start_command () in
        let jobs = IntMap.add pid (exit_handler, t0) jobs in
        let nstarted = nstarted + 1 in
        let max_njobs =
          if nstarted > max_njobs then nstarted else max_njobs in
        iter nstarted max_njobs jobs
    else
      wait_for_slot nstarted max_njobs jobs

  and wait_for_slot nstarted max_njobs jobs =
    if nstarted > 0 then
      let (pid, status) =
        if MinUnix.os_type = MinUnix.WINDOWS then
          let list = ref [] in
          IntMap.iter (fun pid _ -> list := pid :: !list) jobs;
          let pids = Array.of_list !list in
          BuildMisc.waitpids (Array.length pids) pids
        else OnlyUnix.wait () in
      let status =
        match status with
        | MinUnix.WEXITED status -> Some status
        | MinUnix.WSIGNALED signal -> Some (-100-signal)
        | MinUnix.WSTOPPED _ -> None
      in
      match status with
        None -> wait_for_slot nstarted max_njobs jobs
      | Some status ->
        let (exit_handler, t0) = IntMap.find pid jobs in
        let jobs = IntMap.remove pid jobs in
        let nstarted = nstarted - 1 in
        let t1 = MinUnix.gettimeofday () in
        exit_handler (t1 -.t0) status;
        iter nstarted max_njobs jobs
    else
      max_njobs
  in
  try
    iter 0 0 IntMap.empty
  with e ->
    Printf.eprintf "Error in BuildOCamlTest.job_engine: exception %S\n%!"
      (Printexc.to_string e );
    raise e

let workqueue_extractor workqueue =
  let n = Queue.length workqueue in
  let counter = ref 0 in
  function () ->
    if Queue.is_empty workqueue then None else
      let (name, start_test, check_test) = Queue.take workqueue in
      if verbose 1 then
        Printf.printf "%d/%d\t%s\n%!" !counter n name;
      incr counter;
      Some (start_test, check_test)

let finish tests njobs =
  let cwd = MinUnix.getcwd () in
  let path = BuildConfig.get_PATH () in
  let finally () =
    BuildConfig.set_PATH path;
    MinUnix.chdir cwd
  in
  let t0 = MinUnix.gettimeofday () in
  let t1 =
    if not (Queue.is_empty parallel_workqueue) then begin
      let max_njobs =
        parallel_job_engine njobs (workqueue_extractor parallel_workqueue)
      in
      let t1 = MinUnix.gettimeofday () in
      Printf.eprintf "Parallel tests ran in %.2fs (max %d jobs)\n%!" (t1 -. t0)
        max_njobs;
      t1
    end else t0 in
  if not (Queue.is_empty serial_workqueue) then begin
    let max_njobs = parallel_job_engine 1 (workqueue_extractor serial_workqueue)
    in
    assert (max_njobs = 1);
    let t2 = MinUnix.gettimeofday () in
    Printf.eprintf "Sequential tests ran in %.2fs\n%!" (t2 -. t1);
  end;
  finally ();

  let ntotal = tests.tests_nsuccesses + tests.tests_nfailures in
  Printf.printf "FAILED: %d/%d\n" tests.tests_nfailures ntotal;
  List.iter (fun (test_name, result) ->
    Printf.printf "  %s (%s)\n%!" test_name result
  ) (List.sort compare tests.tests_failures);
  Printf.printf "SUCCESS: %d/%d\n" tests.tests_nsuccesses ntotal;
  if tests.tests_timings <> [] then begin
    List.iter (fun (test_name, timing) ->
      Printf.printf "  %.2fs\t%s\n%!" timing test_name
    ) (List.sort compare tests.tests_timings);
  end;
  if tests.tests_nfailures > 0 then begin
    Printf.eprintf "You can read failed test outputs in _obuild/_tests/PACKAGE/\n%!";
    BuildMisc.clean_exit 1
  end

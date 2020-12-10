let set_debug_level n sections =
  let debug_sections =
    List.fold_left (fun map elt -> OpamCoreConfig.StringMap.add elt None map) OpamCoreConfig.StringMap.empty sections
  in
  OpamCoreConfig.(update ~noop:()) ~debug_level:n ~debug_sections ()

let test_dir = "patcher-test"

let write_file ~dir ~name use_crlf ?(eol_at_eof = true) pattern =
  let ch = open_out_bin (Filename.concat dir name) in
  let eol = if use_crlf then "\r\n" else "\n" in
  List.fold_left (fun a (n, s) -> for i = a to a + n - 1 do Printf.fprintf ch "Line %d%s" i eol done; a + n + s) 1 pattern |> ignore;
  output_string ch "End of file";
  if eol_at_eof then
    output_string ch eol;
  close_out ch

let write_single_line ~dir ~name line =
  let ch = open_out_bin (Filename.concat dir name) in
  output_string ch line;
  close_out ch

let touch ~dir name =
  close_out (open_out_bin (Filename.concat dir name))

let setup_directory ~dir =
  OpamSystem.remove dir;
  OpamSystem.mkdir dir;
  OpamSystem.chdir dir;
  List.iter OpamSystem.mkdir ["a"; "b"; "c"]

let pattern1 ?(test1 = false) ?(test2 = false) ?(test3 = false) ?(test4 = false) ?(eoleof_cr = false) dir =
  write_file ~dir ~name:"always-lf" false [(5, 0)];
  write_file ~dir ~name:"always-crlf" true [(5, 0)];
  write_file ~dir ~name:"no-eol-at-eof" eoleof_cr ~eol_at_eof:false [(5, 0)];
  write_single_line ~dir ~name:"no-eol-at-all" "Original line";
  write_file ~dir ~name:"test1" test1 [(5, 0)];
  write_file ~dir ~name:"test2" test2 [(5, 0)];
  write_file ~dir ~name:"test3" test3 [(5, 0)];
  touch ~dir "null-file";
  write_file ~dir ~name:"will-null-file" test4 [(5, 0)]

let pattern2 dir =
  write_file ~dir ~name:"always-lf" false [(3, 1); (1, 0)];
  write_file ~dir ~name:"always-crlf" true [(6, 0)];
  write_file ~dir ~name:"no-eol-at-eof" false ~eol_at_eof:false [(3, 1); (1, 0)];
  write_single_line ~dir ~name:"no-eol-at-all" "Patched line";
  write_file ~dir ~name:"test1" false [(6, 0)];
  write_file ~dir ~name:"test2" false [(1, 1); (1, 1); (1, 0)];
  write_file ~dir ~name:"null-file" false [(5, 0)];
  touch ~dir "will-null-file"

let eol_style ch fn =
  let s =
    match OpamSystem.get_eol_encoding fn with
    | None -> "mixed"
    | Some true -> "CRLF"
    | Some false -> "LF"
  in
  let s =
    let ch = open_in_bin fn in
    let l = in_channel_length ch in
    let r =
    if l = 0 || (seek_in ch (l - 1); input_char ch <> '\n') then
      s ^ " (no eol-at-eof)"
    else
      s
    in close_in ch; r
  in
  output_string ch s

let print_directory dir =
  List.iter (fun fn -> Printf.eprintf "  %s: %a\n%!" (OpamSystem.back_to_forward fn) eol_style fn) (OpamSystem.ls dir)

let generate_patch () =
  flush stderr; flush stdout;
  if Sys.command "diff -Naur a b > input.patch" <> 1 then (Printf.eprintf "patch generation failed\n%!"; exit 2);
  set_debug_level (-3) ["PATCH"];
  OpamSystem.translate_patch ~dir:"c" "input.patch" "output.patch";
  set_debug_level 0 [];
  OpamSystem.chdir "c";
  Printf.eprintf "Before patch state of c:\n";
  print_directory ".";
  flush stdout;
  if Sys.command "patch -p1 -i ../output.patch" <> 0 then (Printf.eprintf "patch application failed\n%!"; exit 2);
  Printf.eprintf "After patch state of c:\n";
  print_directory ".";
  OpamSystem.chdir Filename.parent_dir_name

let tests () =
  set_debug_level 0 [];
  let cwd = Sys.getcwd () in
  setup_directory ~dir:test_dir;
  pattern1 "a";
  pattern2 "b";
  pattern1 "c";
  generate_patch ();
  pattern1 ~test2:true ~test4:true ~eoleof_cr:true "c";
  generate_patch ();
  OpamSystem.chdir cwd;
  OpamSystem.remove test_dir

let () =
  (* This causes Windows to use LF endings instead of CRLF, which simplifies the comparison with the reference file *)
  Unix.putenv "LC_ALL" "C";
  set_binary_mode_out stdout true;
  Unix.dup2 Unix.stdout Unix.stderr;
  tests ()

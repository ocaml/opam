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
open BuildEngineTypes
open BuildEngineGlobals
(*open BuildGlobals *)
(*open BuildConfig *)

(* TODO do this somewhere
- On Windows, st_ino always returns 0. So, we need another way of finding
equivalent directories.
- On Windows, we have no way to identify identical directories. Let's try this:
first, create a special file in our top directory. now, in every directory,
check whether we have reached that directory. If yes, we know where we are !
*)

let on_Windows = Sys.os_type = "Win32"

(*
let name_of_filename = Hashtbl.create 113


type directory_name =
 | RootDirectory of string
 | SubDirectory of directory_name * string

let rec directory_name dirname =
  try
    Hashtbl.find name_of_filename dirname
  with Not_found ->
    let dirname =
      if Filename.is_relative dirname then
        Filename.concat (Sys.getcwd()) dirname
      else
        dirname
    in
    let name =
      let basename = Filename.basename dirname in
      let parent_name = Filename.dirname dirname in
      if parent_name = dirname then
        RootDirectory dirname
      else
        SubDirectory (directory_name parent_name, basename)
   in
   Hashtbl.add name_of_filename dirname name;
   name
*)

let get_file_uid filename =
  let st = MinUnix.lstat filename in
  if st.MinUnix.st_ino = 0 &&
    match MinUnix.os_type with
      MinUnix.UNIX -> false
    | MinUnix.CYGWIN | MinUnix.WINDOWS -> true
    then
    let ft = OnlyWin32.getFileInformationByName filename in
    st, (ft.OnlyWin32.dwVolumeSerialNumber,
	 ft.OnlyWin32.nFileIndex)
  else
    st, (st.MinUnix.st_dev, Int64.of_int st.MinUnix.st_ino)

(*
    then
      let dir = directory_name filename in
      let ino =try
          Hashtbl.find ino_of_name dir
        with Not_found ->
          incr inode_counter;
          Hashtbl.add ino_of_name dir !inode_counter;
          !inode_counter
      in
      { st with MinUnix.st_ino = ino }
    else st in
  st
*)

(*
let find_directory db filename =
  let name = directory_name db filename in
  Hashtbl.find db.dir_db name

let rec add_directory db filename =
  let name = directory_name db filename in
  try
    Hashtbl.find db.dir_db name
  with Not_found ->
    let dir =
      match name with
        RootDirectory basename ->
	    let rec dir = {
	      dir_key = key;
	      dir_id = new_dir_id b;
	      dir_basename = basename;
	      dir_parent = dir;
              dir_file = File.of_string basename; (* was "/" *)
	      dir_files = StringMap.empty;
	      dir_dirs = StringMap.empty;
	      dir_fullname = filename;
	    } in
	    dir
     | SubDirectory (parent, basename) ->
		let dir =
		  try
		    StringMap.find basename parent_dir.dir_dirs
		  with Not_found ->
		    let dir = {
		      dir_basename = basename;
		      dir_parent = parent_dir;
                      dir_file = File.add_basename parent_dir.dir_file basename;
		      dir_key = key;
		      dir_id = new_dir_id b;
		      dir_files = StringMap.empty;
		      dir_dirs = StringMap.empty;
		      dir_fullname = Filename.concat parent_dir.dir_fullname basename;
		    } in
		    parent_dir.dir_dirs <- StringMap.add basename dir parent_dir.dir_dirs;
		    dir
		in
		dir
	end
	| _ -> assert false
    in
    Hashtbl.add b.build_directories key dir;
    dir
*)

let find_directory b dirname =
  if verbose 5 then
    Printf.fprintf stderr
       "BuildEngineContext.find_directory %s\n"
       dirname;
  let _, key = get_file_uid dirname in
  Hashtbl.find b.build_directories key

let rec add_directory b filename =
  if verbose 5 then
    Printf.fprintf stderr
      "BuildEngineContext.add_directory %s\n"
      filename;
  let st, key = get_file_uid filename in
  if verbose 5 then
    Printf.fprintf stderr
      "BuildEngineContext.add_directory |%s| (%d,%Ld)\n"
      filename (fst key) (snd key);
  try
    let dir = Hashtbl.find b.build_directories key in
    if verbose 5 then
      Printf.fprintf stderr "Found\n";
    dir
  with Not_found ->
    if verbose 5 then
      Printf.fprintf stderr "Not found\n";
    let dir =
      let dirname = Filename.dirname filename in
      if verbose 5 then
        Printf.fprintf stderr "\tdirname = %s\n" filename;
      match st.MinUnix.st_kind with
	MinUnix.S_LNK ->
	let link = OnlyUnix.readlink filename in
	let filename =
	  if Filename.is_relative link then
	    Filename.concat dirname link
	  else link
	in
	add_directory b filename
      | MinUnix.S_DIR -> begin
	  let basename = Filename.basename filename in
          if verbose 5 then
            Printf.fprintf stderr "\tfilename = %s\n" filename;
	  if dirname = filename then
	    let rec dir = {
	      dir_key = key;
	      dir_id = new_dir_id b;
	      dir_basename = basename;
	      dir_parent = dir;
              dir_file = File.of_string (if on_Windows then dirname else "/");
	      dir_files = StringMap.empty;
	      dir_dirs = StringMap.empty;
	      dir_fullname = filename;
	    } in
	    dir
	  else
	    let parent_dir = add_directory b dirname in
	    match basename with
	      "." -> parent_dir
	    | ".." -> parent_dir.dir_parent
	    | _ ->
              (* educated guess : this does not really make sense on Windows,
                 as inode numbers are generated and not read from the file-system. *)
	      let dirname = parent_dir.dir_fullname in
	      let basename =
		try
                  let _, key2 = get_file_uid (Filename.concat dirname basename) in
		  if key = key2
	          then
		    basename
		  else raise Not_found
		with _ ->
		  let files = Sys.readdir dirname in
		  let nfiles = Array.length files in
		  let rec iter i =
		    assert (i < nfiles);
		    let file = files.(i) in
		    let _, key2 =           get_file_uid  (Filename.concat dirname file) in
		    if key = key2 then
		      file
		    else
		      iter (i+1)
		  in
		  iter 0
	      in
	      let dir =
		try
		  StringMap.find basename parent_dir.dir_dirs
		with Not_found ->
		  let dir = {
		    dir_basename = basename;
		    dir_parent = parent_dir;
                    dir_file = File.add_basename parent_dir.dir_file basename;
		    dir_key = key;
		    dir_id = new_dir_id b;
		    dir_files = StringMap.empty;
		    dir_dirs = StringMap.empty;
		    dir_fullname = Filename.concat parent_dir.dir_fullname basename;
		  } in
		  parent_dir.dir_dirs <- StringMap.add basename dir parent_dir.dir_dirs;
		  dir
	      in
	      dir
	end
      | _ -> assert false
    in
    Hashtbl.add b.build_directories key dir;
    dir

let find_dir dir basename =
  StringMap.find basename dir.dir_dirs

let find_file dir basename =
  StringMap.find basename dir.dir_files










(* Convert all filenames starting with the drive name to Unix filenames,
   on Cygwin *)
let truename dirname =
  match MinUnix.os_type with
      MinUnix.CYGWIN ->
        let len = String.length dirname in
          if (len > 1 && dirname.[1] = ':' &&
              match dirname.[0] with
                  'a'..'z' | 'A'..'Z' -> true
              | _ -> false)
          then
            Printf.sprintf "/cygdrive/%c%s"
              (Char.lowercase dirname.[0])
              (String.sub dirname 2 (len-2))
          else
            dirname
    | _ -> dirname

let add_directory b dirname = add_directory b (truename dirname)
let find_directory b dirname = find_directory b (truename dirname)




(* let build_dir = add_directory b build_dir_filename *)

let add_any_file b dir filename file_kind =
  let dirname = Filename.dirname filename in
  let basename = Filename.basename filename in
  let dir = if dirname = "." || dirname = "" then dir else
      add_directory b (Filename.concat dir.dir_fullname dirname)
  in
  try find_file dir basename with Not_found ->
    let file = {
      file_id = new_file_id b;
      file_kind = file_kind;
      file_basename = basename;
      file_dir = dir;
      file_file = File.add_basename dir.dir_file basename;
      file_exists = false; (* shall we do that now ? *)
      file_mtime = BuildEngineMtime.zero;
      file_target_of = [];
      file_source_for = [];
    } in
    dir.dir_files <- StringMap.add basename file dir.dir_files;
    Hashtbl.add b.build_files file.file_id file;
    file

let add_virtual_file b dir basename =
  add_any_file b dir basename FILE_VIRTUAL

let make_virtual_file f =
  f.file_kind <- FILE_VIRTUAL

let add_temp_file b dir basename =
  add_any_file b dir basename FILE_TEMPORARY

let add_file b dir basename =
  add_any_file b dir basename FILE_REAL

let add_filename = add_file  (* TODO: remove *)


let create current_dir_filename build_dir_filename =
(*  Printf.eprintf "BuildEngineContext.create %s\n%!" build_dir_filename; *)
  let (build_rules : (int, build_rule) Hashtbl.t) = Hashtbl.create 1111 in
  let (build_files : (int, build_file) Hashtbl.t) = Hashtbl.create 1111 in
  let (build_directories : (int * int64,   build_directory) Hashtbl.t) = Hashtbl.create 1111 in

  let build_cache_content = ref 0 in
  let build_cache = ref DigestMap.empty in
  let build_cache_filename = Filename.concat build_dir_filename "cache.cmd" in
(*  Printf.fprintf stderr "build_cache_filename = %s\n%!" build_cache_filename; *)
  begin
    match try
	    let ic = open_in build_cache_filename in
	    Some ic
      with e -> None
    with
	None -> ()
      | Some ic ->
	try
	  while true do
	    let line = input_line ic in
	    if String.length line > 0 then
	    match line.[0] with
		'#' -> ()
	      | _ ->
	      let targets, command = OcpString.cut_at line ' ' in
	      let targets = OcpDigest.of_hex targets in
	      let command = OcpDigest.of_hex command in
	      incr build_cache_content;
	      build_cache := DigestMap.add targets command !build_cache;
	  done
	with End_of_file -> close_in ic
  end;
  let build_cache_log = open_out (build_cache_filename ^ ".log") in
  let build_log = open_out
    (Filename.concat build_dir_filename "build.log") in
  if verbose 5 then
    Printf.eprintf "Cache: %d digests loaded\n" !build_cache_content;
  let b =
    {
      build_should_restart = false;
      build_directories;
      build_files;
      build_rules;
      build_next_dir_id = 0;
      build_next_file_id = 0;
      build_next_rule_id = 0;
      build_next_process_id = 0;

      build_dir_filename = build_dir_filename;                   (* "/..../_obuild" *)
      build_dir_basename = Filename.basename build_dir_filename; (* "_obuild" *)
      build_dir = File.of_string build_dir_filename;

      build_log = build_log;

      build_cache_input = !build_cache;
      build_cache_entries = IntMap.empty;
      build_cache_filename = build_cache_filename;
      build_cache_log = build_cache_log;

      cross_arg = None;
      stop_on_error_arg = true;

      (* to display progress *)
      build_stats_to_execute = 0;
      build_stats_executed = 0;
      build_stats_running_rules = [];
      build_stats_lastpoint = 0;

      queue_inactive = [];
      queue_ready = IntMap.empty;
      queue_waiting = IntMap.empty;
      queue_not_waiting = IntMap.empty;
      temp_files = IntMap.empty;
      unmanaged_dependencies = [];
      fatal_errors = [];
      errors = [];
      stats_command_executed = 0;
       stats_files_generated = 0;
      stats_total_time = 0.;
    }

  in
(*  Printf.eprintf "add_directory MASTER: %s\n" current_dir_filename; *)
  let dir = add_directory b current_dir_filename in
  dir.dir_fullname <- ".";
  dir.dir_file <- File.of_string ".";
  let dir2 = add_directory b current_dir_filename in
  assert (dir == dir2);
  b

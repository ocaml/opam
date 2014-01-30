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

(* open SafeCaml *)

exception ExitStatus of int
let clean_exit n = raise (ExitStatus n)

let at_sigint_actions = ref []

let at_sigint name f = at_sigint_actions := !at_sigint_actions @ [name, f]
let (_ : unit) =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
      List.iter (fun (name, f) ->
        try
          f ()
        with e ->
          Printf.eprintf "Warning: sigint handler %s raised exception %s\n%!"
            name (Printexc.to_string e)
      ) !at_sigint_actions
    ))


let print_loc filename pos =
  let line = ref 1 in
  let last_line_pos = ref 0 in
  begin try
	  let s = File.string_of_file filename in
	  for i = 0 to pos - 1 do
	    if s.[i] = '\n' then begin
	      incr line;
	      last_line_pos := i
	    end
	  done
    with _ -> ()
  end;
  let pos = pos - !last_line_pos in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n%!"
    filename !line pos pos

exception ParseError

let token_list_of_filename filename lexer =
  let s = File.string_of_file filename in
  let str1 = Stream.of_string s in
  let str2 = lexer str1 in
  let list = ref [] in
  try
    Stream.iter (fun token ->
      let token_pos = Stream.count str1 in
      list := (token, token_pos) :: !list) str2;
    List.rev !list
  with
      Stream.Error error ->
	print_loc filename (Stream.count str1);
	Printf.eprintf "Error: %s\n%!" error;
	raise ParseError


let os_type = MinUnix.os_type

let waitpids = OnlyWin32.waitpids

module MinUnix2 : sig
  open MinUnix

  type error_handler = exn -> unit

  val create_process : error_handler ->
    string -> string array -> string option -> file_descr -> file_descr -> file_descr -> int

(*
  val create_process_env : error_handler ->
    string -> string array -> string array -> file_descr -> file_descr ->
    file_descr -> int
*)

end = struct
  open MinUnix
  type error_handler = exn -> unit


let string_rev s =
  let len = String.length s in
  let ss = String.create len in
  let lenMinus1 = len-1 in
  for i = 0 to len-1 do
    ss.[i] <- s.[lenMinus1-i];
  done;
  ss

(* This is supposed to be implemented as Filename.quote ? Verify ! *)
let win_buf_escape_argument b arg =
  let len = String.length arg in
  Buffer.add_char b '"';
  let rec iter escaped b s pos =
    if pos > 0 then
    let pos = pos-1 in
    let c = s.[pos] in
    match c, escaped with
    | '"', _
    | '\\', true ->
          Buffer.add_char b c;
          Buffer.add_char b '\\'; (* after, because we reverse afterwards ! *)
          iter true b s pos
    | _ ->
          Buffer.add_char b c;
          iter false b s pos
  in
  iter true b arg len;
  Buffer.add_char b '"'

let win_cmdline_of_args args =
  let b = Buffer.create 100 in
  let rec iter b need_space args =
    match args with
      [] -> ()
     | arg :: tail ->
       if need_space then Buffer.add_char b ' ';
       win_buf_escape_argument b arg;
       iter b true tail
  in
  iter b false args;
  Buffer.contents b

let rec safe_dup fd =
  let new_fd = dup fd in
  if Obj.magic new_fd >= 3 then (* TODO: windows incompatibility *)
    new_fd
  else begin
    let res = safe_dup fd in
    close new_fd;
    res
  end

let safe_close fd =
  try close fd with Unix_error(_,_,_) -> ()

let perform_redirections new_stdin new_stdout new_stderr =
  let newnewstdin = safe_dup new_stdin in
  let newnewstdout = safe_dup new_stdout in
  let newnewstderr = safe_dup new_stderr in
  safe_close new_stdin;
  safe_close new_stdout;
  safe_close new_stderr;
  dup2 newnewstdin stdin; close newnewstdin;
  dup2 newnewstdout stdout; close newnewstdout;
  dup2 newnewstderr stderr; close newnewstderr

let create_process error_handler cmd args maybe_chdir new_stdin new_stdout new_stderr =
  match OnlyUnix.fork() with
    0 ->
    begin try
      perform_redirections new_stdin new_stdout new_stderr;
      begin
        match maybe_chdir with
          None -> ()
        | Some dir ->
          MinUnix.chdir dir
      end;
      execvp cmd args
    with e ->
      error_handler e;
      exit 127
    end
  | id -> id


(* A cache for commands already found in the PATH *)
let in_path = Hashtbl.create 113
let home_dir = try Sys.getenv "HOME" with _ -> " "
let path = try OcpString.split (Sys.getenv "PATH")
  (if home_dir.[0] = '/' then ':' else ';') (* on Cygwin, use : instead of ; *)
 with _ -> ["."]

let test f =
  Printf.eprintf "Testing %s (%b,%b)\n%!" f
    (Sys.file_exists f)
    (try ignore (MinUnix.stat f); true with _ -> false)

let win_find_in_path cmd =
  try Hashtbl.find in_path cmd with Not_found ->
    let rec iter path cmd =
      match path with
        [] ->
        failwith (Printf.sprintf "BuildMisc.win_find_in_path: file %s not found in PATH\n%!" cmd)
       | dir :: tail ->
          let test1 = Filename.concat dir cmd in
(*          test test1; *)
          if Sys.file_exists test1 then test1 else
          let test2 = Filename.concat dir (cmd ^ ".exe") in
(*          test test2; *)
          if Sys.file_exists test2 then test2 else
          iter tail cmd
    in
    let cmd_found = iter path cmd in
    Hashtbl.add in_path cmd cmd_found;
    cmd_found

module Win32Unix = struct
(* High-level process management (system, popen) *)

let make_cmdline args =
  let maybe_quote f =
    if f = "" || String.contains f ' ' || String.contains f '\"'
    then Filename.quote f
    else f in
  String.concat " " (List.map maybe_quote (Array.to_list args))

let create_process_chdir prog args maybe_chdir fd1 fd2 fd3 =
  OnlyWin32.create_process_chdir
    prog (make_cmdline args) None fd1 fd2 fd3 maybe_chdir
end

let create_process error_handler cmd args maybe_chdir fd1 fd2 fd3 =
  if os_type = WINDOWS then
    let cmd = if Filename.is_implicit cmd then win_find_in_path cmd
      else cmd in
    Win32Unix.create_process_chdir cmd args maybe_chdir fd1 fd2 fd3
  else
    create_process error_handler cmd args maybe_chdir fd1 fd2 fd3
end

let open_for_pipe filename =
  let oc = open_out_bin filename in
  MinUnix.descr_of_out_channel oc

let create_process cmd maybe_chdir stdin stdout stderr =
  match cmd with
      [] -> assert false
    | cmd :: args ->
      let stdin_fd = match stdin with
	  None -> MinUnix.stdin
	| Some filename ->
           let ic = open_in_bin filename in
           MinUnix.descr_of_in_channel ic
      in
      let stdout_fd = match stdout with
	  None -> MinUnix.stdout
	| Some filename -> open_for_pipe filename
      in
      let stderr_fd = match stderr with
	  None -> MinUnix.stderr
	| Some filename -> open_for_pipe filename
      in
      let error_handler e =
        Printf.eprintf "Error while executing subprocess\n";
        Printf.eprintf "  exception %s\n%!" (Printexc.to_string e);
      in

      let pid = MinUnix2.create_process error_handler cmd (Array.of_list (cmd :: args)) maybe_chdir
	stdin_fd stdout_fd stderr_fd in
      (match stdin with None -> () | Some _ -> MinUnix.close stdin_fd);
      (match stdout with None -> () | Some _ -> MinUnix.close stdout_fd);
      (match stderr with None -> () | Some _ -> MinUnix.close stderr_fd);
      pid

let rec wait_command pid =
  try
    let rec iter pid =
      let (_, status) = MinUnix.waitpid [] pid in
      match status with
	  MinUnix.WEXITED n -> n
	| _ -> iter pid
    in
    iter pid
  with e ->
    Printf.eprintf "Exception %s in waitpid\n%!" (Printexc.to_string e);
    clean_exit 2

let rec uninterrupted_wait () =
  let rec iter () =
    try
      OnlyUnix.wait ()
    with MinUnix.Unix_error(MinUnix.EINTR, _, _) ->
      Printf.eprintf "MinUnix.wait interrupted. Restarting\n%!";
      iter ()
  in
  iter ()


let new_counter_int0 () =
  let counter = ref 0 in
  fun () ->
  let id = !counter in
  incr counter;
  id


let _ =
  Printexc.register_printer (fun exn ->
    match exn with
	MinUnix.Unix_error (error, s1, s2) ->
          Some (Printf.sprintf "Unix_error(%s, %s, %s)"
		  (MinUnix.error_message error) s1 s2)
      | _ -> None)
;;

let safe_mkdir = File.RawIO.safe_mkdir
(*
let rec safe_mkdir filename =
  try
    let st = MinUnix.stat filename in
    match st.MinUnix.st_kind with
	MinUnix.S_DIR ->
	  ()
      | _ ->
	Printf.fprintf stderr
	  "Error in BuildGlobals.safe_mkdir: %s is not a directory. Exiting.\n"
	  filename;
	exit 2
  with _ ->
    let dirname = Filename.dirname filename in
    safe_mkdir dirname;
    let basename = Filename.basename filename in
    match basename with
    | "." | ".." -> ()
    | _ ->
      MinUnix.mkdir filename 0o755
*)

let rename fa1 fa2 =
(*  let target_of_rename = fa2 in *)
  if MinUnix.os_type = MinUnix.WINDOWS &&
    Sys.file_exists fa2 then begin
      try (* on Windows, Sys.rename will fail if target
             exists.  This breaks atomicity of rename, so
             using -njobs > 1 might fail on Windows. There
             is an atomic rename available on Windows too,
             but only versions > XP (we should use it in the
             future). *)
        Sys.remove fa2;
      with e -> ()
    end;
  Sys.rename fa1 fa2

(* val get_stdout_lines : string list -> string list -> int * string list *)

let get_stdout_lines cmd args =
  let temp_file = Filename.temp_file "ocp-build-" ".out" in
  let pid = create_process (cmd@args) None None (Some temp_file) None in
  let status = wait_command pid in
  let lines = ref [] in
  begin try
	  let ic = open_in temp_file in
	  begin

	    try
	      while true do
		lines := (input_line ic) :: !lines
	      done
	    with _ -> ()
	  end;
	  close_in ic;
	  Sys.remove temp_file;
    with _ -> ()
  end;
  (status, List.rev !lines)

let getcwd_cache = ref None

let getcwd () =
  match !getcwd_cache with
    None ->
    let dir = MinUnix.getcwd () in
    getcwd_cache := Some dir;
    dir
  | Some dir -> dir

let chdir dir =
  MinUnix.chdir dir;
  getcwd_cache := None

let exists_as_directory filename =
  try
    Sys.is_directory filename
  with _ -> false


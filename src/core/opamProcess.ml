(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let log ?level fmt =
  OpamConsole.log "PROC" ?level fmt

(** Shell commands *)
type command = {
  cmd: OpamExternalTools.t;
  cmd_text: string option;
  cmd_dir: string option;
  cmd_env: string array option;
  cmd_stdin: bool option;
  cmd_stdout: string option;
  cmd_verbose: bool option;
  cmd_name: string option;
  cmd_metadata: (string * string) list option;
}

let string_of_command c =
  let cmd, args = OpamExternalTools.unpack c.cmd in
  String.concat " " (cmd::args)
let text_of_command c = c.cmd_text
let default_verbose () = OpamCoreConfig.(!r.verbose_level) >= 2
let is_verbose_command c =
  OpamStd.Option.default (default_verbose ()) c.cmd_verbose

let make_command_text ?(color=`green) str ?(args=[]) cmd =
  let summary =
    match
      List.filter (fun s ->
          String.length s > 0 && s.[0] <> '-' &&
          not (String.contains s '/') && not (String.contains s '='))
        args
    with
    | hd::_ -> String.concat " " [cmd; hd]
    | [] -> cmd
  in
  Printf.sprintf "[%s: %s]" (OpamConsole.colorise color str) summary

let command ?env ?verbose ?name ?metadata ?dir ?allow_stdin ?stdout ?text
    cmd =
  { cmd;
    cmd_env=env; cmd_verbose=verbose; cmd_name=name; cmd_metadata=metadata;
    cmd_dir=dir; cmd_stdin=allow_stdin; cmd_stdout=stdout; cmd_text=text; }


(** Running processes *)

type t = {
  p_name   : string;
  p_args   : string list;
  p_pid    : int;
  p_cwd    : string;
  p_time   : float;
  p_stdout : string option;
  p_stderr : string option;
  p_env    : string option;
  p_info   : string option;
  p_metadata: (string * string) list;
  p_verbose: bool;
  p_tmp_files: string list;
}

let open_flags =  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND]

let output_lines oc lines =
  List.iter (fun line ->
    output_string oc line;
    output_string oc "\n";
    flush oc;
  ) lines;
  output_string oc "\n";
  flush oc

let option_map fn = function
  | None   -> None
  | Some o -> Some (fn o)

let option_default d = function
  | None   -> d
  | Some v -> v

let make_info ?code ?signal
    ~cmd ~args ~cwd ~env_file ~stdout_file ~stderr_file ~metadata () =
  let b = ref [] in
  let home = OpamStd.Sys.home () in
  let print name str =
    let str =
      if OpamStd.String.starts_with ~prefix:home str
      then "~"^OpamStd.String.remove_prefix ~prefix:home str
      else str
    in
    b := (name, str) :: !b
  in
  let print_opt name = function
    | None   -> ()
    | Some s -> print name s in

  List.iter (fun (k,v) -> print k v) metadata;
  print     "path"         cwd;
  print     "command"      (String.concat " " (cmd :: args));
  print_opt "exit-code"    (option_map string_of_int code);
  print_opt "signalled"    (option_map string_of_int signal);
  print_opt "env-file"     env_file;
  if stderr_file = stdout_file then
    print_opt "output-file"  stdout_file
  else (
    print_opt "stdout-file"  stdout_file;
    print_opt "stderr-file"  stderr_file;
  );

  List.rev !b

let string_of_info ?(color=`yellow) info =
  let b = Buffer.create 1024 in
  List.iter
    (fun (k,v) -> Printf.bprintf b "%s %-20s %s\n"
        (OpamConsole.colorise color "#")
        (OpamConsole.colorise color k) v) info;
  Buffer.contents b

(** [create cmd args] create a new process to execute the command
    [cmd] with arguments [args]. If [stdout_file] or [stderr_file] are
    set, the channels are redirected to the corresponding files.  The
    outputs are discarded is [verbose] is set to false. The current
    environment can also be overriden if [env] is set. The environment
    which is used to run the process is recorded into [env_file] (if
    set). *)
let create ?info_file ?env_file ?(allow_stdin=true) ?stdout_file ?stderr_file ?env ?(metadata=[]) ?dir
    ~verbose ~tmp_files cmd =
  let cmd, args = OpamExternalTools.unpack cmd in
  let nothing () = () in
  let tee f =
    let fd = Unix.openfile f open_flags 0o644 in
    let close_fd () = Unix.close fd in
    fd, close_fd in
  let oldcwd = Sys.getcwd () in
  let cwd = OpamStd.Option.default oldcwd dir in
  OpamStd.Option.iter Unix.chdir dir;
  let stdin_fd,close_stdin =
    if allow_stdin then Unix.stdin, nothing else
    let fd,outfd = Unix.pipe () in
    let close_stdin () = Unix.close fd in
    Unix.close outfd; fd, close_stdin
  in
  let stdout_fd, close_stdout = match stdout_file with
    | None   -> Unix.stdout, nothing
    | Some f -> tee f in
  let stderr_fd, close_stderr = match stderr_file with
    | None   -> Unix.stderr, nothing
    | Some f ->
      if stdout_file = Some f then stdout_fd, nothing
      else tee f
  in
  let env = match env with
    | None   -> Unix.environment ()
    | Some e -> e in
  let time = Unix.gettimeofday () in

  let () =
    (* write the env file before running the command*)
    match env_file with
    | None   -> ()
    | Some f ->
      let chan = open_out f in
      let env = Array.to_list env in
      (* Remove dubious variables *)
      let env =
        List.filter (fun line -> not (OpamStd.String.contains_char line '$'))
          env
      in
      output_lines chan env;
      close_out chan in

  let () =
    (* write the info file *)
    match info_file with
    | None   -> ()
    | Some f ->
      let chan = open_out f in
      let info =
        make_info ~cmd ~args ~cwd ~env_file ~stdout_file ~stderr_file ~metadata () in
      output_string chan (string_of_info info);
      close_out chan in

  let pid =
    try
      Unix.create_process_env
        cmd
        (Array.of_list (cmd :: args))
        env
        stdin_fd stdout_fd stderr_fd
    with e ->
      close_stdin  ();
      close_stdout ();
      close_stderr ();
      raise e in
  close_stdin  ();
  close_stdout ();
  close_stderr ();
  Unix.chdir oldcwd;
  {
    p_name   = cmd;
    p_args   = args;
    p_pid    = pid;
    p_cwd    = cwd;
    p_time   = time;
    p_stdout = stdout_file;
    p_stderr = stderr_file;
    p_env    = env_file;
    p_info   = info_file;
    p_metadata = metadata;
    p_verbose = verbose;
    p_tmp_files = tmp_files;
  }

type result = {
  r_code     : int;
  r_signal   : int option;
  r_duration : float;
  r_info     : (string * string) list;
  r_stdout   : string list;
  r_stderr   : string list;
  r_cleanup  : string list;
}

(* XXX: the function might block for ever for some channels kinds *)
let read_lines f =
  try
    let ic = open_in f in
    let lines = ref [] in
    begin
      try
        while true do
          let line = input_line ic in
          lines := line :: !lines;
        done
      with End_of_file | Sys_error _ -> ()
    end;
    close_in ic;
    List.rev !lines
  with Sys_error _ -> []

(* Compat function (Windows) *)
let interrupt p =
  if OpamStd.Sys.is_windows then Unix.kill p.p_pid Sys.sigkill
  else Unix.kill p.p_pid Sys.sigint

let run_background command =
  let { cmd;
        cmd_env=env; cmd_verbose=_; cmd_name=name; cmd_text=_;
        cmd_metadata=metadata; cmd_dir=dir;
        cmd_stdin=allow_stdin; cmd_stdout } =
    command
  in
  let verbose = is_verbose_command command in
  let allow_stdin = OpamStd.Option.default false allow_stdin in
  let env = match env with Some e -> e | None -> Unix.environment () in
  let file ext = match name with
    | None -> None
    | Some n ->
      let d =
        if Filename.is_relative n then
          match dir with
            | Some d -> d
            | None -> OpamCoreConfig.(!r.log_dir)
        else ""
      in
      Some (Filename.concat d (Printf.sprintf "%s.%s" n ext))
  in
  let stdout_file =
    OpamStd.Option.Op.(cmd_stdout >>+ fun () -> file "out")
  in
  let stderr_file =
    if OpamCoreConfig.(!r.merged_output) then file "out" else file "err"
  in
  let env_file    = file "env" in
  let info_file   = file "info" in
  let tmp_files =
    OpamStd.List.filter_some [
      info_file;
      env_file;
      stderr_file;
      if cmd_stdout <> None || stderr_file = stdout_file then None
      else stdout_file;
    ]
  in
  create ~env ?info_file ?env_file ?stdout_file ?stderr_file ~verbose ?metadata
    ~allow_stdin ?dir ~tmp_files cmd

let dry_run_background c =
  let cmd, args = OpamExternalTools.unpack c.cmd in {
  p_name   = cmd;
  p_args   = args;
  p_pid    = -1;
  p_cwd    = OpamStd.Option.default (Sys.getcwd ()) c.cmd_dir;
  p_time   = Unix.gettimeofday ();
  p_stdout = None;
  p_stderr = None;
  p_env    = None;
  p_info   = None;
  p_metadata = OpamStd.Option.default [] c.cmd_metadata;
  p_verbose = is_verbose_command c;
  p_tmp_files = [];
}

let verbose_print_cmd p =
  OpamConsole.msg "%s %s %s%s\n"
    (OpamConsole.colorise `yellow "+")
    p.p_name
    (OpamStd.List.concat_map " " (Printf.sprintf "%S") p.p_args)
    (if p.p_cwd = Sys.getcwd () then ""
     else Printf.sprintf " (CWD=%s)" p.p_cwd)

let verbose_print_out =
  let pfx = OpamConsole.colorise `yellow "- " in
  fun s ->
    print_string pfx;
    print_string s;
    print_char '\n'

(** Semi-synchronous printing of the output of a command *)
let set_verbose_f, print_verbose_f, isset_verbose_f, stop_verbose_f =
  let verbose_f = ref None in
  let stop () = match !verbose_f with
    | None -> ()
    | Some (ics,_) ->
      List.iter close_in_noerr ics;
      verbose_f := None
  in
  let set files =
    stop ();
    (* implem relies on sigalrm, not implemented on win32.
       This will fall back to buffered output. *)
    if OpamStd.Sys.is_windows then () else
    let files = OpamStd.List.sort_nodup compare files in
    let ics =
      List.map
        (open_in_gen [Open_nonblock;Open_rdonly;Open_text;Open_creat] 0o600)
        files
    in
    let f () =
      List.iter (fun ic ->
          try while true do verbose_print_out (input_line ic) done
          with End_of_file -> flush stdout
        ) ics
    in
    verbose_f := Some (ics, f)
  in
  let print () = match !verbose_f with
    | Some (_, f) -> f ()
    | None -> ()
  in
  let isset () = !verbose_f <> None in
  let flush_and_stop () = print (); stop () in
  set, print, isset, flush_and_stop

let set_verbose_process p =
  if p.p_verbose then
    let fs = OpamStd.List.filter_some [p.p_stdout;p.p_stderr] in
    if fs <> [] then (
      verbose_print_cmd p;
      set_verbose_f fs
    )

let exit_status p return =
  let duration = Unix.gettimeofday () -. p.p_time in
  let stdout = option_default [] (option_map read_lines p.p_stdout) in
  let stderr = option_default [] (option_map read_lines p.p_stderr) in
  let cleanup = p.p_tmp_files in
  let code,signal = match return with
    | Unix.WEXITED r -> Some r, None
    | Unix.WSIGNALED s | Unix.WSTOPPED s -> None, Some s
  in
  if isset_verbose_f () then
    stop_verbose_f ()
  else if p.p_verbose then
    (verbose_print_cmd p;
     List.iter verbose_print_out stdout;
     List.iter verbose_print_out stderr;
     flush Pervasives.stdout);
  let info =
    make_info ?code ?signal
      ~cmd:p.p_name ~args:p.p_args ~cwd:p.p_cwd ~metadata:p.p_metadata
      ~env_file:p.p_env ~stdout_file:p.p_stdout ~stderr_file:p.p_stderr () in
  {
    r_code     = OpamStd.Option.default 256 code;
    r_signal   = signal;
    r_duration = duration;
    r_info     = info;
    r_stdout   = stdout;
    r_stderr   = stderr;
    r_cleanup  = cleanup;
  }

let safe_wait fallback_pid f x =
  let sh =
    if isset_verbose_f () then
      let hndl _ = print_verbose_f () in
      Some (Sys.signal Sys.sigalrm (Sys.Signal_handle hndl))
    else None
  in
  let cleanup () =
    match sh with
    | Some sh ->
      ignore (Unix.alarm 0); (* cancels the alarm *)
      Sys.set_signal Sys.sigalrm sh
    | None -> ()
  in
  let rec aux () =
    if sh <> None then ignore (Unix.alarm 1);
    match
      try f x with
      | Unix.Unix_error (Unix.EINTR,_,_) -> aux () (* handled signal *)
      | Unix.Unix_error (Unix.ECHILD,_,_) ->
        log "Warn: no child to wait for %d" fallback_pid;
        fallback_pid, Unix.WEXITED 256
      with
      | _, Unix.WSTOPPED _ ->
        (* shouldn't happen as we don't use WUNTRACED *)
        aux ()
      | r -> r
  in
  try let r = aux () in cleanup (); r
  with e -> cleanup (); raise e

let wait p =
  set_verbose_process p;
  let _, return = safe_wait p.p_pid (Unix.waitpid []) p.p_pid in
  exit_status p return

let dontwait p =
  match safe_wait p.p_pid (Unix.waitpid [Unix.WNOHANG]) p.p_pid with
  | 0, _ -> None
  | _, return -> Some (exit_status p return)

let dead_childs = Hashtbl.create 13
let wait_one processes =
  if processes = [] then raise (Invalid_argument "wait_one");
  if OpamStd.Sys.is_windows then
    (* No waiting for any child pid on Windows, this is highly sub-optimal
       but should at least work. Todo: C binding for better behaviour *)
    let p = List.hd processes in
    p, wait p
  else
  try
    let p =
      List.find (fun p -> Hashtbl.mem dead_childs p.p_pid) processes
    in
    let return = Hashtbl.find dead_childs p.p_pid in
    Hashtbl.remove dead_childs p.p_pid;
    p, exit_status p return
  with Not_found ->
    let rec aux () =
      let pid, return = safe_wait (List.hd processes).p_pid Unix.wait () in
      try
        let p = List.find (fun p -> p.p_pid = pid) processes in
        p, exit_status p return
      with Not_found ->
        Hashtbl.add dead_childs pid return;
        aux ()
    in
    aux ()

let dry_wait_one = function
  | {p_pid = -1; _} as p :: _ ->
    if p.p_verbose then (verbose_print_cmd p; flush stdout);
    p,
    { r_code = 0;
      r_signal = None;
      r_duration = 0.;
      r_info = [];
      r_stdout = [];
      r_stderr = [];
      r_cleanup = []; }
  | _ -> raise (Invalid_argument "dry_wait_one")

let run command =
  let command =
    { command with
      cmd_stdin = OpamStd.Option.Op.(command.cmd_stdin ++ Some true) }
  in
  let p = run_background command in
  try wait p with e ->
    match (try dontwait p with _ -> raise e) with
    | None -> (* still running *)
      (try interrupt p with Unix.Unix_error _ -> ());
      raise e
    | _ -> raise e

let is_failure r = r.r_code <> 0 || r.r_signal <> None

let is_success r = not (is_failure r)

let safe_unlink f =
  try
    log ~level:2 "safe_unlink: %s" f;
    Unix.unlink f
  with Unix.Unix_error _ ->
    log ~level:2 "safe_unlink: %s (FAILED)" f

let cleanup ?(force=false) r =
  if force || (not (OpamConsole.debug ()) && is_success r) then
    List.iter safe_unlink r.r_cleanup

let check_success_and_cleanup r =
  List.iter safe_unlink r.r_cleanup;
  is_success r

let log_line_limit = 5 * 80
let truncate_str = "[...]"

(* Truncate long lines *)
let truncate_line str =
  if String.length str <= log_line_limit then
    str
  else
    String.sub str 0 (log_line_limit - String.length truncate_str)
    ^ truncate_str

(* Take the last [n] elements of [l] (trying to keep an unindented header line
   for context, like diff) *)
let truncate l =
  let unindented s =
    String.length s > 0 && s.[0] <> ' ' && s.[0] <> '\t'
  in
  let rec cut n acc = function
    | [] -> acc
    | [x] when n = 0 -> truncate_line x :: acc
    | _ when n = 0 -> truncate_str :: acc
    | x::l when n = 1 ->
      (if unindented x then truncate_str :: truncate_line x :: acc else
       try truncate_line (List.find unindented l) :: truncate_str :: acc
       with Not_found -> truncate_str :: truncate_line x :: acc)
    | x::r -> cut (n-1) (truncate_line x :: acc) r
  in
  let len = OpamCoreConfig.(!r.errlog_length) in
  if len <= 0 then l
  else cut len [] (List.rev l)

let string_of_result ?(color=`yellow) r =
  let b = Buffer.create 2048 in
  let print = Buffer.add_string b in
  let println str =
    print str;
    Buffer.add_char b '\n' in

  print (string_of_info ~color r.r_info);

  if r.r_stdout <> [] then
    if r.r_stderr = r.r_stdout then
      print (OpamConsole.colorise color "### output ###\n")
    else
      print (OpamConsole.colorise color "### stdout ###\n");
  List.iter (fun s ->
      print (OpamConsole.colorise color "# ");
      println s)
    (truncate r.r_stdout);

  if r.r_stderr <> [] && r.r_stderr <> r.r_stdout then (
    print (OpamConsole.colorise color "### stderr ###\n");
    List.iter (fun s ->
        print (OpamConsole.colorise color "# ");
        println s)
      (truncate r.r_stderr)
  );

  Buffer.contents b

let result_summary r =
  Printf.sprintf "%S exited with code %d%s"
    (try List.assoc "command" r.r_info with Not_found -> "command")
    r.r_code
    (if r.r_code = 0 then "" else
     match r.r_stderr, r.r_stdout with
     | [e], _ | [], [e] -> Printf.sprintf " \"%s\"" e
     | [], es | es, _ ->
       try
         Printf.sprintf " \"%s\""
           (List.find
              Re.(execp (compile (seq [ bos; rep (diff any alpha);
                                        no_case (str "error") ])))
              (List.rev es))
       with Not_found -> ""
     | _ -> "")

(* Higher-level interface to allow parallelism *)

module Job = struct
  module Op = struct
    type 'a job = (* Open the variant type *)
      | Done of 'a
      | Run of command * (result -> 'a job)

    (* Parallelise shell commands *)
    let (@@>) command f = Run (command, f)

    (* Sequentialise jobs *)
    let rec (@@+) job1 fjob2 = match job1 with
      | Done x -> fjob2 x
      | Run (cmd,cont) -> Run (cmd, fun r -> cont r @@+ fjob2)

    let (@@|) job f = job @@+ fun x -> Done (f x)
  end

  open Op

  let run =
    let rec aux = function
      | Done x -> x
      | Run (cmd,cont) ->
        OpamStd.Option.iter
          (if OpamConsole.disp_status_line () then
             OpamConsole.status_line "Processing: %s"
           else OpamConsole.msg "%s\n")
          (text_of_command cmd);
        let r = run cmd in
        let k = try cont r with e -> cleanup r; raise e in
        cleanup r;
        aux k
    in
    aux

  let rec dry_run = function
    | Done x -> x
    | Run (_command,cont) ->
      let result = { r_code = 0;
                     r_signal = None;
                     r_duration = 0.;
                     r_info = [];
                     r_stdout = [];
                     r_stderr = [];
                     r_cleanup = []; }
      in dry_run (cont result)

  let rec catch handler fjob =
    try match fjob () with
      | Done x -> Done x
      | Run (cmd,cont) ->
        Run (cmd, fun r -> catch handler (fun () -> cont r))
    with e -> handler e

  let ignore_errors ~default ?message job =
    catch (fun e ->
        OpamStd.Exn.fatal e;
        OpamStd.Option.iter (OpamConsole.error "%s") message;
        Done default)
      job

  let rec finally fin fjob =
    try match fjob () with
      | Done x -> fin (); Done x
      | Run (cmd,cont) ->
        Run (cmd, fun r -> finally fin (fun () -> cont r))
    with e -> fin (); raise e

  let of_list ?(keep_going=false) l =
    let rec aux err = function
      | [] -> Done err
      | cmd::commands ->
        let cont = fun r ->
          if is_success r then aux err commands
          else if keep_going then
            aux OpamStd.Option.Op.(err ++ Some (cmd,r)) commands
          else Done (Some (cmd,r))
        in
        Run (cmd,cont)
    in
    aux None l

  let of_fun_list ?(keep_going=false) l =
    let rec aux err = function
      | [] -> Done err
      | cmdf::commands ->
        let cmd = cmdf () in
        let cont = fun r ->
          if is_success r then aux err commands
          else if keep_going then
            aux OpamStd.Option.Op.(err ++ Some (cmd,r)) commands
          else Done (Some (cmd,r))
        in
        Run (cmd,cont)
    in
    aux None l

  let seq job start = List.fold_left (@@+) (Done start) job

  let seq_map f l =
    List.fold_left (fun job x ->
        job @@+ fun acc -> f x @@| fun y -> y :: acc)
      (Done []) l
    @@| List.rev

  let rec with_text text = function
    | Done _ as j -> j
    | Run (cmd, cont) ->
      Run ({cmd with cmd_text = Some text}, fun r -> with_text text (cont r))
end

type 'a job = 'a Job.Op.job

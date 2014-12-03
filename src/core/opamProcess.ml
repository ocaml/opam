(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(** Shell commands *)
type command = {
  cmd: string;
  args: string list;
  cmd_text: string option;
  cmd_dir: string option;
  cmd_env: string array option;
  cmd_stdin: bool option;
  cmd_verbose: bool option;
  cmd_name: string option;
  cmd_metadata: (string * string) list option;
}

let string_of_command c = String.concat " " (c.cmd::c.args)
let text_of_command c = c.cmd_text

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
  Printf.sprintf "[%s: %s]" (OpamGlobals.colorise color str) summary

let command ?env ?verbose ?name ?metadata ?dir ?allow_stdin ?text cmd args =
  { cmd; args;
    cmd_env=env; cmd_verbose=verbose; cmd_name=name; cmd_metadata=metadata;
    cmd_dir=dir; cmd_stdin=allow_stdin; cmd_text=text; }


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

let make_info ?code ~cmd ~args ~cwd ~env_file ~stdout_file ~stderr_file ~metadata () =
  let b = ref [] in
  let print name str = b := (name, str) :: !b in
  let print_opt name = function
    | None   -> ()
    | Some s -> print name s in

  print     "opam-version" (OpamVersion.to_string OpamVersion.full);
  print     "os"           (OpamGlobals.os_string ());
  print     "command"      (String.concat " " (cmd :: args));
  print     "path"         cwd;
  List.iter (fun (k,v) -> print k v) metadata;
  print_opt "exit-code"    (option_map string_of_int code);
  print_opt "env-file"     env_file;
  print_opt "stdout-file"  stdout_file;
  print_opt "stderr-file"  stderr_file;

  List.rev !b

let string_of_info ?(color=`yellow) info =
  let b = Buffer.create 1024 in
  List.iter
    (fun (k,v) -> Printf.bprintf b "%s %-20s %s\n"
        (OpamGlobals.colorise color "#")
        (OpamGlobals.colorise color k) v) info;
  Buffer.contents b

(** [create cmd args] create a new process to execute the command
    [cmd] with arguments [args]. If [stdout_file] or [stderr_file] are
    set, the channels are redirected to the corresponding files.  The
    outputs are discarded is [verbose] is set to false. The current
    environment can also be overriden if [env] is set. The environment
    which is used to run the process is recorded into [env_file] (if
    set). *)
let create ?info_file ?env_file ?(allow_stdin=true) ?stdout_file ?stderr_file ?env ?(metadata=[]) ?dir
    ~verbose cmd args =
  let nothing () = () in
  let tee f =
    let fd = Unix.openfile f open_flags 0o644 in
    let close_fd () = Unix.close fd in
    fd, close_fd in
  let oldcwd = Sys.getcwd () in
  let cwd = OpamMisc.Option.default oldcwd dir in
  OpamMisc.Option.iter Unix.chdir dir;
  let stdin_fd =
    if allow_stdin then Unix.stdin else
    let fd,outfd = Unix.pipe () in
    Unix.close outfd; fd
  in
  let stdout_fd, close_stdout = match stdout_file with
    | None   -> Unix.stdout, nothing
    | Some f -> tee f in
  let stderr_fd, close_stderr = match stderr_file with
    | None   -> Unix.stderr, nothing
    | Some f -> tee f in
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
      let env = List.filter (fun line -> not (OpamMisc.contains line '$')) env in
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
    Unix.create_process_env
      cmd
      (Array.of_list (cmd :: args))
      env
      stdin_fd stdout_fd stderr_fd in
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
  }

type result = {
  r_code     : int;
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
let interrupt p = match OpamGlobals.os () with
  | OpamGlobals.Win32 -> Unix.kill p.p_pid Sys.sigkill
  | _ -> Unix.kill p.p_pid Sys.sigint

let run_background command =
  let { cmd; args;
        cmd_env=env; cmd_verbose=verbose; cmd_name=name;
        cmd_metadata=metadata; cmd_dir=dir; cmd_stdin=allow_stdin } =
    command
  in
  let verbose = OpamMisc.Option.default !OpamGlobals.verbose verbose in
  let allow_stdin = OpamMisc.Option.default false allow_stdin in
  let env = match env with Some e -> e | None -> Unix.environment () in
  let file ext = match name with
    | None -> None
    | Some n ->
      let d =
        if Filename.is_relative n then
          match dir with
            | Some d -> d
            | None -> (Filename.concat !OpamGlobals.root_dir "log")
        else ""
      in
      Some (Filename.concat d (Printf.sprintf "%s.%s" n ext))
  in
  let stdout_file = file "out" in
  let stderr_file = file "err" in
  let env_file    = file "env" in
  let info_file   = file "info" in
  create ~env ?info_file ?env_file ?stdout_file ?stderr_file ~verbose ?metadata
    ~allow_stdin ?dir cmd args

let exit_status p code =
  let duration = Unix.gettimeofday () -. p.p_time in
  let stdout = option_default [] (option_map read_lines p.p_stdout) in
  let stderr = option_default [] (option_map read_lines p.p_stderr) in
  let cleanup =
    OpamMisc.filter_map (fun x -> x) [ p.p_info; p.p_env; p.p_stderr; p.p_stdout ]
  in
  if p.p_verbose then
    (OpamGlobals.msg "%s %s %s\n" (OpamGlobals.colorise `yellow "+")
       p.p_name (String.concat " " (List.map (Printf.sprintf "%S") p.p_args));
     let pfx = OpamGlobals.colorise `yellow "- " in
     let p s = print_string pfx; print_string s; print_newline () in
     List.iter p stdout;
     List.iter p stderr;
     flush Pervasives.stdout);
  let info =
    make_info ~code ~cmd:p.p_name ~args:p.p_args ~cwd:p.p_cwd ~metadata:p.p_metadata
      ~env_file:p.p_env ~stdout_file:p.p_stdout ~stderr_file:p.p_stderr () in
  {
    r_code     = code;
    r_duration = duration;
    r_info     = info;
    r_stdout   = stdout;
    r_stderr   = stderr;
    r_cleanup  = cleanup;
  }

let rec wait p =
  match
    try Some (Unix.waitpid [] p.p_pid)
    with Unix.Unix_error (Unix.EINTR,_,_) -> None (* handled signal *)
  with
  | Some (_, Unix.WEXITED code) -> exit_status p code
  | _ -> wait p

let rec dontwait p =
  match Unix.waitpid [Unix.WNOHANG] p.p_pid with
  | 0, _ -> None
  | _, Unix.WEXITED code -> Some (exit_status p code)
  | _, _ -> dontwait p

let dead_childs = Hashtbl.create 13
let wait_one processes =
  if processes = [] then raise (Invalid_argument "wait_one");
  if OpamGlobals.os () = OpamGlobals.Win32 then
    (* No waiting for any child pid on Windows, this is highly sub-optimal
       but should at least work. Todo: C binding for better behaviour *)
    let p = List.hd processes in
    let rec aux () = match Unix.waitpid [] p.p_pid with
      | _, Unix.WEXITED code ->
        p, exit_status p code
      | _ -> aux ()
    in
    aux ()
  else
  try
    let p =
      List.find (fun p -> Hashtbl.mem dead_childs p.p_pid) processes
    in
    let code = Hashtbl.find dead_childs p.p_pid in
    Hashtbl.remove dead_childs p.p_pid;
    p, exit_status p code
  with Not_found ->
    let rec aux () =
      try
        match Unix.wait () with
        | pid, Unix.WEXITED code ->
          (try
             let p = List.find (fun p -> p.p_pid = pid) processes in
             p, exit_status p code
           with Not_found ->
           Hashtbl.add dead_childs pid code;
           aux ())
        | _ -> aux ()
      with Unix.Unix_error (Unix.EINTR,_,_) ->
        aux () (* Happens on handled signal *)
    in
    aux ()

let run command =
  let command =
    { command with
      cmd_stdin = OpamMisc.Option.Op.(command.cmd_stdin ++ Some true) }
  in
  let p = run_background command in
  try wait p with e ->
    match (try dontwait p with _ -> raise e) with
    | None -> (* still running *)
      (try interrupt p with Unix.Unix_error _ -> ());
      raise e
    | _ -> raise e

let is_success r = r.r_code = 0

let is_failure r = r.r_code <> 0

let safe_unlink f =
  try Unix.unlink f with Unix.Unix_error _ -> ()

let cleanup ?(force=false) r =
  if force || not !OpamGlobals.debug || is_success r then
    List.iter safe_unlink r.r_cleanup

let truncate_str = "[...]"

(* Truncate long lines *)
let truncate_line str =
  if String.length str <= OpamGlobals.log_line_limit then
    str
  else
    String.sub str 0 (OpamGlobals.log_line_limit - String.length truncate_str)
    ^ truncate_str

(* Take the last [n] elements of [l] (trying to keep an unindented header line
   for context, like diff) *)
let truncate l =
  let l = List.rev l in
  let rec cut n acc = function
    | [] -> acc
    | [x] when n = 0 -> x :: acc
    | _ when n = 0 -> truncate_str :: acc
    | x::_ as l when n = 1 ->
      (try
         List.find
           (fun s -> String.length s > 0 && s.[0] <> ' ' && s.[0] <> '\t')
           l
         :: truncate_str :: acc
       with Not_found ->
         truncate_str :: x :: acc)
    | x::r -> cut (n-1) (x::acc) r
  in
  cut OpamGlobals.log_limit [] l

let string_of_result ?(color=`yellow) r =
  let b = Buffer.create 2048 in
  let print = Buffer.add_string b in
  let println str =
    print str;
    Buffer.add_char b '\n' in

  print (string_of_info ~color r.r_info);

  if r.r_stdout <> [] then
    print (OpamGlobals.colorise color "### stdout ###\n");
  List.iter (fun s ->
      print (OpamGlobals.colorise color "# ");
      println s)
    (truncate r.r_stdout);

  if r.r_stderr <> [] then
    print (OpamGlobals.colorise color "### stderr ###\n");
  List.iter (fun s ->
      print (OpamGlobals.colorise color "# ");
      println s)
    (truncate r.r_stderr);

  Buffer.contents b


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
        OpamMisc.Option.iter
          (if OpamGlobals.disp_status_line () then
             OpamGlobals.status_line "Processing: %s"
           else OpamGlobals.msg "%s\n")
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
                     r_duration = 0.;
                     r_info = [];
                     r_stdout = [];
                     r_stderr = [];
                     r_cleanup = []; }
      in dry_run (cont result)

  let rec catch handler = function
    | Done x -> Done x
    | Run (cmd,cont) ->
      let cont r =
        match
          try `Cont (cont r) with e -> `Hndl (handler e)
        with
        | `Cont job -> catch handler job
        | `Hndl job -> job
      in
      Run (cmd, cont)

  let rec finally fin = function
    | Done x -> fin (); Done x
    | Run (cmd,cont) ->
      Run (cmd, fun r -> finally fin (try cont r with e -> fin (); raise e))

  let of_list ?(keep_going=false) l =
    let rec aux err = function
      | [] -> Done err
      | cmd::commands ->
        let cont = fun r ->
          if is_success r then aux err commands
          else if keep_going then
            aux OpamMisc.Option.Op.(err ++ Some (cmd,r)) commands
          else Done (Some (cmd,r))
        in
        Run (cmd,cont)
    in
    aux None l

  let rec with_text text = function
    | Done _ as j -> j
    | Run (cmd, cont) ->
      Run ({cmd with cmd_text = Some text}, fun r -> with_text text (cont r))
end

type 'a job = 'a Job.Op.job

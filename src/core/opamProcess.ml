(***********************************************************************)
(*                                                                     *)
(*    Copyright 2011-2012 OCamlPro                                     *)
(*    Copyright 2011-2012 INRIA                                        *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

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
}

let open_flags =  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]

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

let make_info ?code ~cmd ~args ~cwd ~env_file ~stdout_file ~stderr_file () =
  let b = Buffer.create 2048 in
  let print name str =
    Printf.bprintf b "# %-15s %s\n" name str in
  let print_opt name = function
    | None   -> ()
    | Some s -> print name s in

  print     "command" (String.concat " " (cmd :: args));
  print     "path"   cwd;
  print_opt "exit-code" (option_map string_of_int code);
  print_opt "env-file" env_file;
  print_opt "stdout-file" stdout_file;
  print_opt "stderr-file" stderr_file;

  Buffer.contents b

let create ?info_file ?env_file ?stdout_file ?stderr_file ?env ~verbose cmd args =
  let nothing () = () in
  let tee f =
    let fd = Unix.openfile f open_flags 0o644 in
    let close_fd () = Unix.close fd in
    if verbose then (
      let chan = Unix.open_process_out ("tee " ^ Filename.quote f) in
      let close () =
        match Unix.close_process_out chan with
        | _ -> close_fd () in
      Unix.descr_of_out_channel chan, close
    ) else
      fd, close_fd in
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
  let cwd = Sys.getcwd () in

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
      let info = make_info ~cmd ~args ~cwd ~env_file ~stdout_file ~stderr_file () in
      output_string chan info;
      close_out chan in

  let pid =
    Unix.create_process_env
      cmd
      (Array.of_list (cmd :: args))
      env
      Unix.stdin stdout_fd stderr_fd in
  close_stdout ();
  close_stderr ();
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
  }

type result = {
  r_code     : int;
  r_duration : float;
  r_info     : string;
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
      with _ -> ()
    end;
    close_in ic;
    List.rev !lines
  with _ -> []

let wait p =
  try
    let rec iter () =
      let _, status = Unix.waitpid [] p.p_pid in
      match status with
        | Unix.WEXITED code ->
          let duration = Unix.gettimeofday () -. p.p_time in
          let stdout = option_default [] (option_map read_lines p.p_stdout) in
          let stderr = option_default [] (option_map read_lines p.p_stderr) in
          let cleanup =
            OpamMisc.filter_map (fun x -> x) [ p.p_info; p.p_env; p.p_stderr; p.p_stdout ] in
          let info =
            make_info ~code ~cmd:p.p_name ~args:p.p_args ~cwd:p.p_cwd
              ~env_file:p.p_env ~stdout_file:p.p_stdout ~stderr_file:p.p_stderr () in
          {
            r_code     = code;
            r_duration = duration;
            r_info     = info;
            r_stdout   = stdout;
            r_stderr   = stderr;
            r_cleanup  = cleanup;
          }
        | _ -> iter () in
    iter ()
  with e ->
    OpamGlobals.error "Exception %s in waitpid" (Printexc.to_string e);
    OpamGlobals.exit 2

let run ?env ~verbose ~name cmd args =
  try
    let stdout_file = Printf.sprintf "%s.out" name in
    let stderr_file = Printf.sprintf "%s.err" name in
    let env_file    = Printf.sprintf "%s.env" name in
    let info_file   = Printf.sprintf "%s.info" name in
    let env = match env with Some e -> e | None -> Unix.environment () in

    let p = create ~env ~info_file ~env_file ~stdout_file ~stderr_file ~verbose cmd args in
    wait p
  with e ->
    OpamGlobals.error "Exception %s in run" (Printexc.to_string e);
    OpamGlobals.exit 4

let is_success r = r.r_code = 0

let is_failure r = r.r_code <> 0

let safe_unlink f =
  try Unix.unlink f with _ -> ()

let clean_files r =
  List.iter safe_unlink r.r_cleanup

let truncate_str = "...[truncated]"

(* Truncate long lines *)
let truncate_line str =
  if String.length str <= OpamGlobals.log_line_limit then
    str
  else
    String.sub str 0 (OpamGlobals.log_line_limit) ^ truncate_str

(* Take the last [n] elements of [l] *)
let rec truncate = function
  | [] -> []
  | l  ->
    if List.length l < OpamGlobals.log_limit then
      List.map truncate_line l
    else if List.length l = OpamGlobals.log_limit then
      truncate_str :: l
    else match l with
    | []     -> []
    | _ :: t -> truncate t

let string_of_result r =
  let b = Buffer.create 2048 in
  let print = Buffer.add_string b in
  let println str =
    print str;
    Buffer.add_char b '\n' in

  print r.r_info;

  if r.r_stdout <> [] then
    print "### stdout ###\n";
  List.iter println (truncate r.r_stdout);

  if r.r_stderr <> [] then
    print "### stderr ###\n";
  List.iter println (truncate r.r_stderr);

  Buffer.contents b

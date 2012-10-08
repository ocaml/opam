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
  p_name   : string;        (* Command name *)
  p_args   : string list;   (* Command args *)
  p_pid    : int;           (* Process PID *)
  p_time   : float;         (* Process start time *)
  p_stdout : string option; (* stdout dump file *)
  p_stderr : string option; (* stderr dump file *)
  p_info   : string option; (* dump info file *)
}

let open_flags =  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]

let create ?info ?stdout ?stderr ?env ~verbose cmd args =
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
  let stdout_fd, close_stdout = match stdout with
    | None   -> Unix.stdout, nothing
    | Some f -> tee f in
  let stderr_fd, close_stderr = match stderr with
    | None   -> Unix.stderr, nothing
    | Some f -> tee f in
  let env = match env with
    | None   -> Unix.environment ()
    | Some e -> e in
  let time = Unix.gettimeofday () in
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
    p_time   = time;
    p_stdout = stdout;
    p_stderr = stderr;
    p_info   = info;
  }

type result = {
  r_proc     : t;           (* Process *)
  r_code     : int;         (* Process exit code *)
  r_duration : float;       (* Process duration *)
  r_info     : string list; (* Env variables *)
  r_stdout   : string list; (* Content of stdout dump file *)
  r_stderr   : string list; (* Content of stderr dump file *)
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

let option_map fn = function
  | None   -> None
  | Some o -> Some (fn o)

let option_default d = function
  | None   -> d
  | Some v -> v

let wait p =
  try
    let rec iter () =
      let _, status = Unix.waitpid [] p.p_pid in
      match status with
        | Unix.WEXITED code ->
          let duration = Unix.gettimeofday () -. p.p_time in
          let info =
            option_default [] (option_map read_lines p.p_info) in
          let stdout =
            option_default [] (option_map read_lines p.p_stdout) in
          let stderr =
            option_default [] (option_map read_lines p.p_stderr) in
          {
            r_proc     = p;
            r_code     = code;
            r_duration = duration;
            r_info     = info;
            r_stdout   = stdout;
            r_stderr   = stderr;
          }
        | _ -> iter () in
    iter ()
  with e ->
    OpamGlobals.error "Exception %s in waitpid" (Printexc.to_string e);
    OpamGlobals.exit 2

let output_lines oc lines =
  List.iter (fun line ->
    output_string oc line;
    output_string oc "\n";
    flush oc;
  ) lines;
  output_string oc "\n";
  flush oc

let run ?env ~verbose ~name cmd args =
  try
    let stdout = Printf.sprintf "%s.out" name in
    let stderr = Printf.sprintf "%s.err" name in
    let info   = Printf.sprintf "%s.info" name in

    let env = match env with Some e -> e | None -> Unix.environment () in

    (* Write info file *)
    let chan = open_out info in
    output_lines chan
      [ Printf.sprintf "[RUN] %S" (String.concat " " (cmd :: args)) ;
        Printf.sprintf "[CWD] %S" (Unix.getcwd ()) ;
        String.concat "\n" (Array.to_list env)
      ];
    close_out chan;

    let p = create ~env ~info ~stdout ~stderr ~verbose cmd args in
    wait p
  with e ->
    OpamGlobals.error "Exception %s in run" (Printexc.to_string e);
    OpamGlobals.exit 2

let is_success r = r.r_code = 0

let is_failure r = r.r_code <> 0

let option_iter fn = function
  | None   -> ()
  | Some v -> fn v

let safe_unlink f =
  try Unix.unlink f with _ -> ()

let clean_files r =
  option_iter safe_unlink r.r_proc.p_stdout;
  option_iter safe_unlink r.r_proc.p_stderr;
  option_iter safe_unlink r.r_proc.p_info

let display_error_message r =
  if is_failure r then (
    List.iter (OpamGlobals.error "= %s") r.r_info;
    List.iter (OpamGlobals.error ". %s") r.r_stdout;
    List.iter (OpamGlobals.error "* %s") r.r_stderr;
  )

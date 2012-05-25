(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                 Thomas Gazagnaire, Fabrice Le Fessant                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

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

let create ?info ?stdout ?stderr ?env cmd args =
  let stdout_fd = match stdout with
    | None   -> Unix.stdout
    | Some f -> Unix.openfile f open_flags 0o644 in
  let stderr_fd = match stderr with
    | None   -> Unix.stderr
    | Some f -> Unix.openfile f open_flags 0o644 in
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
  (match stdout with None -> () | Some _ -> Unix.close stdout_fd);
  (match stderr with None -> () | Some _ -> Unix.close stderr_fd);
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
  r_stdout   : string list; (* Content of stdout dump file *)
  r_stderr   : string list; (* Content of stderr dump file *)
}

(* XXX: the function might block for ever for some channels kinds *)
let read_lines f =
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
          let stdout =
            option_default [] (option_map read_lines p.p_stdout) in
          let stderr =
            option_default [] (option_map read_lines p.p_stderr) in
          {
            r_proc     = p;
            r_code     = code;
            r_duration = duration;
            r_stdout   = stdout;
            r_stderr   = stderr;
          }
        | _ -> iter () in
    iter ()
  with e ->
    Printf.printf "Exception %s in waitpid\n%!" (Printexc.to_string e);
    exit 2

let output_lines oc lines =
  List.iter (fun line ->
    output_string oc line;
    output_string oc "\n";
    flush oc;
  ) lines;
  output_string oc "\n";
  flush oc

let run ?env ~name cmd args =
  let stdout = Printf.sprintf "%s.out" name in
  let stderr = Printf.sprintf "%s.err" name in
  let info   = Printf.sprintf "%s.info" name in
  
  let env = match env with Some e -> e | None -> Unix.environment () in

  (* Write info file *)
  let chan = open_out info in
  output_lines chan
    [ String.concat " " (cmd :: args) ;
      Unix.getcwd () ;
      String.concat "\n" (Array.to_list env)
    ];
  close_out chan;

  let p = create ~env ~info ~stdout ~stderr cmd args in
  wait p

let is_success r = r.r_code = 0

let is_failure r = r.r_code <> 0

let option_iter fn = function
  | None   -> ()
  | Some v -> fn v

let clean_files r =
  option_iter Unix.unlink r.r_proc.p_stdout;
  option_iter Unix.unlink r.r_proc.p_stderr;
  option_iter Unix.unlink r.r_proc.p_info

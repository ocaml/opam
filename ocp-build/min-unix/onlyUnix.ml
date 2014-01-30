

open MinUnix


external wait : unit -> int * process_status = "unix_wait"
external fork : unit -> int = "unix_fork"
external getppid : unit -> int = "unix_getppid"
external nice : int -> int = "unix_nice"

external truncate : string -> int -> unit = "unix_truncate"
external ftruncate : file_descr -> int -> unit = "unix_ftruncate"

let system cmd =
  match fork() with
     0 -> begin try
            execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
          with _ ->
            exit 127
          end
  | id -> snd(waitpid [] id)

external isatty : file_descr -> bool = "unix_isatty"

type dir_handle

external opendir : string -> dir_handle = "unix_opendir"
external readdir : dir_handle -> string = "unix_readdir"
external rewinddir : dir_handle -> unit = "unix_rewinddir"
external closedir : dir_handle -> unit = "unix_closedir"
external chroot : string -> unit = "unix_chroot"

external symlink : string -> string -> unit = "unix_symlink"
external readlink : string -> string = "unix_readlink"
external mkfifo : string -> file_perm -> unit = "unix_mkfifo"

external fchmod : file_descr -> file_perm -> unit = "unix_fchmod"
external chown : string -> int -> int -> unit = "unix_chown"
external fchown : file_descr -> int -> int -> unit = "unix_fchown"
external umask : int -> int = "unix_umask"

(* High-level process management (system, popen) *)

external fd_of_filedescr : file_descr -> int = "%identity"

let rec safe_dup fd =
  let new_fd = dup fd in
  if fd_of_filedescr new_fd >= 3 then
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

let create_process cmd args new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      begin try
        perform_redirections new_stdin new_stdout new_stderr;
        execvp cmd args
      with _ ->
        exit 127
      end
  | id -> id

let create_process_env cmd args env new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      begin try
        perform_redirections new_stdin new_stdout new_stderr;
        execvpe cmd args env
      with _ ->
        exit 127
      end
  | id -> id

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
  | Process_full of in_channel * out_channel * in_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

(* FD_CLOEXEC should be supported on all Unix systems these days,
   but just in case... *)
let try_set_close_on_exec fd =
  try set_close_on_exec fd; true with Invalid_argument _ -> false

let open_proc cmd proc input output toclose =
  let cloexec = List.for_all try_set_close_on_exec toclose in
  match fork() with
     0 -> if input <> stdin then begin dup2 input stdin; close input end;
          if output <> stdout then begin dup2 output stdout; close output end;
          if not cloexec then List.iter close toclose;
          begin try execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
          with _ -> exit 127
          end
  | id -> Hashtbl.add popen_processes proc id

let open_process_in cmd =
  let (in_read, in_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  begin
    try
      open_proc cmd (Process_in inchan) stdin in_write [in_read];
    with e ->
      close_in inchan;
      close in_write;
      raise e
  end;
  close in_write;
  inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe() in
  let outchan = out_channel_of_descr out_write in
  begin
    try
      open_proc cmd (Process_out outchan) out_read stdout [out_write];
    with e ->
      close_out outchan;
      close out_read;
      raise e
  end;
  close out_read;
  outchan

let open_process cmd =
  let (in_read, in_write) = pipe() in
  let fds_to_close = ref [in_read;in_write] in
  try
    let (out_read, out_write) = pipe() in
    fds_to_close := [in_read;in_write;out_read;out_write];
    let inchan = in_channel_of_descr in_read in
    let outchan = out_channel_of_descr out_write in
    open_proc cmd (Process(inchan, outchan)) out_read in_write
                                           [in_read; out_write];
    close out_read;
    close in_write;
    (inchan, outchan)
  with e ->
    List.iter close !fds_to_close;
    raise e

let open_proc_full cmd env proc input output error toclose =
  let cloexec = List.for_all try_set_close_on_exec toclose in
  match fork() with
     0 -> dup2 input stdin; close input;
          dup2 output stdout; close output;
          dup2 error stderr; close error;
          if not cloexec then List.iter close toclose;
          begin try execve "/bin/sh" [| "/bin/sh"; "-c"; cmd |] env
          with _ -> exit 127
          end
  | id -> Hashtbl.add popen_processes proc id

let open_process_full cmd env =
  let (in_read, in_write) = pipe() in
  let fds_to_close = ref [in_read;in_write] in
  try
    let (out_read, out_write) = pipe() in
    fds_to_close := out_read::out_write:: !fds_to_close;
    let (err_read, err_write) = pipe() in
    fds_to_close := err_read::err_write:: !fds_to_close;
    let inchan = in_channel_of_descr in_read in
    let outchan = out_channel_of_descr out_write in
    let errchan = in_channel_of_descr err_read in
    open_proc_full cmd env (Process_full(inchan, outchan, errchan))
      out_read in_write err_write [in_read; out_write; err_read];
    close out_read;
    close in_write;
    close err_write;
    (inchan, outchan, errchan)
  with e ->
    List.iter close !fds_to_close;
    raise e

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise(Unix_error(EBADF, fun_name, ""))

let rec waitpid_non_intr pid =
  try waitpid [] pid
  with Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  snd(waitpid_non_intr pid)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  snd(waitpid_non_intr pid)

let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan;
  begin try close_out outchan with Sys_error _ -> () end;
  snd(waitpid_non_intr pid)

let close_process_full (inchan, outchan, errchan) =
  let pid =
    find_proc_id "close_process_full"
                 (Process_full(inchan, outchan, errchan)) in
  close_in inchan;
  begin try close_out outchan with Sys_error _ -> () end;
  close_in errchan;
  snd(waitpid_non_intr pid)

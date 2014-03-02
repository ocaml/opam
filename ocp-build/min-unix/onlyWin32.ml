
open MinUnix

external waitpids : int -> int array -> int * MinUnix.process_status
  = "onlyWin32_waitpids_ml"

type fileinfo = {
  dwFileAttributes : int;
  ftCreationTime : float; (* in Unix seconds *)
  ftLastAccessTime : float; (* in Unix seconds *)
  ftLastWriteTime : float; (* in Unix seconds *)
  dwVolumeSerialNumber : int;
  nFileSize : int64;
  nNumberOfLinks : int;
  nFileIndex : int64;
}

external getFileInformationByHandle : MinUnix.file_descr -> fileinfo
  = "onlyWin32_getFileInformationByHandle_ml"

external getFileInformationByName : string -> fileinfo
  = "onlyWin32_getFileInformationByName_ml"

let rec waitpid1 pid =
   let (_, status) = waitpid [] pid in
   match status with
     | MinUnix.WEXITED n -> n
     | MinUnix.WSIGNALED n -> -n
     | MinUnix.WSTOPPED n -> -1000-n

let rec safe_waitpid pid =
   try
     waitpid1 pid
   with MinUnix.Unix_error (MinUnix.EINTR, _, _) -> safe_waitpid pid


(* High-level process management (system, popen) *)

external create_process_chdir : string -> string -> string option ->
  file_descr -> file_descr -> file_descr -> string option -> int
  = "onlyWin32_create_process_chdir"
    "onlyWin32_create_process_chdir_native"

let make_cmdline args =
  let maybe_quote f =
    if String.contains f ' ' || String.contains f '\"'
    then Filename.quote f
    else f in
  String.concat " " (List.map maybe_quote (Array.to_list args))

let create_process prog args fd1 fd2 fd3 =
  create_process_chdir prog (make_cmdline args) None fd1 fd2 fd3 None

let create_process_env prog args env fd1 fd2 fd3 =
  create_process_chdir prog (make_cmdline args)
                     (Some(String.concat "\000" (Array.to_list env) ^ "\000"))
                     fd1 fd2 fd3 None

external system: string -> process_status = "onlyWin32_system"

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
  | Process_full of in_channel * out_channel * in_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd optenv proc input output error =
  let shell =
    try Sys.getenv "COMSPEC"
    with Not_found -> raise(Unix_error(ENOEXEC, "open_proc", cmd)) in
  let pid =
    create_process_chdir shell (shell ^ " /c " ^ cmd) optenv
                       input output error None in
  Hashtbl.add popen_processes proc pid

let open_process_in cmd =
  let (in_read, in_write) = pipe() in
  set_close_on_exec in_read;
  let inchan = in_channel_of_descr in_read in
  open_proc cmd None (Process_in inchan) stdin in_write stderr;
  close in_write;
  inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe() in
  set_close_on_exec out_write;
  let outchan = out_channel_of_descr out_write in
  open_proc cmd None (Process_out outchan) out_read stdout stderr;
  close out_read;
  outchan

let open_process cmd =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  set_close_on_exec in_read;
  set_close_on_exec out_write;
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd None (Process(inchan, outchan)) out_read in_write stderr;
  close out_read; close in_write;
  (inchan, outchan)

let open_process_full cmd env =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let (err_read, err_write) = pipe() in
  set_close_on_exec in_read;
  set_close_on_exec out_write;
  set_close_on_exec err_read;
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  let errchan = in_channel_of_descr err_read in
  open_proc cmd (Some(String.concat "\000" (Array.to_list env) ^ "\000"))
                (Process_full(inchan, outchan, errchan))
                out_read in_write err_write;
  close out_read; close in_write; close err_write;
  (inchan, outchan, errchan)

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise(Unix_error(EBADF, fun_name, ""))

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  snd(waitpid [] pid)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  snd(waitpid [] pid)

let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan; close_out outchan;
  snd(waitpid [] pid)

let close_process_full (inchan, outchan, errchan) =
  let pid =
    find_proc_id "close_process_full"
                 (Process_full(inchan, outchan, errchan)) in
  close_in inchan; close_out outchan; close_in errchan;
  snd(waitpid [] pid)


type dir_entry =
    Dir_empty
  | Dir_read of string
  | Dir_toread

type dir_handle =
  { dirname: string; mutable handle: int; mutable entry_read: dir_entry }

external findfirst : string -> string * int = "onlyWin32_findfirst"
external findnext : int -> string= "onlyWin32_findnext"

let opendir dirname =
  try
    let (first_entry, handle) = findfirst (Filename.concat dirname "*.*") in
    { dirname = dirname; handle = handle; entry_read = Dir_read first_entry }
  with End_of_file ->
    { dirname = dirname; handle = 0; entry_read = Dir_empty }

let readdir d =
  match d.entry_read with
    Dir_empty -> raise End_of_file
  | Dir_read name -> d.entry_read <- Dir_toread; name
  | Dir_toread -> findnext d.handle

external win_findclose : int -> unit = "onlyWin32_findclose"

let closedir d =
  match d.entry_read with
    Dir_empty -> ()
  | _ -> win_findclose d.handle

let rewinddir d =
  closedir d;
  try
    let (first_entry, handle) = findfirst (d.dirname ^ "\\*.*") in
    d.handle <- handle; d.entry_read <- Dir_read first_entry
  with End_of_file ->
    d.handle <- 0; d.entry_read <- Dir_empty

let command argv =
(*    Printf.fprintf stderr "exec %s\n%!" filename; *)
    let pid = try
      create_process argv.(0) argv
         MinUnix.stdin MinUnix.stdout MinUnix.stderr
      with e ->
	      Printf.fprintf Pervasives.stderr "Error \"%s\" executing %s\n%!"
                (Printexc.to_string e) Sys.argv.(0);
              exit 2
    in
    let status = safe_waitpid pid in
(*    Printf.fprintf stderr "waitpid returned %d\n%!" status; *)
    status

let simulate_exec argv =
   let status = command argv in
   exit status

open MinUnix

val safe_waitpid : int -> int

val command : string array -> int
val simulate_exec : string array -> 'a

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

(** {6 High-level process and redirection management} *)


val create_process :
  string -> string array -> file_descr -> file_descr -> file_descr -> int
(** [create_process prog args new_stdin new_stdout new_stderr]
   forks a new process that executes the program
   in file [prog], with arguments [args]. The pid of the new
   process is returned immediately; the new process executes
   concurrently with the current process.
   The standard input and outputs of the new process are connected
   to the descriptors [new_stdin], [new_stdout] and [new_stderr].
   Passing e.g. [stdout] for [new_stdout] prevents the redirection
   and causes the new process to have the same standard output
   as the current process.
   The executable file [prog] is searched in the path.
   The new process has the same environment as the current process. *)

val create_process_env :
  string -> string array -> string array -> file_descr -> file_descr ->
    file_descr -> int
(** [create_process_env prog args env new_stdin new_stdout new_stderr]
   works as {!Unix.create_process}, except that the extra argument
   [env] specifies the environment passed to the program. *)


val open_process_in : string -> in_channel
(** High-level pipe and process management. This function
   runs the given command in parallel with the program.
   The standard output of the command is redirected to a pipe,
   which can be read via the returned input channel.
   The command is interpreted by the shell [/bin/sh] (cf. [system]). *)

val open_process_out : string -> out_channel
(** Same as {!Unix.open_process_in}, but redirect the standard input of
   the command to a pipe.  Data written to the returned output channel
   is sent to the standard input of the command.
   Warning: writes on output channels are buffered, hence be careful
   to call {!Pervasives.flush} at the right times to ensure
   correct synchronization. *)

val open_process : string -> in_channel * out_channel
(** Same as {!Unix.open_process_out}, but redirects both the standard input
   and standard output of the command to pipes connected to the two
   returned channels.  The input channel is connected to the output
   of the command, and the output channel to the input of the command. *)

val open_process_full :
  string -> string array -> in_channel * out_channel * in_channel
(** Similar to {!Unix.open_process}, but the second argument specifies
   the environment passed to the command.  The result is a triple
   of channels connected respectively to the standard output, standard input,
   and standard error of the command. *)

val close_process_in : in_channel -> process_status
(** Close channels opened by {!Unix.open_process_in},
   wait for the associated command to terminate,
   and return its termination status. *)

val close_process_out : out_channel -> process_status
(** Close channels opened by {!Unix.open_process_out},
   wait for the associated command to terminate,
   and return its termination status. *)

val close_process : in_channel * out_channel -> process_status
(** Close channels opened by {!Unix.open_process},
   wait for the associated command to terminate,
   and return its termination status. *)

val close_process_full :
  in_channel * out_channel * in_channel -> process_status
(** Close channels opened by {!Unix.open_process_full},
   wait for the associated command to terminate,
   and return its termination status. *)

type dir_handle
(** The type of descriptors over opened directories. *)

val opendir : string -> dir_handle
(** Open a descriptor on a directory *)

val readdir : dir_handle -> string
(** Return the next entry in a directory.
   @raise End_of_file when the end of the directory has been reached. *)

val rewinddir : dir_handle -> unit
(** Reposition the descriptor to the beginning of the directory *)

val closedir : dir_handle -> unit
(** Close a directory descriptor. *)


external create_process_chdir : string -> string -> string option ->
  file_descr -> file_descr -> file_descr -> string option -> int
  = "onlyWin32_create_process_chdir"
    "onlyWin32_create_process_chdir_native"

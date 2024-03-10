
(* Based on some empirical testing, this default is an apparent sweet spot.
   The parallel algorithms used so far don't appear to benefit from
   more cores than this. *)
val max_default_num_cores : int

(* Hook for command-line args to override domains used. *)
val requested_num_cores : int option ref

(* Creates a task pool and runs the given thunk with the task pool provided. *)
val run_with_task_pool : (Domainslib.Task.pool -> 'a) -> 'a

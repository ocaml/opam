open Domainslib

(* Based on some empirical testing, this default is an apparent sweet spot.
   The parallel algorithms used so far don't appear to benefit more from
   more cores. *)
let max_default_num_cores = 5

let requested_num_cores = ref None

let create_task_pool () =
  let num_cores_to_use =
    match !requested_num_cores with
    | Some i -> i
    | None ->
      let num_cores = OpamSysPoll.cores () in
      min max_default_num_cores num_cores
  in
  let num_domains = pred num_cores_to_use in
  let task_pool = Task.setup_pool ~num_domains () in
  task_pool

let run_with_task_pool f =
  let task_pool = create_task_pool () in
  let res = Task.run task_pool (fun () -> f task_pool) in
  Task.teardown_pool task_pool;
  res

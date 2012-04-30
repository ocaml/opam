(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

module type PROCESS = 
sig
  type 'a return
  type ('a, 'b) t

  type 'a plist = 'a list (* order irrelevant *)

  type state = 
    | Has_began of int (* pid *)
    | Not_yet_begun

  val cores : int (* above [cores + 1], the parallel running gain is not significant *)

  val init : int (* maximum number of parallel running task *) 
    -> ('a -> 'b return) (* function to execute in parallel *)
    -> ('a -> string)
    -> ('a, 'b) t

  (** Run or continue the execution of the given list of processes. 
      The function returns as soon as one process terminates.
      NB The parallel running is performed on at most : [max 1 "the number of tasks indicated at [init] time"] . *)
  val filter_finished : ('a, 'b) t -> (state * 'a) plist (** By convention : list not empty *) -> ('a, 'b) t * (state * 'a) plist * ('a * 'b return (* finished *))
end

module Process_multi : PROCESS with type 'a return = unit =
struct
  open BatMap

  type 'a return = unit
  type ('a, 'b) t = { cores_max : int ; running : int ; f : 'a -> unit ; to_str : 'a -> string }

  type 'a plist = 'a list (* order irrelevant *)

  type state = 
    | Has_began of int
    | Not_yet_begun

  let cores = 4 (* TODO compute the number of processors of the computer *)

  let zero_running = 0

  let init cores_max f to_str = { cores_max = max 1 cores_max ; running = zero_running ; f ; to_str } 

  (* This function always execute in parallel. *)
  let filter_finished t l = 
    let rec aux map_pid = function
      | 0 as nb, l | nb, ([] as l) -> nb, l, map_pid
      | nb, (Has_began pid, _ as proc) :: xs -> aux (IntMap.add pid proc map_pid) (nb, xs)
      | nb, (Not_yet_begun, x) :: xs -> 
          match Unix.fork () with
            | 0 -> let () = t.f x in exit 0
            | pid -> aux (IntMap.add pid (Has_began pid, x) map_pid) (pred nb, xs) in

    let nb_remained, l_not_begun, map_pid = aux IntMap.empty (t.cores_max - t.running, l) in
    let pid, error = U.wait map_pid in
    let _, x = IntMap.find pid map_pid in
    match error with
      | Unix.WEXITED 0 -> 
          { t with running = max zero_running (t.running + pred nb_remained) }, 
          IntMap.values (IntMap.remove pid map_pid) @ l_not_begun, 
          (x, ())
      | _ -> Globals.error_and_exit "command %s failed" (t.to_str x)
end

module Process_single : PROCESS with type 'a return = 'a =
struct
  type ('a, 'b) t = { nb_proc : int ; f : 'a -> 'b ; to_str : 'a -> string }

  type 'a plist = 'a list (* order irrelevant *)

  type state = 
    | Has_began of int
    | Not_yet_begun

  type 'a return = 'a

  let cores = 1

  let init nb_proc f to_str = 
    if nb_proc = 1 then
      { nb_proc = max 1 nb_proc ; f ; to_str } 
    else
      Globals.error_and_exit "The number of processor requested is not implemented in this module."

  (* Given a list of function to execute in parallel, this function always execute the first element. *)
  let filter_finished t = function
    | [] -> assert false (* by convention this is empty *)
    | (_, x) :: xs -> t, xs, (x, t.f x)

  (* Given a list of function to execute in parallel, this function always asks the user which to execute in case the list contains more than one element. *)
  let filter_finished t = function
    | [] -> assert false (* by convention this is empty *)
    | [_] as l -> filter_finished t l
    | l -> 
        Globals.msg " Choose which number to execute :\n%s\n(between 1 and %d) ? " (String.concat "\n" (List.mapi (fun i (_, x) -> Printf.sprintf "  [%d] %s" (succ i) (t.to_str x)) l)) (List.length l);

        match 
          try
            List.split_nth ((try int_of_string (read_line ()) with _ -> 1) - 1) l
          with
            | _ -> List.split_nth 1 l
        with 
          | l1, (_, x) :: l2 ->
              let xs = l1 @ l2 in
              t, xs, (x, t.f x)
          | _ -> assert false
end

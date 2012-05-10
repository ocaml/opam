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

open Utils

let log fmt = Globals.log "PARALLEL" fmt

module type G = sig
  include Graph.Sig.G
  include Graph.Topological.G with type t := t and module V := V
  val string_of_vertex: V.t -> string
end

module type SIG = sig

  module G : G

  (** [iter n t pre child paren] parallel iteration on [n]
      cores. [child] is evaluated in a remote process and when it as
      finished, whereas [pre] and [post] are evaluated on the current
      process (respectively before and after the child process has
      been created). *)
  val iter: int -> G.t ->
    pre:(G.V.t -> unit) ->
    child:(G.V.t -> unit) ->
    post:(G.V.t -> unit) ->
    unit

  exception Error of G.V.t

end

module Make (G : G) = struct

  module G = G

  module V = struct include G.V let compare = compare end
  module M = Map.Make (V)
  module S = Set.Make (V)

  type t = {
    graph       : G.t ;
    visited_node: IntSet.t ; (* [int] represents the hash of [G.V.t] *)
    roots       : S.t ;
    degree      : int M.t ;
  }

  let print_state t =
    let string_of_set s =
      let l = S.fold (fun v l -> G.string_of_vertex v :: l) s [] in
      String.concat ", " l in
    let string_of_imap m =
      let s v i = Printf.sprintf "%s:%d" (G.string_of_vertex v) i in
      let l = M.fold (fun v i l -> s v i :: l) m [] in
      String.concat ", " l in
    log "ROOTS:  %s" (string_of_set t.roots);
    log "DEGREE: %s" (string_of_imap t.degree)

  let init graph = 
    let degree = ref M.empty in
    let add_degree v d = degree := M.add v d !degree in
    let roots = 
      G.fold_vertex
        (fun v todo ->
          let d = G.in_degree graph v in
          if d = 0 then
            S.add v todo
          else (
            add_degree v d;
            todo
          )
        ) graph S.empty in
    { graph ; roots ; degree = !degree ; visited_node = IntSet.empty }
      
  let visit t x =
    if IntSet.mem (G.V.hash x) t.visited_node then
      invalid_arg "This node has already been visited.";
    if not (S.mem x t.roots) then
      invalid_arg "This node is not a root node";
      (* Add the node to the list of visited nodes *)
    let t = { t with visited_node = IntSet.add (G.V.hash x) t.visited_node } in
      (* Remove the node from the list of root nodes *)
    let roots = S.remove x t.roots in
    let degree = ref t.degree in
    let remove_degree x = degree := M.remove x !degree in
    let replace_degree x d = degree := M.add x d (M.remove x !degree) in
      (* Update the children of the node by decreasing by 1 their in-degree *)
    let roots = 
      G.fold_succ
        (fun x l ->
          let d = M.find x t.degree in
          if d = 1 then (
            remove_degree x;
            S.add x l
          ) else (
            replace_degree x (d-1);
            l
          )
        ) t.graph x roots in
    { t with roots; degree = !degree }

  (* the [Unix.wait] might return a processus which has not been created
     by [Unix.fork]. [wait pids] waits until a process in [pids]
     terminates. *)
  (* XXX: this will not work under windows *)
  let wait pids = 
    let rec aux () = 
      let pid, status = Unix.wait () in
      if IntMap.mem pid pids then
        pid, status
      else
        aux () in
    aux ()

  exception Error of G.V.t

  let iter n g ~pre ~child ~post =
    let t = ref (init g) in
    let pids = ref IntMap.empty in
    let todo = ref (!t.roots) in

(*    let print_pids m =
      let s i n = Printf.sprintf "%d:%s" i (G.string_of_vertex n) in
      let l = IntMap.fold (fun i n l -> s i n :: l) m [] in
      log "PIDS:   %s" (String.concat ", " l) in *)

    (* nslots is the number of free slots *)
    let rec loop nslots =

      if nslots <= 0 || IntMap.cardinal !pids = S.cardinal !t.roots then (

        (* if no slots are available, wait for a child process to finish *)
        let pid, status = wait !pids in
        let n = IntMap.find pid !pids in
        match status with
        | Unix.WEXITED 0 ->
            pids := IntMap.remove pid !pids;
            t := visit !t n;
            post n;
            loop (nslots + 1)
        | _ -> raise (Error n)

      ) else (

        (* otherwise, look at the todo list *)
        if S.is_empty !todo then begin
            (* refill the todo list if it is empty *)
            if not (S.is_empty !t.roots) then begin
              todo := IntMap.fold (fun _ n accu -> S.remove n accu) !pids !t.roots;
              loop nslots
            end
        end else begin
          (* if the todo list contains at least a node action, process it *)
          let n = S.choose !todo in
          todo := S.remove n !todo;
          match Unix.fork () with
          | 0   -> child n
          | pid ->
              pids := IntMap.add pid n !pids;
              pre n;
              loop (nslots - 1)
        end
      )
    in
    loop n

end

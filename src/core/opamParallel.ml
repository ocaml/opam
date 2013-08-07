(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

let log fmt = OpamGlobals.log "PARALLEL" fmt

module type G = sig
  include Graph.Sig.I
  include Graph.Topological.G with type t := t and module V := V
  val has_cycle: t -> bool
  val scc_list: t -> V.t list list
  val string_of_vertex: V.t -> string
end

type error =
  | Process_error of OpamProcess.result
  | Internal_error of string

module type SIG = sig

  module G : G

  val iter: int -> G.t ->
    pre:(G.V.t -> unit) ->
    child:(G.V.t -> unit) ->
    post:(G.V.t -> unit) ->
    unit

  val iter_l: int -> G.vertex list ->
    pre:(G.V.t -> unit) ->
    child:(G.V.t -> unit) ->
    post:(G.V.t -> unit) ->
    unit

  val map_reduce: int -> G.t ->
    map:(G.V.t -> 'a) ->
    merge:('a -> 'a -> 'a) ->
    init:'a ->
    'a

  val map_reduce_l: int -> G.vertex list ->
    map:(G.V.t -> 'a) ->
    merge:('a -> 'a -> 'a) ->
    init:'a ->
    'a

  val create: G.V.t list -> G.t

  exception Errors of (G.V.t * error) list * G.V.t list
  exception Cyclic of G.V.t list list
end

module Make (G : G) = struct

  module G = G

  module V = struct include G.V let compare = compare end
  module M = Map.Make (V)
  module S = Set.Make (V)

  type t = {
    graph  : G.t ;      (* The original graph *)
    visited: S.t ;      (* The visited nodes *)
    roots  : S.t ;      (* The current roots *)
    degree : int M.t ;  (* Node degrees *)
  }

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
    { graph ; roots ; degree = !degree ; visited = S.empty }

  let visit t x =
    if S.mem x t.visited then
      invalid_arg "This node has already been visited.";
    if not (S.mem x t.roots) then
      invalid_arg "This node is not a root node";
    (* Add the node to the list of visited nodes *)
    let t = { t with visited = S.add x t.visited } in
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
  let string_of_pids pids =
    Printf.sprintf "{%s}"
      (String.concat ","
         (OpamMisc.IntMap.fold (fun e _ l -> string_of_int e :: l) pids []))

  let string_of_status st =
    let st, n = match st with
      | Unix.WEXITED n -> "exit", n
      | Unix.WSIGNALED n -> "signal", n
      | Unix.WSTOPPED n -> "stop", n in
    Printf.sprintf "%s %d" st n

  let wait pids =
    let rec aux () =
      let pid, status = Unix.wait () in
      if OpamMisc.IntMap.mem pid pids then (
        log "%d is dead (%s)" pid (string_of_status status);
        pid, status
      ) else (
        log "%d: unknown child (pids=%s)!"
          pid
          (string_of_pids pids);
        aux ()
      ) in

    try aux ()
    with Sys.Break as e ->
      OpamGlobals.msg " Interrupted!\n";
      ignore (aux ());
      raise e

  exception Errors of (G.V.t * error) list * G.V.t list
  exception Cyclic of G.V.t list list

  let (--) = S.diff
  let (++) = S.union
  let (=|=) s1 s2 =
    S.cardinal s1 = S.cardinal s2

  (* write and close the output channel *)
  let write_error oc r =
    log "write_error";
    Marshal.to_channel oc r [];
    close_out oc

  (* read and close the input channel *)
  let read_error ic =
    log "read_error";
    let r : error =
      try Marshal.from_channel ic
      with _ -> Internal_error "Cannot read the error file" in
    close_in ic;
    r

  let iter n g ~pre ~child ~post =
    let t = ref (init g) in
    (* pid -> node * (fd to read the error code) *)
    let pids = ref OpamMisc.IntMap.empty in
    (* The nodes to process *)
    let todo = ref (!t.roots) in
    (* node -> error *)
    let errors = ref M.empty in

    (* All the node with a current worker currently doing some processing. *)
    let worker_nodes () =
      OpamMisc.IntMap.fold (fun _ (n, _) accu -> S.add n accu) !pids S.empty in
    (* All the error nodes. *)
    let error_nodes () =
      M.fold (fun n _ accu -> S.add n accu) !errors S.empty in
    (* All the node not successfully proceeded. This include error
       worker and error nodes. *)

    log "Iterate over %d task(s) with %d process(es)" (G.nb_vertex g) n;

    if G.has_cycle !t.graph then (
      let sccs = G.scc_list !t.graph in
      let sccs = List.filter (fun l -> List.length l > 1) sccs in
      raise (Cyclic sccs)
    );

    (* nslots is the number of free slots *)
    let rec loop nslots =

      if OpamMisc.IntMap.is_empty !pids
      && (S.is_empty !t.roots || not (M.is_empty !errors)
                                 && !t.roots =|= error_nodes ()) then

        (* Nothing more to do *)
        if M.is_empty !errors then
          log "loop completed (without errors)"
        else
          (* Generate the remaining nodes in topological order *)
          let error_nodes = error_nodes () in
          let remaining =
            G.fold_vertex (fun v l ->
              if S.mem v !t.visited
              || S.mem v error_nodes then
                l
              else
                v::l) !t.graph [] in
          let remaining = List.rev remaining in
          raise (Errors (M.bindings !errors, remaining))

      else if nslots <= 0 || (worker_nodes () ++ error_nodes ()) =|= !t.roots then (

        (* if either 1/ no slots are available or 2/ no action can be performed,
           then wait for a child process to finish its work *)
        log "waiting for a child process to finish";
        let pid, status = wait !pids in
        let n, from_child = OpamMisc.IntMap.find pid !pids in
        pids := OpamMisc.IntMap.remove pid !pids;
        begin match status with
          | Unix.WEXITED 0 ->
            t := visit !t n;
            (* we execute the 'post' function of the parent process *)
            post n
          | _ ->
            let from_child = from_child () in
            let error = read_error from_child in
            errors := M.add n error !errors
        end;
        loop (nslots + 1)
      ) else (

        (* otherwise, if the todo list is empty, then refill it *)
        if S.is_empty !todo then (
          log "refilling the TODO list";
          todo := !t.roots -- worker_nodes () -- error_nodes ();
        );

        (* finally, if the todo list contains at least a node action,
           then simply process it *)
        let n = S.choose !todo in
        todo := S.remove n !todo;

        (* Set-up a channel from the child to the parent *)
        let error_file = OpamSystem.temp_file "error" in

        (* We execute the 'pre' function before the fork *)
        pre n;

        match Unix.fork () with
        | -1  -> OpamGlobals.error_and_exit "Cannot fork a new process"
        | 0   ->
          log "Spawning a new process";
          Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
            OpamGlobals.error "Interrupted";
            exit 1)
          );
          Sys.catch_break true;
          let return p =
            let to_parent = open_out_bin error_file in
            write_error to_parent p;
            exit 1 in
          begin
            (* the 'child' function is executed on the child *)
            try child n; log "OK"; exit 0
            with
            | OpamSystem.Process_error p  -> return (Process_error p)
            | OpamSystem.Internal_error s -> return (Internal_error s)
            | e ->
              let b = OpamMisc.pretty_backtrace () in
              let e = Printexc.to_string e in
              let error =
                if b = ""
                then e
                else e ^ "\n" ^ b in
              return (Internal_error error)
          end
        | pid ->
          log "Creating process %d" pid;
          let from_child () = open_in_bin error_file in
          pids := OpamMisc.IntMap.add pid (n, from_child) !pids;
          loop (nslots - 1)
      ) in
    loop n

  let map_reduce jobs g ~map ~merge ~init =
    let files = ref [] in
    let file repo = List.assoc repo !files in

    let pre repo =
      let tmpfile = OpamSystem.temp_file "map-reduce" in
      log "pre %S (%s)"(G.string_of_vertex repo) tmpfile;
      files := (repo, tmpfile) :: !files
    in

    let child repo =
      log "child %S" (G.string_of_vertex repo);
      let file = file repo in
      let result = map repo in
      let oc = open_out file in
      Marshal.to_channel oc result []
    in

    let acc = ref init in
    let post repo =
      log "post %S" (G.string_of_vertex repo);
      let file = file repo in
      let ic = open_in_bin file in
      let result =
        try Marshal.from_channel ic
        with _ -> OpamSystem.internal_error "Cannot read the result file" in
      close_in ic;
      Unix.unlink file;
      files := List.filter (fun (_,f) -> f<>file) !files;
      acc := merge result !acc
    in

    try
      iter jobs g ~pre ~child ~post;
      !acc
    with
    | Errors (errors,_) ->
      let string_of_error = function
        | Process_error r  -> OpamProcess.string_of_result r
        | Internal_error s -> s in
      List.iter (fun (v, e) ->
        OpamGlobals.error "Error while processing %s\n%s"
          (G.string_of_vertex v)
          (string_of_error e);
      ) errors;
      OpamGlobals.exit 2

  let create l =
    let g = G.create () in
    List.iter (G.add_vertex g) l;
    g

  let map_reduce_l jobs list ~map ~merge ~init = match list with
    | []    -> init
    | [elt] -> merge (map elt) init
    | _     ->
      if jobs = 1 then
        List.fold_left (fun acc repo -> merge (map repo) acc) init list
      else
        let g = create list in
        map_reduce jobs g ~map ~merge ~init

  let iter_l jobs list ~pre ~child ~post = match list with
    | []    -> ()
    | [elt] -> pre elt; child elt; post elt
    | list  ->
      if jobs = 1 then
        List.iter (fun elt -> pre elt; child elt; post elt) list
      else
        let g = create list in
        iter jobs g ~pre ~post ~child

end

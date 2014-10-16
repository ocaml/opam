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

open OpamMisc.OP

let log fmt = OpamGlobals.log "PARALLEL" fmt
let slog = OpamGlobals.slog

module type VERTEX = sig
  include OpamMisc.OrderedType
  include Graph.Sig.COMPARABLE with type t := t
end

module type G = sig
  include Graph.Sig.I
  module Vertex: VERTEX with type t = V.t
  module Topological: sig
    val fold: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  end
  val has_cycle: t -> bool
  val scc_list: t -> V.t list list
end

type command = {
  cmd: string;
  args: string list;
  cmd_text: string option;
  cmd_dir: OpamFilename.Dir.t option;
  cmd_env: string array option;
  cmd_verbose: bool option;
  cmd_name: string option;
  cmd_metadata: (string * string) list option
}

let command ?env ?verbose ?name ?metadata ?dir ?text cmd args =
  { cmd; args;
    cmd_env=env; cmd_verbose=verbose; cmd_name=name; cmd_metadata=metadata;
    cmd_dir=dir; cmd_text=text; }

let string_of_command c = String.concat " " (c.cmd::c.args)

type 'a job =
  | Done of 'a
  | Run of command * (OpamProcess.result -> 'a job)

module type SIG = sig

  module G : G

  val iter:
    jobs:int ->
    command:(pred:(G.V.t * 'a) list -> G.V.t -> 'a job) ->
    G.t ->
    unit

  val iter_l:
    jobs:int ->
    command:(pred:(G.V.t * 'a) list -> G.V.t -> 'a job) ->
    G.V.t list ->
    unit

  exception Errors of (G.V.t * exn) list * G.V.t list
  exception Cyclic of G.V.t list list
end

module Make (G : G) : SIG with module G = G
= struct

  module G = G

  module V = G.Vertex
  module M = OpamMisc.Map.Make (V)
  module S = OpamMisc.Set.Make (V)

  exception Errors of (V.t * exn) list * V.t list
  exception Cyclic of V.t list list

  open S.Op

  (* Returns a map (node -> return value) *)
  let map ~jobs ~command g =
    log "Iterate over %a task(s) with %d process(es)"
      (slog @@ G.nb_vertex @> string_of_int) g jobs;

    if G.has_cycle g then (
      let sccs = G.scc_list g in
      let sccs = List.filter (function _::_::_ -> true | _ -> false) sccs in
      raise (Cyclic sccs)
    );

    let print_status (running: (OpamProcess.t * 'a * string option) M.t) =
      let texts =
        OpamMisc.filter_map (fun (_,_,t) -> t) (M.values running) in
      if texts <> [] then
        (OpamGlobals.msg "\r[KRunning: %s\r" (String.concat " " texts);
         print_string "[K")
    in

    (* nslots is the number of free slots *)
    let rec loop
        (nslots: int) (* number of free slots *)
        (results: 'b M.t)
        (running: (OpamProcess.t * 'a * string option) M.t)
        (ready: S.t)
      =
      let run_seq_command nslots ready n = function
        | Done r ->
          log "Job %a finished" (slog (string_of_int @* V.hash)) n;
          let results = M.add n r results in
          let running = M.remove n running in
          print_status running;
          let new_ready =
            List.filter
              (fun n -> List.for_all (fun n -> M.mem n results) (G.pred g n))
              (G.succ g n)
          in
          loop (nslots + 1) results running (ready ++ S.of_list new_ready)
        | Run (cmd, cont) ->
          log "Next task in job %a: %a" (slog (string_of_int @* V.hash)) n
            (slog (String.concat " ")) (cmd.cmd::cmd.args);
          let run_process () =
            OpamProcess.run_background
              ?env:cmd.cmd_env ?verbose:cmd.cmd_verbose ?name:cmd.cmd_name
              ?metadata:cmd.cmd_metadata
              ~allow_stdin:false (* bad idea in parallel ! *)
              cmd.cmd cmd.args
          in
          let p = match cmd.cmd_dir with
            | None -> run_process ()
            | Some dir -> OpamFilename.in_dir dir run_process
          in
          let running = M.add n (p,cont,cmd.cmd_text) running in
          print_status running;
          loop nslots results running ready
      in

      let fail node error =
        log "Exception while computing job %a: %a"
          (slog (string_of_int @* V.hash)) node
          (slog V.to_string) node;
        OpamGlobals.error "%s" (Printexc.to_string error);
        (* Cleanup *)
        let errors,pend =
          M.fold (fun n (p,cont,_text) (errors,pend) ->
              try
                match OpamProcess.dontwait p with
                | None -> (* process still running *)
                  Unix.kill p.OpamProcess.p_pid Sys.sigint;
                  (* XXX sigkill only on windows *)
                  (n,OpamSystem.Internal_error "User interruption") :: errors,
                  p::pend
                | Some result ->
                  match cont result with
                  | Done _ -> errors, pend
                  | Run _ ->
                    (n,OpamSystem.Internal_error "User interruption") :: errors,
                    pend
              with
              | Unix.Unix_error _ -> errors, pend
              | e -> (n,e)::errors, pend)
            running ([node,error],[])
        in
        (try List.iter (fun _ -> ignore (OpamProcess.wait_one pend)) pend
         with e -> log "%a in sub-process cleanup" (slog Printexc.to_string) e);
        (* Generate the remaining nodes in topological order *)
        let remaining =
          G.Topological.fold (fun n remaining ->
              if M.mem n results || List.mem_assoc n errors then remaining
              else n::remaining)
            g [] in
        raise (Errors (errors, List.rev remaining))
      in

      if M.is_empty running && S.is_empty ready then
        results
      else if nslots > 0 && not (S.is_empty ready) then
        (* Start a new process *)
        let n = S.choose ready in
        log "Starting job %a (worker %d/%d): %a"
          (slog (string_of_int @* V.hash)) n (jobs - nslots + 1) jobs
          (slog V.to_string) n;
        let pred = G.pred g n in
        let pred = List.map (fun n -> n, M.find n results) pred in
        let cmd = try command ~pred n with e -> fail n e in
        run_seq_command (nslots - 1) (S.remove n ready) n cmd
      else
      (* Wait for a process to end *)
      let processes = M.fold (fun n (p,x,_) acc -> (p,(n,x)) :: acc) running [] in
      let process,result =
        try match List.map fst processes with
          | [p] -> p, OpamProcess.wait p
          | _ -> OpamProcess.wait_one (List.map fst processes)
        with e -> fail (fst (snd (List.hd processes))) e
      in
      let n,cont = List.assoc process processes in
      log "Collected task for job %a" (slog (string_of_int @* V.hash)) n;
      let next =
        try cont result with e ->
          OpamProcess.cleanup result;
          fail n e in
      OpamProcess.cleanup result;
      run_seq_command nslots ready n next
    in
    let roots =
      G.fold_vertex
        (fun n roots -> if G.in_degree g n = 0 then S.add n roots else roots)
        g S.empty
    in
    loop jobs M.empty M.empty roots

  let iter ~jobs ~command g =
    ignore (map ~jobs ~command g)

  let flat_graph_of_list l =
    let g = G.create () in
    List.iter (G.add_vertex g) l;
    g

  let iter_l ~jobs ~command l =
    iter ~jobs ~command (flat_graph_of_list l)

end

module type GRAPH = sig
  include Graph.Sig.I
  include Graph.Oper.S with type g = t
  module Topological : sig
    val fold : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
    val iter : (V.t -> unit) -> t -> unit
  end
  module Parallel : SIG with type G.t = t
                         and type G.V.t = vertex
  module Dot : sig val output_graph : out_channel -> t -> unit end
end

module MakeGraph (X: OpamMisc.OrderedType) : GRAPH with type V.t = X.t
= struct
  module Vertex = struct
    include X
    let hash = Hashtbl.hash
    let equal x y = compare x y = 0
  end
  module PG = Graph.Imperative.Digraph.ConcreteBidirectional (Vertex)
  module Topological = Graph.Topological.Make (PG)
  module Traverse = Graph.Traverse.Dfs(PG)
  module Components = Graph.Components.Make(PG)
  module Parallel = Make (struct
      include PG
      module Vertex = Vertex
      module Topological = Topological
      include Traverse
      include Components
    end)
  module Dot = Graph.Graphviz.Dot (struct
      let edge_attributes _ = []
      let default_edge_attributes _ = []
      let get_subgraph _ = None
      let vertex_attributes _ = []
      let vertex_name v = Printf.sprintf "%S" (Vertex.to_string v)
      let default_vertex_attributes _ = []
      let graph_attributes _ = []
      include PG
    end)
  include PG
  include Graph.Oper.I (PG)
end

module Job = struct
  (* Parallelise shell commands *)
  let (@@>) command f = Run (command, f)

  let rec (@@+) job1 fjob2 = match job1 with
    | Done x -> fjob2 x
    | Run (cmd,cont) -> Run (cmd, fun r -> cont r @@+ fjob2)

  let rec run = function
    | Done x -> x
    | Run (cmd,cont) ->
      let run_process () =
        OpamProcess.run
          ?env:cmd.cmd_env ?verbose:cmd.cmd_verbose ?name:cmd.cmd_name
          ?metadata:cmd.cmd_metadata
          cmd.cmd cmd.args
      in
      let result = match cmd.cmd_dir with
        | None -> run_process ()
        | Some dir -> OpamFilename.in_dir dir run_process
      in
      run (cont result)

  let rec dry_run = function
    | Done x -> x
    | Run (_command,cont) ->
      let result = { OpamProcess.
                     r_code = 0;
                     r_duration = 0.;
                     r_info = [];
                     r_stdout = [];
                     r_stderr = [];
                     r_cleanup = []; }
      in dry_run (cont result)
end

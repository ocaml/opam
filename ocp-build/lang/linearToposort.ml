(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

let verbose = DebugVerbosity.verbose [ "OcpLang" ] "LinearToposort"

type node = {
  mutable node_incoming_edges : node IntMap.t;
  mutable node_outgoing_edges : node IntMap.t;
  mutable node_outgoing_nbr : int;
  mutable node_id : int;
  mutable node_position : int;
  mutable node_name : string;
}

let node_ids = ref 0

let new_node () =
  incr node_ids;
  {
    node_id = !node_ids;
    node_incoming_edges = IntMap.empty;
    node_outgoing_edges = IntMap.empty;
    node_outgoing_nbr = 0;
    node_position = 0;
    node_name = "";
  }

module Make(M : sig

  type t
  val node : t -> node
  val iter_edges : (t -> unit) -> t -> unit
  val name : t -> string
  val debug : bool ref

end) = (struct

  let debug = M.debug

  let sort list =

    let graph = ref IntMap.empty in
    let initial_position = ref 0 in
    let add_edges = ref [] in

    (* Phase 1: add all nodes to the graph, computing edges and initial
       positions *)

    let rec add t node =
      if not (IntMap.mem node.node_id !graph) then begin
        graph := IntMap.add node.node_id (t,node) !graph;
        node.node_position <- !initial_position;
        node.node_incoming_edges <- IntMap.empty;
        node.node_outgoing_edges <- IntMap.empty;
        node.node_outgoing_nbr <- 0;
        node.node_name <- M.name t;
        if !debug then
          Printf.eprintf "Adding %d:%s to graph\n%!"
            node.node_id node.node_name;
        incr initial_position;
        M.iter_edges (fun t ->
          let t_node = M.node t in
          add_edges := (node, t, t_node) :: !add_edges) t
      end
    in
    List.iter (fun t ->
      let node = M.node t in
      add t node)
      (List.rev list);
    while !add_edges != [] do
      match !add_edges with
          [] -> assert false
        | (node, t, t_node) :: tail ->
          add_edges := tail;
          add t t_node;
          if not (IntMap.mem node.node_id t_node.node_incoming_edges) then begin
            t_node.node_incoming_edges <- IntMap.add node.node_id node t_node.node_incoming_edges;
            if !debug then
              Printf.eprintf "Adding edge %d:%s -> %d:%s\n%!" node.node_id node.node_name
                t_node.node_id t_node.node_name;
            node.node_outgoing_nbr <- node.node_outgoing_nbr + 1;
            node.node_outgoing_edges <-
              IntMap.add t_node.node_id t_node node.node_outgoing_edges
          end
    done;
    if !debug then
      Printf.eprintf "Nnodes : %d\n%!" !initial_position;


    let orphans = ref IntMap.empty in
    IntMap.iter (fun _ (_,node) ->
      if !debug then
        Printf.eprintf "Total %d (%d)\n%!" node.node_id node.node_outgoing_nbr;
      if node.node_outgoing_nbr = 0 then begin
        if !debug then
          Printf.eprintf "orphan : %d\n%!" node.node_id;
        orphans := IntMap.add node.node_position node !orphans;
      end
    ) !graph;

    let final_position = ref 0 in
    while !orphans <> IntMap.empty do
      let (_, node) = IntMap.max_binding !orphans in
      orphans := IntMap.remove node.node_position !orphans;
      graph := IntMap.remove node.node_id !graph;
      node.node_position <- !final_position;
      if !debug then
        Printf.eprintf "with orphan : %d (%s) position := %d\n%!" node.node_id node.node_name node.node_position;
      incr final_position;
      IntMap.iter (fun _ node2 ->
        node2.node_outgoing_nbr <- node2.node_outgoing_nbr - 1;
        node2.node_outgoing_edges <- IntMap.remove node.node_id
            node2.node_outgoing_edges;
        if !debug then
          Printf.eprintf "\tremove edge %d to %d (%d)\n%!" node.node_id node2.node_id  node2.node_outgoing_nbr;
        if node2.node_outgoing_nbr = 0 then begin
          if !debug then
            Printf.eprintf "orphan : %d\n%!" node2.node_id;
          orphans := IntMap.add node2.node_position node2 !orphans
        end
      ) node.node_incoming_edges
    done;
    let sorted = List.stable_sort (fun n1 n2 ->
        let s1 = M.node n1 in
        let s2 = M.node n2 in
        s1.node_position - s2.node_position) list in

    if !graph = IntMap.empty then
      (sorted, [], [])
    else begin
      let removed = ref [] in
      let rec remove_node t node =
        if !debug then
          Printf.eprintf "Removing node %d\n%!" node.node_id;
        removed := t :: !removed;
        graph := IntMap.remove node.node_id !graph;
        IntMap.iter (fun _ node2 ->
          node2.node_incoming_edges <-
            IntMap.remove node.node_id node2.node_incoming_edges;
          if node2.node_incoming_edges = IntMap.empty then
            let (t2, _) = IntMap.find node2.node_id !graph in
            remove_node t2 node2;
        ) node.node_outgoing_edges;
        node.node_outgoing_edges <- IntMap.empty
      in
      IntMap.iter (fun _ (t, node) ->
        if node.node_incoming_edges = IntMap.empty then
          remove_node t node
      ) !graph;
      if !debug then
        IntMap.iter (fun _ (_,node) ->
          Printf.eprintf "Remaining node %d (out=%d/%d,in=%d,pos=%d)\n%!"
            node.node_id
            node.node_outgoing_nbr
            (IntMap.cardinal node.node_outgoing_edges)
            (IntMap.cardinal node.node_incoming_edges)
            node.node_position;
        ) !graph;
      let cycle = ref [] in
      IntMap.iter (fun _ (t, node) ->
        let incoming_edges = ref [] in
        IntMap.iter (fun node2_id node2 ->
          let (t2, _) = IntMap.find node2_id !graph in
          incoming_edges := t2 :: !incoming_edges;
        ) node.node_incoming_edges;
        let outgoing_edges = ref [] in
        IntMap.iter (fun node2_id node2 ->
          let (t2, _) = IntMap.find node2_id !graph in
          outgoing_edges := t2 :: !outgoing_edges;
        ) node.node_outgoing_edges;
        cycle := (t, !incoming_edges, !outgoing_edges) :: !cycle
      ) !graph;
      (sorted, !cycle, !removed)
    end



end)

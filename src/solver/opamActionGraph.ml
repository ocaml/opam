(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 OCamlPro                                             *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

module type ACTION = sig
  type package
  module Pkg : GenericPackage with type t = package
  include OpamParallel.VERTEX with type t = package action
  val to_string: [< t ] -> string
  val to_aligned_strings:
    ?append:(package -> string) -> [< t ] list -> string list list
  module Set: OpamStd.SET with type elt = package action
  module Map: OpamStd.MAP with type key = package action
end

let name_of_action = function
  | `Remove _ -> "remove"
  | `Install _ -> "install"
  | `Change (`Up,_,_) -> "upgrade"
  | `Change (`Down,_,_) -> "downgrade"
  | `Reinstall _ -> "recompile"
  | `Build _ -> "build"

let symbol_of_action = function
  | `Remove _ -> "\xe2\x8a\x98 " (* U+2298 *)
  | `Install _ -> "\xe2\x88\x97 " (* U+2217 *)
  | `Change (`Up,_,_) -> "\xe2\x86\x97 " (* U+2197 *)
  | `Change (`Down,_,_) -> "\xe2\x86\x98 " (* U+2198 *)
  | `Reinstall _ -> "\xe2\x86\xbb " (* U+21BB *)
  | `Build _ -> "\xce\xbb " (* U+03BB *)

let action_strings ?utf8 a =
  if utf8 = None && (OpamConsole.utf8 ()) || utf8 = Some true
  then symbol_of_action a
  else name_of_action a

let action_color c =
  OpamConsole.colorise (match c with
      | `Install _ | `Change (`Up,_,_) -> `green
      | `Remove _ | `Change (`Down,_,_) -> `red
      | `Reinstall _ -> `yellow
      | `Build _ -> `cyan)

module MakeAction (P: GenericPackage) : ACTION with type package = P.t
= struct
  module Pkg = P
  type package = P.t
  type t = package action

  let compare t1 t2 =
    (* `Install > `Build > `Upgrade > `Reinstall > `Downgrade > `Remove *)
    match t1,t2 with
    | `Remove p, `Remove q
    | `Install p, `Install q
    | `Reinstall p, `Reinstall q
    | `Build p, `Build q
      -> P.compare p q
    | `Change (`Up,p0,p), `Change (`Up,q0,q)
    | `Change (`Down,p0,p), `Change (`Down,q0,q)
      ->
      let c = P.compare p q in
      if c <> 0 then c else P.compare p0 q0
    | `Install _, _ | _, `Remove _ -> 1
    | _, `Install _ | `Remove _, _ -> -1
    | `Build _, _ | _, `Change (`Down,_,_) -> 1
    | `Change (`Down,_,_), _ | _, `Build _ -> -1
    | `Change (`Up,_,_), `Reinstall _ -> 1
    | `Reinstall _, `Change(`Up,_,_) -> -1

  let hash a = Hashtbl.hash (OpamTypesBase.map_action P.hash a)

  let equal t1 t2 = compare t1 t2 = 0

  let to_string a = match a with
    | `Remove p | `Install p | `Reinstall p | `Build p ->
      Printf.sprintf "%s %s" (action_strings a) (P.to_string p)
    | `Change (_,p0,p) ->
      Printf.sprintf "%s.%s %s %s"
        (P.name_to_string p0)
        (P.version_to_string p0)
        (action_strings a)
        (P.version_to_string p)

  let to_aligned_strings ?(append=(fun _ -> "")) l =
    List.map (fun a ->
        let a = (a :> package action) in
        (if OpamConsole.utf8 ()
         then action_color a (symbol_of_action a)
         else "-")
        :: name_of_action a
        :: OpamConsole.colorise `bold
          (P.name_to_string (OpamTypesBase.action_contents a))
        :: match a with
        | `Remove p | `Install p | `Reinstall p | `Build p ->
          (P.version_to_string p ^ append p) :: []
        | `Change (_,p0,p) ->
          Printf.sprintf "%s to %s"
            (P.version_to_string p0 ^ append p0)
            (P.version_to_string p ^ append p)
          :: [])
      l

  let to_json = function
    | `Remove p -> `O ["remove", P.to_json p]
    | `Install p -> `O ["install", P.to_json p]
    | `Change (_, o, p) ->
      `O ["change", `A [P.to_json o;P.to_json p]]
    | `Reinstall p -> `O ["recompile", P.to_json p]
    | `Build p -> `O ["build", P.to_json p]

  module O = struct
      type t = package action
      let compare = compare
      let to_string = to_string
      let to_json = to_json
    end

  module Set = OpamStd.Set.Make(O)
  module Map = OpamStd.Map.Make(O)

end

module type SIG = sig
  type package
  include OpamParallel.GRAPH with type V.t = package OpamTypes.action
  val reduce: t -> t
  val explicit: ?noop_remove:(package -> bool) -> t -> t
  val fold_descendants: (V.t -> 'a -> 'a) -> 'a -> t -> V.t -> 'a
end

module Make (A: ACTION) : SIG with type package = A.package = struct
  type package = A.package

  include OpamParallel.MakeGraph(A)

  module Map = OpamStd.Map.Make (A.Pkg)
  module Set = OpamStd.Set.Make (A.Pkg)

  (* Turn concrete actions (only install, remove and build) to higher-level
     actions (install, remove, up/downgrade, recompile). Builds are removed when
     they directly precede an install, which should be the case when [explicit]
     is used. *)
  let reduce g =
    let g = copy g in
    let removals =
      fold_vertex (fun v acc -> match v with
          | `Remove p ->
            OpamStd.String.Map.add (A.Pkg.name_to_string p) p acc
          | _ -> acc)
        g OpamStd.String.Map.empty
    in
    iter_vertex (function
        | `Build p as build ->
          (match
             fold_succ (fun v _ -> if v = `Install p then Some v else None)
               g build None
           with
           | None -> ()
           | Some inst ->
             iter_pred (fun pred -> add_edge g pred inst) g build;
             remove_vertex g build)
        | _ -> ())
      g;
    let reduced = ref Map.empty in
    let g =
      map_vertex (function
          | `Install p as act ->
            (try
               let p0 = OpamStd.String.Map.find (A.Pkg.name_to_string p) removals in
               let act =
                 match A.Pkg.compare p0 p with
                 | 0 -> `Reinstall p
                 | c -> `Change ((if c < 0 then `Up else `Down), p0, p)
               in
               reduced := Map.add p0 act !reduced;
               act
             with Not_found -> act)
          | act -> act)
        g
    in
    Map.iter (fun p act ->
        let rm_act = `Remove p in
        iter_pred (fun v -> add_edge g v act) g rm_act;
        remove_vertex g rm_act
      ) !reduced;
    g

  let compute_closed_predecessors noop_remove g =
    let closed_g = copy g in
    transitive_closure closed_g;
    let closed_packages =
      (* The set of package that do not have dependencies
         (in the action graph). *)
      fold_vertex (fun a acc ->
          match a with
          | `Build p ->
            let pred =
              (* We ignore predecessors that do not modify the prefix *)
              List.filter
                (function
                  | `Remove nv -> not (noop_remove nv)
                  | _ -> true)
                (pred closed_g a) in
            if pred = [] then Set.add p acc else acc
          | _ -> acc) g Set.empty
    in
    let dependent_base_packages =
      fold_vertex (fun a acc ->
          match a with
          | `Install p | `Reinstall p | `Change (_,_,p) ->
            let preds =
              List.filter
                (function
                  | `Build p -> Set.mem p closed_packages
                  | _ -> false)
                (pred closed_g a) in
            OpamStd.String.Map.add (A.Pkg.name_to_string p) preds acc
          | _ -> acc) g OpamStd.String.Map.empty in
    function p ->
    match
      OpamStd.String.Map.find_opt
        (A.Pkg.name_to_string p)
        dependent_base_packages
    with
    | None -> []
    | Some pred -> pred

  let explicit ?(noop_remove = (fun _ -> false)) g0 =
    let g = copy g0 in
    let same_name p1 p2 = A.Pkg.(name_to_string p1 = name_to_string p2) in
    (* We insert a "build" action before any "install" action.
       Except, between the removal and installation of the same package
       (the removal might be postponed after a succesfull build. *)
    iter_vertex (fun a ->
        match a with
        | `Install p | `Reinstall p | `Change (_,_,p) ->
          let b = `Build p in
          iter_pred (function
              | `Remove p1 when same_name p p1 -> ()
              | pred -> remove_edge g pred a; add_edge g pred b)
            g0 a;
          add_edge g b a
        | `Remove _ -> ()
        | `Build _ -> assert false)
      g0;
    (* For delaying removal a little bit, for each action "remove A" we add
       a constraint "build B -> remove A" for transitive predecessors
       of "A" that do not have dependencies.

       For adding a little bit more delay, we ignore dependencies that do not
       modify the prefix (see [OpamAction.noop_remove]) *)
    let closed_predecessors = compute_closed_predecessors noop_remove g in
    iter_vertex (function
        | `Remove p as a ->
          List.iter
            (fun b -> add_edge g b a)
            (closed_predecessors p)
        | `Install _ | `Reinstall _ | `Change _ | `Build _ -> ())
      g;
    g

  let fold_descendants f acc t v =
    let rec aux seen f acc t v =
      if A.Set.mem v seen then seen, acc else
        fold_succ (fun v (seen, acc) -> aux seen f acc t v)
          t v (A.Set.add v seen, f v acc)
    in
    snd (aux A.Set.empty f acc t v)
end

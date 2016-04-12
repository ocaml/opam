(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 OCamlPro                                             *)
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

open OpamTypes
open OpamStd.Op

module type ACTION = sig
  type package
  module Pkg : GenericPackage with type t = package
  include OpamParallel.VERTEX with type t = package action
  val to_string: [< t ] -> string
  val to_aligned_strings: [< t ] list -> string list
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
  | `Build _ -> "\xce\xbb " (* U+039B *)

let action_strings ?utf8 a =
  if utf8 = None && (OpamConsole.utf8 ()) || utf8 = Some true
  then symbol_of_action a
  else name_of_action a

let action_color c =
  OpamConsole.colorise (match c with
      | `Install _ | `Change (`Up,_,_) -> `green
      | `Remove _ | `Change (`Down,_,_) -> `red
      | `Reinstall _ | `Build _ -> `yellow)

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

  let to_aligned_strings l =
    let tbl =
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
            P.version_to_string p :: []
          | `Change (_,p0,p) ->
            Printf.sprintf "%s to %s"
              (P.version_to_string p0) (P.version_to_string p)
            :: [])
        l
    in
    List.map (String.concat " ") (OpamStd.Format.align_table tbl)

  let to_json = function
    | `Remove p -> `O ["remove", P.to_json p]
    | `Install p -> `O ["install", P.to_json p]
    | `Change (_, o, p) ->
      `O ["change", `A [P.to_json o;P.to_json p]]
    | `Reinstall p -> `O ["recompile", P.to_json p]
    | `Build p -> `O ["build", P.to_json p]

end

module type SIG = sig
  type package
  include OpamParallel.GRAPH with type V.t = package OpamTypes.action
  val reduce: t -> t
  val explicit: t -> t
  val removals_last: t -> t
end

module Make (A: ACTION) : SIG with type package = A.package = struct
  type package = A.package

  include OpamParallel.MakeGraph(A)

  module Map = OpamStd.Map.Make (A.Pkg)

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

  let explicit g0 =
    let g = copy g0 in
    let same_name p1 p2 = A.Pkg.(name_to_string p1 = name_to_string p2) in
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
    g

  module Set = OpamStd.Set.Make (A)

  (* Adds any unrelated, non-remove action as an antecedent of every remove
     action, to be sure it's done as late as possible *)
  let removals_last g0 =
    let g = copy g0 in
    let closure = transitive_closure g in
    let nonremove_actions =
      fold_vertex (fun a acc -> match a with
          | `Remove _ -> acc
          | a -> Set.add a acc)
        g Set.empty
    in
    let rm_list l set =
      List.fold_left (fun acc x -> Set.remove x acc) set l
    in
    iter_vertex (function
        | `Remove _ as rm ->
          let unrelated =
            nonremove_actions
            |> rm_list (pred closure rm)
            |> rm_list (succ closure rm)
          in
          Set.iter (fun a -> add_edge g a rm) unrelated
        | _ -> ())
      g;
    g

end

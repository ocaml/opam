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

module type ACTION = sig
  type package
  module Pkg : GenericPackage with type t = package
  include OpamParallel.VERTEX with type t = package action
end

module MakeAction (P: GenericPackage) : ACTION with type package = P.t
= struct
  module Pkg = P
  type package = P.t
  type t = package action
  let contents = function To_change (_, p) | To_recompile p | To_delete p -> p
  let compare t1 t2 = P.compare (contents t1) (contents t2)
  let hash t = P.hash (contents t)
  let equal t1 t2 = P.equal (contents t1) (contents t2)
  let to_string = function
    | To_change (None, p)   ->
      Printf.sprintf "install   %s" (P.to_string p)
    | To_change (Some o, p) ->
      let vo = P.version_to_string o and vp = P.version_to_string p in
      Printf.sprintf "%s %s from %s to %s"
        (if P.compare o p < 0 then "upgrade  " else "downgrade")
        (P.name_to_string o) vo vp
    | To_recompile p -> Printf.sprintf "recompile %s" (P.to_string p)
    | To_delete p    -> Printf.sprintf "remove    %s" (P.to_string p)
  let to_json = function
    | To_change (None, p)   -> `O ["install", P.to_json p]
    | To_change (Some o, p) -> `O ["change", `A [P.to_json o;P.to_json p]]
    | To_recompile p -> `O ["recompile", P.to_json p]
    | To_delete p    -> `O ["remove", P.to_json p]

end

module type SIG = sig
  type package
  include OpamParallel.GRAPH with type V.t = package OpamTypes.action
  val reduce: t -> t
end

module Make (A: ACTION) : SIG with type package = A.package = struct
  type package = A.package

  include OpamParallel.MakeGraph(A)

  module Map = OpamMisc.Map.Make (A.Pkg)

  (* Turn atomic actions (only install and remove) to higher-level actions
     (install, remove, up/downgrade, recompile) *)
  let reduce g =
    let removals =
      fold_vertex (fun v acc -> match v with
          | To_delete p ->
            OpamMisc.StringMap.add (A.Pkg.name_to_string p) p acc
          | _ -> acc)
        g OpamMisc.StringMap.empty
    in
    let reduced = ref Map.empty in
    let g =
      map_vertex (function
          | To_change (None, p) as act ->
            (try
               let p0 = OpamMisc.StringMap.find (A.Pkg.name_to_string p) removals in
               let act =
                 if A.Pkg.equal p0 p then To_recompile p
                 else To_change (Some p0, p)
               in
               reduced := Map.add p0 act !reduced;
               act
             with Not_found -> act)
          | act -> act)
        g
    in
    Map.iter (fun p act ->
        let rm_act = To_delete p in
        iter_pred (fun v -> add_edge g v act) g rm_act;
        remove_vertex g rm_act
      ) !reduced;
    g
end

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
  val to_aligned_strings: t list -> string list
end

let action_strings ?utf8 x =
  if utf8 = None && (OpamConsole.utf8 ()) || utf8 = Some true then
    List.assoc x
      [`inst,   "\xe2\x88\x97 ";
       `up,     "\xe2\x86\x97 ";
       `down,   "\xe2\x86\x98 ";
       `reinst, "\xe2\x86\xbb ";
       `rm,     "\xe2\x8a\x98 "]
  else
    List.assoc x
      [`inst,   "install";
       `up,     "upgrade";
       `down,   "downgrade";
       `reinst, "recompile";
       `rm,     "remove"]

let action_color c =
  OpamConsole.colorise (match c with
      | `inst | `up -> `green
      | `rm | `down -> `red
      | `reinst -> `yellow)

module MakeAction (P: GenericPackage) : ACTION with type package = P.t
= struct
  module Pkg = P
  type package = P.t
  type t = package action

  let compare t1 t2 =
    (* To_change > To_recompile > To_delete *)
    match t1,t2 with
    | To_delete p, To_delete q
    | To_recompile p, To_recompile q ->
      P.compare p q
    | To_change (po,p), To_change (qo,q) ->
      let c = P.compare p q in
      if c <> 0 then c else OpamMisc.Option.compare P.compare po qo
    | To_change _, _ | _, To_delete _ -> 1
    | _, To_change _ | To_delete _, _ -> -1

  let hash = function
    | To_change (None, p) -> Hashtbl.hash (`C, P.hash p)
    | To_change (Some p1, p2) -> Hashtbl.hash (`C, P.hash p1, P.hash p2)
    | To_delete p -> Hashtbl.hash (`D, P.hash p)
    | To_recompile p -> Hashtbl.hash (`R, P.hash p)

  let equal t1 t2 = compare t1 t2 = 0

  let to_string =
    function
    | To_change (None, p)   ->
      Printf.sprintf "%s %s" (action_strings `inst) (P.to_string p)
    | To_change (Some o, p) ->
      Printf.sprintf "%s.%s %s %s"
        (P.name_to_string o)
        (P.version_to_string o)
        (action_strings (if P.compare o p < 0 then `up else `down))
        (P.version_to_string p)
    | To_recompile p ->
      Printf.sprintf "%s %s" (action_strings `reinst) (P.to_string p)
    | To_delete p    ->
      Printf.sprintf "%s %s" (action_strings `rm) (P.to_string p)

  let to_aligned_strings l =
    let code c =
      if (OpamConsole.utf8 ()) then action_color c (action_strings c)
      else "-"
    in
    let name p = OpamConsole.colorise `bold (P.name_to_string p) in
    let tbl =
      List.map (function
          | To_change (None, p) ->
            [ code `inst; "install";
              name p; P.version_to_string p ]
          | To_change (Some o, p) ->
            let up = P.compare o p < 0 in
            [ code (if up then `up else `down);
              if up then "upgrade" else "downgrade";
              name p;
              Printf.sprintf "%s to %s"
                (P.version_to_string o) (P.version_to_string p) ]
          | To_recompile p ->
            [ code `reinst; "recompile";
              name p; P.version_to_string p ]
          | To_delete p ->
            [ code `rm; "remove";
              name p; P.version_to_string p ])
        l
    in
    List.map (String.concat " ") (OpamMisc.Format.align_table tbl)

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
            OpamMisc.String.Map.add (A.Pkg.name_to_string p) p acc
          | _ -> acc)
        g OpamMisc.String.Map.empty
    in
    let reduced = ref Map.empty in
    let g =
      map_vertex (function
          | To_change (None, p) as act ->
            (try
               let p0 = OpamMisc.String.Map.find (A.Pkg.name_to_string p) removals in
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

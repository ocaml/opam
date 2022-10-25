(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamTypesBase

let log ?level fmt = OpamConsole.log ?level "CUDF" fmt
let slog = OpamConsole.slog

(* custom cudf field labels *)
let s_source = "opam-name"
let s_source_number = "opam-version"
let s_reinstall = "reinstall"
let s_installed_root = "installed-root"
let s_pinned = "pinned"
let s_version_lag = "version-lag"

let opam_invariant_package_name =
  Dose_common.CudfAdd.encode "=opam-invariant"

let opam_invariant_package_version = 1

let opam_invariant_package =
  opam_invariant_package_name, opam_invariant_package_version

let opam_deprequest_package_name =
  Dose_common.CudfAdd.encode "=opam-deprequest"

let opam_deprequest_package_version = 1

let opam_deprequest_package =
  opam_deprequest_package_name, opam_deprequest_package_version

let is_opam_invariant p =
  p.Cudf.package = opam_invariant_package_name

let is_opam_deprequest p =
  p.Cudf.package = opam_deprequest_package_name

let unavailable_package_name =
  Dose_common.CudfAdd.encode "=unavailable"
let unavailable_package_version = 1
let unavailable_package = unavailable_package_name, unavailable_package_version
let is_unavailable_package p = p.Cudf.package = unavailable_package_name

let cudf2opam cpkg =
  if is_opam_invariant cpkg then
    OpamConsole.error_and_exit `Internal_error
      "Internal error: tried to access the CUDF opam invariant as an opam \
       package";
  if is_opam_deprequest cpkg then
    OpamConsole.error_and_exit `Internal_error
      "Internal error: tried to access the CUDF dependency request as an opam \
       package";
  let sname = Cudf.lookup_package_property cpkg s_source in
  let name = OpamPackage.Name.of_string sname in
  let sver = Cudf.lookup_package_property cpkg s_source_number in
  let version = OpamPackage.Version.of_string sver in
  OpamPackage.create name version

let cudfnv2opam ?version_map ?cudf_universe (name,v) =
  let nv = match cudf_universe with
    | None -> None
    | Some u ->
      try Some (cudf2opam (Cudf.lookup_package u (name,v)))
      with Not_found -> None
  in
  match nv with
  | Some nv -> nv
  | None ->
    let name = OpamPackage.Name.of_string (Dose_common.CudfAdd.decode name) in
    match version_map with
    | Some vmap ->
      let nvset =
        OpamPackage.Map.filter
          (fun nv cv -> nv.name = name && cv = v)
          vmap
      in
      fst (OpamPackage.Map.choose nvset)
    | None -> raise Not_found

let string_of_package p =
  let installed = if p.Cudf.installed then "installed" else "not-installed" in
  Printf.sprintf "%s.%d(%s)"
    p.Cudf.package
    p.Cudf.version installed

let string_of_packages l =
  OpamStd.List.to_string string_of_package l

module Json = struct

  let (>>=) = OpamStd.Option.Op.(>>=)

  let int_to_json n : OpamJson.t = `Float (float_of_int n)
  let int_of_json = function
    | `Float x -> Some (int_of_float x)
    | _ -> None

  let string_to_json s : OpamJson.t = `String s
  let string_of_json = function
    | `String s -> Some s
    | _ -> None

  let pkgname_to_json name : OpamJson.t = string_to_json name
  let pkgname_of_json json = string_of_json json

  let bool_to_json bool : OpamJson.t = `Bool bool
  let bool_of_json = function
    | `Bool b -> Some b
    | _ -> None

  let list_to_json elem_to_json li : OpamJson.t =
    `A (List.map elem_to_json li)
  let list_of_json elem_of_json = function
    | `A jsons ->
      begin try
          let get = function
            | None -> raise Not_found
            | Some v -> v
          in
          Some (List.map (fun json -> get (elem_of_json json)) jsons)
        with Not_found -> None
      end
    | _ -> None

  let option_to_json elem_to_json = function
    | None -> `Null
    | Some elem ->
      let json = elem_to_json elem in
      assert (json <> `Null);
      json
  let option_of_json elem_of_json = function
    | `Null -> Some None
    | other ->
      elem_of_json other >>= fun elem -> Some (Some elem)

  let pair_to_json
      fst_field fst_to_json
      snd_field snd_to_json (fst, snd) =
    `O [(fst_field, fst_to_json fst);
        (snd_field, snd_to_json snd)]

  let pair_of_json
      fst_field fst_of_json
      snd_field snd_of_json : OpamJson.t -> _ = function
    | `O dict ->
      begin try
          fst_of_json (List.assoc fst_field dict) >>= fun fst ->
          snd_of_json (List.assoc snd_field dict) >>= fun snd ->
          Some (fst, snd)
        with Not_found -> None
      end
    | _ -> None

  let version_to_json n = int_to_json n
  let version_of_json json = int_of_json json

  let relop_to_json : Cudf_types.relop -> _ = function
    | `Eq -> `String "eq"
    | `Neq -> `String "neq"
    | `Geq -> `String "geq"
    | `Gt -> `String "gt"
    | `Leq -> `String "leq"
    | `Lt -> `String "lt"

  let relop_of_json : _ -> Cudf_types.relop option = function
    | `String "eq" -> Some `Eq
    | `String "neq" -> Some `Neq
    | `String "geq" -> Some `Geq
    | `String "gt" -> Some `Gt
    | `String "leq" -> Some `Leq
    | `String "lt" -> Some `Lt
    | _ -> None

  let enum_keep_to_json = function
    | `Keep_version -> `String "keep_version"
    | `Keep_package -> `String "keep_package"
    | `Keep_feature -> `String "keep_feature"
    | `Keep_none -> `String "keep_none"
  let enum_keep_of_json = function
    | `String "keep_version" -> Some (`Keep_version)
    | `String "keep_package" -> Some (`Keep_package)
    | `String "keep_feature" -> Some (`Keep_feature)
    | `String "keep_none" -> Some (`Keep_none)
    | _ -> None

  let constr_to_json constr =
    option_to_json
      (pair_to_json "relop" relop_to_json "version" version_to_json)
      constr
  let constr_of_json json =
    option_of_json
      (pair_of_json "relop" relop_of_json "version" version_of_json)
      json

  let vpkg_to_json v =
    pair_to_json "pkgname" pkgname_to_json "constr" constr_to_json v
  let vpkg_of_json json =
    pair_of_json "pkgname" pkgname_of_json "constr" constr_of_json json

  let vpkglist_to_json (vpkglist : Cudf_types.vpkglist) =
    list_to_json vpkg_to_json vpkglist
  let vpkglist_of_json jsons : Cudf_types.vpkglist option =
    list_of_json vpkg_of_json jsons

  let veqpkg_to_json veqpkg = vpkg_to_json (veqpkg :> Cudf_types.vpkg)
  let veqpkg_of_json json =
    vpkg_of_json json >>= function
    | (pkgname, None) -> Some (pkgname, None)
    | (pkgname, Some (`Eq, version)) -> Some (pkgname, Some (`Eq, version))
    | (_pkgname, Some (_, _version)) -> None

  let veqpkglist_to_json veqpkglist = list_to_json veqpkg_to_json veqpkglist
  let veqpkglist_of_json jsons = list_of_json veqpkg_of_json jsons

  let vpkgformula_to_json formula =
    list_to_json (list_to_json vpkg_to_json) formula
  let vpkgformula_of_json json =
    list_of_json (list_of_json vpkg_of_json) json

  let binding_to_json value_to_json v =
    pair_to_json "key" string_to_json "value" value_to_json v
  let binding_of_json value_of_json v =
    pair_of_json "key" string_of_json "value" value_of_json v

  let stanza_to_json value_to_json stanza =
    list_to_json (binding_to_json value_to_json) stanza
  let stanza_of_json value_of_json json =
    list_of_json (binding_of_json value_of_json) json

  let type_schema_to_json tag value_to_json value =
    pair_to_json
      "type" string_to_json
      "default" (option_to_json value_to_json)
      (tag, value)
  let rec typedecl1_to_json = function
    | `Int n ->
      type_schema_to_json "int" int_to_json n
    | `Posint n ->
      type_schema_to_json "posint" int_to_json n
    | `Nat n ->
      type_schema_to_json "nat" int_to_json n
    | `Bool b ->
      type_schema_to_json "bool" bool_to_json b
    | `String s ->
      type_schema_to_json "string" string_to_json s
    | `Pkgname s ->
      type_schema_to_json "pkgname" pkgname_to_json s
    | `Ident s ->
      type_schema_to_json "ident" string_to_json s
    | `Enum (enums, v) ->
      pair_to_json
        "type" string_to_json
        "default" (pair_to_json
                     "set" (list_to_json string_to_json)
                     "default" (option_to_json string_to_json))
        ("enum", (enums, v))
    | `Vpkg v ->
      type_schema_to_json "vpkg" vpkg_to_json v
    | `Vpkgformula v ->
      type_schema_to_json "vpkgformula" vpkgformula_to_json v
    | `Vpkglist v ->
      type_schema_to_json "vpkglist" vpkglist_to_json v
    | `Veqpkg v ->
      type_schema_to_json "veqpkg" veqpkg_to_json v
    | `Veqpkglist v ->
      type_schema_to_json "veqpkglist" veqpkglist_to_json v
    | `Typedecl td ->
      type_schema_to_json "typedecl" typedecl_to_json td
  and typedecl_to_json td =
    stanza_to_json typedecl1_to_json td

  let rec typedecl1_of_json json =
    pair_of_json "type" string_of_json "default" (fun x -> Some x) json >>=
    fun (tag, json) ->
    match tag with
    | "int" -> option_of_json int_of_json json >>= fun x -> Some (`Int x)
    | "posint" -> option_of_json int_of_json json >>= fun x -> Some (`Posint x)
    | "nat" -> option_of_json int_of_json json >>= fun x -> Some (`Nat x)
    | "bool" -> option_of_json bool_of_json json >>= fun x -> Some (`Bool x)
    | "string" -> option_of_json string_of_json json >>= fun x -> Some (`String x)
    | "pkgname" -> option_of_json string_of_json json >>= fun x -> Some (`Pkgname x)
    | "ident" -> option_of_json string_of_json json >>= fun x -> Some (`Ident x)
    | "enum" ->
      pair_of_json
        "set" (list_of_json string_of_json)
        "default" (option_of_json string_of_json)
        json >>= fun x -> Some (`Enum x)
    | "vpkg" ->
      option_of_json vpkg_of_json json >>= fun x -> Some (`Vpkg x)
    | "vpkgformula" ->
      option_of_json vpkgformula_of_json json >>= fun x -> Some (`Vpkgformula x)
    | "vpkglist" ->
      option_of_json vpkglist_of_json json >>= fun x -> Some (`Vpkglist x)
    | "veqpkg" ->
      option_of_json veqpkg_of_json json >>= fun x -> Some (`Veqpkg x)
    | "veqpkglist" ->
      option_of_json veqpkglist_of_json json >>= fun x -> Some (`Veqpkglist x)
    | "typedecl" ->
      option_of_json typedecl_of_json json >>= fun x -> Some (`Typedecl x)
    | _ -> None
  and typedecl_of_json json =
    stanza_of_json typedecl1_of_json json

  let type_tagged_to_json tag value_to_json value =
    pair_to_json "type" string_to_json "value" value_to_json (tag, value)

  let typed_value_to_json : Cudf_types.typed_value -> _ = function
    | `Int n ->
      type_tagged_to_json "int" int_to_json n
    | `Posint n ->
      type_tagged_to_json "posint" int_to_json n
    | `Nat n ->
      type_tagged_to_json "nat" int_to_json n
    | `Bool b ->
      type_tagged_to_json "bool" bool_to_json b
    | `String s ->
      type_tagged_to_json "string" string_to_json s
    | `Pkgname name ->
      type_tagged_to_json "pkgname" pkgname_to_json name
    | `Ident id ->
      type_tagged_to_json "ident" string_to_json id
    | `Enum (enums, value) ->
      type_tagged_to_json "enum"
        (pair_to_json
           "set" (list_to_json string_to_json)
           "choice" string_to_json) (enums, value)
    | `Vpkg vpkg ->
      type_tagged_to_json "vpkg" vpkg_to_json vpkg
    | `Vpkgformula vpkgformula ->
      type_tagged_to_json "vpkgformula" vpkgformula_to_json vpkgformula
    | `Vpkglist vpkglist ->
      type_tagged_to_json "vpkglist" vpkglist_to_json vpkglist
    | `Veqpkg veqpkg ->
      type_tagged_to_json "veqpkg" veqpkg_to_json veqpkg
    | `Veqpkglist veqpkglist ->
      type_tagged_to_json "veqpkglist" veqpkglist_to_json veqpkglist
    | `Typedecl typedecl ->
      type_tagged_to_json "typedecl" typedecl_to_json typedecl

  let typed_value_of_json json : Cudf_types.typed_value option =
    pair_of_json "type" string_of_json "value" (fun x -> Some x) json >>=
    fun (tag, json) ->
    match tag with
    | "int" -> int_of_json json >>= fun x -> Some (`Int x)
    | "posint" -> int_of_json json >>= fun x -> Some (`Posint x)
    | "nat" -> int_of_json json >>= fun x -> Some (`Nat x)
    | "bool" -> bool_of_json json >>= fun x -> Some (`Bool x)
    | "string" -> string_of_json json >>= fun x -> Some (`String x)
    | "pkgname" -> string_of_json json >>= fun x -> Some (`Pkgname x)
    | "ident" -> string_of_json json >>= fun x -> Some (`Ident x)
    | "enum" -> pair_of_json
                     "set" (list_of_json string_of_json)
                     "choice" string_of_json
                     json >>= fun p -> Some (`Enum p)
    | "vpkg" -> vpkg_of_json json >>= fun x -> Some (`Vpkg x)
    | "vpkgformula" -> vpkgformula_of_json json >>= fun x -> Some (`Vpkgformula x)
    | "vpkglist" -> vpkglist_of_json json >>= fun x -> Some (`Vpkglist x)
    | "veqpkg" -> veqpkg_of_json json >>= fun x -> Some (`Veqpkg x)
    | "veqpkglist" -> veqpkglist_of_json json >>= fun x -> Some (`Veqpkglist x)
    | "typedecl" -> typedecl_of_json json >>= fun x -> Some (`Typedecl x)
    | _ -> None

  let package_to_json p =
    `O [ ("name", pkgname_to_json p.Cudf.package);
         ("version", version_to_json p.Cudf.version);
         ("depends", vpkgformula_to_json p.Cudf.depends);
         ("conflicts", vpkglist_to_json p.Cudf.conflicts);
         ("provides", veqpkglist_to_json p.Cudf.provides);
         ("installed", bool_to_json p.Cudf.installed);
         ("was_installed", bool_to_json p.Cudf.was_installed);
         ("keep", enum_keep_to_json p.Cudf.keep);
         ("pkg_extra", stanza_to_json typed_value_to_json p.Cudf.pkg_extra);
       ]

  let package_of_json = function
    | `O dict ->
      begin try
          pkgname_of_json (List.assoc "name" dict) >>= fun package ->
          version_of_json (List.assoc "version" dict) >>= fun version ->
          vpkgformula_of_json (List.assoc "depends" dict) >>= fun depends ->
          vpkglist_of_json (List.assoc "conflicts" dict) >>= fun conflicts ->
          veqpkglist_of_json (List.assoc "provides" dict) >>= fun provides ->
          bool_of_json (List.assoc "installed" dict) >>= fun installed ->
          bool_of_json (List.assoc "was_installed" dict) >>= fun was_installed ->
          enum_keep_of_json (List.assoc "keep" dict) >>= fun keep ->
          stanza_of_json typed_value_of_json (List.assoc "pkg_extra" dict) >>= fun pkg_extra ->
          Some { Cudf.package = package;
            version;
            depends;
            conflicts;
            provides;
            installed;
            was_installed;
            keep;
            pkg_extra;
          }
        with Not_found -> None
      end
    | _ -> None
end

let to_json = Json.package_to_json
let of_json = Json.package_of_json

(* Graph of cudf packages *)
module Package = struct
  type t = Cudf.package
  include Dose_common.CudfAdd
  let to_string = string_of_package
  let name_to_string t = t.Cudf.package
  let version_to_string t = string_of_int t.Cudf.version
  let to_json = to_json
  let of_json = of_json
end

module Action = OpamActionGraph.MakeAction(Package)
module ActionGraph = OpamActionGraph.Make(Action)

let string_of_action = Action.to_string

let string_of_actions l =
  OpamStd.List.to_string (fun a -> " - " ^ string_of_action a) l

let action_contents = function
  | `Remove p | `Install p | `Reinstall p | `Change (_,_,p) -> p
  | `Build _ | `Fetch _ ->
    OpamConsole.error_and_exit `Internal_error
      "Shouldn't have a multi package action on printing solution"

exception Solver_failure of string
exception Cyclic_actions of Action.t list list

type conflict_case =
  | Conflict_dep of (unit -> Dose_algo.Diagnostic.reason list)
  | Conflict_cycle of Cudf.package action list list
type conflict =
  Cudf.universe * int package_map * conflict_case

module Map = OpamStd.Map.Make(Package)
module Set = OpamStd.Set.Make(Package)

let strong_and_weak_deps u deps =
  (* strong deps are mandatory (constraint appearing in the top conjunction)
     weak deps correspond to optional occurrences of a package, as part of a
     disjunction: e.g. in (A>=4 & (B | A<5)), A>=4 is strong, and the other two
     are weak. In the end we want to retain B and A>=4. *)
  List.fold_left (fun (strong_deps, weak_deps) l ->
      let names =
        List.fold_left (fun acc (n, _) ->
            OpamStd.String.Map.add n Set.empty acc)
          OpamStd.String.Map.empty l
      in
      let set =
        List.fold_left (fun acc (n, cstr) ->
            List.fold_left (fun s x -> Set.add x s)
              acc (Cudf.lookup_packages ~filter:cstr u n))
          Set.empty l
      in
      let by_name =
        Set.fold (fun p ->
            OpamStd.String.Map.update
              p.Cudf.package (Set.add p) Set.empty)
          set names
      in
      if OpamStd.String.Map.is_singleton by_name then
        let name, versions = OpamStd.String.Map.choose by_name in
        OpamStd.String.Map.update name (Set.inter versions) versions
          strong_deps,
        OpamStd.String.Map.remove name weak_deps
      else
      let by_name =
        OpamStd.String.Map.filter
          (fun name _ -> not (OpamStd.String.Map.mem name strong_deps))
          by_name
      in
      strong_deps, OpamStd.String.Map.union Set.union weak_deps by_name)
    (OpamStd.String.Map.empty, OpamStd.String.Map.empty)
    deps

(* From a CUDF dependency CNF, extract the set of packages that can possibly be
   part of a solution.

   This is much finer than [Dose_common.CudfAdd.resolve_deps] which doesn't handle
   conjunctions of versions (see [Graph.of_universe] below) *)
let dependency_set u deps =
  let strong_deps, weak_deps = strong_and_weak_deps u deps in
  OpamStd.String.Map.fold (fun _ -> Set.union) strong_deps @@
  OpamStd.String.Map.fold (fun _ -> Set.union) weak_deps @@
  Set.empty

(* From a CUDF dependency CNF, extract the set of packages that will necessarily
   be part of any solution (with a choice among packages of the same name of
   course) *)
let _strong_dependency_set u deps =
  let strong_deps, _ = strong_and_weak_deps u deps in
  OpamStd.String.Map.fold (fun _ -> Set.union) strong_deps Set.empty

let rec_strong_dependency_map u deps =
  let module SM = OpamStd.String.Map in
  let rec aux seen deps =
    let strong_deps, _ = strong_and_weak_deps u deps in
    OpamStd.String.Map.fold (fun name ps (seen, acc) ->
        let seen, common_strong_deps =
          Set.fold (fun p (seen, acc) ->
              let seen, dmap =
                try seen, Map.find p seen with Not_found ->
                  let seen, r = aux (Map.add p SM.empty seen) p.Cudf.depends in
                  Map.add p r seen, r
              in
              seen,
              Some (match acc with
                  | None -> dmap
                  | Some m ->
                    SM.merge (fun _ a b -> match a, b with
                        | Some a, Some b -> Some (Set.union a b)
                        | _ -> None)
                      m dmap))
            ps (seen, None)
        in
        let strong_deps =
          SM.add name ps
            (OpamStd.Option.default SM.empty common_strong_deps)
        in
        seen, SM.union Set.inter acc strong_deps)
      strong_deps (seen, SM.empty)
  in
  snd (aux Map.empty deps)

let _rec_strong_dependency_set u deps =
  OpamStd.String.Map.fold (fun _ -> Set.union)
    (rec_strong_dependency_map u deps)
    Set.empty

module Graph = struct

  module PG = struct
    include Dose_algo.Defaultgraphs.PackageGraph.G
    let succ g v =
      try succ g v
      with e -> OpamStd.Exn.fatal e; []
  end

  module PO = Dose_algo.Defaultgraphs.GraphOper (PG)

  module Topo = Graph.Topological.Make (PG)

  let of_universe u =
    (* {[Dose_algo.Defaultgraphs.PackageGraph.dependency_graph u]}
       -> doesn't handle conjunctive dependencies correctly
       (e.g. (a>3 & a<=4) is considered as (a>3 | a<=4) and results in extra
       edges).
       Here we handle the fact that a conjunction with the same pkgname is an
       intersection, while a conjunction between different names is an union *)
    let t = OpamConsole.timer () in
    let g = PG.create ~size:(Cudf.universe_size u) () in
    let iter_deps f deps =
      (* List.iter (fun d -> List.iter f (Dose_common.CudfAdd.resolve_deps u d)) deps *)
      Set.iter f (dependency_set u deps)
    in
    Cudf.iter_packages
      (fun p ->
         PG.add_vertex g p;
         iter_deps (PG.add_edge g p) p.Cudf.depends)
      u;
    log ~level:3 "Graph generation: %.3f" (t ());
    g

  let output g filename =
    let fd = open_out (filename ^ ".dot") in
    Dose_algo.Defaultgraphs.PackageGraph.DotPrinter.output_graph fd g;
    close_out fd

  let transitive_closure g =
    PO.O.add_transitive_closure g

  let linearize g pkgs =
    Topo.fold (fun p acc -> if Set.mem p pkgs then p::acc else acc) g []

  let mirror = PO.O.mirror

  include PG
end

(** Special package used by Dose internally, should generally be filtered out *)
let dose_dummy_request = Dose_algo.Depsolver.dummy_request.Cudf.package
let is_artefact cpkg =
  is_opam_invariant cpkg ||
  is_opam_deprequest cpkg ||
  cpkg.Cudf.package = dose_dummy_request

let dependencies universe packages =
  Set.fixpoint (fun p -> dependency_set universe p.Cudf.depends) packages
(* similar to Dose_algo.Depsolver.dependency_closure but with finer results on
   version sets *)

let reverse_dependencies universe =
  let tbl = Array.make (Cudf.universe_size universe) [] in
  Cudf.iteri_packages (fun uid p ->
      Set.iter
        (fun q ->
           let i = Cudf.uid_by_package universe q in
           tbl.(i) <- uid :: tbl.(i))
        (dependency_set universe p.Cudf.depends))
    universe;
  Set.fixpoint
    (fun p ->
       List.fold_left
         (fun acc uid -> Set.add (Cudf.package_by_uid universe uid) acc)
         Set.empty
         tbl.(Cudf.uid_by_package universe p))
(* similar to Dose_algo.Depsolver.reverse_dependency_closure but more reliable
   and faster *)

let dependency_sort universe packages =
  let graph = Graph.of_universe universe in
  Graph.linearize graph packages |> List.rev

let string_of_atom (p, c) =
  let const = function
    | None       -> ""
    | Some (r,v) -> Printf.sprintf " (%s %d)" (OpamPrinter.FullPos.relop_kind r) v in
  Printf.sprintf "%s%s" p (const c)

let string_of_vpkgs constr =
  let constr = List.sort (fun (a,_) (b,_) -> String.compare a b) constr in
  OpamFormula.string_of_conjunction string_of_atom constr

let string_of_universe u =
  string_of_packages (List.sort Dose_common.CudfAdd.compare (Cudf.get_packages u))

let vpkg2atom cudfnv2opam (name,cstr) =
  match cstr with
  | None ->
    OpamPackage.Name.of_string (Dose_common.CudfAdd.decode name), None
  | Some (relop,v) ->
    let nv = cudfnv2opam (name,v) in
    nv.name, Some (relop, nv.version)
(* Should be unneeded now that we pass a full version_map along
   [{
      log "Could not find corresponding version in cudf universe: %a"
        (slog string_of_atom) (name,cstr);
      let candidates =
        Cudf.lookup_packages cudf_universe name in
      let solutions =
        Cudf.lookup_packages ~filter:cstr cudf_universe name in
      let module OVS = OpamPackage.Version.Set in
      let to_version_set l =
        OVS.of_list
          (List.map (fun p -> OpamPackage.version (cudf2opam p)) l) in
      let solutions = to_version_set solutions in
      let others = OVS.Op.(to_version_set candidates -- solutions) in
      OpamPackage.Name.of_string (Dose_common.CudfAdd.decode name),
      match relop, OVS.is_empty solutions, OVS.is_empty others with
      | _, true, true -> None
      | `Leq, false, _ | `Lt, false, true -> Some (`Leq, OVS.max_elt solutions)
      | `Lt, _, false | `Leq, true, false -> Some (`Lt, OVS.min_elt others)
      | `Geq, false, _ | `Gt, false, true -> Some (`Geq, OVS.min_elt solutions)
      | `Gt, _, false | `Geq, true, false -> Some (`Gt, OVS.max_elt others)
      | `Eq, false, _ -> Some (`Eq, OVS.choose solutions)
      | `Eq, true, _ ->
        Some (`Eq, OpamPackage.Version.of_string "<unavailable version>")
      | `Neq, false, true -> None
      | `Neq, _, false -> Some (`Neq, OVS.choose others)
   }]
*)

let conflict_empty ~version_map univ =
  Conflicts (univ, version_map, Conflict_dep (fun () -> []))
let make_conflicts ~version_map univ = function
  | {Dose_algo.Diagnostic.result = Dose_algo.Diagnostic.Failure f; _} ->
    Conflicts (univ, version_map, Conflict_dep f)
  | {Dose_algo.Diagnostic.result = Dose_algo.Diagnostic.Success _; _} ->
    raise (Invalid_argument "make_conflicts")
let cycle_conflict ~version_map univ cycle =
  Conflicts (univ, version_map, Conflict_cycle cycle)

let arrow_concat sl =
  let arrow =
    Printf.sprintf " %s "
      (OpamConsole.utf8_symbol OpamConsole.Symbols.rightwards_arrow "->")
  in
  String.concat (OpamConsole.colorise `yellow arrow) sl

let formula_of_vpkgl cudfnv2opam all_packages vpkgl =
  let atoms =
    List.map (fun vp ->
        try vpkg2atom cudfnv2opam vp
        with Not_found ->
          OpamPackage.Name.of_string (Dose_common.CudfAdd.decode (fst vp)), None)
      vpkgl
  in
  let names = OpamStd.List.sort_nodup compare (List.map fst atoms) in
  let by_name =
    List.map (fun name ->
        let formula =
          OpamFormula.ors (List.map (function
              | n, Some atom when n = name -> Atom atom
              | _ -> Empty)
              atoms)
        in
        let all_versions = OpamPackage.versions_of_name all_packages name in
        let formula = OpamFormula.simplify_version_set all_versions formula in
        Atom (name, formula))
      names
  in
  OpamFormula.ors by_name

(* - Conflict message handling machinery - *)


(* chain sets: sets of package lists *)
module ChainSet = struct

  include OpamStd.Set.Make (struct
      type t = Package.t list
      let rec compare t1 t2 = match t1, t2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | p1::r1, p2::r2 ->
          match Package.compare p1 p2 with 0 -> compare r1 r2 | n -> n
      let to_string t =
        arrow_concat (List.rev_map Package.to_string t)
      let to_json t = Json.list_to_json Package.to_json t
      let of_json j = Json.list_of_json Package.of_json j
    end)

  (** Turns a set of lists into a list of sets *)
  let rec transpose cs =
    let hds, tls =
      fold (fun c (hds, tls) -> match c with
          | hd::tl -> Set.add hd hds, add tl tls
          | [] -> hds, tls)
        cs (Set.empty, empty)
    in
    if Set.is_empty hds then []
    else hds :: transpose tls

  (** cs1 precludes cs2 if it contains a list that is prefix to all elements of
      cs2 *)
  let precludes cs1 cs2 =
    let rec list_is_prefix pfx l = match pfx, l with
      | [], _ -> true
      | a::r1, b::r2 when Package.equal a b -> list_is_prefix r1 r2
      | _ -> false
    in
    exists (fun pfx -> for_all (fun l -> list_is_prefix pfx l) cs2) cs1

  let length cs = fold (fun l acc -> min (List.length l) acc) cs max_int
end

type explanation =
  [ `Conflict of string option * string list * bool
  | `Missing of string option * string *
                (OpamPackage.Name.t * OpamFormula.version_formula)
                  OpamFormula.formula
  ]

module Pp_explanation = struct
  let pp_package fmt pkg =
    let name = pkg.Cudf.package in
    let version =
      match List.assoc_opt "opam-version" pkg.Cudf.pkg_extra with
      | Some (`String v) -> v
      | None | Some _ -> "???"
    in
    Format.fprintf fmt "%s.%s" name version

  let pp_relop fmt = function
    | `Eq -> Format.pp_print_string fmt "="
    | `Neq -> Format.pp_print_string fmt "!="
    | `Geq -> Format.pp_print_string fmt ">="
    | `Gt -> Format.pp_print_string fmt ">"
    | `Leq -> Format.pp_print_string fmt "<="
    | `Lt -> Format.pp_print_string fmt "<"

  let pp_vpkg cudfnv2opam fmt vpkg = match vpkg2atom cudfnv2opam vpkg with
    | pkg, None ->
      Format.pp_print_string fmt (OpamPackage.Name.to_string pkg)
    | pkg, Some (relop, v) ->
      Format.pp_print_string fmt (OpamPackage.Name.to_string pkg);
      pp_relop fmt relop;
      Format.pp_print_string fmt (OpamPackage.Version.to_string v)
    | exception Not_found ->
      Format.pp_print_string fmt "??"

  let pp_inline_list f fmt = function
    | [] ->
      Format.pp_print_string fmt "[]"
    | x::xs ->
      Format.fprintf fmt "[%a" f x;
      List.iter (Format.fprintf fmt ", %a" f) xs;
      Format.pp_print_string fmt "]"

  let pp_reason cudfnv2opam fmt = function
    | Dose_algo.Diagnostic.Conflict (a, b, vpkg) ->
      Format.fprintf fmt "Conflict (%a, %a, %a)" pp_package a pp_package b (pp_vpkg cudfnv2opam) vpkg
    | Dose_algo.Diagnostic.Dependency (a, vpkglist, pkglist) ->
      Format.fprintf fmt "Dependency (%a, %a, %a)" pp_package a (pp_inline_list (pp_vpkg cudfnv2opam)) vpkglist (pp_inline_list pp_package) pkglist
    | Dose_algo.Diagnostic.Missing (a, vpkglist) ->
      Format.fprintf fmt "Missing (%a, %a)" pp_package a (pp_inline_list (pp_vpkg cudfnv2opam)) vpkglist

  let pp_list f fmt = function
    | [] ->
      Format.pp_print_string fmt "[]"
    | l ->
      Format.pp_print_string fmt "[\n";
      List.iter (Format.fprintf fmt "  %a;\n" f) l;
      Format.pp_print_string fmt "]"

  let pp_reasonlist cudfnv2opam fmt l = pp_list (pp_reason cudfnv2opam) fmt l

  let pp_option f fmt = function
    | None -> Format.pp_print_string fmt "None"
    | Some x -> Format.fprintf fmt "Some (%a)" f x

  let pp_formula fmt formula =
    let aux (name, vformula) =
      let aux (relop, v) =
        Format.asprintf "%a%s" pp_relop relop (OpamPackage.Version.to_string v)
      in
      OpamPackage.Name.to_string name^" {"^OpamFormula.string_of_formula aux vformula^"}"
    in
    Format.pp_print_string fmt (OpamFormula.string_of_formula aux formula)

  let pp_explanation fmt = function
    | `Conflict (x, y, b) ->
      Format.fprintf fmt "`Conflict (%a, %a, %b)" (pp_option Format.pp_print_string) x (pp_inline_list Format.pp_print_string) y b
    | `Missing (x, y, formula) ->
      Format.fprintf fmt "`Missing (%a, %s, %a)" (pp_option Format.pp_print_string) x y pp_formula formula

  let pp_explanationlist fmt l = pp_list pp_explanation fmt l
end

let extract_explanations packages cudfnv2opam reasons : explanation list =
  log "Conflict reporting";
  let open Dose_algo.Diagnostic in
  let module CS = ChainSet in
  (* Definitions and printers *)
  log ~level:3 "Reasons: %a" (Pp_explanation.pp_reasonlist cudfnv2opam) reasons;
  let all_opam =
    let add p set =
      if is_artefact p then set
      else OpamPackage.Set.add (cudf2opam p) set
    in
    List.fold_left (fun acc -> function
        | Conflict (l, r, _) -> add l @@ add r @@ acc
        | Dependency (l, _, rs) ->
          List.fold_left (fun acc p -> add p acc) (add l acc) rs
        | Missing (p, _) -> add p acc)
      OpamPackage.Set.empty
      reasons
  in
  let print_set pkgs =
    if Set.exists is_artefact pkgs then
      if Set.exists is_opam_invariant pkgs then "(invariant)"
      else "(request)"
    else
    let nvs =
      OpamPackage.to_map @@
      Set.fold (fun p s -> OpamPackage.Set.add (cudf2opam p) s)
        pkgs OpamPackage.Set.empty
    in
    let strs =
      OpamPackage.Name.Map.mapi (fun name versions ->
          let all_versions = OpamPackage.versions_of_name all_opam name in
          let formula =
            OpamFormula.formula_of_version_set all_versions versions
          in
          OpamFormula.to_string (Atom (name, formula)))
        nvs
    in
    String.concat ", " (OpamPackage.Name.Map.values strs)
  in
  let cs_to_string ?(hl_last=true) cs =
    let rec aux vpkgl = function
      | [] -> []
      | pkgs :: r ->
        let vpkgl1 =
          List.fold_left (fun acc -> function
              | Dependency (p1, vpl, _) when Set.mem p1 pkgs ->
                List.rev_append vpl acc
              | _ -> acc)
            [] reasons
        in
        if Set.exists is_artefact pkgs then
          if Set.exists is_opam_invariant pkgs then
            Printf.sprintf "(invariant)"
            :: aux vpkgl1 r
          else if r = [] then ["(request)"]
          else aux vpkgl1 r (* request *)
        else if vpkgl = [] then
          print_set pkgs :: aux vpkgl1 r
        else
        let f =
          let vpkgl =
            List.filter
              (fun (n, _) -> Set.exists (fun p -> p.package = n) pkgs)
              vpkgl
          in
          (* TODO: We should aim to use what does give us not guess the formula *)
          (* Dose is precise enough from what i'm seeing *)
          formula_of_vpkgl cudfnv2opam packages vpkgl
        in
        let s = OpamFormula.to_string f in
        (if hl_last && r = [] then OpamConsole.colorise' [`red;`bold]  s else s)
        :: aux vpkgl1 r
    in
    arrow_concat (aux [] (CS.transpose (CS.map List.rev cs)))
  in
  let get t x = try Hashtbl.find t x with Not_found -> Set.empty in
  let add_set t l set =
    match Hashtbl.find t l with
    | exception Not_found -> Hashtbl.add t l set
    | s -> Hashtbl.replace t l (Set.union set s)
  in
  (* Gather all data in hashtables *)
  let ct = Hashtbl.create 53 in
  let deps = Hashtbl.create 53 in
  let rdeps = Hashtbl.create 53 in
  let missing = Hashtbl.create 53 in
  List.iter (function
      | Conflict (l, r, (_pkgname, _constr)) ->
        add_set ct l (Set.singleton r);
        add_set ct r (Set.singleton l);
      | Dependency (l, _, rs) ->
        add_set deps l (Set.of_list rs);
        List.iter (fun r -> add_set rdeps r (Set.singleton l)) rs
      | Missing (p, deps) ->
        Hashtbl.add missing p deps)
    reasons;
  (* Get paths from the conflicts to requested or invariant packages *)
  let roots =
    let add_artefacts set =
      Hashtbl.fold (fun p _ acc ->
          if is_artefact p then Set.add p acc else acc)
        set
    in
    Set.empty |> add_artefacts deps |> add_artefacts missing |> add_artefacts ct
  in
  let _seen, ct_chains =
    (* get a covering tree from the roots to all reachable packages. *)
    let rec aux seen ct_chains =
      Map.fold (fun pkg parent_chain (seen, ct_chains) ->
          if Set.mem pkg seen then (seen, ct_chains) else
          let dependencies = get deps pkg in
          let seen = Set.add pkg seen in
          Set.fold (fun dep (seen, ct_chains) ->
              let chain = CS.map (fun c -> dep :: c) parent_chain in
              let ct_chains = Map.update dep (CS.union chain) CS.empty ct_chains in
              aux seen ct_chains
            ) dependencies (seen, ct_chains)
        ) ct_chains
        (seen, ct_chains)
    in
    let init_chains =
      Set.fold (fun p -> Map.add p (CS.singleton [p])) roots Map.empty
    in
    aux Set.empty init_chains
  in
  let ct_chains =
    (* We keep only shortest chains. *)
    (* TODO: choose is probably not the right thing here. *)
    (* e.g. if two lists have the size, we should probably have both. *)
    Map.map (fun x -> CS.singleton (CS.choose x)) ct_chains
  in
  let reasons =
    (* order "reasons" by most interesting first: version conflicts then package
       then missing + shortest chains first *)
    let clen p = try CS.length (Map.find p ct_chains) with Not_found -> 0 in
    let version_conflict = function
        | Conflict (l, r, _) -> l.Cudf.package = r.Cudf.package
        | _ -> false
    in
    let cmp a b = match a, b with
      | Conflict (l1, r1, _), Conflict (l2, r2, _) ->
        let va = version_conflict a and vb = version_conflict b in
        if va && not vb then -1 else
        if vb && not va then 1 else
          (match compare (clen l1 + clen r1) (clen l2 + clen r2) with
           | 0 -> (match Package.compare l1 l2 with
               | 0 -> Package.compare r1 r2
               | n -> n)
           | n -> n)
      | _, Conflict _ -> 1
      | Conflict _, _ -> -1
      | Missing (p1, _), Missing (p2, _) ->
        (match compare (clen p1) (clen p2) with
         | 0 -> Package.compare p1 p2
         | n -> n)
      | _, Missing _ -> 1
      | Missing _, _ -> -1
      | Dependency _, Dependency _ -> 0 (* we don't care anymore *)
    in
    List.sort_uniq cmp reasons
  in

  let has_invariant p =
    let chain_has_invariant cs =
      CS.exists (List.exists is_opam_invariant) cs
    in
    try chain_has_invariant (Map.find p ct_chains)
    with Not_found -> false
  in

  let explanations, _remaining_ct_chains =
    List.fold_left (fun (explanations, ct_chains) re ->
        let cst ?hl_last ct_chains p =
          let chains = Map.find p ct_chains in
          Map.filter (fun _ c -> not (CS.precludes chains c)) ct_chains,
          cs_to_string ?hl_last chains
        in
        try
          match re with
          | Conflict (l, r, _) ->
            let ct_chains, csl = cst ct_chains l in
            let ct_chains, csr = cst ct_chains r in
            let msg1 =
              if l.Cudf.package = r.Cudf.package then
                Some (Package.name_to_string l)
              else
                None
            in
            let msg2 = List.sort_uniq compare [csl; csr] in
            let msg3 =
              (has_invariant l || has_invariant r) &&
                 not (List.exists (function `Conflict (_,_,has_invariant) -> has_invariant | _ -> false) explanations)
            in
            let msg = `Conflict (msg1, msg2, msg3) in
            if List.mem msg explanations then raise Not_found else
              msg :: explanations, ct_chains
          | Missing (p, deps) ->
            let ct_chains, csp = cst ~hl_last:false ct_chains p in
            let msg =
              if List.exists
                  (fun (name, _) -> name = unavailable_package_name)
                  deps
              then
                let msg =
                  Printf.sprintf "%s: no longer available"
                    (OpamPackage.to_string (cudf2opam p))
                in
                `Missing (Some csp, msg, OpamFormula.Empty)
              else
              let fdeps = formula_of_vpkgl cudfnv2opam packages deps in
              let sdeps = OpamFormula.to_string fdeps in
              `Missing (Some csp, sdeps, fdeps)
            in
            if List.mem msg explanations then raise Not_found else
              msg :: explanations, ct_chains
          | Dependency _ ->
            explanations, ct_chains
        with Not_found ->
          explanations, ct_chains)
      ([], ct_chains) reasons
  in

  let same_depexts sdeps fdeps =
    List.for_all (function
        | `Missing (_, sdeps', fdeps') -> sdeps = sdeps' && fdeps = fdeps'
        | _ -> false)
  in
  log ~level:3 "Explanations: %a" Pp_explanation.pp_explanationlist explanations;
  match explanations with
  | [] ->
    OpamConsole.error_and_exit `Internal_error
      "Internal error while computing conflict explanations:\n\
       sorry about that. Please report how you got here in \
       https://github.com/ocaml/opam/discussions/5130 if possible."
  | `Missing (_, sdeps, fdeps) :: rest when same_depexts sdeps fdeps rest ->
    [`Missing (None, sdeps, fdeps)]
  | _ -> explanations

let strings_of_cycles cycles =
  let string_of_cycle cycle =
    List.map string_of_action cycle
    |> arrow_concat in
  List.map string_of_cycle cycles

let string_of_conflict ?(start_column=0) (msg1, msg2, msg3) =
  let width = OpamStd.Sys.terminal_columns () - start_column - 2 in
  OpamStd.Format.reformat ~start_column ~indent:2 msg1 ^
  OpamStd.List.concat_map ~left:"\n- " ~nil:"" "\n- "
    (fun s -> OpamStd.Format.reformat ~indent:2 ~width s) msg2 ^
  OpamStd.List.concat_map ~left:"\n" ~nil:"" "\n"
    (fun s -> OpamStd.Format.reformat ~indent:2 ~width s) msg3

let conflict_explanations_raw packages = function
  | univ, version_map, Conflict_dep reasons ->
    let r = reasons () in
    let cudfnv2opam = cudfnv2opam ~cudf_universe:univ ~version_map in
    List.rev (extract_explanations packages cudfnv2opam r),
    []
  | _univ, _version_map, Conflict_cycle cycles ->
    [], cycles

let string_of_explanation unav_reasons = function
  | `Conflict (kind, packages, has_invariant) ->
    let msg1 =
      let format_package_name p =
        Printf.sprintf "No agreement on the version of %s:"
          (OpamConsole.colorise `bold p)
      in
      OpamStd.Option.map_default format_package_name
        "Incompatible packages:" kind
    and msg3 =
      if has_invariant then
        ["You can temporarily relax the switch invariant with \
          `--update-invariant'"]
      else
        []
    in
    (msg1, packages, msg3)
  | `Missing (csp, sdeps, fdeps) ->
    let sdeps = OpamConsole.colorise' [`red;`bold] sdeps in
    let msg1 = "Missing dependency:"
    and msg2 =
      OpamStd.Option.map_default (fun csp -> arrow_concat [csp; sdeps]) sdeps csp
    and msg3 = OpamFormula.fold_right (fun a x -> unav_reasons x::a) [] fdeps
    in
    (msg1, [msg2], msg3)

let conflict_explanations packages unav_reasons = function
  | univ, version_map, Conflict_dep reasons ->
    let r = reasons () in
    let cudfnv2opam = cudfnv2opam ~cudf_universe:univ ~version_map in
    let explanations = extract_explanations packages cudfnv2opam r in
    List.rev_map (string_of_explanation unav_reasons) explanations, []
  | _univ, _version_map, Conflict_cycle cycles ->
    [], strings_of_cycles cycles

let conflict_cycles = function
  | _cudf_universe, _version_map, Conflict_cycle cycles ->
     Some (List.map (List.map (map_action cudf2opam)) cycles)
  | _ -> None

let string_of_explanations unav_reasons (cflts, cycles) =
  let cflts = List.map (string_of_explanation unav_reasons) cflts in
  let cycles = strings_of_cycles cycles in
  let b = Buffer.create 1024 in
  let pr_items b l =
    Buffer.add_string b
      (OpamStd.Format.itemize (fun s -> s) l)
  in
  if cycles <> [] then
    Printf.bprintf b
      "The actions to process have cyclic dependencies:\n%a"
      pr_items cycles;
  if cflts <> [] then
    Buffer.add_string b
      (OpamStd.Format.itemize ~bullet:(OpamConsole.colorise `red "  * ")
         (string_of_conflict ~start_column:4) cflts);
  if cflts = [] && cycles = [] then (* No explanation found *)
    Printf.bprintf b
      "Sorry, no solution found: \
       there seems to be a problem with your request.\n";
  Buffer.add_string b "\n";
  Buffer.contents b

let string_of_conflicts packages unav_reasons conflict =
  string_of_explanations unav_reasons
    (conflict_explanations_raw packages conflict)

let check flag p =
  try Cudf.lookup_typed_package_property p flag = `Bool true
  with Not_found -> false

let need_reinstall = check s_reinstall

(*
let is_installed_root = check s_installed_root

let is_pinned = check s_pinned
*)

let default_preamble =
  let l = [
    (s_source,         `String None);
    (s_source_number,  `String None);
    (s_reinstall,      `Bool (Some false));
    (s_installed_root, `Bool (Some false));
    (s_pinned,         `Bool (Some false));
    (s_version_lag,    `Nat (Some 0));
  ] in
  Dose_common.CudfAdd.add_properties Cudf.default_preamble l

let remove universe name constr =
  let filter p =
    p.Cudf.package <> name
    || not (Cudf.version_matches p.Cudf.version constr) in
  let packages = Cudf.get_packages ~filter universe in
  Cudf.load_universe packages

let uninstall_all universe =
  let packages = Cudf.get_packages universe in
  let packages = List.rev_map (fun p -> { p with Cudf.installed = false }) packages in
  Cudf.load_universe packages

let install universe package =
  let p = Cudf.lookup_package universe (package.Cudf.package, package.Cudf.version) in
  let p = { p with Cudf.installed = true } in
  let packages =
    let filter p =
      p.Cudf.package <> package.Cudf.package
      || p.Cudf.version <> package.Cudf.version in
    Cudf.get_packages ~filter universe in
  Cudf.load_universe (p :: packages)

let remove_all_uninstalled_versions_but universe name constr =
  let filter p =
    p.Cudf.installed
    || p.Cudf.package <> name
    || Cudf.version_matches p.Cudf.version constr in
  let packages = Cudf.get_packages ~filter universe in
  Cudf.load_universe packages

let to_cudf univ (req: Cudf_types.vpkg request) =
  let install =
    let conj = OpamFormula.ands_to_list req.wish_install in
    OpamStd.List.filter_map (function
        | Atom vpkg -> Some vpkg
        | Empty -> None
        | _ -> invalid_arg "OpamCudf.to_cudf: 'install' not a conjunction")
      conj
  in
  Dose_common.CudfAdd.add_properties default_preamble
    (List.map (fun s -> s, `Int (Some 0)) req.extra_attributes),
  univ,
  { Cudf.request_id = "opam";
    install         = install;
    remove          = req.wish_remove;
    upgrade         = req.wish_upgrade;
    req_extra       = [] }

let string_of_request r =
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (OpamFormula.string_of_formula string_of_atom r.wish_install)
    (string_of_vpkgs r.wish_remove)
    (string_of_vpkgs r.wish_upgrade)

let solver_calls = ref 0

let dump_universe oc univ =
  Cudf_printer.pp_cudf oc
    (default_preamble, univ, Cudf.default_request)

let dump_cudf_request ~version_map (_, univ,_ as cudf) criteria =
  function
  | None   -> None
  | Some f ->
    ignore ( version_map: int OpamPackage.Map.t );
    incr solver_calls;
    let filename = Printf.sprintf "%s-%d.cudf" f !solver_calls in
    let oc = open_out filename in
    let module Solver = (val OpamSolverConfig.(Lazy.force !r.solver)) in
    Printf.fprintf oc "# Solver: %s\n"
      (OpamCudfSolver.get_name (module Solver));
    Printf.fprintf oc "# Criteria: %s\n" criteria;
    Cudf_printer.pp_cudf oc cudf;
    OpamPackage.Map.iter (fun (pkg:OpamPackage.t) (vnum: int) ->
      let name = OpamPackage.name_to_string pkg in
      let version = OpamPackage.version_to_string pkg in
      Printf.fprintf oc "#v2v:%s:%d=%s\n" name vnum version;
    ) version_map;
    close_out oc;
    Graph.output (Graph.of_universe univ) f;
    Some filename

let dump_cudf_error ~version_map univ req =
  let cudf_file = match OpamSolverConfig.(!r.cudf_file) with
    | Some f -> f
    | None ->
      let (/) = Filename.concat in
      OpamCoreConfig.(!r.log_dir) /
      ("solver-error-"^string_of_int (OpamStubs.getpid())) in
  match
    dump_cudf_request (to_cudf univ req) ~version_map
      (OpamSolverConfig.criteria req.criteria)
      (Some cudf_file)
  with
  | Some f -> f
  | None -> assert false

let vpkg2set univ vp =
  Set.of_list (Dose_common.CudfAdd.resolve_deps univ vp)

let compute_conflicts univ packages =
  let open Set.Op in
  let to_map set =
    Set.fold (fun p ->
        OpamStd.String.Map.update p.Cudf.package (Set.add p) Set.empty)
      set OpamStd.String.Map.empty
  in
  let direct_conflicts p =
    let base_conflicts =
      Set.filter (fun q -> not (String.equal q.Cudf.package p.Cudf.package))
        (vpkg2set univ p.Cudf.conflicts)
    in
    (* Dependencies not matching constraints are also conflicts *)
    List.fold_left (fun acc -> function
        | (n, c) :: disj when List.for_all (fun (m, _) -> String.equal m n) disj ->
          let coset = function
            | Some (op, v) ->
              let filter = Some (OpamFormula.neg_relop op, v) in
              Set.of_list (Cudf.lookup_packages ~filter univ n)
            | None -> Set.empty
          in
          acc ++
          List.fold_left (fun acc (_, c) -> acc %% coset c) (coset c) disj
        | _ -> acc)
      base_conflicts p.Cudf.depends
  in
  let cache = Hashtbl.create 513 in
  let cache_direct = Hashtbl.create 513 in
  (* Don't explore deeper than that for transitive conflicts *)
  let max_dig_depth = OpamSolverConfig.(!r.dig_depth) in
  let rec transitive_conflicts seen p =
    try Hashtbl.find cache p with Not_found ->
      let direct =
        try Hashtbl.find cache_direct p with Not_found ->
          let conflicts = direct_conflicts p in
          Hashtbl.add cache_direct p conflicts;
          conflicts
      in
      if Set.mem p seen || Set.cardinal seen >= max_dig_depth - 1 then direct
      else
      let seen = Set.add p seen in
      let conflicts =
        direct ++
        List.fold_left (fun acc disj ->
            acc ++
            Set.map_reduce ~default:Set.empty
              (transitive_conflicts seen)
              Set.inter
              (vpkg2set univ disj))
          Set.empty
          p.Cudf.depends
      in
      Hashtbl.add cache p conflicts;
      conflicts
  in
  OpamStd.String.Map.fold (fun _ ps acc ->
      acc ++
      Set.map_reduce ~default:Set.empty
        (transitive_conflicts Set.empty)
        Set.inter
        ps)
    (to_map packages)
    Set.empty

let preprocess_cudf_request (props, univ, creq) criteria =
  let chrono = OpamConsole.timer () in
  let univ0 = univ in
  let do_trimming =
    match OpamSolverConfig.(!r.cudf_trim) with
    | Some "simple" -> Some false
    | b ->
      match OpamStd.Option.Op.(b >>= OpamStd.Config.bool_of_string) with
      | Some false -> None
      | Some true -> Some true
      | None ->
        (* Full trimming is only correct when there is no maximisation criteria,
           so automatically set it to true in this case *)
        let neg_crit_re =
          Re.(seq [char '-';
                   rep1 (diff any (set ",[("));
                   opt (seq [set "[("; rep1 (diff any (set ")]")); set ")]"])])
        in
        let all_neg_re =
          Re.(whole_string (seq [rep (seq [neg_crit_re; char ',']);
                                 neg_crit_re]))
        in
        Some (Re.execp (Re.compile all_neg_re) criteria)
  in
  let univ =
    let open Set.Op in
    let to_install =
      vpkg2set univ creq.Cudf.install
      ++ Set.of_list (Cudf.lookup_packages univ opam_invariant_package_name)
      ++ Set.of_list (Cudf.lookup_packages univ opam_deprequest_package_name)
    in
    let to_install_formula =
      List.map (fun x -> [x]) @@
      (opam_invariant_package_name, None) ::
      (opam_deprequest_package_name, None) ::
      creq.Cudf.install @ creq.Cudf.upgrade
    in
    let packages =
      match do_trimming with
      | None ->
        Set.of_list (Cudf.get_packages univ)
      | Some false -> (* "simple" trimming *)
        let strong_deps_cone =
          rec_strong_dependency_map univ to_install_formula
        in
        (* only limit visible versions of packages appearing in
           strong_deps_cone *)
        let filter p =
          p.Cudf.installed ||
          match OpamStd.String.Map.find_opt p.Cudf.package strong_deps_cone
          with
          | Some ps -> Set.mem p ps
          | None -> true
        in
        Set.of_list (Cudf.get_packages ~filter univ)
      | Some true -> (* "full" trimming *)
        let strong_deps_cone =
          rec_strong_dependency_map univ to_install_formula
        in
        (* limit visibility to only "possibly interesting" packages; this
           includes all installed packages, including their other versions (the
           changes to installed packages may need up/downgrades). *)
        let interesting_set =
          List.fold_left (fun acc p ->
              let name = p.Cudf.package in
              if OpamStd.String.Map.mem name strong_deps_cone then acc
              else acc ++ Set.of_list (Cudf.lookup_packages univ name))
            (OpamStd.String.Map.fold (fun _ -> Set.union)
               strong_deps_cone Set.empty)
            (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) univ)
        in
        (* we will also need all the weak dependencies of all these packages *)
        Set.fixpoint (fun p ->
            Set.filter (fun d ->
                not (OpamStd.String.Map.mem d.Cudf.package strong_deps_cone))
              (dependency_set univ p.Cudf.depends))
          interesting_set
    in
    let conflicts = compute_conflicts univ to_install in
    log "Conflicts: %a (%a) pkgs to remove"
      (slog OpamStd.Op.(string_of_int @* Set.cardinal)) conflicts
      (slog OpamStd.Op.(string_of_int @* Set.cardinal)) (conflicts %% packages);
    Cudf.load_universe (Set.elements (packages -- conflicts))
  in
  log "Preprocess cudf request (trimming: %s): from %d to %d packages in %.2fs"
    (match do_trimming with
       None -> "none" | Some false -> "simple" | Some true -> "full")
    (Cudf.universe_size univ0)
    (Cudf.universe_size univ)
    (chrono ());
  props, univ, creq

let trim_universe univ packages =
  let chrono = OpamConsole.timer () in
  let n = Cudf.universe_size univ in
  let conflicts = compute_conflicts univ packages in
  let univ =
    Cudf.load_universe
      (Cudf.get_packages ~filter:(fun p -> not (Set.mem p conflicts)) univ)
  in
  log "Pre-remove conflicts (%s): from %d - %d to %d packages in %.2fs"
    (Set.to_string packages)
    n (Set.cardinal conflicts) (Cudf.universe_size univ) (chrono ());
  univ

exception Timeout of Dose_algo.Depsolver.solver_result option

let call_external_solver ~version_map univ req =
  let cudf_request = to_cudf univ req in
  if Cudf.universe_size univ > 0 then
    let criteria = OpamSolverConfig.criteria req.criteria in
    let chrono = OpamConsole.timer () in
    ignore (dump_cudf_request ~version_map cudf_request
              criteria OpamSolverConfig.(!r.cudf_file));
    (* Wrap a return of exn Timeout through Depsolver *)
    let check_request_using ~call_solver ~explain req =
      let timed_out = ref false in
      let call_solver args =
        try call_solver args with
        | OpamCudfSolver.Timeout (Some s) -> timed_out := true; s
        | OpamCudfSolver.Timeout None -> raise (Timeout None)
      in
      let r =
        Dose_algo.Depsolver.check_request_using ~call_solver ~explain req
      in
      if !timed_out then raise (Timeout (Some r)) else r
    in
    try
      let cudf_request =
        if not OpamSolverConfig.(!r.preprocess) then cudf_request
        else preprocess_cudf_request cudf_request criteria
      in
      let r =
        check_request_using
          ~call_solver:(OpamSolverConfig.call_solver ~criteria)
          ~explain:true cudf_request
      in
      log "Solver call done in %.3fs" (chrono ());
      r
    with
    | Timeout (Some sol) ->
      log "Solver call TIMED OUT with solution after %.3fs" (chrono ());
      OpamConsole.warning
        "Resolution of the installation set timed out, so the following \
         solution might not be optimal.\n\
         You may want to make your request more precise, increase the value \
         of OPAMSOLVERTIMEOUT (currently %.1fs), or try a different solver."
        OpamSolverConfig.(OpamStd.Option.default 0. !r.solver_timeout);
      sol
    | Timeout None ->
      let msg =
        Printf.sprintf
          "Sorry, resolution of the request timed out.\n\
           Try to specify a more precise request, use a different solver, or \
           increase the allowed time by setting OPAMSOLVERTIMEOUT to a bigger \
           value (currently, it is set to %.1f seconds)."
          OpamSolverConfig.(OpamStd.Option.default 0. !r.solver_timeout)
      in
      raise (Solver_failure msg)
    | Failure msg ->
      let msg =
        Printf.sprintf
          "Solver failure: %s\nThis may be due to bad settings (solver or \
           solver criteria) or a broken solver installation. Check \
           $OPAMROOT/config, and the --solver and --criteria options."
          msg
      in
      raise (Solver_failure msg)
    | e ->
      OpamStd.Exn.fatal e;
      let bt = Printexc.get_raw_backtrace () in
      let msg =
        Printf.sprintf "Solver failed: %s" (Printexc.to_string e)
      in
      Printexc.raise_with_backtrace (Solver_failure msg) bt
  else
    Dose_algo.Depsolver.Sat(None,Cudf.load_universe [])

let check_request ?(explain=true) ~version_map univ req =
  match Dose_algo.Depsolver.check_request ~explain (to_cudf univ req) with
  | Dose_algo.Depsolver.Unsat
      (Some ({Dose_algo.Diagnostic.result = Dose_algo.Diagnostic.Failure _; _} as r)) ->
    make_conflicts ~version_map univ r
  | Dose_algo.Depsolver.Sat (_,u) ->
    Success (remove u dose_dummy_request None)
  | Dose_algo.Depsolver.Error msg ->
    let f = dump_cudf_error ~version_map univ req in
    let msg =
      Printf.sprintf "Internal solver failed with %s Request saved to %S"
        msg f
    in
    raise (Solver_failure msg)
  | Dose_algo.Depsolver.Unsat _ -> (* normally when [explain] = false *)
    conflict_empty ~version_map univ

(* Return the universe in which the system has to go *)
let get_final_universe ~version_map univ req =
  let fail msg =
    let f = dump_cudf_error ~version_map univ req in
    let msg =
      Printf.sprintf "External solver failed with %s Request saved to %S"
        msg f
    in
    raise (Solver_failure msg) in
  match call_external_solver ~version_map univ req with
  | Dose_algo.Depsolver.Sat (_,u) -> Success (remove u dose_dummy_request None)
  | Dose_algo.Depsolver.Error "(CRASH) Solution file is empty" ->
    (* XXX Is this still needed with latest dose? *)
    Success (Cudf.load_universe [])
  | Dose_algo.Depsolver.Error str -> fail str
  | Dose_algo.Depsolver.Unsat r   ->
    match r with
    | Some ({Dose_algo.Diagnostic.result = Dose_algo.Diagnostic.Failure _; _}
            as r) ->
      make_conflicts ~version_map univ r
    | Some {Dose_algo.Diagnostic.result = Dose_algo.Diagnostic.Success _; _}
    | None ->
      conflict_empty ~version_map univ

let diff univ sol =
  let before =
    Set.of_list
      (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) univ)
  in
  let after =
    Set.of_list
      (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) sol)
  in
  let open Set.Op in
  let reinstall = Set.filter need_reinstall after in
  let install = after -- before ++ reinstall in
  let remove = before -- after ++ reinstall in
  install, remove

(* Transform a diff from current to final state into a list of
   actions. At this point, we don't know about the root causes of the
   actions, they will be computed later. *)
let actions_of_diff (install, remove) =
  let actions = [] in
  let actions = Set.fold (fun p acc -> `Install p :: acc) install actions in
  let actions = Set.fold (fun p acc -> `Remove p :: acc) remove actions in
  actions

let resolve ~extern ~version_map universe request =
  log "resolve request=%a" (slog string_of_request) request;
  let resp =
    let check () = check_request ~version_map universe request in
    let solve () = get_final_universe ~version_map universe request in
    if not extern then check () else
    let module Solver : OpamCudfSolver.S =
      (val Lazy.force OpamSolverConfig.(!r.solver))
    in
    let wrong_unsat_msg =
      Printf.sprintf
        "The solver (%s) pretends there is no solution while that's apparently \
         false.\n\
         This is likely an issue with the solver interface, please try a \
         different solver and report if you were using a supported one."
        Solver.name
    in
    if Solver.preemptive_check
    then match check () with
      | Conflicts _ as conflicts -> conflicts
      | Success _ -> match solve () with
        | Success _ as success -> success
        | Conflicts _ -> raise (Solver_failure wrong_unsat_msg)
    else match solve () with
      | Success _ as success -> success
      | Conflicts _ -> match check () with
        | Success _ -> raise (Solver_failure wrong_unsat_msg)
        | Conflicts _ as conflicts -> conflicts
  in
  let cleanup univ =
    Cudf.remove_package univ opam_invariant_package;
    Cudf.remove_package univ opam_deprequest_package
  in
  let () = match resp with
    | Success univ -> cleanup univ
    | Conflicts (univ, _, _) -> cleanup univ
  in
  resp

let to_actions universe result =
  let aux u1 u2 =
    let diff = diff u1 u2 in
    actions_of_diff diff
  in
  map_success (aux universe) result

let create_graph filter universe =
  let pkgs = Cudf.get_packages ~filter universe in
  let u = Cudf.load_universe pkgs in
  Graph.of_universe u

let find_cycles g =
  let open ActionGraph in
  let roots =
    fold_vertex (fun v acc -> if in_degree g v = 0 then v::acc else acc) g [] in
  let roots =
    if roots = [] then fold_vertex (fun v acc -> v::acc) g []
    else roots in
  let rec prefix_find acc v = function
    | x::_ when x = v -> Some (x::acc)
    | x::r -> prefix_find (x::acc) v r
    | [] -> None in
  let seen = Hashtbl.create 17 in
  let rec follow v path =
    match prefix_find [] v path with
    | Some cycle ->
      Hashtbl.add seen v ();
      [cycle@[v]]
    | None ->
      if Hashtbl.mem seen v then [] else
        let path = v::path in
        Hashtbl.add seen v ();
        List.fold_left (fun acc s -> follow s path @ acc) []
          (succ g v) in
  List.fold_left (fun cycles root ->
    follow root [] @ cycles
  ) [] roots

(* Compute the original causes of the actions, from the original set of
   packages in the user request. In the restricted dependency graph, for each
   action we find the closest package belonging to the user request and print
   out the closest neighbour that gets there. This way, if a -> b -> c and the
   user requests a to be installed, we can print:
   - install a - install b [required by a] - intall c [required by b] *)
let compute_root_causes g requested reinstall available =
  let module StringSet = OpamStd.String.Set in
  let requested_pkgnames =
    OpamPackage.Name.Set.fold (fun n s ->
        StringSet.add (Dose_common.CudfAdd.encode (OpamPackage.Name.to_string n)) s)
      requested StringSet.empty in
  let reinstall_pkgnames =
    OpamPackage.Set.fold (fun nv s ->
        StringSet.add (Dose_common.CudfAdd.encode (OpamPackage.name_to_string nv)) s)
      reinstall StringSet.empty in
  let actions =
    ActionGraph.fold_vertex (fun a acc -> Map.add (action_contents a) a acc)
      g Map.empty in
  let requested_actions =
    Map.filter (fun pkg _ ->
        StringSet.mem pkg.Cudf.package requested_pkgnames)
      actions in
  let merge_causes (c1,depth1) (c2,depth2) =
    (* When we found several causes explaining the same action, only keep the
       most likely one *)
    if c2 = Unknown || depth1 < depth2 then c1, depth1 else
    if c1 = Unknown || depth2 < depth1 then c2, depth2 else
    let (@) =
      List.fold_left (fun l a -> if List.mem a l then l else a::l)
    in
    match c1, c2 with
    | Required_by a, Required_by b -> Required_by (a @ b), depth1
    | Use a, Use b -> Use (a @ b), depth1
    | Conflicts_with a, Conflicts_with b -> Conflicts_with (a @ b), depth1
    | Unavailable, Requested | Requested, Unavailable ->
      Requested, depth1
    | Unavailable, _ | _, Unavailable -> Unavailable, depth1
    | Requested, a | a, Requested
    | Unknown, a | a, Unknown
    | Upstream_changes , a | a, Upstream_changes -> a, depth1
    | _, c -> c, depth1
  in
  let direct_cause consequence order cause =
    (* Investigate the reason of action [consequence], that was possibly
       triggered by [cause], where the actions are ordered as [consequence]
       [order] [cause]. *)
    match consequence, order, cause with
    | (`Install _ | `Change _), `Before, (`Install p | `Change (_,_,p)) ->
      (* Prerequisite *)
      Required_by [p]
    | `Change _, `After, (`Install p | `Change (_,_,p)) ->
      (* Change caused by change in dependencies *)
      Use [p]
    | `Reinstall _, `After, a ->
      (* Reinstall caused by action on deps *)
      Use [action_contents a]
    | (`Remove _ | `Change _ ), `Before, `Remove p ->
      (* Removal or change caused by the removal of a dependency *)
      Use [p]
    | `Remove _, `Before, (`Install p | `Change (_,_,p) | `Reinstall p) ->
      (* Removal caused by conflict *)
      Conflicts_with [p]
    | (`Install _ | `Change _), `Before, `Reinstall p ->
      (* New dependency of p? *)
      Required_by [p]
    | `Change _, _, _ ->
      (* The only remaining cause for changes is upstream *)
      Upstream_changes
    | (`Install _ | `Remove _), `After, _  ->
      (* Nothing can cause these actions after itself *)
      Unknown
    | (`Install _ | `Reinstall _), `Before, _ ->
      (* An install or reinstall doesn't cause any other actions on its
         dependendants *)
      Unknown
    | `Build _, _, _ | _, _, `Build _ -> assert false
    | `Fetch _, _, _ | _, _, `Fetch _ -> assert false (* XXX CHECK *)
  in
  let get_causes acc roots =
    let rec aux seen depth pkgname causes =
      if depth > 100 then
        (OpamConsole.error
           "Internal error computing action causes: sorry, please report.";
         causes)
      else
      let action = Map.find pkgname actions in
      let seen = Set.add pkgname seen in
      let propagate causes actions direction =
        List.fold_left (fun causes act ->
            let p = action_contents act in
            if Set.mem p seen then causes else
            let cause = direct_cause act direction action in
            if cause = Unknown then causes else
            try
              Map.add p (merge_causes (cause,depth) (Map.find p causes)) causes
            with Not_found ->
              aux seen (depth + 1) p (Map.add p (cause,depth) causes)
          ) causes actions in
      let causes = propagate causes (ActionGraph.pred g action) `Before in
      let causes = propagate causes (ActionGraph.succ g action) `After in
      causes
    in
    let start = Map.fold (fun k _ acc -> Set.add k acc) roots Set.empty in
    let acc = Map.union (fun a _ -> a) acc roots in
    Set.fold (aux start 1) start acc
  in
  (* Compute the roots of the action given a condition *)
  let make_roots causes base_cause f =
    ActionGraph.fold_vertex (fun act acc ->
        if Map.mem (action_contents act) causes then acc else
        if f act then Map.add (action_contents act) (base_cause,0) acc else
          acc)
      g Map.empty in
  let causes = Map.empty in
  let causes =
    let roots =
      if Map.is_empty requested_actions then (* Assume a global upgrade *)
        make_roots causes Requested (function
            | `Change (`Up,_,_) -> true
            | _ -> false)
      else (Map.map (fun _ -> Requested, 0) requested_actions) in
    let roots2 =
      make_roots causes Unavailable (function
          | `Remove p ->
            not (OpamPackage.Set.mem (cudf2opam p) available)
          | _ -> false)
    in
    get_causes causes (Map.union (fun a _ -> a)  roots roots2) in
  let causes =
    (* Compute causes for changed no longer available packages *)
    let roots =
      make_roots causes Unavailable (function
          | `Change (_,p,_) ->
            not (OpamPackage.Set.mem (cudf2opam p) available)
          | _ -> false)
    in
    get_causes causes roots in
  let causes =
    (* Compute causes for remaining upgrades
       (maybe these could be removed from the actions altogether since they are
       unrelated to the request?) *)
    let roots = make_roots causes Unknown (function
        | `Change _ as act ->
          List.for_all
            (function `Change _ -> false | _ -> true)
            (ActionGraph.pred g act)
        | _ -> false) in
    get_causes causes roots in
  let causes =
    (* Compute causes for marked reinstalls *)
    let roots =
      make_roots causes Upstream_changes (function
          | `Reinstall p ->
            (* need_reinstall p is not available here *)
            StringSet.mem p.Cudf.package reinstall_pkgnames
          | _ -> false)
    in
    get_causes causes roots in
  Map.map fst causes

(* Compute a full solution from a set of root actions. This means adding all
   required reinstallations and computing the graph of dependency of required
   actions *)
let atomic_actions ~simple_universe ~complete_universe root_actions =
  log ~level:2 "graph_of_actions root_actions=%a"
    (slog string_of_actions) root_actions;

  let to_remove, to_install =
    List.fold_left (fun (rm,inst) a -> match a with
        | `Change (_,p1,p2) ->
          Set.add p1 rm, Set.add p2 inst
        | `Install p -> rm, Set.add p inst
        | `Reinstall p -> Set.add p rm, Set.add p inst
        | `Remove p -> Set.add p rm, inst)
      (Set.empty, Set.empty) root_actions in

  (* transitively add recompilations *)
  let to_remove, to_install =
    let packages = Set.union to_remove to_install in
    let package_graph =
      let filter p = p.Cudf.installed || Set.mem p packages in
      Graph.mirror (create_graph filter simple_universe)
    in
    Graph.Topo.fold (fun p (rm,inst) ->
        let actionned p = Set.mem p rm || Set.mem p inst in
        if not (actionned p) &&
           List.exists actionned (Graph.pred package_graph p)
        then Set.add p rm, Set.add p inst
        else rm, inst)
      package_graph (to_remove, to_install)
  in
  let pkggraph set = create_graph (fun p -> Set.mem p set) complete_universe in

  (* Build the graph of atomic actions: Removals or installs *)
  let g = ActionGraph.create () in
  Set.iter (fun p -> ActionGraph.add_vertex g (`Remove p)) to_remove;
  Set.iter (fun p -> ActionGraph.add_vertex g (`Install (p))) to_install;
  (* reinstalls and upgrades: remove first *)
  Set.iter
    (fun p1 ->
       try
         let p2 =
           Set.find (fun p2 -> p1.Cudf.package = p2.Cudf.package) to_install
         in
         ActionGraph.add_edge g (`Remove p1) (`Install (p2))
       with Not_found -> ())
    to_remove;
  (* uninstall order *)
  Graph.iter_edges (fun p1 p2 ->
      ActionGraph.add_edge g (`Remove p1) (`Remove p2)
    ) (pkggraph to_remove);
  (* install order *)
  Graph.iter_edges (fun p1 p2 ->
      if Set.mem p1 to_install then
        let cause =
          if Set.mem p2 to_install then `Install ( p2) else `Remove p2
        in
        ActionGraph.add_edge g cause (`Install ( p1))
    ) (pkggraph (Set.union to_install to_remove));
  (* conflicts *)
  let conflicts_graph =
    let filter p = Set.mem p to_remove || Set.mem p to_install in
    Dose_algo.Defaultgraphs.PackageGraph.conflict_graph
      (Cudf.load_universe (Cudf.get_packages ~filter complete_universe))
  in
  Dose_algo.Defaultgraphs.PackageGraph.UG.iter_edges (fun p1 p2 ->
      if Set.mem p1 to_remove && Set.mem p2 to_install then
        ActionGraph.add_edge g (`Remove p1) (`Install ( p2))
      else if Set.mem p2 to_remove && Set.mem p1 to_install then
        ActionGraph.add_edge g (`Remove p2) (`Install ( p1)))
    conflicts_graph;
  (* check for cycles *)
  match find_cycles g with
  | [] -> g
  | cycles -> raise (Cyclic_actions cycles)

let trim_actions univ req g =
  if OpamPackage.Name.Set.is_empty req then () else
  let post_dependencies_map =
    let packages_actions =
      ActionGraph.fold_vertex (fun a ->
          Map.update (action_contents a) (Action.Set.add a) Action.Set.empty)
        g Map.empty
    in
    let univ =
      Cudf.load_universe @@ Cudf.get_packages univ
        ~filter:(fun p -> Map.mem p packages_actions)
    in
    let deps univ p =
      let p = Cudf.lookup_package univ (p.Cudf.package, p.Cudf.version) in
      List.fold_right (List.fold_right Set.add)
        (Dose_common.CudfAdd.who_depends univ p)
        Set.empty
    in
    ActionGraph.fold_vertex (fun a ->
        Action.Map.add a
          (Set.fold (fun p -> Action.Set.union (Map.find p packages_actions))
             (deps univ (action_contents a)) Action.Set.empty))
      g Action.Map.empty
  in
  let root_actions, other_actions =
    ActionGraph.fold_vertex (fun a (root,other) ->
        let pkg = action_contents a in
        let name =
          OpamPackage.Name.of_string (Cudf.lookup_package_property pkg s_source)
        in
        if OpamPackage.Name.Set.mem name req
        then Action.Set.add a root, other
        else root, Action.Set.add a other)
      g Action.Set.(empty, empty)
  in
  let connex_actions =
    Action.Set.fixpoint (fun a ->
        ActionGraph.fold_succ Action.Set.add g a @@
        ActionGraph.fold_pred Action.Set.add g a @@
        Action.Map.find a post_dependencies_map)
      root_actions
  in
  let discard_actions = Action.Set.diff other_actions connex_actions in
  log "Removed unrelated actions: %s" (Action.Set.to_string discard_actions);
  Action.Set.iter (ActionGraph.remove_vertex g) discard_actions

let packages u = Cudf.get_packages u

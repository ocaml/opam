(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCudfSolverSig

let log f = OpamConsole.log "Z3" f

let name = "builtin-z3"

let ext = ref None

let is_present () = true

let command_name = None

let preemptive_check = false

let default_criteria = {
  crit_default = "-removed,\
                  -count[avoid-version,changed],\
                  -count[version-lag,request],\
                  -count[version-lag,changed],\
                  -count[missing-depexts,changed],\
                  -changed";
  crit_upgrade = "-removed,\
                  -count[avoid-version,changed],\
                  -count[version-lag,solution],\
                  -count[missing-depexts,changed],\
                  -new";
  crit_fixup = "-changed,\
                -count[avoid-version,changed],\
                -count[version-lag,solution],\
                -count[missing-depexts,changed]";
  crit_best_effort_prefix = Some "+count[opam-query,solution],";
}

let ( @^ ) opt l = match opt with
  | None -> l
  | Some x -> x :: l

let (@@^) o l = match o with
  | None -> l
  | Some l1 -> List.rev_append l1 l

let xrmap f = function
  | [] -> Some []
  | l -> match List.fold_left (fun acc x -> f x @^ acc) [] l with
    | [] -> None
    | l -> Some l

(*
let xmap f l = match xrmap f l with
  | Some l -> Some (List.rev l)
  | None -> None
*)
open OpamStd.Option.Op

(* Mutable Z3 context expanded with generation intermediate information *)
type ctx = {
  z3: Z3.context;
  pkgs: (Cudf.package, Z3.Expr.expr) Hashtbl.t;
  constrs: (Cudf_types.vpkglist list, Z3.Expr.expr) Hashtbl.t;
  mutable constr_defs: Z3.Expr.expr list;
}

let mk_or ctx lopt =
  lopt >>= fun l ->
  if List.exists Z3.Boolean.is_true l then
    Some (Z3.Boolean.mk_true ctx.z3)
  else match List.filter OpamStd.Op.(not @* Z3.Boolean.is_false) l with
    | [] -> Some (Z3.Boolean.mk_false ctx.z3)
    | [p] -> Some p
    | l -> Some (Z3.Boolean.mk_or ctx.z3 l)

let mk_and ctx lopt =
  lopt >>= fun l ->
  if List.exists Z3.Boolean.is_false l then
    Some (Z3.Boolean.mk_false ctx.z3)
  else match List.filter OpamStd.Op.(not @* Z3.Boolean.is_true) l with
  | [] -> None
  | [p] -> Some p
  | l -> Some (Z3.Boolean.mk_and ctx.z3 l)

let psym ctx = Hashtbl.find_opt ctx.pkgs

let mk_constr ctx vpkgll pkgs =
  let vpkgll = List.rev_map (List.sort_uniq compare) vpkgll in
  let vpkgll = List.sort_uniq compare vpkgll in
  try Some (Hashtbl.find ctx.constrs vpkgll) with Not_found ->
    let c = mk_or ctx (xrmap (psym ctx) pkgs) in
    match c, pkgs with
    | None, _ | _, ([] | [_] | [_;_]) -> c
    | Some c, _ ->
      let id =
        Z3.Boolean.mk_const_s ctx.z3
          (OpamStd.List.concat_map " & "
             Cudf_types_pp.string_of_vpkglist vpkgll)
      in
      ctx.constr_defs <- Z3.Boolean.mk_eq ctx.z3 id c :: ctx.constr_defs;
      Hashtbl.add ctx.constrs vpkgll id;
      (Some id)

let expand_constraint universe ctx (name, constr) =
  let pkgs = Cudf.lookup_packages universe ~filter:constr name in
  mk_constr ctx [[name, constr]] pkgs

let def_packages ctx (_preamble, universe, _request) =
  let psym_exn p =
    match psym ctx p
    with None -> raise Not_found | Some p -> p
  in
  (* variable definitions *)
  Cudf.iter_packages (fun pkg ->
      Hashtbl.add ctx.pkgs pkg
        (Z3.Boolean.mk_const_s ctx.z3
           (Printf.sprintf "%s.%d" pkg.Cudf.package pkg.Cudf.version)))
    universe;
  let def_exprs = [] in
  let def_exprs =
    (* "keep" flags *)
    Cudf.fold_packages_by_name (fun e _name pkgs ->
        let keep =
          match List.find (fun p -> p.Cudf.keep = `Keep_version) pkgs with
          | p -> psym ctx p
          | exception Not_found ->
            if List.exists (fun p -> p.Cudf.keep = `Keep_package) pkgs then
              mk_or ctx @@ xrmap (psym ctx) pkgs
            else None
        in
        keep @^ e)
      def_exprs
      universe
  in
  let def_exprs =
    Cudf.fold_packages (fun e pkg ->
        let module SM = OpamStd.String.Map in
        let cudf_depends, cudf_depends_map =
          List.fold_left (fun (rem, map) -> function
              | (name, _) :: r as disj
                when List.for_all (fun (n1, _) -> n1 = name) r ->
                rem, SM.update name (fun conj -> disj :: conj) [] map
              | disj -> disj :: rem, map)
            ([], SM.empty)
            pkg.Cudf.depends
        in
        let depends =
          xrmap
            (fun disj ->
               mk_or ctx @@ xrmap (expand_constraint universe ctx) disj)
            cudf_depends @@^
          SM.fold (fun name conj e ->
              (match
                 List.fold_left (fun plist disj ->
                     let r =
                       List.filter (fun p ->
                           List.exists
                             (fun (_, cstr) ->
                                Cudf.version_matches p.Cudf.version cstr)
                             disj)
                         plist
                     in
                     r)
                   (Cudf.lookup_packages universe name)
                   conj
               with
               | [] -> Some (Z3.Boolean.mk_false ctx.z3)
               | pkgs ->
                 mk_constr ctx conj pkgs)
              @^ e)
            cudf_depends_map
            []
        in
        let conflicts =
          let conflicts =
            List.filter (fun cs -> cs <> (pkg.Cudf.package, None))
              pkg.Cudf.conflicts
          in
          mk_or ctx @@ xrmap
            (fun (name, filter) ->
               let pkgs = Cudf.lookup_packages universe ~filter name in
               if List.mem pkg pkgs then (* Avoid self-conflict *)
                 mk_constr ctx
                   [ [name, filter]; [name, Some (`Neq, pkg.Cudf.version)] ]
                   (List.filter (fun p -> not (Cudf.( =% ) pkg p)) pkgs)
               else
                 mk_constr ctx[[name, filter]] pkgs)
            conflicts
          >>| Z3.Boolean.mk_not ctx.z3
        in
        let cft_deps =
          Some (conflicts @^ depends) |> mk_and ctx
          >>| Z3.Boolean.mk_implies ctx.z3 (psym_exn pkg)
        in
        cft_deps @^ e)
      def_exprs
      universe
  in
  let self_conflicts =
    Cudf.fold_packages_by_name (fun e name pkgs ->
        let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx.z3 0 in
        let one = Z3.Arithmetic.Integer.mk_numeral_i ctx.z3 1 in
        let cft_pkgs =
          List.filter (fun p -> List.mem (name, None) p.Cudf.conflicts) pkgs
        in
        if List.length cft_pkgs >= 2 then
          Z3.Boolean.mk_implies ctx.z3
            (match mk_constr ctx [[name, None]] pkgs with
             | Some c -> c
             | None -> assert false)
            (Z3.Arithmetic.mk_ge ctx.z3 one
               (Z3.Arithmetic.mk_add ctx.z3
                  (List.map
                     (fun p -> Z3.Boolean.mk_ite ctx.z3 (psym_exn p) one zero)
                     cft_pkgs)))
          :: e
        else e)
      []
      universe
  in
  List.rev_append self_conflicts @@
  List.rev def_exprs

let def_request ctx (_preamble, universe, request) =
  let inst =
    xrmap (expand_constraint universe ctx) request.Cudf.install
  in
  let rem =
    xrmap (fun vpkg ->
        expand_constraint universe ctx vpkg >>| Z3.Boolean.mk_not ctx.z3)
      request.Cudf.remove
  in
  let up =
    xrmap (fun (name, constr) ->
        match Cudf.get_installed universe name with
        | [] ->
          mk_or ctx @@ xrmap (psym ctx)
            (Cudf.lookup_packages universe ~filter:constr name)
        | p::l ->
          let vmin =
            List.fold_left (fun vmin p -> max vmin p.Cudf.version) p.Cudf.version l
          in
          Cudf.lookup_packages universe ~filter:constr name |>
          List.filter (fun p -> p.Cudf.version >= vmin) |>
          xrmap (psym ctx) |>
          (* fixme: the spec states that an 'upgrade' request should guarantee
             that only one version of the package will be installed. Since it's
             already a constraint in opam, and it's non trivial to encode, we
             ignore it here. *)
          mk_or ctx)
      request.Cudf.upgrade
  in
  inst @@^ rem @@^ up @@^ []

let sum ctx (_, universe, _) filter value =
  let ite filt iftrue iffalse =
    Z3.Boolean.mk_ite ctx.z3 filt
      (Z3.Arithmetic.Integer.mk_numeral_i ctx.z3 iftrue)
      (Z3.Arithmetic.Integer.mk_numeral_i ctx.z3 iffalse)
  in
  Cudf.fold_packages (fun e pkg ->
      match filter pkg with
      | None -> e
      | Some filt ->
        match value pkg with
        | 0 -> e
        | n ->
          if Z3.Boolean.is_not filt then
            match Z3.Expr.get_args filt with
            | [filt] -> ite filt 0 n :: e
            | _ -> assert false
          else
            ite filt n 0 :: e)
    []
    universe

type filter = Installed | Changed | Removed | New |
              Upgraded | Downgraded | Requested
type property = string option
type sign = Plus | Minus

type criterion = sign * filter * property

let def_criterion ctx opt (preamble, universe, request as cudf)
    (sign, filter, property : criterion) =
  let psym_exn p =
    match psym ctx p
    with None -> raise Not_found | Some p -> p
  in
  let filter_f = match filter with
    | Installed -> fun p -> psym ctx p
    | Changed ->
      fun p ->
        if p.Cudf.installed then
          (* true on both the removed version and the new version, if any,
             according to the spec *)
          mk_or ctx @@ OpamStd.Option.some @@
          (Z3.Boolean.mk_not ctx.z3 (psym_exn p) ::
           (OpamStd.Option.default [] @@
            xrmap (psym ctx)
              (Cudf.lookup_packages universe ~filter:(Some (`Neq, p.Cudf.version))
                 p.Cudf.package)))
        else psym ctx p
    | Removed ->
      fun p ->
        if p.Cudf.installed then
          mk_or ctx @@ xrmap (psym ctx)
            (Cudf.lookup_packages universe p.Cudf.package)
          >>| Z3.Boolean.mk_not ctx.z3
        else None
    | New ->
      fun p ->
        if p.Cudf.installed then None
        else
          mk_or ctx @@ xrmap (psym ctx)
            (Cudf.lookup_packages universe p.Cudf.package)
    | Upgraded ->
      fun p ->
        if p.Cudf.installed then None
        else (match Cudf.get_installed universe p.Cudf.package with
            | [] -> None
            | l when List.for_all (fun p1 -> p1.Cudf.version < p.Cudf.version) l
              -> psym ctx p
            | _ -> None)
    | Downgraded ->
      fun p ->
        if p.Cudf.installed then None
        else (match Cudf.get_installed universe p.Cudf.package with
            | [] -> None
            | l when List.exists (fun p1 -> p1.Cudf.version > p.Cudf.version) l
              -> psym ctx p
            | _ -> None)
    | Requested ->
      fun p ->
        if
          List.exists (fun (name, cstr) ->
              p.Cudf.package = name && Cudf.version_matches p.Cudf.version cstr)
            request.Cudf.install ||
          List.exists (fun (name, cstr) ->
              p.Cudf.package = name && Cudf.version_matches p.Cudf.version cstr)
            request.Cudf.upgrade
        then psym ctx p
        else None
  in
  let value_f = match property with
    | None -> fun _ -> 1
    | Some prop ->
      fun p ->
        match Cudf.lookup_typed_package_property p prop with
        | `Int n | `Nat n -> n
        | `Bool true -> 1
        | `Bool false -> 0
        | _ -> 0
        | exception Not_found ->
          match List.assoc prop preamble.Cudf.property with
          | `Int (Some n) | `Nat (Some n) -> n
          | `Bool (Some true) -> 1
          | `Bool (Some false) -> 0
          | _ -> 0
          | exception Not_found ->
            failwith ("Undefined CUDF property: "^prop)
  in
  let tot = sum ctx cudf filter_f value_f in
  if tot = [] then None else OpamStd.Option.some @@
    (match sign with
     | Plus -> Z3.Optimize.maximize
     | Minus -> Z3.Optimize.minimize)
    opt
    (Z3.Arithmetic.mk_add ctx.z3 tot)

let def_criteria ctx opt cudf crits =
  OpamStd.List.filter_map (def_criterion ctx opt cudf) crits

module Syntax = struct

  let criterion_of_string (s,params) =
    let sign = match s.[0] with
      | '+' -> Plus
      | '-' -> Minus
      | c -> failwith (Printf.sprintf "criteria_of_string sign=%c" c)
      | exception Invalid_argument _ ->
        failwith "criteria_of_string sign=EOF"
    in
    let s = String.sub s 1 (String.length s - 1) in
    let subset_of_string = function
      | "new" -> New
      | "removed" -> Removed
      | "changed" -> Changed
      | "up" -> Upgraded
      | "down" -> Downgraded
      | "installed" | "solution" -> Installed
      | "request" -> Requested
      | s -> failwith ("criteria_of_string subset="^s)
    in
    match s, params with
    | "count", [field; subset] ->
      sign, subset_of_string subset, Some field
    | s, [] -> sign, subset_of_string s, None
    | s, _ -> failwith ("criteria_of_string s="^s)
(*
  let string_of_criterion (sign, filter, property: criterion) =
    Printf.sprintf "%c%s%s"
      (match sign with Plus -> '+' | Minus -> '-')
      (match filter with
       | Installed -> "installed"
       | Changed -> "changed"
       | Removed -> "removed"
       | New -> "new"
       | Upgraded -> "up"
       | Downgraded -> "down"
       | Requested -> "request")
      (match property with None -> "" | Some p -> "["^p^"]")
*)
  let criteria_of_string s =
    let start = ref 0 in
    let crits = ref [] in
    let params = ref None in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | ',' ->
        let sub = String.sub s !start (i - !start) in
        start := i + 1;
        if sub <> "" then
          (match !params with
           | None -> crits := (sub, []) :: !crits
           | Some (name, ps) -> params := Some (name, sub :: ps))
      | '[' ->
        let sub = String.sub s !start (i - !start) in
        start := i + 1;
        if !params <> None then failwith "criteria_of_string";
        params := Some (sub, [])
      | ']' ->
        let sub = String.sub s !start (i - !start) in
        start := i + 1;
        (match !params with
         | None -> failwith "criteria_of_string"
         | Some (name, ps) ->
           params := None;
           crits := (name, List.rev (sub::ps)) :: !crits)
      | _ -> ()
    done;
    if !start < String.length s then
      crits := (String.sub s !start (String.length s - !start), []) :: !crits;
    if !params <> None then failwith "criteria_of_string";
    let r = List.rev_map criterion_of_string !crits in
    r

end

let extract_solution_packages universe opt =
  match Z3.Optimize.get_model opt with
  | Some model ->
    Z3.Model.get_const_decls model |>
    List.fold_left (fun pkgs decl ->
        match Z3.Model.get_const_interp model decl with
        | Some p when Z3.Boolean.is_true p ->
          (match OpamStd.String.cut_at
                   (Z3.Symbol.get_string (Z3.FuncDecl.get_name decl))
                   '.'
           with
           | None -> pkgs
           | Some (p, v) ->
             let p = Cudf.lookup_package universe (p, int_of_string v) in
             {p with
              Cudf.was_installed = p.installed;
              Cudf.installed = true}
             :: pkgs)
        | _ -> pkgs)
      []
  | None -> failwith "no model ??"

let call ~criteria ?timeout (preamble, universe, _ as cudf) =
  (* try *)
  log "Generating problem...";
  let cfg = match timeout with
    | None -> []
    | Some secs -> ["timeout", string_of_int (int_of_float (1000. *. secs))]
  in
  let ctx = {
    z3 = Z3.mk_context cfg;
    pkgs = Hashtbl.create 2731;
    constrs = Hashtbl.create 2731;
    constr_defs = [];
  } in
  log "Generating package definitions";
  let packages_defs = def_packages ctx cudf in
  log "Generating request";
  let request_def = def_request ctx cudf in
  log "Generating optimization criteria";
  let opt = Z3.Optimize.mk_opt ctx.z3 in
  let _criteria_def_handles =
    def_criteria ctx opt cudf (Syntax.criteria_of_string criteria)
  in
  log "Sending the problem to Z3";
  let params =
    Z3.Params.mk_params ctx.z3
    (* |> (fun p -> Z3.Params.add_symbol p
     *        (Z3.Symbol.mk_string ctx "priority")
     *        (Z3.Symbol.mk_string ctx "box"); p) *)
    (* |> (fun p -> Z3.Params.add_symbol p
     *        (Z3.Symbol.mk_string ctx "maxsat_engine")
     *        (Z3.Symbol.mk_string ctx "wmax"); p) *)
  in
  Z3.Optimize.set_parameters opt params;
  Z3.Optimize.add opt packages_defs;
  Z3.Optimize.add opt ctx.constr_defs;
  Z3.Optimize.add opt request_def;
  log "Resolving...";
  (match Sys.getenv "OPAMZ3DEBUG" with
   | exception Not_found -> ()
   | f ->
     let debug = open_out (f^".smt2") in
     output_string debug (Z3.Optimize.to_string opt);
     close_out debug);
  match Z3.Optimize.check opt with
  | UNSATISFIABLE ->
    log "UNSAT";
    raise Dose_common.CudfSolver.Unsat
  | UNKNOWN ->
    log "UNKNOWN";
    (try
       let universe =
         Cudf.load_universe (extract_solution_packages universe opt)
       in
       raise (Timeout (Some (Some preamble, universe)))
     with Failure _ ->
       raise (Timeout None))
  | SATISFIABLE ->
    log "SAT: extracting model";
    let universe =
      Cudf.load_universe (extract_solution_packages universe opt)
    in
    Some preamble, universe
  (* with
   * | (Timeout | Dose_common.CudfSolver.Unsat | Failure _) as e -> raise e
   * | e ->
   *   OpamConsole.error "Z3 error: %s" (Printexc.to_string e);
   *   OpamConsole.errmsg "%s" (Printexc.get_backtrace ());
   *   raise e *)

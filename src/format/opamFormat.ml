(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamTypesBase
open OpamStd.Op

type bad_format = pos option * string

exception Bad_format of bad_format
exception Bad_format_list of bad_format list

let bad_format ?pos fmt =
  Printf.ksprintf
    (fun str ->
       raise (Bad_format (pos,str)))
    fmt

let add_pos pos = function
  | Bad_format (pos_opt,msg) as e ->
    if pos_opt = None || pos_opt = Some pos_null
    then Bad_format (Some pos, msg)
    else e
  | e -> e

let rec string_of_bad_format ?file e =
  match e, file with
  | Bad_format (None, msg), Some f
  | Bad_format (Some (f, -1, -1), msg), _ ->
    Printf.sprintf "In %s:\n%s"
      (OpamFilename.to_string f) msg
  | Bad_format (Some pos, msg), _ ->
    Printf.sprintf "At %s:\n%s"
      (string_of_pos pos) msg
  | Bad_format (None, msg), None ->
    Printf.sprintf "Input error:\n%s" msg
  | Bad_format_list bfl, _ ->
    OpamStd.List.concat_map "\n"
      (fun bf -> string_of_bad_format ?file (Bad_format bf)) bfl
  | _ -> Printexc.to_string e

let () = Printexc.register_printer @@ function
  | (Bad_format _ | Bad_format_list _ as e) ->
    Some (string_of_bad_format ?file:None e)
  | _ -> None

let item_pos = function
  | Section (pos,_) | Variable (pos,_,_) -> pos

let value_pos = function
  | Bool (pos, _) | Int (pos, _) | String (pos, _)
  | Logop (pos, _, _, _) | Pfxop (pos, _, _)
  | Relop (pos, _, _, _) | Prefix_relop (pos, _, _)
  | Ident (pos, _) | List (pos, _) | Group (pos, _) | Option (pos, _, _)
  | Env_binding (pos, _, _, _)
    -> pos

let values_pos = function
  | [] -> None
  | x::_ -> Some (value_pos x)

module Pp = struct

  type ('a,'b) t = {
    parse: pos:pos -> 'a -> 'b;
    print: 'b -> 'a;
    ppname: string;
    name_constr: string -> string;
  }

  let pp ?(name="") ?(name_constr=fun x -> x) parse print =
    {
      parse; print; ppname = name; name_constr;
    }

  let of_pair name (simple_parse, print) =
    pp ~name (fun ~pos:_ -> simple_parse) print

  (** Utility functions *)

  exception Unexpected of pos option
  let unexpected ?pos () = raise (Unexpected pos)

  let warn ?pos ?(strict=OpamFormatConfig.(!r.strict)) ?exn fmt =
    if strict then
      match exn with
      | Some e -> raise e
      | None -> bad_format ?pos fmt
    else
    Printf.ksprintf (fun s ->
          if OpamConsole.verbose () then
            match exn with
            | None ->
              OpamConsole.warning "%s"
                (string_of_bad_format (Bad_format (pos, s)))
            | Some e ->
              OpamConsole.warning "%s" (string_of_bad_format e))
      fmt

  (** Basic pp usage *)

  let parse pp ~pos x = try pp.parse ~pos x with
    | Bad_format _ | Bad_format_list _ as e -> raise (add_pos pos e)
    | Unexpected (Some pos) -> bad_format ~pos "expected %s" pp.ppname
    | Unexpected None -> bad_format ~pos "expected %s" pp.ppname
    | Failure msg ->
      bad_format ~pos "%s%s"
        (if pp.ppname <> "" then Printf.sprintf "while expecting %s: " pp.ppname
         else "")
        msg
    | e ->
      OpamStd.Exn.fatal e;
      bad_format ~pos "%s%s"
        (if pp.ppname <> "" then Printf.sprintf "while expecting %s: " pp.ppname
         else "")
        (Printexc.to_string e)

  let print pp x = pp.print x


  (** Pp combination and transformation *)

  (** Piping *)
  let (-|) pp1 pp2 = {
    parse = (fun ~pos x ->
        let y = pp1.parse ~pos x in
        parse pp2 ~pos y
      );
    print = pp1.print @* pp2.print;
    ppname =
      (match pp2.ppname with "" -> pp1.ppname
                           | name -> pp1.name_constr name);
    name_constr = pp1.name_constr @* pp2.name_constr;
  }

  let identity = {
    parse = (fun ~pos:_ x -> x);
    print = (fun x -> x);
    ppname = "";
    name_constr = (fun x -> x);
  }

  let ignore = {
    parse = (fun ~pos:_ -> OpamStd.Option.none);
    print = (fun _ -> assert false);
    ppname = "ignored";
    name_constr = (fun _ -> "<ignored>");
  }

  let check ?name ?errmsg f =
    pp
      ?name
      (fun ~pos x ->
         if not (f x) then
           match errmsg with
           | Some m -> bad_format ~pos "%s" m
           | None -> unexpected ()
         else x)
      (fun x ->
         assert (
           f x ||
           (OpamConsole.error "Check failed on value printing%s%s"
              (match name with Some n -> " at "^n | None -> "")
              (match errmsg with Some e -> " ("^e^")" | None -> "");
            false));
         x)

  let map_pair ?name ?posf1 ?posf2 (pp1: ('a,'b) t) (pp2: ('c,'d) t) =
    let name = match name with
      | None -> Printf.sprintf "(%s, %s)" pp1.ppname pp2.ppname
      | Some n -> n
    in
    pp ~name
      (fun ~pos (a,b) ->
         let posf1 = OpamStd.Option.default (fun _ -> pos) posf1 in
         parse pp1 ~pos:(posf1 a) a,
         let posf2 = OpamStd.Option.default (fun _ -> pos) posf2 in
         parse pp2 ~pos:(posf2 b) b)
      (fun (a,b) -> print pp1 a, print pp2 b)

  let map_fst pp1 =
    pp
      (fun ~pos (a,b) -> pp1.parse ~pos a, b)
      (fun (a, b) -> pp1.print a, b)

  let map_snd pp1 =
    pp
      (fun ~pos (a,b) -> a, pp1.parse ~pos b)
      (fun (a, b) -> a, pp1.print b)

  let map_list ?name ?posf pp1 =
    let name = match name with
      | None -> pp1.ppname ^ "*"
      | Some n -> n
    in
    pp ~name
      (fun ~pos l ->
         let posf = OpamStd.Option.default (fun _ -> pos) posf in
         List.rev (List.rev_map (fun x -> parse pp1 ~pos:(posf x) x) l))
      (List.rev @* List.rev_map (print pp1))

  let map_option ?name pp1 =
    let name = match name with
      | None -> pp1.ppname ^ "?"
      | Some n -> n
    in
    pp ~name
      (fun ~pos -> OpamStd.Option.map (parse pp1 ~pos))
      (OpamStd.Option.map (print pp1))

  let singleton = {
    parse = (fun ~pos:_ -> function [x] -> x | _ -> unexpected ());
    print = (fun x -> [x]);
    ppname = "";
    name_constr = (fun x -> x);
  }

  (** Pps from strings *)

  module type STR = sig
    type t
    val of_string: string -> t
    val to_string: t -> string
  end

  let of_module (type a) name m =
    let module X = (val m: STR with type t = a) in
    pp ~name
      (fun ~pos:_ -> X.of_string)
      X.to_string

  (** low-level Pps for the Lines parser ([string list list]) *)

  type lines = string list list

  let lines_set ~empty ~add ~fold pp1 =
    pp
      ~name:(Printf.sprintf "(%s) lines" pp1.ppname)
      (fun ~pos:(file,_,_) lines ->
         List.fold_left (fun (i,acc) -> function
             | [] -> i + 1, acc
             | line -> i + 1, add (parse pp1 ~pos:(file,i,0) line) acc)
           (1, empty) lines
         |> snd)
      (fun x ->
         List.rev (fold (fun v acc -> print pp1 v::acc) x []))

  let lines_map ~empty ~add ~fold pp1 =
    pp
      ~name:(Printf.sprintf "(%s) lines" pp1.ppname)
      (fun ~pos:(file,_,_) lines ->
         List.fold_left (fun (i,acc) -> function
             | [] -> i + 1, acc
             | line ->
               let k,v = parse pp1 ~pos:(file,i,0) line in
               i + 1, add k v acc)
           (1, empty) lines
         |> snd)
      (fun x ->
         List.rev (fold (fun k v acc -> print pp1 (k,v)::acc) x []))

  (** Build tuples from lists *)
  let (^+) pp1 pp2 =
    pp
      ~name:(Printf.sprintf "%s %s" pp1.ppname pp2.ppname)
      (fun ~pos -> function
         | x::r -> parse pp1 ~pos x, parse pp2 ~pos r
         | [] -> unexpected ())
      (fun (x,y) -> print pp1 x :: print pp2 y)

  let last = singleton

  let opt pp1 =
    pp
      ~name:("?"^pp1.ppname)
      (fun ~pos -> function [] -> None | l -> Some (pp1.parse ~pos l))
      (function Some x -> pp1.print x | None -> [])

  let default d =
    pp
      (fun ~pos:_ -> function None -> d | Some x -> x)
      (fun x -> Some x)

  module Op = struct
    let ( -| ) = ( -| )
    let ( ^+ ) = ( ^+ )
  end

(*
  let list2 pp1 pp2 =
    pp ~name:(Printf.sprintf "%s %s" pp1.ppname pp2.ppname)
      (function [a; b] -> parse pp1 a, parse pp2 b
              | _ -> unexpected ())
      (fun (x,y) -> [print pp1 x; print pp2 y])
*)

  (** All Pps dealing with the [value] type *)
  module V = struct

    (** Low-level Pps *)

    let bool =
      pp ~name:"bool"
        (fun ~pos:_ -> function Bool (_,b) -> b | _ -> unexpected ())
        (fun b -> Bool (pos_null,b))

    let int =
      pp ~name:"int"
        (fun ~pos:_ -> function Int (_,i) -> i | _ -> unexpected ())
        (fun i -> Int (pos_null,i))

    let pos_int = int -| check ~name:"positive-int" (fun i -> i >= 0)

    let ident =
      pp ~name:"ident"
        (fun ~pos:_ -> function Ident (_,i) -> i | _ -> unexpected ())
        (fun str -> Ident (pos_null,str))

    let string =
      pp ~name:"string"
        (fun ~pos:_ -> function String (_,s) -> s | _ -> unexpected ())
        (fun str -> String (pos_null,str))

    let string_tr = string -| pp (fun ~pos:_ -> OpamStd.String.strip) (fun x -> x)

    let simple_arg =
      pp ~name:"ident-or-string"
        (fun ~pos:_ -> function
           | Ident (_,i) -> CIdent i
           | String (_,s) -> CString s
           | _ -> unexpected ())
        (function
          | CIdent i -> Ident (pos_null, i)
          | CString s -> String (pos_null, s))

    let variable_contents =
      pp ~name:"string-or-bool"
        (fun ~pos:_ -> function
           | String (_,s) -> S s
           | Bool (_,b) -> B b
           | _ -> unexpected ())
        (function
          | S s -> String (pos_null, s)
          | B b -> Bool (pos_null, b))

    let list =
      pp ~name:"list" ~name_constr:(Printf.sprintf "[%s]")
        (fun ~pos:_ -> function
          | List (_,l) -> l
          | x -> [x])
        (fun l -> List (pos_null, l))

    let group =
      pp ~name:"group" ~name_constr:(Printf.sprintf "(%s)")
        (fun ~pos:_ -> function
          | Group (_,l) -> l
          | x -> [x])
        (fun l -> Group (pos_null, l))

    let option =
      pp ~name:"option"
        (fun ~pos:_ -> function
          | Option (_,k,l) -> k, l
          | k -> k, [])
        (function
          | (v, []) -> v
          | (v, l) -> Option (pos_null, v, l))

    let map_group pp1 = group -| map_list ~posf:value_pos pp1

    let list_depth expected_depth =
      let rec depth = function
        | List (_,[]) -> 1
        | List (_,(v::_)) -> 1 + depth v
        | Option (_,v,_) -> depth v
        | _ -> 0
      in
      let rec wrap n v =
        if n <= 0 then v else wrap (n-1) (List (pos_null, [v]))
      in
      let rec lift n v =
        if n <= 0 then v else
        match v with
        | List (_, [v]) -> lift (n-1) v
        | v -> v
      in
      pp
        (fun ~pos:_ v -> wrap (expected_depth - depth v) v)
        (fun v -> lift expected_depth v)

    let map_list ?(depth=0) pp1 =
      list_depth depth -|
      pp ~name:(Printf.sprintf "[%s]" pp1.ppname)
        (fun ~pos:_ v ->
           match v with
           | List (_, l) ->
             List.rev @@
             List.rev_map (fun v -> parse pp1 ~pos:(value_pos v) v) l
           | _ -> unexpected ())
        (function
          | l -> List (pos_null, List.rev @@ List.rev_map (print pp1) l))

    let map_option pp1 pp2 =
      option -|
      map_pair ~name:(Printf.sprintf "%s ?{%s}" pp1.ppname pp2.ppname)
        ~posf1:value_pos
        ~posf2:(fun v -> OpamStd.Option.default pos_null (values_pos v))
        pp1 pp2

    let map_pair pp1 pp2 =
      pp ~name:(Printf.sprintf "[%s %s]" pp1.ppname pp2.ppname)
        (fun ~pos:_ -> function
          | List (_,[a; b]) ->
            parse pp1 ~pos:(value_pos a) a, parse pp2 ~pos:(value_pos b) b
          | _ -> unexpected ())
        (fun (a, b) -> List (pos_null, [pp1.print a; pp2.print b]))

    let map_triple pp1 pp2 pp3 =
      pp ~name:(Printf.sprintf "[%s %s %s]" pp1.ppname pp2.ppname pp3.ppname)
        (fun ~pos:_ -> function
          | List (_,[a; b; c]) ->
            parse pp1 ~pos:(value_pos a) a,
            parse pp2 ~pos:(value_pos b) b,
            parse pp3 ~pos:(value_pos c) c
          | _ -> unexpected ())
        (fun (a, b, c) ->
           List (pos_null, [pp1.print a; pp2.print b; pp3.print c]))

    (** Pps for the [value] type to higher level types *)

    let url = string -| of_module "url" (module OpamUrl)

    let url_with_backend backend =
      string -|
      pp ~name:"url"
        (fun ~pos:_ -> OpamUrl.parse ~backend ~handle_suffix:false)
        (fun url -> OpamUrl.to_string url)

    (* a hack to allow "system" compiler as ident rather than string. For
       backwards-compat. Deprecated, for migration only *)
    let compiler_version =
      let system_compiler = "system" in
      let parse ~pos:_ = function
        | Ident (_, v) when v = system_compiler -> v
        | String (_, v) -> v
        | _ -> unexpected ()
      in
      let print v =
        if v = system_compiler then print ident v
        else print string v
      in
      pp ~name:"compiler-version" parse print

    let filter_ident =
      ident -|
      pp ~name:"filter-ident"
        (fun ~pos:_ -> filter_ident_of_string)
        string_of_filter_ident

    let filter =
      let rec parse_filter ~pos l =
        let rec aux = function
          | Bool (_,b) -> FBool b
          | String (_,s) -> FString s
          | Ident (pos,_) as id -> FIdent (parse ~pos filter_ident id)
          | Group (pos,g) -> parse_filter ~pos g
          | Relop (_,op,e,f) -> FOp (aux e, op, aux f)
          | Pfxop (_,`Not,e) -> FNot (aux e)
          | Logop(_,`And,e,f)-> FAnd (aux e, aux f)
          | Logop(_,`Or, e,f)-> FOr (aux e, aux f)
          | _ -> unexpected ()
        in
        match l with
        | [] -> FBool true
        | [Group (_, ([] | _::_::_))] | _::_::_ ->
          bad_format ~pos "expected a single filter expression"
        | [Group(_,[f])] | [f] -> aux f
      in
      let print_filter f =
        let rec aux ?(context=`Or) f =
          let group_if ?(cond=false) f =
            if cond || OpamFormatConfig.(!r.all_parens)
            then Group (pos_null, [f])
            else f
          in
          match f with
          | FString s  -> print string s
          | FIdent fid -> print filter_ident fid
          | FBool b    -> print bool b
          | FOp(e,s,f) ->
            group_if ~cond:(context <> `Or && context <> `And)
              (Relop (pos_null, s, aux ~context:`Relop e, aux ~context:`Relop f))
          | FOr(e,f) ->
            group_if ~cond:(context <> `Or)
              (Logop (pos_null, `Or, aux ~context:`Or e, aux ~context:`Or f))
          | FAnd(e,f) ->
            group_if ~cond:(context <> `Or && context <> `And)
              (Logop (pos_null, `And, aux ~context:`And e, aux ~context:`And f))
          | FNot f ->
            group_if ~cond:(context = `Relop)
              (Pfxop (pos_null, `Not, aux ~context:`Not f))
          | FUndef _ -> assert false
        in
        match f with
        | FBool true -> []
        | f -> [aux f]
      in
      pp ~name:"filter-expression" parse_filter print_filter

    let arg = map_option simple_arg (opt filter)

    let command = map_option (map_list arg) (opt filter)

    let constraints version =
      let rec parse_constraints ~pos:_ l =
        let rec aux = function
          | Prefix_relop (pos, op, v) ->
            Atom (op, parse version ~pos v)
          | Logop (_, `And, l, r) ->
            And (aux l, aux r)
          | Logop (_, `Or, l, r) ->
            Or (aux l, aux r)
          | Pfxop (_,`Not,v) ->
            OpamFormula.neg (fun (op, s) -> (OpamFormula.neg_relop op, s)) (aux v)
          | Group (pos, g) ->
            Block (parse_constraints ~pos g)
          | v -> unexpected ~pos:(value_pos v) ()
        in
        OpamFormula.ands (List.map aux l)
      in
      let rec print_constraints cs =
        let rec aux ?(in_and=false) cs =
          let group_if ?(cond=false) f =
            if cond || OpamFormatConfig.(!r.all_parens)
            then Group (pos_null, [f])
            else f
          in
          match cs with
          | Empty       -> assert false
          | Atom (r, v) ->
            group_if (Prefix_relop (pos_null, r, print version v))
          | And (x, y)  ->
            group_if
              (Logop (pos_null, `And, aux ~in_and:true x, aux ~in_and:true y))
          | Or (x, y)   ->
            group_if ~cond:in_and
              (Logop (pos_null, `Or, aux x, aux y))
          | Block g     -> Group (pos_null, print_constraints g)
        in
        match cs with
        | Empty -> []
        | cs -> [aux cs]
      in
      pp ~name:(version.ppname ^ "-constraints")
        parse_constraints print_constraints

    let filtered_constraints version =
      let rec parse_cs ~pos:_ items =
        let rec aux_parse = function
          | Prefix_relop (pos, op, v) ->
            Atom (Constraint (op, parse version ~pos v))
          | Logop (_, `And, a, b) -> OpamFormula.ands [aux_parse a; aux_parse b]
          | Logop (_, `Or, a, b) -> OpamFormula.ors [aux_parse a; aux_parse b]
          | Group (pos, g) -> OpamFormula.Block (parse_cs ~pos g)
          | Pfxop (pos, `Not, v) ->
            parse_cs ~pos [v] |>
            OpamFormula.neg (function
                | Constraint (op, v) ->
                  Constraint (OpamFormula.neg_relop op, v)
                | Filter f ->
                  Filter (FNot f))
          | filt ->
            let f = filter.parse ~pos:(value_pos filt) [filt] in
            Atom (Filter f)
        in
        OpamFormula.ands (List.map aux_parse items)
      in
      let rec print_cs cs =
        let rec aux ?(in_and=false) cs =
          let group_if ?(cond=false) f =
            if cond || OpamFormatConfig.(!r.all_parens)
            then Group (pos_null, [f])
            else f
          in
          match cs with
          | Empty -> assert false
          | And (x, y) ->
            group_if
              (Logop (pos_null, `And, aux ~in_and:true x, aux ~in_and:true y))
          | Or (x, y) ->
            group_if ~cond:in_and
              (Logop (pos_null, `Or, aux x, aux y))
          | Block g -> Group (pos_null, print_cs g)
          | Atom (Constraint (op,v)) ->
            group_if (Prefix_relop (pos_null, op, print version v))
          | Atom (Filter flt) ->
            (match filter.print flt with
             | f1::fr ->
               group_if
                 (List.fold_left (fun a b -> Logop (pos_null, `And, a, b))
                    f1 fr)
             | [] -> Group (pos_null, []))
        in
        match cs with
        | Empty -> []
        | cs -> [aux cs]
      in
      pp ~name:"filtered-constraints" parse_cs print_cs

    let version =
      string -| of_module "version" (module OpamPackage.Version)

    let ext_version =
      pp ~name:"version-expr"
        (fun ~pos:_ -> function
           | String (_,s) -> FString s
           | Ident (_,s) -> FIdent (filter_ident_of_string s)
           | _ -> unexpected ())
        (function
          | FString s -> String (pos_null, s)
          | FIdent id -> Ident (pos_null, string_of_filter_ident id)
          | _ -> assert false)

    let package_atom constraints =
      map_option
        (string -| of_module "pkg-name" (module OpamPackage.Name))
        constraints

    let package_formula kind constraints =
      let split, join = match kind with
        | `Conj -> OpamFormula.(ands_to_list, ands)
        | `Disj -> OpamFormula.(ors_to_list, ors)
      in
      let rec parse_formula ~pos:_ l =
        let rec aux = function
          | String (pos,_) | Option (pos,_,_) as at ->
            Atom (parse (package_atom constraints) ~pos at)
          | Group (pos,g) -> Block (parse_formula ~pos g)
          | Logop (_, `Or, e1, e2) -> let left = aux e1 in Or (left, aux e2)
          | Logop (_, `And, e1, e2) -> let left = aux e1 in And (left, aux e2)
          | v -> unexpected ~pos:(value_pos v) ()
        in
        join (List.map aux l)
      in
      let rec print_formula f =
        let rec aux ?(in_and=false) f =
          let group_if ?(cond=false) f =
            if cond || OpamFormatConfig.(!r.all_parens)
            then Group (pos_null, [f])
            else f
          in
          match f with
          | Empty -> assert false
          | Block f -> Group (pos_null, print_formula f)
          | And (e,f) ->
            group_if
              (Logop (pos_null, `And, aux ~in_and:true e, aux ~in_and:true f))
          | Or (e,f) ->
            group_if ~cond:in_and
              (Logop (pos_null, `Or, aux e, aux f))
          | Atom at -> group_if (print (package_atom constraints) at)
        in
        List.map (aux ~in_and:false) (split f)
      in
      list -| pp ~name:"pkg-formula" parse_formula print_formula

    let env_binding =
      let parse ~pos:_ = function
        | Relop (_, `Eq, Ident (_,i), String (_,s)) -> i, Eq, s, None
        | Env_binding (_, Ident (_,i), op, String (_,s)) -> i, op, s, None
        | _ -> unexpected ()
      in
      let print (id, op, str, _) =
        Env_binding (pos_null, print ident id, op, print string str)
      in
      list -| singleton -| pp ~name:"env-binding" parse print

    let features =
      let var = ident -| of_module "variable" (module OpamVariable) in
      let doc_filt = map_option string filter in
      let rec parse_features ~pos = function
        | [] -> []
        | [_] -> unexpected ()
        | id :: opt :: r ->
          let doc, filt = parse doc_filt ~pos:(value_pos opt) opt in
          (parse var ~pos id, doc, filt) ::
          parse_features ~pos:(OpamStd.Option.default pos (values_pos r)) r
      in
      let print ft =
        List.fold_right (fun (id, doc, filt) acc ->
            print var id :: print doc_filt (doc, filt) :: acc)
          ft []
      in
      list -| pp ~name:"(variable \"doc\" {filter})*" parse_features print

    (* Only used by the deprecated "os" field *)
    let os_constraint =
      let rec parse_osc ~pos:_ l =
        let rec aux = function
          | Group (pos,g) -> Block (parse_osc ~pos g)
          | String (_,os) -> Atom (true, os)
          | Logop (_,`And,l,r) -> And (aux l, aux r)
          | Logop (_,`Or,l,r) -> Or (aux l, aux r)
          | Pfxop (_,`Not,v) ->
            OpamFormula.neg (fun (b, s) -> (not b, s)) (aux v)
          | v -> unexpected ~pos:(value_pos v) ()
        in
        OpamFormula.ors (List.map aux l)
      in
      let print_osc f =
        let rec aux = function
          | Empty -> assert false
          | Atom (true , os) -> print string os
          | Atom (false, os) -> Pfxop (pos_null, `Not, print string os)
          | Block g -> Group (pos_null, [aux g])
          | And(e,f) -> Logop (pos_null, `And, aux e, aux f)
          | Or(e,f) -> Logop (pos_null, `Or, aux e, aux f)
        in
        match f with
        | Empty -> []
        | f -> [aux f]
      in
      list -| pp ~name:"os-constraint" parse_osc print_osc

  end

  (** Pps for file contents (item lists), mostly list of [Variable(...)]
      fields *)

  type ('a, 'value) field_parser = ('a * 'value option, 'a) t

  (** add setter/getter and an accumulator to a pp; useful to use
      to get/set field records *)
  let ppacc_opt
    (* : ('a -> 'b -> 'a) -> ('a -> 'b option) -> ('value, 'b) t -> 'a field_parser *)
    = fun ?(cleanup = fun ~pos:_ _acc x -> x) set get pp1 ->
      let parse ~pos = function
        | acc, Some s ->
          set (cleanup ~pos acc (pp1.parse ~pos s)) acc
        | acc, None -> acc
      in
      let print s = s, OpamStd.Option.map pp1.print (get s) in
      {
        parse; print;
        ppname = pp1.ppname;
        name_constr = (fun x -> x);
      }

  let ppacc ?cleanup set get pp =
    ppacc_opt set (fun x -> Some (get x)) ?cleanup pp

  let ppacc_ignore = {
    parse = (fun ~pos:_ (acc,_) -> acc);
    print = (fun s -> s, None);
    ppname = "<ignored>";
    name_constr = (fun _ -> "<ignored>");
  }

  let embed set get ppacc = {
    parse = (fun ~pos (acc, x) -> set (ppacc.parse ~pos (get acc, x)) acc);
    print = (fun s -> let s1, v = ppacc.print (get s) in set s1 s, v);
    ppname = ppacc.ppname;
    name_constr = ppacc.name_constr;
  }

  (** Parsers for item lists (standard opam file contents: list of field
      bindings). *)
  module I = struct

    let file =
      pp ~name:"opam-file"
        (fun ~pos:_ file ->
           OpamFilename.of_string file.file_name,
           file.file_contents)
        (fun (file_name, file_contents) ->
           { file_name = OpamFilename.to_string file_name;
             file_contents })

    let map_file pp1 = file -| map_snd pp1

    let item =
      pp ~name:"field-binding"
        (fun ~pos:_ -> function
           | Section (pos,sec) ->
             bad_format ~pos "Unexpected section %s" sec.section_kind
           | Variable (_,k,v) -> k,v)
        (fun (k,v) -> Variable (pos_null, k, v))

    let items = map_list ~posf:item_pos item

    let section kind =
      pp ~name:"file-section"
        (fun ~pos:_ -> function
           | Section (_, ({section_kind; _} as s)) when section_kind = kind ->
             s.section_name, s.section_items
           | Section (pos,sec) ->
             bad_format ~pos "Unexpected section %s" sec.section_kind
           | Variable (pos,k,_) ->
             bad_format ~pos "Unexpected field %s" k)
        (fun (section_name, section_items) ->
           Section (pos_null, { section_kind=kind; section_name; section_items }))

    type ('a, 'value) fields_def = (string * ('a, 'value) field_parser) list

    let good_fields ?name ?(allow_extensions=false) ?(sections=[]) fields =
      let parse ~pos:_ items =
        let rem_fields, rem_sections, ok_items, extra_items =
          List.fold_left (fun (fields,sections,ok,extra) -> function
              | Section (_, {section_kind = k; _}) as s ->
                if List.mem_assoc k sections
                then fields, List.remove_assoc k sections, s::ok, extra
                else fields, sections, ok, s :: extra
              | Variable (_,k,_) as v ->
                if List.mem_assoc k fields
                then List.remove_assoc k fields, sections, v::ok, extra
                else if
                  allow_extensions &&
                  OpamStd.String.starts_with ~prefix:"x-" k &&
                  not @@ List.exists
                    (function Variable (_,k1,_) -> k1 = k | _ -> false)
                    ok
                then fields, sections, v::ok, extra
                else fields, sections, ok, v :: extra)
            (fields,sections,[],[]) items
        in
        rem_fields, rem_sections, List.rev ok_items, List.rev extra_items
      in
      let print (_, _, valid_items, _invalid) = valid_items in
      pp ?name parse print

    let check_fields ?name ?(allow_extensions=false) ?strict ?(sections=[])
        fields =
      let in_name = OpamStd.Option.Op.((name >>| fun n -> " in "^n) +! "") in
      let parse ~pos:_ items =
        let _, _, valid_fields, extra_fields =
          List.fold_left (fun (fields,sections,ok,extra) -> function
              | Section (pos, {section_kind = k; _}) as s ->
                if List.mem_assoc k sections
                then fields, List.remove_assoc k sections, s::ok, extra
                else fields, sections, ok, (pos, k) :: extra
              | Variable (pos,k,_) as v ->
                if List.mem_assoc k fields
                then List.remove_assoc k fields, sections, v::ok, extra
                else if
                  allow_extensions &&
                  OpamStd.String.starts_with ~prefix:"x-" k &&
                  not @@ List.exists
                    (function Variable (_,k1,_) -> k1 = k | _ -> false)
                    ok
                then fields, sections, v::ok, extra
                else fields, sections, ok, (pos,k) :: extra)
            (fields,sections,[],[]) items
        in
        match extra_fields with
        | [] -> items
        | (pos,_) :: _  ->
          warn ~pos ?strict "Unexpected or duplicate fields or sections%s:\n%s"
            in_name
            (OpamStd.Format.itemize
               (fun (pos,k) ->
                  Printf.sprintf "'%s:' at %s" k (string_of_pos pos))
               (List.rev extra_fields));
          valid_fields
      in
      let print items =
        assert (List.for_all (function
            | Section (_, {section_kind = k; _}) -> List.mem_assoc k sections
            | Variable (_,k,_) -> List.mem_assoc k fields)
            items);
        items
      in
      pp ?name parse print

    let fields ?name ?strict ~empty ?(sections=[]) ppas =
      let in_name =
        OpamStd.Option.Op.((name >>| Printf.sprintf "In %s, ") +! "")
      in
      let parse ~pos items =
        (* For consistency, always read fields in ppa order, ignoring file
           order. Some parsers may depend on it. *)
        let section_map, field_map =
          List.fold_left
            (fun (section_map, field_map) -> function
               | Section (pos, {section_kind=k; section_items=v; _}) ->
                 OpamStd.String.Map.add k (pos,v) section_map, field_map
               | Variable (pos, k, v) ->
                 section_map, OpamStd.String.Map.add k (pos,v) field_map)
            OpamStd.String.Map.(empty, empty) items
        in
        let errs, r =
          List.fold_left
            (fun (errs,acc) (field,ppa) ->
               try
                 let pos, v = OpamStd.String.Map.find field field_map in
                 try errs, parse ppa ~pos (acc, Some v) with
                 | Bad_format (pos, msg) ->
                   let msg =
                     Printf.sprintf "%sfield '%s:', %s" in_name field msg
                   in
                   (field, (pos, msg)) :: errs,
                   acc
               with
               | Not_found -> errs, acc)
            ([],empty) ppas
        in
        let errs, r =
          List.fold_left
            (fun (errs,acc) (section,ppa) ->
               try
                 let pos, v = OpamStd.String.Map.find section section_map in
                 try errs, parse ppa ~pos (acc, Some v) with
                 | Bad_format (pos,msg) ->
                   let msg =
                     Printf.sprintf "%ssection '%s' %s" in_name section msg
                   in
                   (section,(pos, msg)) :: errs, acc
               with
               | Not_found -> errs, acc)
            (errs, r) sections
        in
        if errs <> [] then
          warn ~pos ?strict ~exn:(Bad_format_list (List.map snd errs))
            "Errors in fields: %s" (OpamStd.List.concat_map ", " fst errs);
        r
      in
      let print acc =
        OpamStd.List.filter_map
          (fun (field,ppa) ->
             match snd (ppa.print acc) with
             | None | Some (List (_,[]) | Group (_,[])) -> None
             | Some value -> Some (Variable (pos_null, field, value)))
          ppas
        @
        OpamStd.List.filter_map
          (fun (section, ppa) ->
             match snd (ppa.print acc) with
             | None | Some [] -> None
             | Some items ->
               Some (Section (pos_null, {
                   section_kind = section;
                   section_name = None;
                   section_items = items;
                 })))
          sections
      in
      pp ?name parse print

    let partition filter =
      pp
        (fun ~pos:_ -> List.partition filter)
        (fun (a,b) -> a @ b)

    let partition_fields filter =
      partition @@ function
      | Variable (_,k,_) -> filter k
      | _ -> false

    let field name parse =
      pp
        (fun ~pos items ->
           match
             OpamStd.List.filter_map (function
                 | Variable (_,k,v) when k = name -> Some v
                 | _ -> None)
               items
           with
           | [] -> None, items
           | _::_::_ -> bad_format ~pos "Duplicate '%s:' field" name
           | [v] -> Some (parse ~pos v), items)
        (fun (_,x) -> x)


    let extract_field name =
      partition_fields ((=) name) -|
      (map_fst @@ opt @@
       singleton -| item -|
       pp ~name:(Printf.sprintf "'%s:' field" name)
         (fun ~pos:_ (_,v) -> v)
         (fun v -> name,v))

    let check_opam_version
        ?(optional=false)
        ?(f=fun v -> OpamVersion.(compare current_nopatch (nopatch v) >= 0))
        ()
      =
      let name = "opam-version" in
      let opam_v = V.string -| of_module "opam-version" (module OpamVersion) in
      let f v =
        OpamFormatConfig.(!r.skip_version_checks) || match v with
        | Some v -> f v
        | None -> optional
      in
      field name (parse opam_v) -|
      map_fst (check ~name ~errmsg:"unsupported or missing file format version" f)  -|
      pp
        (fun ~pos:_ (_,x) -> x)
        (fun x ->
           (* re-extract the field using parse when printing, to check *)
           parse ~pos:pos_null (field name (parse opam_v)) x)

    type signature = string * string * string

    let signature =
      V.list -| (V.string ^+ V.string ^+ last -| V.string) -|
      pp (fun ~pos:_ (a,(b,c)) -> a,b,c) (fun (a,b,c) -> a,(b,c))

    exception Invalid_signature of pos * (string*string*string) list option

    let signed ~check =
      let pp_sig = V.map_list ~depth:2 signature in
      extract_field "signature" -|
      pp ~name:"signed-file"
        (fun ~pos -> function
          | Some sgs, items ->
            let sgs = parse ~pos pp_sig sgs in
            let str = OpamPrinter.Normalise.items items in
            if not (check sgs str) then
              raise (Invalid_signature (pos, Some sgs))
            else (sgs, items)
          | None, _ ->
            raise (Invalid_signature (pos, None)))
        (fun (sgs, items) ->
           assert (check sgs (OpamPrinter.Normalise.items items));
           Some (print pp_sig sgs),
           items)

  end
end

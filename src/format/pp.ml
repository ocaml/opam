open OpamTypes
open OpamTypesBase
open OpamStd.Op
open OpamFormat

let log f = OpamConsole.log "FORMAT" f

module Normalise = struct
  (** OPAM normalised file format, for signatures:
      - each top-level field on a single line
      - file ends with a newline
      - spaces only after [fieldname:], between elements in lists, before braced
        options, between operators and their operands
      - fields are sorted lexicographically by field name (using [String.compare])
      - newlines in strings turned to ['\n'], backslashes and double quotes
        escaped
      - no comments (they don't appear in the internal file format anyway)
      - fields containing an empty list, or a singleton list containing an empty
        list, are not printed at all
  *)

  let escape_string s =
    let len = String.length s in
    let buf = Buffer.create (len * 2) in
    Buffer.add_char buf '"';
    for i = 0 to len -1 do
      match s.[i] with
      | '\\' | '"' as c -> Buffer.add_char buf '\\'; Buffer.add_char buf c
      | '\n' -> Buffer.add_string buf "\\n"
      | c -> Buffer.add_char buf c
    done;
    Buffer.add_char buf '"';
    Buffer.contents buf

  let rec value = function
    | Relop (_,op,l,r) ->
      String.concat " " [value l; OpamFormula.string_of_relop op; value r]
    | Logop (_,op,l,r) ->
      String.concat " " [value l; string_of_logop op; value r]
    | Pfxop (_,op,r) ->
      String.concat " " [string_of_pfxop op; value r]
    | Prefix_relop (_,op,r) ->
      String.concat " " [string_of_relop op; value r]
    | Ident (_,s) -> s
    | Int (_,i) -> string_of_int i
    | Bool (_,b) -> string_of_bool b
    | String (_,s) -> escape_string s
    | List (_, l) -> OpamStd.List.concat_map ~left:"[" ~right:"]" " " value l
    | Group (_,g) -> OpamStd.List.concat_map ~left:"(" ~right:")" " " value g
    | Option(_,v,l) ->
      OpamStd.List.concat_map ~left:(value v ^ " {") ~right: "}"
        " " value l
    | Env_binding (_,op,id,v) ->
      String.concat " " ["["; value id; op; value v; "]"]

  let rec item = function
    | Variable (_, _, List (_,([]|[List(_,[])]))) -> ""
    | Variable (_, i, List (_,l)) ->
      OpamStd.List.concat_map ~left:(i ^ ": [") ~right:"]" " "
        value l
    | Variable (_, i, v) -> String.concat ": " [i; value v]
    | Section (_,s) ->
      Printf.sprintf "%s %s {\n%s\n}"
        s.section_kind
        (escape_string s.section_name)
        (OpamStd.List.concat_map "\n" item s.section_items)

  let item_order a b = match a,b with
    | Section _, Variable _ -> 1
    | Variable _, Section _ -> -1
    | Variable (_,i,_), Variable (_,j,_) -> String.compare i j
    | Section (_,s), Section (_,t) ->
      String.compare s.section_name t.section_name

  let items its =
    let its = List.sort item_order its in
    OpamStd.List.concat_map ~right:"\n" "\n" item its
end

module Pp = struct

  type ('a,'b) t = {
    parse: 'a -> 'b;
    print: 'b -> 'a;
    name: string;
    pos: 'a -> pos option;
    name_constr: string -> string;
  }

  let pp ?(name="") ?(pos=fun _ -> None) ?(name_constr=fun x -> x) parse print =
    {
      parse; print; name; pos; name_constr;
    }

  (** Utility functions *)

  exception Unexpected
  let unexpected () = raise Unexpected

  let raise_with_pos pos_opt e = match pos_opt with
    | Some p -> raise (add_pos p e)
    | None -> raise e

  (** Basic pp usage *)

  let parse pp x = try pp.parse x with
    | Bad_format (None,_,_) as e -> raise_with_pos (pp.pos x) e
    | Bad_format _ as e -> raise e
    | Unexpected -> bad_format ?pos:(pp.pos x) "Expected %s" pp.name
    | Failure msg ->
      bad_format ?pos:(pp.pos x) "while expecting %s: %s" pp.name msg
    | e ->
      OpamStd.Exn.fatal e;
      bad_format ?pos:(pp.pos x) "while expecting %s: %s"
        pp.name (Printexc.to_string e)

  let print pp x = pp.print x


  (** Pp combination and transformation *)

  (** Piping *)
  let (-|) pp1 pp2 = {
    parse = (fun x ->
        let y = pp1.parse x in
        try parse pp2 y with e -> raise_with_pos (pp1.pos x) e
      );
    print = pp1.print @* pp2.print;
    name = (match pp2.name with "" -> pp1.name | name -> pp1.name_constr name);
    pos = pp1.pos;
    name_constr = pp1.name_constr @* pp2.name_constr;
  }

  let identity = {
    parse = (fun x -> x);
    print = (fun x -> x);
    name = "";
    pos = OpamStd.Option.none;
    name_constr = (fun x -> x);
  }

  let ignore = {
    parse = OpamStd.Option.none;
    print = (fun _ -> assert false);
    name = "ignored";
    pos = OpamStd.Option.none;
    name_constr = (fun _ -> "<ignored>");
  }

  let check ?name ?errmsg f =
    pp
      ?name
      (fun x ->
         if not (f x) then
           match errmsg with
           | Some m -> bad_format "%s" m
           | None -> unexpected ()
         else x)
      (fun x -> assert (f x); x)

  let map_pair ?name pp1 pp2 =
    let name = match name with
      | None -> Printf.sprintf "(%s, %s)" pp1.name pp2.name
      | Some n -> n
    in
    pp ~name
      (fun (a,b) -> parse pp1 a, parse pp2 b)
      (fun (a,b) -> print pp1 a, print pp2 b)

  let map_list ?name pp1 =
    let name = match name with
      | None -> pp1.name ^ "*"
      | Some n -> n
    in
    pp ~name
      (List.rev @* List.rev_map (parse pp1))
      (List.rev @* List.rev_map (print pp1))

  let map_option ?name pp1 =
    let name = match name with
      | None -> pp1.name ^ "?"
      | Some n -> n
    in
    pp ~name
      (OpamStd.Option.map (parse pp1))
      (OpamStd.Option.map (print pp1))

  let singleton = {
    parse = (function [x] -> x | _ -> unexpected ());
    print = (fun x -> [x]);
    name = "";
    pos = OpamStd.Option.none;
    name_constr = (fun x -> x);
  }

  (** Pps from strings *)

  module type STRINGABLE = sig
    type t
    val of_string: string -> t
    val to_string: t -> string
  end

  let of_module :
    type a. string -> (module STRINGABLE with type t = a) -> (string, a) t
    =
    fun name (module X: STRINGABLE with type t = a) ->
    pp ~name X.of_string X.to_string

  (** low-level Pps for the Lines parser ([string list list]) *)

  let lines_set empty add fold pp1 =
    pp
      ~name:(Printf.sprintf "(%s) lines" pp1.name)
      (fun lines ->
         List.fold_left (fun acc -> function
             | [] -> acc
             | line -> add (parse pp1 line) acc)
           empty lines)
      (fun x ->
         List.rev (fold (fun v acc -> print pp1 v::acc) x []))

  let lines_map empty add fold pp1 =
    pp
      ~name:(Printf.sprintf "(%s) lines" pp1.name)
      (fun lines ->
         List.fold_left (fun acc -> function
             | [] -> acc
             | line -> let k,v = parse pp1 line in add k v acc)
           empty lines)
      (fun x ->
         List.rev (fold (fun k v acc -> print pp1 (k,v)::acc) x []))

  (** Build tuples from lists *)
  let (^+) pp1 pp2 =
    pp
      ~name:(Printf.sprintf "%s %s" pp1.name pp2.name)
      (function x::r -> parse pp1 x, parse pp2 r | [] -> unexpected ())
      (fun (x,y) -> print pp1 x :: print pp2 y)

  let last = singleton

  let opt pp1 =
    pp
      ~name:("?"^pp1.name)
      (function [] -> None | l -> Some (pp1.parse l))
      (function Some x -> pp1.print x | None -> [])

  let default d =
    pp
      (function None -> d | Some x -> x)
      (fun x -> Some x)

  module Op = struct
    let ( -| ) = ( -| )
    let ( ^+ ) = ( ^+ )
  end

(*
  let list2 pp1 pp2 =
    pp ~name:(Printf.sprintf "%s %s" pp1.name pp2.name)
      (function [a; b] -> parse pp1 a, parse pp2 b
              | _ -> unexpected ())
      (fun (x,y) -> [print pp1 x; print pp2 y])
*)

  (** All Pps dealing with the [value] type *)
  module V = struct

    (** Low-level Pps *)

    let pos v = Some (value_pos v)

    let bool =
      pp ~name:"bool" ~pos
        (function Bool (_,b) -> b | _ -> unexpected ())
        (fun b -> Bool (pos_null,b))

    let int =
      pp ~name:"int" ~pos
        (function Int (_,i) -> i | _ -> unexpected ())
        (fun i -> Int (pos_null,i))

    let pos_int = int -| check ~name:"positive-int" (fun i -> i >= 0)

    let ident =
      pp ~name:"ident" ~pos
        (function Ident (_,i) -> i | _ -> unexpected ())
        (fun str -> Ident (pos_null,str))

    let string =
      pp ~name:"string" ~pos
        (function String (_,s) -> s | _ -> unexpected ())
        (fun str -> String (pos_null,str))

    let simple_arg =
      pp ~name:"ident-or-string" ~pos
        (function
          | Ident (_,i) -> CIdent i
          | String (_,s) -> CString s
          | _ -> unexpected ())
        (function
          | CIdent i -> Ident (pos_null, i)
          | CString s -> String (pos_null, s))

    let variable_contents =
      pp ~name:"string-or-bool" ~pos
        (function
          | String (_,s) -> S s
          | Bool (_,b) -> B b
          | _ -> unexpected ())
        (function
          | S s -> String (pos_null, s)
          | B b -> Bool (pos_null, b))

    let list =
      pp ~name:"list" ~name_constr:(Printf.sprintf "[%s]") ~pos
        (function
          | List (_,l) -> l
          | x -> [x])
        (fun l -> List (pos_null, l))

    let group =
      pp ~name:"group" ~name_constr:(Printf.sprintf "(%s)") ~pos
        (function
          | Group (_,l) -> l
          | x -> [x])
        (fun l -> Group (pos_null, l))

    let option =
      pp ~name:"option" ~pos
        (function
          | Option (_,k,l) -> k, l
          | k -> k, [])
        (function
          | (v, []) -> v
          | (v, l) -> Option (pos_null, v, l))

    let map_group pp1 = group -| map_list pp1

    let map_list pp1 = list -| map_list pp1

    let map_option pp1 pp2 =
      option -|
      map_pair ~name:(Printf.sprintf "%s ?{%s}" pp1.name pp2.name) pp1 pp2

    let map_pair pp1 pp2 =
      pp ~name:(Printf.sprintf "[%s %s]" pp1.name pp2.name) ~pos
        (function
          | List (_,[a; b]) -> (parse pp1 a, parse pp2 b)
          | _ -> unexpected ())
        (fun (a, b) -> List (pos_null, [pp1.print a; pp2.print b]))
(*
  let triplet pp1 pp2 pp3 =
    pp
      ~name:(Printf.sprintf "[%s %s %s]" pp1.name pp2.name pp3.name)
      ~pos:value_pos_opt
      (function
        | List (_,[a; b; c]) -> (parse pp1 a, parse pp2 b, parse pp3 c)
        | _ -> unexpected ())
      (fun (a,b,c) -> List (pos_null, [pp1.print a; pp2.print b; pp3.print c]))
*)
(*
    let single_option pp1 pp2 =
      map_option pp1 (singleton -| pp2)
*)

    (** Pps for the [value] type to higher level types *)

    let address =
      string -|
      pp ~name:"url" address_of_string (fun a -> string_of_address a)

    let url =
      string -|
      pp
        ~name:"url"
        (OpamTypesBase.(address_of_string @> parse_url))
        (fun (addr,kind) -> OpamTypesBase.string_of_address ~kind addr)

    let filter_ident =
      ident -|
      pp ~name:"filter-ident" filter_ident_of_string string_of_filter_ident

    (* a hack to allow "system" compiler as ident rather than string. For
       backwards-compat. *)
    let compiler_version =
      let str_system = OpamCompiler.(to_string system) in
      let comp = of_module "compiler-version" (module OpamCompiler.Version) in
      let parse = function
        | Ident (_, v) when v = str_system -> parse comp v
        | String (_, v) -> parse comp v
        | _ -> unexpected ()
      in
      let print v =
        let v = print comp v in
        if v = str_system then print ident v
        else print string v
      in
      pp ~name:"compiler-version" parse print

    let filter =
      let rec parse_filter l =
        let rec aux = function
          | Bool (_,b) -> FBool b
          | String (_,s) -> FString s
          | Ident _ as id -> FIdent (parse filter_ident id)
          | Group (_,g) -> parse_filter g
          | Relop (_,op,e,f) -> FOp (aux e, op, aux f)
          | Pfxop (_,`Not,e) -> FNot (aux e)
          | Logop(_,`And,e,f)-> FAnd (aux e, aux f)
          | Logop(_,`Or, e,f)-> FOr (aux e, aux f)
          | _ -> unexpected ()
        in
        match l with
        | [] -> FBool true
        | [Group (_, ([] | _::_::_))] | _::_::_ as x ->
          bad_format ?pos:(values_pos x) "Expected a single filter expression"
        | [Group(_,[f])] | [f] -> aux f
      in
      let print_filter f =
        let rec aux ?paren f =
          let group ?kind f =
            if OpamFormatConfig.(!r.all_parens) ||
               (paren <> None && paren <> kind)
            then Group (pos_null, [f]) else f
          in
          match f with
          | FString s  -> print string s
          | FIdent fid -> print filter_ident fid
          | FBool b    -> print bool b
          | FOp(e,s,f) -> group (Relop (pos_null, s, aux e, aux f))
          | FOr(e,f) -> (* And, Or have the same priority, left-associative *)
            group ~kind:`Or (Logop (pos_null, `Or, aux e, aux ~paren:`Or f))
          | FAnd(e,f) ->
            group ~kind:`And (Logop (pos_null, `And, aux e, aux ~paren:`And f))
          | FNot f -> group (Pfxop (pos_null, `Not, aux ~paren:`Not f))
          | FUndef -> assert false
        in
        [aux f]
      in
      pp ~name:"filter-expression" ~pos:values_pos parse_filter print_filter

    let arg = map_option simple_arg (opt filter)

    let command = map_option (map_list arg) (opt filter)

    let constraints version =
      let rec parse_constraints l =
        let rec aux = function
          | Prefix_relop (_, op, v) ->
            Atom (op, parse version v)
          | Logop (_, `And, l, r) ->
            And (aux l, aux r)
          | Logop (_, `Or, l, r) ->
            Or (aux l, aux r)
          | Pfxop (_,`Not,v) ->
            OpamFormula.neg (fun (op, s) -> (OpamFormula.neg_relop op, s)) (aux v)
          | Group (_, g) ->
            Block (parse_constraints g)
          | _ -> unexpected ()
        in
        OpamFormula.ands (List.map aux l)
      in
      let rec print_constraints cs =
        let rec aux = function
          | Empty       -> assert false
          | Atom (r, v) -> Prefix_relop (pos_null, r, print version v)
          | And (x, y)  -> Logop (pos_null, `And, aux x, aux y)
          | Or (x, y)   -> Logop (pos_null, `Or, aux x, aux y)
          | Block g     -> Group (pos_null, print_constraints g)
        in
        match cs with
        | Empty -> []
        | cs -> [aux cs]
      in
      pp ~name:(version.name ^ "-constraints") ~pos:values_pos
        parse_constraints print_constraints

    let dep_flag =
      string -| pp ~name:"dependency-flag" dep_flag_of_string string_of_dep_flag

    (* Version constraints with additional leading keywords
       ("build","test"...), used for dependency flags *)
    let ext_constraints version =
      let rec parse_ext_constraints = function
        | Ident (_, _) as kw :: r ->
          let kws, f = parse_ext_constraints r in
          parse_dep_flag kw :: kws, f
        | Logop (_, `And, t1, t2) :: r -> parse_ext_constraints (t1::t2::r)
        | t -> [], parse (constraints version) t
      in
      let print (kws, cs) =
        (* The kws must be aggregated with an '&' to the first constraint, if
           any *)
        match print (constraints version) cs, kws with
        | [], [] -> []
        | [], kw::kws ->
          [List.fold_left (fun acc kw ->
               Logop (pos_null, `And, make_dep_flag kw, acc))
              (make_dep_flag kw) kws]
        | c::cs, kws ->
          List.fold_left (fun acc kw ->
              Logop (pos_null, `And, make_dep_flag kw, acc))
            c kws
          :: cs
      in
      pp ~name:"ext-constraints" ~pos:values_pos parse_ext_constraints print

    let package_atom constraints =
      map_option
        (string -| of_module "pkg-name" (module OpamPackage.Name))
        (constraints
           (string -| of_module "pkg-version" (module OpamPackage.Version)))

    let package_formula kind constraints =
      let split, join = match kind with
        | `Conj -> OpamFormula.(ands_to_list, ands)
        | `Disj -> OpamFormula.(ors_to_list, ors)
      in
      let rec parse_formula l =
        let rec aux = function
          | String _ | Option _ as at ->
            Atom (parse (package_atom constraints) at)
          | Group (_,g) -> Block (parse_formula g)
          | Logop (_, `Or, e1, e2) -> let left = aux e1 in Or (left, aux e2)
          | Logop (_, `And, e1, e2) -> let left = aux e1 in And (left, aux e2)
          | _ -> unexpected ()
        in
        join (List.map aux l)
      in
      let rec print_formula f =
        let rec aux = function
          | Empty -> assert false
          | Block f -> Group (pos_null, print_formula f)
          | And (e,f) -> Logop (pos_null, `And, aux e, aux f)
          | Or (e,f) -> Logop (pos_null, `Or, aux e, aux f)
          | Atom at -> print (package_atom constraints) at
        in
        List.map aux (split f)
      in
      list -| pp ~name:"pkg-formula" ~pos:values_pos parse_formula print_formula

    let env_binding =
      let parse = function
        | Relop (_, `Eq, Ident (_,i), String (_,s)) -> i, "=", s
        | Env_binding (_, op, Ident (_,i), String (_,s)) -> i, op, s
        | _ -> unexpected ()
      in
      let print (id, op, str) =
        Env_binding (pos_null, op, print ident id, print string str)
      in
      list -| singleton -| pp ~name:"env-binding" parse print

    let features =
      let var = ident -| of_module "variable" (module OpamVariable) in
      let doc_filt = map_option string filter in
      let rec parse_features = function
        | [] -> []
        | [_] -> unexpected ()
        | id :: opt :: r ->
          let doc, filt = parse doc_filt opt in
          (parse var id, doc, filt) :: parse_features r
      in
      let print ft =
        List.fold_right (fun (id, doc, filt) acc ->
            print var id :: print doc_filt (doc, filt) :: acc)
          ft []
      in
      list -| pp ~name:"(variable \"doc\" {filter})*" parse_features print

    (* Only used by the deprecated "os" field *)
    let os_constraint =
      let rec parse_osc l =
        let rec aux = function
          | Group (_,g) -> Block (parse_osc g)
          | String (_,os) -> Atom (true, os)
          | Logop (_,`And,l,r) -> And (aux l, aux r)
          | Logop (_,`Or,l,r) -> Or (aux l, aux r)
          | Pfxop (_,`Not,v) ->
            OpamFormula.neg (fun (b, s) -> (not b, s)) (aux v)
          | _ -> unexpected ()
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
        [aux f]
      in
      list -| pp ~name:"os-constraint" parse_osc print_osc

  end

  (** Pps for file contents (item lists), mostly list of [Variable(...)]
      fields *)

  (* type 'a field_parser = ('a * value option, 'a) t *)

  (** add setter/getter and an accumulator to a pp; useful to use
      to get/set field records *)
  let ppacc_opt
    (* : ('a -> 'b -> 'a) -> ('a -> 'b option) -> ('value, 'b) t -> 'a field_parser *)
    = fun set get pp1 -> {
        name = pp1.name;
        pos = (function _, Some v -> pp1.pos v | _, None -> None);
        parse = (function acc, Some s -> set acc (pp1.parse s) | acc, None -> acc);
        print = (fun s -> s, OpamStd.Option.map pp1.print (get s));
        name_constr = (fun x -> x);
      }

  let ppacc set get pp = ppacc_opt set (fun x -> Some (get x)) pp

  let ppacc_ignore = {
    parse = (fun (acc,_) -> acc);
    print = (fun s -> s, None);
    name = "<ignored>";
    pos = OpamStd.Option.none;
    name_constr = (fun _ -> "<ignored>");
  }

  (** Parsers for item lists (standard opam file contents: list of field
      bindings). *)
  module I = struct

    let item =
      pp ~name:"field-binding"
        (function
          | Section (pos,sec) ->
            bad_format ~pos "Unexpected section %s" sec.section_name
          | Variable (_,k,v) -> k,v)
        (fun (k,v) -> Variable (pos_null, k, v))

    let items = map_list item

    let check_fields ?name ?(allow_extensions=false) fields =
      let in_name = OpamStd.Option.Op.((name >>| fun n -> " in "^n) +! "") in
      let parse items =
        let _, valid_fields, extra_fields =
          List.fold_left (fun (fields,ok,extra) -> function
              | Section (pos,sec) ->
                bad_format ~pos "Unexpected section %s%s"
                  sec.section_name in_name
              | Variable (pos,k,_) as v ->
                if List.mem_assoc k fields
                then List.remove_assoc k fields, v::ok, extra
                else if
                  allow_extensions &&
                  OpamStd.String.starts_with ~prefix:"x-" k &&
                  not @@ List.exists
                    (function Variable (_,k1,_) -> k1 = k | _ -> false)
                    ok
                then fields, v::ok, extra
                else fields, ok, (pos,k) :: extra)
            (fields,[],[]) items
        in
        match extra_fields with
        | [] -> items
        | (pos,_) :: _  ->
          let msg =
            Printf.sprintf "Unexpected or duplicate fields%s:%s" in_name
              (OpamStd.Format.itemize
                 (fun (pos,k) ->
                    Printf.sprintf "'%s:' at %s" k (string_of_pos pos))
                 extra_fields)
          in
          if OpamFormatConfig.(!r.strict) then bad_format ~pos "%s" msg
          else log "Warning: %s" msg;
          valid_fields
      in
      let print items =
        assert (List.for_all (function
            | Variable (_,k,_) -> List.mem_assoc k fields
            | _ -> false)
            items);
        items
      in
      pp ?name parse print

    let fields ?name ~empty ppas =
      let in_name = OpamStd.Option.Op.((name >>| fun n -> " in "^n) +! "") in
      let parse items =
        List.fold_left
          (fun acc -> function
             | Section _ -> acc
             | Variable (pos, k, v) ->
               try (List.assoc k ppas).parse (acc, Some v) with
               | Not_found ->
                 if OpamFormatConfig.(!r.strict) then
                   bad_format ~pos "Field '%s:' unrecognised%s" k in_name;
                 log "Field '%s:' ignored: unknown%s" k in_name;
                 acc
               | e when OpamFormatConfig.(!r.strict) -> raise (add_pos pos e)
               | Bad_format _ as e ->
                 log "Field '%s:' ignored%s: %s"
                   k in_name (string_of_bad_format (add_pos pos e));
                 acc
               | e ->
                 log "Field '%s:' ignored%s: At %s: %s"
                   k in_name (string_of_pos pos) (Printexc.to_string e);
                 acc)
          empty items
      in
      let print acc =
        OpamStd.List.filter_map
          OpamStd.Option.Op.(fun (field,ppa) ->
              snd (ppa.print acc) >>| fun value ->
              Variable (pos_null, field, value))
          ppas
      in
      pp ?name ~pos:items_pos parse print

    let partition_fields filter =
      pp
        (List.partition (function Variable (_,k,_) -> filter k | _ -> false))
        (fun (a,b) -> a @ b)

    let extract_field name =
      partition_fields ((=) name) -|
      map_pair
        (opt (singleton -| item -| pp ~name:(Printf.sprintf "'%s:' field" name)
                (fun (_,v) -> v) (fun v -> name,v)))
        identity

    let opam_version
        ?(v=OpamVersion.current_nopatch)
        ?(f=fun v -> OpamVersion.(compare current_nopatch (nopatch v) >= 0))
        ()
      =
      let opam_v = V.string -| of_module "opam-version" (module OpamVersion) in
      let check_v v = OpamFormatConfig.(!r.skip_version_checks) ||
                      OpamStd.Option.Op.((v >>| f) +! false)
      in
      extract_field "opam-version" -|
      map_pair
        (map_option opam_v -| check check_v)
        identity -|
      pp (fun (v,items) -> items) (fun items -> Some v,items)

    let signature =
      V.list -| (V.string ^+ V.string ^+ last -| V.string)

    exception Invalid_signature of pos * (string*string*string) option
(*
    let signed check =
      extract_field "signature" -|
      pp ~name:"signed-file"
        (function
          | Some (pos,sgs), items ->
            let sgs = parse (V.map_list signature) sgs in
            let str = Normalise.items items in
            if not (check sgs str) then
              raise (Invalid_signature (pos, Some sgs))
            else items
          | None, _ ->
            raise (Invalid_signature
                     (OpamStd.Option.default pos_null (items_pos items),
                      None)))
        (fun _ -> assert false)
*)
  end


end

include Pp


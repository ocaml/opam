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
  }

  let pp ?(name="") ?(pos=fun _ -> None) parse print = {
    parse; print; name; pos;
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
    name = (match pp2.name with "" -> pp1.name | name -> name);
    pos = pp1.pos;
  }

  let identity = {
    name = "";
    pos = OpamStd.Option.none;
    parse = (fun x -> x);
    print = (fun x -> x);
  }

  let ignore = {
    name = "ignored";
    pos = OpamStd.Option.none;
    parse = OpamStd.Option.none;
    print = Pervasives.ignore;
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

  let singleton = {
    name = "";
    pos = OpamStd.Option.none;
    parse = (function [x] -> x | _ -> unexpected ());
    print = (fun x -> [x]);
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
    let (-|) = (-|)
    let (^+) = (^+)
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
      pp ~name:"list" ~pos
        (function
          | List (_,l) -> l
          | x -> [x])
        (fun l -> List (pos_null, l))

    let group =
      pp ~name:"group" ~pos
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
          | (v, [])   -> v
          | (v, l) -> Option (pos_null, v, l))

    let map_group pp1 =
      group -| map_list ~name:(Printf.sprintf "(%s*)" pp1.name) pp1

    let map_list pp1 =
      list -| map_list ~name:(Printf.sprintf "[%s*]" pp1.name) pp1

    let map_option pp1 pp2 =
      option -|
      map_pair ~name:(Printf.sprintf "%s {%s}" pp1.name pp2.name) pp1 pp2

    let ignore = {
      name = "ignored";
      pos = OpamStd.Option.none;
      parse = OpamStd.Option.none;
      print = fun _ -> Group (pos_null, []);
    }

(*
  let pair pp1 pp2 =
    pp
      ~name:(Printf.sprintf "[%s %s]" pp1.name pp2.name)
      ~pos:value_pos_opt
      (function
        | List (_,[a; b]) -> (parse pp1 a, parse pp2 b)
        | _ -> unexpected ())
      (fun (a, b) -> List (pos_null, [pp1.print a; pp2.print b]))

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
          | FUndef -> make_ident "#undefined"
        in
        [aux f]
      in
      pp ~name:"filter-expression" parse_filter print_filter

    let arg = map_option simple_arg (opt filter)

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
      }

  let ppacc set get pp = ppacc_opt set (fun x -> Some (get x)) pp


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

    let extract_field name items =
      List.fold_left (fun (found, others) -> function
          | Variable (pos, k, v) when k = name ->
            if found = None then Some (pos,v), others
            else bad_format ~pos "Duplicate '%s:' field" name
          | x -> found, x::others)
        (None,[]) items

    let opam_version
        ?(v=OpamVersion.current_nopatch)
        ?(f=fun v -> OpamVersion.(compare current_nopatch (nopatch v) >= 0))
        ()
      =
      let parse items =
        match extract_field "opam-version" items with
        | Some (pos,v), items ->
          let v = OpamVersion.of_string (V.string.parse v) in
          if not OpamFormatConfig.(!r.skip_version_checks) && not (f v) then
            bad_format ~pos "Unsupported opam file format version %S"
              (OpamVersion.to_string v);
          items
        | None, items ->
          let pos = match items with i::_ -> item_pos i | [] -> pos_null in
          log "Error: missing 'opam-version:' field at %s" (string_of_pos pos);
          if OpamFormatConfig.(!r.strict) then
            bad_format ~pos "Missing 'opam-version:' field";
          items
      in
      let print items =
        let v = V.string.print OpamVersion.(to_string v) in
        Variable (pos_null, "opam-version", v) :: items
      in
      pp ~name:"opam-version:" ~pos:items_pos parse print

    let signature =
      V.list -| (V.string ^+ V.string ^+ (last -| V.string))

    exception Invalid_signature of pos

    let signed check =
      let parse items =
        match extract_field "signature" items with
        | Some (pos,sgs), items ->
          let sgs = parse (V.map_list signature) sgs in
          let str = Normalise.items items in
          if not (check sgs str) then
            raise (Invalid_signature pos)
          else items
        | None, _ ->
          raise (Invalid_signature
                   (OpamStd.Option.default pos_null (items_pos items)))
      in
      let print _ = assert false in
      pp ~name:"signature:" parse print
  end


end

include Pp


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

(* Base parsing functions *)
module Pp = struct

  type ('a,'b) t = {
    parse: 'a -> 'b;
    print: 'b -> 'a;
    name: string;
    pos: 'a -> pos option;
  }

  exception Unexpected
  let unexpected () = raise Unexpected

  let raise_with_pos pos_opt e = match pos_opt with
    | Some p -> raise (add_pos p e)
    | None -> raise e

  let parse pp x = try pp.parse x with
    | Bad_format (None,_,_) as e -> raise_with_pos (pp.pos x) e
    | Bad_format _ as e -> raise e
    | Unexpected -> bad_format ?pos:(pp.pos x) "Expected %s" pp.name
    | e ->
      OpamStd.Exn.fatal e;
      bad_format ?pos:(pp.pos x) "while parsing for %s: %s"
        pp.name (Printexc.to_string e)

  let print pp x = pp.print x

  let (++) pp1 pp2 = {
    parse = (fun x ->
        let y = pp1.parse x in
        try parse pp2 y with e -> raise_with_pos (pp1.pos x) e
      );
    print = pp1.print @* pp2.print;
    name = (match pp2.name with "" -> pp1.name | name -> name);
    pos = pp1.pos;
  }

  module Op = struct
    let (++) = (++)
  end

  let pp ?(name="") ?(pos=fun _ -> None) parse print = {
    parse; print; name; pos;
  }

  let value_pos_opt v = Some (value_pos v)

  let bool =
    pp
      ~name:"bool"
      ~pos:value_pos_opt
      (function Bool (_,b) -> b | _ -> unexpected ())
      (fun b -> Bool (pos_null,b))

  let int =
    pp
      ~name:"int"
      ~pos:value_pos_opt
      (function Int (_,i) -> i | _ -> unexpected ())
      (fun i -> Int (pos_null,i))

  let ident =
    pp
      ~name:"ident"
      ~pos:value_pos_opt
      (function Ident (_,i) -> i | _ -> unexpected ())
      (fun str -> Ident (pos_null,str))

  let string =
    pp
      ~name:"string"
      ~pos:value_pos_opt
      (function String (_,s) -> s | _ -> unexpected ())
      (fun str -> String (pos_null,str))

  let list pp1 =
    pp
      ~name:(Printf.sprintf "[%s*]" pp1.name)
      ~pos:value_pos_opt
      (function
        | List (_,s) -> List.rev (List.rev_map (parse pp1) s)
        | x -> [pp1.parse x])
      (fun l -> List (pos_null, List.rev (List.rev_map pp1.print l)))

  let group pp1 =
    pp
      ~name:(Printf.sprintf "(%s)" pp1.name)
      ~pos:value_pos_opt
      (function
        | Group (_,g) -> List.rev (List.rev_map (parse pp1) g)
        | _ -> unexpected ())
      (fun g -> Group (pos_null, List.rev (List.rev_map pp1.print g)))

  let option ppval ppopt =
    pp
      ~name:(Printf.sprintf "%s {%s*}" ppval.name ppopt.name)
      ~pos:value_pos_opt
      (function
        | Option (_,k,l) -> parse ppval k, Some (parse ppopt l)
        | k -> ppval.parse k, None)
      (function
        | (v, None)   -> ppval.print v
        | (v, Some o) -> Option (pos_null, ppval.print v, ppopt.print o))

  let single_option ppval ppopt =
    pp
      ~name:(Printf.sprintf "%s {%s}" ppval.name ppopt.name)
      ~pos:value_pos_opt
      (function
        | Option (_,k,[l]) -> parse ppval k, Some (parse ppopt l)
        | Option _ -> unexpected ()
        | k -> ppval.parse k, None)
      (function
        | (v, None)   -> ppval.print v
        | (v, Some o) -> Option (pos_null, ppval.print v, [ppopt.print o]))

  let singleton pp1 =
    pp
      ~name:(Printf.sprintf "[%s]" pp1.name)
      ~pos:value_pos_opt
      (function List (_, [a]) -> a | _ -> unexpected ())
      (fun x -> List (pos_null, [x]))

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

  let check f msg =
    pp
      (fun x -> if not (f x) then bad_format "%s" msg else x)
      (fun x -> assert (f x); x)

(*
  let check_val msg f pp = {
    parse = (fun v ->
        let x = pp.parse v in
        if not (f x) then bad_format ~pos:(value_pos v) "%s" msg
        else x);
    print = (fun x ->
        assert (f x);
        pp.print x);
  }
*)
  type 'a field_parser = ('a * value option, 'a) t

  (* add setter/getter and an accumulator to a pp; useful to use
     to get/set field records *)
  let ppacc_opt
    : ('a -> 'b -> 'a) -> ('a -> 'b option) -> ('value, 'b) t -> 'a field_parser
    = fun set get pp1 -> {
        name = pp1.name;
        pos = (function _, Some v -> pp1.pos v | _, None -> None);
        parse = (function acc, Some s -> set acc (pp1.parse s) | acc, None -> acc);
        print = (fun s -> s, OpamStd.Option.map pp1.print (get s));
      }

  let ppacc set get pp = ppacc_opt set (fun x -> Some (get x)) pp

  let check_fields ?(allow_extensions=false) fields =
    let parse items =
      let _, valid_fields, extra_fields =
        List.fold_left (fun (fields,ok,extra) -> function
            | Section (pos,sec) ->
              bad_format ~pos "Unexpected section %s" sec.section_name
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
          Printf.sprintf "Unexpected or duplicate fields:%s"
            (OpamStd.Format.itemize
               (fun (pos,k) -> Printf.sprintf "%S at %s" k (string_of_pos pos))
               extra_fields)
        in
        if OpamFormatConfig.(!r.strict) then bad_format ~pos "%s" msg
        else log "Warning: %s" msg;
        valid_fields
    in
    pp parse (fun x -> x)

  let fields ?name ~empty ppas =
    let parse items =
      List.fold_left
        (fun acc -> function
           | Section _ -> acc
           | Variable (pos, k, v) ->
             try (List.assoc k ppas).parse (acc, Some v) with
             | e when OpamFormatConfig.(!r.strict) -> raise (add_pos pos e)
             | Bad_format _ as e ->
               log "Field %S ignored: %s"
                 k (string_of_bad_format (add_pos pos e));
               acc
             | e ->
               log "Field %S ignored: At %s: %s"
                 k (string_of_pos pos) (Printexc.to_string e);
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
        let v = OpamVersion.of_string (string.parse v) in
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
      let v = string.print OpamVersion.(to_string v) in
      Variable (pos_null, "opam-version", v) :: items
    in
    pp ~name:"opam-version:" ~pos:items_pos parse print

  let signature = triplet string string string


  exception Invalid_signature of pos

  let signed check =
    let parse items =
      match extract_field "signature" items with
      | Some (pos,sgs), items ->
        let sgs = parse (list signature) sgs in
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

  let address = string ++ pp address_of_string (fun a -> string_of_address a)

  let url =
    string ++
    pp
      ~name:"url"
      (OpamTypesBase.(address_of_string @> parse_url))
      (fun (addr,kind) -> OpamTypesBase.string_of_address ~kind addr)

end

include Pp


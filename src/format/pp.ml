open OpamTypes
open OpamTypesBase
open OpamStd.Op
open OpamFormat

let log f = OpamConsole.log "FORMAT" f

(* Base parsing functions *)
module Pp = struct

  type ('a,'b) t = {
    parse: 'a -> 'b;
    print: 'b -> 'a;
    name: string option;
    pos: 'a -> pos option;
  }

  exception Unexpected
  let unexpected () = raise Unexpected

  let raise_with_pos pos_opt e = match pos_opt with
    | Some p -> raise (add_pos p e)
    | None -> raise e

  let parse pp x =
    try pp.parse x with
    | Bad_format (None,_,_) as e -> raise_with_pos (pp_pos x) e
    | Unexpected ->
      bad_format ?pos:(pp.pos x) "Expected %s" pp.name
    | e ->
      bad_format ?pos:(pp.pos x) "while parsing for (%s): %s"
        pp.name (Printexc.to_string e)

  let print pp x = pp.print x

  let (++) pp1 pp2 = {
    parse = (fun x ->
        let y = pp1.parse x in
        try parse pp2 y with e -> raise_with_pos (pp1.pos x) e
      );
    print = pp1.print @* pp2.print;
    name = OpamStd.Option.Op.(pp2.name ++ pp1.name);
    pos = pp1.pos;
  }

  module Op = struct
    let (++) = (++)
  end

  let pp ?name ?(pos=fun _ -> None) parse print = { parse; print; name; pos; }

  let protect_value pp =
    { pp with parse = protect_value pp.parse }

  let protect_values pp =
    { pp with parse = protect_values pp.parse }

  let bool =
    let name = "bool" in
    let pos = value_pos in
    let parse = function
      | Bool (_,b) -> b
      | x -> bad_format ~pos:(value_pos x) "Expected a bool"
    and print b = Bool (pos_null,b)
    in { parse; print; name }

  let int =
    let parse = function
      | Int (_,i) -> i
      | x -> bad_format ~pos:(value_pos x) "Expected an int"
    and print i = Int (pos_null,i)
    in { parse; print }

  let ident =
    let parse = function
      | Ident (_,i) -> i
      | x -> bad_format ~pos:(value_pos x) "Expected an ident"
    and print str = Ident (pos_null,str)
    in { parse; print }

  let string =
    let parse = function
      | String (_,s) -> s
      | x -> bad_format ~pos:(value_pos x) "Expected a string"
    and print str = String (pos_null,str)
    in { parse; print }

  let list pp =
    let pp = protect_value pp in
    let parse =
      function
      | List (_,s) -> List.rev (List.rev_map pp.parse s)
      | x -> [pp.parse x]
    and print l = List (pos_null, List.rev (List.rev_map pp.print l))
    in { parse; print }

  let group pp =
    let pp = protect_value pp in
    let parse = function
      | Group (_,g) -> List.rev (List.rev_map pp.parse g)
      | x -> bad_format ~pos:(value_pos x) "Expected a group"
    and print g = Group (pos_null, List.rev (List.rev_map pp.print g))
    in { parse; print }

  let option ppval ppopt =
    let ppval = protect_value ppval in
    let ppopt = protect_values ppopt in
    let parse = function
      | Option (_,k,l) -> ppval.parse k, Some (ppopt.parse l)
      | k -> ppval.parse k, None
    and print = function
      | (v, None)   -> ppval.print v
      | (v, Some o) -> Option (pos_null, ppval.print v, ppopt.print o)
    in { parse; print }

  let single_option ppval ppopt =
    let ppval = protect_value ppval in
    let ppopt = protect_values ppopt in
    let parse = function
      | Option (_,k,l) -> ppval.parse k, Some (ppopt.parse l)
      | k -> ppval.parse k, None
    and print = function
      | (v, None)   -> ppval.print v
      | (v, Some o) -> Option (pos_null, ppval.print v, ppopt.print o)
    in { parse; print }

  let pair pp1 pp2 =
    let pp1 = protect_value pp1 in
    let pp2 = protect_value pp2 in
    let parse = function
      | List (_,[a; b]) -> (pp1.parse a, pp2.parse b)
      | x ->
        bad_format ~pos:(value_pos x) "Expected a pair"
    and print (a, b) = List (pos_null, [pp1.print a; pp2.print b])
    in { parse; print }

  let singleton =
    let parse = function
      | List (_, [a]) -> a
      | x -> bad_format ~pos:(value_pos x) "Expected a singleton"
    and print x = List (pos_null, [x])
    in { parse; print }

  let check f msg = {
    parse = (fun x -> if not (f x) then bad_format "%s" msg else x);
    print = (fun x -> assert (f x); x);
  }
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
    = fun set get pp -> {
        parse = (function acc, Some s -> set acc (pp.parse s) | acc, None -> acc);
        print = (fun s -> s, OpamStd.Option.map pp.print (get s));
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
    and print x = x
    in { parse; print }

  let fields ~empty ppas =
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
    and print acc =
      OpamStd.List.filter_map
        OpamStd.Option.Op.(fun (field,ppa) ->
            snd (ppa.print acc) >>| fun value ->
            Variable (pos_null, field, value))
        ppas
    in { parse; print }

  let opam_version
      ?(v=OpamVersion.current_nopatch)
      ?(f=fun v -> OpamVersion.(compare current_nopatch (nopatch v) >= 0))
      ()
    =
    let parse items =
      let found, items =
        List.fold_left (fun (found, acc) -> function
            | Variable (pos, "opam-version", v) ->
              let v = OpamVersion.of_string (string.parse v) in
              if OpamFormatConfig.(!r.skip_version_checks) || f v then true, acc
              else
                bad_format ~pos "Unsupported opam file format version %S"
                  (OpamVersion.to_string v)
            | x -> found,x::acc)
          (false,[]) items
      in
      if not found then (
        let pos = match items with i::_ -> item_pos i | [] -> pos_null in
        log "Error: missing 'opam-version:' field at %s" (string_of_pos pos);
        if OpamFormatConfig.(!r.strict) then
          bad_format ~pos "Missing 'opam-version:' field";
      );
      items
    and print items =
      let v = string.print OpamVersion.(to_string v) in
      Variable (pos_null, "opam-version", v) :: items
    in
    { parse; print }

  let address = string ++ pp address_of_string string_of_address

  let url =
    string ++ {
      parse = OpamTypesBase.(address_of_string @> parse_url);
      print = fun (addr,kind) -> OpamTypesBase.string_of_address ~kind addr;
    }

end

include Pp

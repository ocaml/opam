(* trivial cppo replacement with just enough features to do conditional compilation *)

let if_ocaml_version =
  Re.(compile @@ seq [
      bol; rep blank;
      str "#if"; rep1 space;
      str "OCAML_VERSION"; rep1 space;
      str ">="; rep1 space;
      char '(';
      group (rep1 digit);
      rep1 (set ", ");
      group (rep1 digit);
      rep1 (set ", ");
      group (rep1 digit);
      char ')'
    ])

let version_extract =
  Re.(compile @@ seq [
      group (rep1 digit);
      char '.';
      group (rep1 digit);
      char '.';
      group (rep1 digit)
    ])

let current_version =
  Scanf.sscanf Sys.ocaml_version "%u.%u.%u" (fun a b c -> (a, b, c))

let greater_or_equal (v : (int * int * int)) = current_version >= v

module State : sig
  type t

  val empty : t
  val is_empty : t -> bool
  val flip_top : t -> t
  val should_output : t -> bool
  val pop : t -> t
  val push : bool -> t -> t
end = struct
  type state = {
    state : bool;
    was_flipped : bool;
  }
  type t = state list

  let empty = [{state = true; was_flipped = true}]
  let is_empty (x : t) = x = empty

  let flip_top = function
    | [] -> failwith "Output stack empty, invalid state"
    | {was_flipped = true; _}::_ -> failwith "#else already used"
    | x::xs -> {state = not x.state; was_flipped = true}::xs

  let should_output l = (List.hd l).state

  let pop = List.tl

  let push state l = {state; was_flipped = false} :: l
end

let is_if_statement s =
  String.length s >= 3 &&
  String.equal (String.sub s 0 3) "#if"

let rec loop ic ~lineno vars =
  match input_line ic with
  | line ->
    let next = loop ic ~lineno:(Int.succ lineno) in
    (match String.trim line with
     | "#else" -> next (State.flip_top vars)
     | "#endif" -> next (State.pop vars)
     | trimmed_line when is_if_statement trimmed_line ->
       (match Re.exec_opt if_ocaml_version line with
        | None ->
          failwith
            (Printf.sprintf "Parsing #if in line %d failed, exiting" lineno)
        | Some groups ->
          let group = Re.Group.get_opt groups in
          (match group 1, group 2, group 3 with
           | Some major, Some minor, Some patch ->
             let major = int_of_string major in
             let minor = int_of_string minor in
             let patch = int_of_string patch in
             next (State.push (greater_or_equal (major, minor, patch)) vars)
           | _ ->
             failwith
               (Printf.sprintf "Parsing #if in line %d failed, exiting" lineno)))
     | _trimmed_line ->
       if State.should_output vars then print_endline line;
       next vars)
  | exception End_of_file ->
    if not (State.is_empty vars) then
      failwith "Output stack messed up, missing #endif?"

let () =
  loop stdin ~lineno:1 State.empty

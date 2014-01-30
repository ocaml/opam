(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

module LowLevel = struct

open Genlex

type option_value =
    Module of option_module
  | StringValue of string
  | IntValue of int
  | FloatValue of float
  | List of option_value list
  | SmallList of option_value list
  | OnceValue of option_value
  | DelayedValue of (Buffer.t -> string -> unit)
and  option_module = (string * option_value) list

exception SideEffectOption
exception OptionNotFound

type 'a option_class =
  { class_name : string;
    from_value : option_value -> 'a;
    to_value : 'a -> option_value;
    mutable class_hooks : ('a config_option -> unit) list;
    mutable string_wrappers : (('a -> string) * (string -> 'a)) option;
  }

and 'a config_option =
  {
    option_class : 'a option_class;
    mutable option_value : 'a;
    option_name : string list;
    option_short_help : string option;
    option_long_help : string list;
    option_default : 'a;
    mutable option_hooks : (unit -> unit) list;
    option_section : config_section;

    (* level indicates the expertise needed to see this option. 0
       means that anybody should see this option, while higher level are
       only for experts. *)
    option_level : int;
    (* volatile options are options that are read once at the beginning of
       the program, but are regenerated before saving them. Thus, modifying
       them after startup is useless. *)
    mutable option_volatile : bool;
  }

and config_file =
    { mutable file_name : File.t;
      mutable file_sections : config_section list;
      mutable file_rc : option_module;
      mutable file_pruned : bool;
      file_header_section : config_section;

      mutable file_before_save_hooks : (int * (unit -> unit)) list;
      mutable file_after_save_hooks : (unit -> unit) list;
      mutable file_after_load_hooks : (unit -> unit) list;
    }

and config_section = {
  section_name : string list;
  section_help : string;
  section_file : config_file;
  mutable section_options : Obj.t config_option list;
}


let create_config_section file section_name section_help =
  let s = {
    section_name = section_name;
    section_help = section_help;
    section_file = file;
    section_options = [];
  }
  in
  file.file_sections <- file.file_sections @ [s];
  s

let create_config_file name =
  let rec file =
    {
      file_name = name;
      file_header_section = header_section;
      file_sections = [ header_section ];
      file_rc = [];
      file_pruned = false;

      file_before_save_hooks = [];
      file_after_save_hooks = [];
      file_after_load_hooks = [];
    }

  and header_section = {
    section_name = [ "Header" ];
    section_help = "These options must be read first";
    section_file = file;
    section_options = [];
  }

  in
  file

let set_config_file opfile name = opfile.file_name <- name
let print_options_not_found = ref false

let define_option_class
    (class_name : string) (from_value : option_value -> 'a)
    (to_value : 'a -> option_value) =
  let c =
    {class_name = class_name; from_value = from_value; to_value = to_value;
     class_hooks = []; string_wrappers = None;}
  in
  c


let rec find_value list m =
  match list with
      [] -> raise Not_found
    | name :: tail ->
      let m = List.assoc name m in
      match m, tail with
          _, [] -> m
        | Module m, _ :: _ -> find_value tail m
        | _ -> raise Not_found

let find_value list m =
  try find_value list m with
      _ -> raise OptionNotFound

let prune_file file = file.file_pruned <- true

let create_section_option
    section (option_name : string list)
    ?short_help
    (long_help : string list) ?(level = 0)
    (option_class : 'a option_class)
    (default_value : 'a) =
  let o =
    { option_name = option_name;
      option_short_help = short_help;
      option_long_help = long_help;
      option_class = option_class;
      option_value = default_value;
      option_default = default_value;
      option_hooks = [];
      option_section = section;
      option_level = level;
      option_volatile = false;}
  in
  section.section_options <-
    section.section_options @ [ (Obj.magic o : Obj.t config_option) ];
  o.option_value <-
    begin try
            o.option_class.from_value (
              find_value option_name section.section_file.file_rc)
      with
          OptionNotFound -> default_value
        | e ->
          Printf.fprintf stderr "Options.define_option, for option %s: "
            (match option_name with
                [] -> "???"
              | name :: _ -> name);
          Printf.fprintf stderr "%s\n" (Printexc.to_string e);
          default_value
    end;
  o

let create_option config_file option_names
    ?short_help long_help ?level
    option_class default_value =
  create_section_option config_file.file_header_section
    option_names ?short_help long_help ?level
    option_class default_value

let once_values = Hashtbl.create 13
let once_values_counter = ref 0
let once_values_rev = Hashtbl.create 13

let lexer = make_lexer ["="; "{"; "}"; "["; "]"; ";"; "("; ")"; ","; "."; "@"]

let rec parse_gwmlrc = parser
  | [< id = parse_id; 'Kwd "="; v = parse_option ;
       eof = parse_gwmlrc >] -> (id, v) :: eof
  | [< >] -> []

and parse_option = parser
  | [< 'Kwd "{"; v = parse_gwmlrc; 'Kwd "}" >] -> Module v
  | [< 'Ident s >] -> StringValue s
  | [< 'String s >] -> StringValue s
  | [< 'Int i >] -> IntValue i
  | [< 'Float f >] -> FloatValue  f
  | [< 'Kwd "@"; 'Int i; v = parse_once_value i >] -> OnceValue v
  | [< 'Char c >] -> StringValue (let s = String.create 1 in s.[0] <- c; s)
  | [< 'Kwd "["; v = parse_list [] >] -> List v
  | [< 'Kwd "("; v = parse_list [] >] -> List v

and parse_once_value i = parser
[< 'Kwd "@" >] ->
  begin
    try Hashtbl.find once_values i with Not_found ->
      Printf.fprintf stderr "Error in saved file: @%d@ not defined\n" i;
      exit 2
  end
  |  [< 'Kwd "="; v = parse_option >] ->
    begin
      Hashtbl.add once_values i v;
      v
    end

and parse_id = parser
[< 'Ident s >] -> s
  |   [< 'String s >] -> s

and parse_list list = parser
[< 'Kwd ";"; strm__ >] -> parse_list (list) strm__
  |   [< 'Kwd "," ; strm__ >] -> parse_list (list) strm__
  |   [< 'Kwd "." ; strm__ >] -> parse_list (list) strm__
  |   [< v = parse_option; strm__ >] -> parse_list (v :: list) strm__
  |   [< 'Kwd "]" >] -> List.rev list
  |   [< 'Kwd ")" >] -> List.rev list


let exec_hooks name list o =
  List.iter
    (fun f ->
      try f o with
          e -> Printf.fprintf stderr "Exception %s in %s hooks"
            (Printexc.to_string e) name
    ) list

let exec_weighted_hooks name list o =
  List.iter
    (fun (_, f) ->
      try f o with
          e -> Printf.fprintf stderr "Exception %s in %s hooks"
            (Printexc.to_string e) name
    ) list

let exec_option_hooks o =
  exec_hooks "option" o.option_hooks ()

let exec_class_hooks o =
  exec_hooks "class" o.option_class.class_hooks o

let really_load filename sections =
  let temp_file = File.add_suffix filename ".tmp" in
  if File.X.exists temp_file then
    begin
      Printf.eprintf "File %s exists\n" (File.to_string temp_file);
      Printf.eprintf "An error may have occurred during previous configuration save.\n";
      Printf.eprintf "Please, check your configurations files, and rename/remove this file\n";
      Printf.eprintf "before restarting\n";
      exit 1
    end
  else
    let ic = File.X.open_in filename in
    try
      let s = Stream.of_channel ic in
      try
        let stream = lexer s in
        Hashtbl.clear once_values;
        let list =
          try parse_gwmlrc stream with
              e ->
                Printf.eprintf "Syntax error while parsing file %s at pos %d:(%s)\n"
                  (File.to_string filename) (Stream.count s) (Printexc.to_string e);
                exit 2
        in
        Hashtbl.clear once_values;
        let affect_option o =
          try
            begin try
                    o.option_value <-
                      o.option_class.from_value (find_value o.option_name list)
              with
                  SideEffectOption -> ()
            end;
            exec_class_hooks o;
            exec_option_hooks o
          with
              SideEffectOption -> ()
            | OptionNotFound ->
              if !print_options_not_found then
                begin
                  Printf.fprintf stderr "Option ";
                  List.iter (fun s -> Printf.fprintf stderr "%s " s) o.option_name;
                  Printf.fprintf stderr "not found in %s\n" (File.to_string filename);
                end
            | e ->
              Printf.fprintf stderr "Exception: %s while handling option:"
                (Printexc.to_string e);
              List.iter (fun s -> Printf.fprintf stderr "%s " s) o.option_name;
              Printf.fprintf stderr "\n";
              Printf.fprintf stderr "  in %s\n" (File.to_string filename);
              Printf.fprintf stderr "Aborting\n.";
              exit 2
        in

      (* The options are affected by sections, from the first defined one to
         the last defined one ("defined" in the order of the program execution).
         Don't change this. *)
      List.iter (fun s ->
          List.iter affect_option s.section_options) sections;
        close_in ic;
        list
      with
        e ->
        Printf.fprintf stderr "Error %s in %s\n" (Printexc.to_string e) (File.to_string filename);
          []
    with
      e -> close_in ic; raise e


let exit_exn = Exit


let unsafe_get = String.unsafe_get
external is_printable : char -> bool = "caml_is_printable"
let unsafe_set = String.unsafe_set

let escaped s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do
    n :=
      !n +
        (match unsafe_get s i with
           '"' | '\\' -> 2
         | '\n' | '\t' -> 1
         | c -> if is_printable c then 1 else 4)
  done;
  if !n = String.length s then s
  else
    let s' = String.create !n in
    n := 0;
    for i = 0 to String.length s - 1 do
      begin match unsafe_get s i with
        '"' | '\\' as c -> unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
      | '\n' | '\t' as c -> unsafe_set s' !n c
      | c ->
          if is_printable c then unsafe_set s' !n c
          else
            let a = int_of_char c in
            unsafe_set s' !n '\\';
            incr n;
            unsafe_set s' !n (char_of_int (48 + a / 100));
            incr n;
            unsafe_set s' !n (char_of_int (48 + a / 10 mod 10));
            incr n;
            unsafe_set s' !n (char_of_int (48 + a mod 10))
      end;
      incr n
    done;
    s'

let safe_string s =
  if s = "" then "\"\""
  else
    try
      match s.[0] with
        'a'..'z' | 'A'..'Z' ->
          for i = 1 to String.length s - 1 do
            match s.[i] with
              'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> ()
            | _ -> raise exit_exn
          done;
          s
      | _ ->
          if Int64.to_string (Int64.of_string s) = s ||
             string_of_float (float_of_string s) = s
          then
            s
          else raise exit_exn
    with
      _ -> Printf.sprintf "\"%s\"" (escaped s)

let with_help = ref false

let comment s =
  Printf.sprintf "(* %s *)"
    (OcpString.replace_chars s  ['\n', " *)\n(* "])

let compact_string oc f =
  let b = Buffer.create 100 in
  f b;
  let s = Buffer.contents b in
  let b = Buffer.create 100 in
  let rec iter b i len s =
    if i < len then
      let c = s.[i] in
      if c = ' ' || c = '\n' || c = '\t' then begin
          iter_space b (i+1) len s
        end
      else begin
          Buffer.add_char b c;
          iter b (i+1) len s
        end
  and  iter_space b i len s =
    if i < len then
      let c = s.[i] in
      if c = ' ' || c = '\n' || c = '\t' then begin
          iter_space b (i+1) len s
        end
      else begin
          Buffer.add_char b ' ';
          Buffer.add_char b c;
          iter b (i+1) len s
        end
  in
  iter b 0 (String.length s) s;
  let ss = Buffer.contents b in
  Buffer.add_string oc (if String.length ss < 80 then ss else s)

let rec save_module indent oc list =
  let subm = ref [] in
  List.iter
    (fun (name, help, value) ->
       match name with
         [] -> assert false
       | [name] ->
           if !with_help && help <> "" then
             Printf.bprintf oc "\n%s\n" (comment help);
           Printf.bprintf oc "%s%s = " indent (safe_string name);
           save_value indent oc value;
           Printf.bprintf oc "\n"
       | m :: tail ->
           let p =
             try List.assoc m !subm with
               e -> let p = ref [] in subm := (m, p) :: !subm; p
           in
           p := (tail, help, value) :: !p)
    list;
  List.iter
    (fun (m, p) ->
       Printf.bprintf oc "%s%s = {\n" indent (safe_string m);
       save_module (indent ^ "  ") oc !p;
       Printf.bprintf oc "%s}\n" indent)
    !subm
and save_list indent oc list =
  match list with
    [] -> ()
  | [v] -> save_value indent oc v
  | v :: tail ->
      save_value indent oc v; Printf.bprintf oc ", "; save_list indent oc tail
and save_list_nl indent oc list =
  match list with
    [] -> ()
  | [v] -> Printf.bprintf oc "\n%s" indent; save_value indent oc v
  | v :: tail ->
      Printf.bprintf oc "\n%s" indent;
      save_value indent oc v;
      Printf.bprintf oc ";";
      save_list_nl indent oc tail
and save_value indent oc v =
  match v with
    StringValue s -> Printf.bprintf oc "%s" (safe_string s)
  | IntValue i -> Printf.bprintf oc "%d" i
  | FloatValue f -> Printf.bprintf oc "%F" f
  | List l ->
      compact_string oc (fun oc ->
          Printf.bprintf oc "[";
          save_list_nl (indent ^ "  ") oc l;
          Printf.bprintf oc "\n%s]" indent)
  | DelayedValue f -> f oc indent
  | SmallList l ->
      Printf.bprintf oc "(";
      save_list (indent ^ "  ") oc l;
      Printf.bprintf oc ")"
  | Module m ->
      compact_string oc (fun oc ->
          Printf.bprintf oc "{";
          save_module_fields (indent ^ "  ") oc m;
          Printf.bprintf oc "%s}" indent)
  | OnceValue v ->
      try
        let i = Hashtbl.find once_values_rev v in Printf.bprintf oc "@%Ld@" i
      with
        Not_found ->
          incr once_values_counter;
          let i = Int64.of_int !once_values_counter in
          Hashtbl.add once_values_rev v i;
          Printf.bprintf oc "@%Ld = " i;
          save_value indent oc v
and save_module_fields indent oc m =
  match m with
    [] -> ()
  | (name, v) :: tail ->
      Printf.bprintf oc "%s%s = " indent (safe_string name);
      save_value indent oc v;
      Printf.bprintf oc "\n";
      save_module_fields indent oc tail

let config_file f = f.file_name

let load opfile =
  try
    opfile.file_rc <-
      really_load opfile.file_name opfile.file_sections;
    exec_hooks "after_load" opfile.file_after_load_hooks ();
  with
    Not_found | Sys_error _ ->
      (* Printf.fprintf stderr "No %s found\n" opfile.file_name *) ()

let append opfile filename =
  try
    opfile.file_rc <-
      really_load filename opfile.file_sections @
        opfile.file_rc
  with
    Not_found -> Printf.fprintf stderr "No %s found\n" (File.to_string filename)

let ( !! ) o = o.option_value
let ( =:= ) o v = o.option_value <- v; exec_class_hooks o; exec_option_hooks o

let rec value_to_string v =
  match v with
    StringValue s -> s
  | IntValue i -> string_of_int i
  | FloatValue f -> string_of_float f
  | OnceValue v -> value_to_string v
  | _ -> failwith "Not a string option"

let safe_value_to_string v =
  match v with
    StringValue s -> s
  | IntValue i -> string_of_int i
  | FloatValue f -> string_of_float f
  | OnceValue v -> value_to_string v
  | _ -> "NaS"

let string_to_value s = StringValue s

let rec value_to_int64 v =
  match v with
    StringValue s -> Int64.of_string s
  | IntValue i -> Int64.of_int i
  | FloatValue i -> Int64.of_float i
  | OnceValue v -> value_to_int64 v
  | _ -> failwith "Options: not an int option"

let rec value_to_int v =
  match v with
    StringValue s -> int_of_string s
  | IntValue i -> i
  | FloatValue i -> int_of_float i
  | OnceValue v -> value_to_int v
  | _ -> failwith "Options: not an int option"

let int_to_value i = IntValue  i
let int64_to_value i = IntValue (Int64.to_int i)

(* The Pervasives version is too restrictive *)
let bool_of_string s =
  match String.lowercase s with
    "true" -> true
  | "false" -> false
  | "yes" -> true
  | "no" -> false
  | "y" -> true
  | "n" -> false
  | _ -> invalid_arg "bool_of_string"

let rec value_to_bool v =
  match v with
    StringValue s -> bool_of_string s
  | IntValue 0 -> false
  | IntValue 1 -> true
  | OnceValue v -> value_to_bool v
  | _ -> failwith "Options: not a bool option"
let bool_to_value i = StringValue (string_of_bool i)

let rec value_to_float v =
  match v with
    StringValue s -> float_of_string s
  | FloatValue f -> f
  | OnceValue v -> value_to_float v
  | _ -> failwith "Options: not a float option"

let float_to_value i = FloatValue i

let rec value_to_string2 v =
  match v with
    List [s1; s2] | SmallList [s1; s2] ->
      value_to_string s1, value_to_string s2
  | OnceValue v -> value_to_string2 v
  | _ -> failwith "Options: not a string2 option"

let string2_to_value (s1, s2) = SmallList [StringValue s1; StringValue s2]

let rec value_to_list v2c v =
  match v with
    List l | SmallList l -> List.rev (List.rev_map v2c l)
  | OnceValue v -> value_to_list v2c v
  | _ -> [v2c v]
(*
      | StringValue s -> [v2c v]
  | FloatValue _ -> failwith "Options: not a list option (FloatValue)"
  | IntValue _ -> failwith "Options: not a list option (IntValue)"
  | Module _ -> failwith "Options: not a list option (Module)"
  | DelayedValue _ -> failwith "Options: not a list option (Delayed)"
*)

let rec value_to_hasharray v2c v =
  match v with
    List l ->
      let hash = Array.init 256 (fun _ -> Hashtbl.create 10) in
      List.iter
        (fun a ->
           let (num, md4, peer) = v2c a in Hashtbl.add hash.(num) md4 peer)
        (List.rev l);
      hash
  | OnceValue v -> value_to_hasharray v2c v
  | _ -> failwith (Printf.sprintf "Options: not a list option for list2")

let rec value_to_safelist v2c v =
  match v with
    List l | SmallList l ->
      let rec iter list left =
        match left with
          [] -> list
        | x :: tail ->
            let list =
              try v2c x :: list with
                _ -> list
            in
            iter list tail
      in
      List.rev (iter [] (List.rev l))
  | OnceValue v -> value_to_safelist v2c v
  | StringValue s ->
      failwith
        (Printf.sprintf "Options: not a list option (StringValue [%s])" s)
  | FloatValue _ -> failwith "Options: not a list option (FloatValue)"
  | IntValue _ -> failwith "Options: not a list option (IntValue)"
  | Module _ -> failwith "Options: not a list option (Module)"
  | DelayedValue _ -> failwith "Options: not a list option (Delayed)"

let rec value_to_intmap f v2c v =
  match v with
    List l | SmallList l ->
      let rec iter map left =
        match left with
          [] -> map
        | x :: tail ->
            let map =
              try let v = v2c x in let num = f v in IntMap.add num v map with
                _ -> map
            in
            iter map tail
      in
      iter IntMap.empty l
  | OnceValue v -> value_to_intmap f v2c v
  | StringValue s ->
      failwith
        (Printf.sprintf "Options: not a list option (StringValue [%s])" s)
  | FloatValue _ -> failwith "Options: not a list option (FloatValue)"
  | IntValue _ -> failwith "Options: not a list option (IntValue)"
  | Module _ -> failwith "Options: not a list option (Module)"
  | DelayedValue _ -> failwith "Options: not a list option (Delayed)"

let rec value_to_listiter v2c v =
  match v with
    List l | SmallList l ->
      List.iter
        (fun v ->
           try ignore (v2c v) with
             SideEffectOption -> ())
        l;
      raise SideEffectOption
  | OnceValue v -> value_to_listiter v2c v
  | StringValue s ->
      failwith
        (Printf.sprintf "Options: not a list option (StringValue [%s])" s)
  | FloatValue _ -> failwith "Options: not a list option (FloatValue)"
  | IntValue _ -> failwith "Options: not a list option (IntValue)"
  | Module _ -> failwith "Options: not a list option (Module)"
  | DelayedValue _ -> failwith "Options: not a list option (Delayed)"

let rec convert_list c2v l res =
  match l with
    [] -> List.rev res
  | v :: list ->
      match
        try Some (c2v v) with
          e ->
            Printf.fprintf stderr "Exception %s in Options.convert_list\n"
              (Printexc.to_string e);
            None
      with
        None -> convert_list c2v list res
      | Some v -> convert_list c2v list (v :: res)

let option_to_value c2v o =
  match o with
    None -> StringValue "None"
  | Some c -> c2v c

let rec value_to_option v2c v =
  match v with
    | StringValue ""
    | StringValue "None" -> None
  | OnceValue v -> value_to_option v2c v
  | _ -> Some (v2c v)

let save_delayed_list_value oc indent c2v =
  let indent = indent ^ "  " in
  fun v ->
    try
      let v = c2v v in
      Printf.bprintf oc "\n%s" indent;
      save_value indent oc v;
      Printf.bprintf oc ";"
    with _ -> ()

let list_to_value c2v l =
  DelayedValue
    (fun oc indent ->
       Printf.bprintf oc "[";
       List.iter (save_delayed_list_value oc indent c2v) l;
       Printf.bprintf oc "\n%s]" indent)

let intmap_to_value name c2v map =
  DelayedValue
    (fun oc indent ->
       let save = save_delayed_list_value oc indent c2v in
       Printf.bprintf oc "[";
       IntMap.iter (fun _ v -> save v) map;
       Printf.bprintf oc "\n%s]" indent)

let hasharray_to_value x c2v l =
  DelayedValue
    (fun oc indent ->
       Printf.bprintf oc "[";
       let save = save_delayed_list_value oc indent c2v in
       for i = 0 to Array.length l - 1 do
         Hashtbl.iter (fun a b -> save (0, x, b)) l.(i)
       done;
       Printf.bprintf oc "\n%s]" indent)

let smalllist_to_value c2v l = SmallList (convert_list c2v l [])

(*
let value_to_path v =
  List.map File.of_string
    (let rec iter v =
       match v with
         StringValue s -> Filepath.string_to_colonpath s
       | OnceValue v -> iter v
       | List l ->
           List.map
             (fun v ->
                match v with
                  StringValue s -> File.of_string s
                | _ -> failwith "Options: not a path option")
             l
       | _ -> failwith "Options: not path bool option"
     in
     iter v)

let path_to_value list =
  StringValue (Filepath.colonpath_to_string (List.map File.to_string list))
  *)

let string_option =
  define_option_class "String" value_to_string string_to_value
let color_option = define_option_class "Color" value_to_string string_to_value
let font_option = define_option_class "Font" value_to_string string_to_value

let int_option = define_option_class "Int" value_to_int int_to_value
let int64_option = define_option_class "Int64" value_to_int64 int64_to_value


let bool_option = define_option_class "Bool" value_to_bool bool_to_value
let float_option = define_option_class "Float" value_to_float float_to_value
(*let path_option = define_option_class "Path" value_to_path path_to_value *)

let string2_option =
  define_option_class "String2" value_to_string2 string2_to_value

let option_option cl =
  define_option_class (cl.class_name ^ " Option")
    (value_to_option cl.from_value) (option_to_value cl.to_value)

let list_option cl =
  define_option_class (cl.class_name ^ " List") (value_to_list cl.from_value)
    (list_to_value cl.to_value)

let value_to_array from_value a =
  Array.of_list (value_to_list from_value a)
let array_to_value to_value v =
  list_to_value to_value (Array.to_list v)

let array_option cl =
  define_option_class (cl.class_name ^ " Array")
    (fun v -> Array.of_list (value_to_list cl.from_value v))
    (fun v -> list_to_value cl.to_value (Array.to_list v))

let hasharray_option x cl =
  define_option_class "Hashtable array" (value_to_hasharray cl.from_value)
    (hasharray_to_value x cl.to_value)

let safelist_option cl =
  define_option_class (cl.class_name ^ " List")
    (value_to_safelist cl.from_value)
    (list_to_value cl.to_value)

let intmap_option f cl =
  define_option_class (cl.class_name ^ " IntMap")
    (value_to_intmap f cl.from_value)
    (intmap_to_value cl.class_name cl.to_value)

let listiter_option cl =
  define_option_class (cl.class_name ^ " List")
    (value_to_listiter cl.from_value)
    (list_to_value cl.to_value)

let smalllist_option cl =
  define_option_class (cl.class_name ^ " List") (value_to_list cl.from_value)
    (smalllist_to_value cl.to_value)

let to_value cl = cl.to_value
let from_value cl = cl.from_value

let rec value_to_sum l v =
  match v with
    StringValue s -> List.assoc s l
  | OnceValue v -> value_to_sum l v
  | _ -> failwith "Options: not a sum option"

let sum_to_value l v = StringValue (List.assq v l)

let sum_option l =
  let ll = List.map (fun (a1, a2) -> a2, a1) l in
  define_option_class "Sum" (value_to_sum l) (sum_to_value ll)

let option_to_value o =
  o.option_name,
  String.concat "\n" o.option_long_help,
  (try o.option_class.to_value o.option_value with
     e ->
       Printf.fprintf stderr "Error while saving option \"%s\": %s\n"
         (try List.hd o.option_name with
            _ -> "???")
         (Printexc.to_string e);
       StringValue "")

let string_of_string_list list =
  let rec iter s list =
    match list with
      [] -> s
    | ss :: tail ->
        iter (Printf.sprintf "%s.%s" s ss) tail
  in
  match list with
    [] -> ""
  | s :: tail -> iter s tail

let title_opfile = ref true;;

let save opfile =
  exec_weighted_hooks "before_save" opfile.file_before_save_hooks ();
  let filename = opfile.file_name in
  let temp_file = File.add_suffix filename ".tmp" in
  let old_file =
    let old_file = File.add_suffix filename ".old" in
    let old_old_file = File.add_suffix old_file ".old" in
    if File.X.exists old_old_file then Sys.remove (File.to_string old_old_file);
    if File.X.exists old_file then
      Sys.rename (File.to_string old_file) (File.to_string old_old_file);
    old_file in
  let oc = Buffer.create 1000 in
  (*  let oc = open_out temp_file in *)
  try
    once_values_counter := 0;
    title_opfile := true;
    Hashtbl.clear once_values_rev;
    let advanced = ref false in
    List.iter (fun s ->
      let options =
        List.filter (fun o ->
          if o.option_level > 0 then advanced := true;
          o.option_level = 0) s.section_options in
      if options <> [] then begin
        if s.section_name <> [] then begin
          Printf.bprintf oc "\n\n";
          Printf.bprintf oc "(*************************************)\n";
          if !title_opfile then begin
            Printf.bprintf oc "(*   Never edit options files while  *)\n";
            Printf.bprintf oc "(*       the program is running      *)\n";
            Printf.bprintf oc "(*************************************)\n";
            title_opfile := false;
          end;
          Printf.bprintf oc "(* SECTION : %-23s *)\n" (string_of_string_list s.section_name);
          Printf.bprintf oc "(* %-33s *)\n" s.section_help;
          Printf.bprintf oc "(*************************************)\n";
          Printf.bprintf oc "\n\n";
        end;
        save_module "" oc (List.map option_to_value options)
      end
    ) opfile.file_sections;
    if !advanced then begin
      Printf.bprintf oc "\n\n\n";
      Printf.bprintf oc "(*****************************************************************)\n";
      Printf.bprintf oc "(*                                                               *)\n";
      Printf.bprintf oc "(*                       ADVANCED OPTIONS                        *)\n";
      Printf.bprintf oc "(*                                                               *)\n";
      Printf.bprintf oc "(*        All the options after this line are for the expert     *)\n";
      Printf.bprintf oc "(*        user. Do not modify them if you are not   sure.        *)\n";
      Printf.bprintf oc "(*                                                               *)\n";
      Printf.bprintf oc "(*****************************************************************)\n";
      Printf.bprintf oc "\n\n\n";
      List.iter (fun s ->
        let options = List.filter (fun o -> o.option_level > 0)
          s.section_options in
        if options = [] then () else let _ = () in
                                     Printf.bprintf oc "\n\n";
                                     Printf.bprintf oc "(*************************************)\n";

                                     Printf.bprintf oc "(* SECTION : %-11s FOR EXPERTS *)\n" (string_of_string_list s.section_name);
                                     Printf.bprintf oc "(* %-33s *)\n" s.section_help;
                                     Printf.bprintf oc "(*************************************)\n";
                                     Printf.bprintf oc "\n\n";
                                     save_module "" oc (List.map option_to_value options)
      ) opfile.file_sections;
    end;
    if not opfile.file_pruned then
      begin
        let rem = ref [] in
        List.iter
          (fun (name, value) ->
            try
              List.iter
                (fun s ->
                  List.iter
                    (fun o ->
                      match o.option_name with
                          n :: _ -> if n = name then raise Exit
                        | _ -> ())
                    s.section_options)
                opfile.file_sections;
              rem := (name, value) :: !rem;
            with
                Exit -> ()
              | e ->
                Printf.fprintf stderr "Exception %s in Options.save\n"
                  (Printexc.to_string e);
          )
          opfile.file_rc;
        if !rem <> [] then begin
          Printf.bprintf oc "\n(*\n The following options are not used (errors, obsolete, ...) \n*)\n";
          List.iter (fun (name, value) ->
            Printf.bprintf oc "%s = " (safe_string name);
            save_value "  " oc value;
            Printf.bprintf oc "\n"
          ) !rem;
        end;
        opfile.file_rc <- !rem
      end;
    Hashtbl.clear once_values_rev;
    File.X.write_of_string temp_file (Buffer.contents oc);
    begin try File.X.rename filename old_file with  _ -> () end;
    begin try File.X.rename temp_file filename with _ -> () end;
    exec_hooks "after_save" opfile.file_after_save_hooks ();
  with
      e ->
        File.X.write_of_string temp_file (Buffer.contents oc);
        exec_hooks "after_save" opfile.file_after_save_hooks ();
        raise e

let save_with_help opfile =
  with_help := true;
  begin try save opfile with
    _ -> ()
  end;
  with_help := false

let option_hook option f = option.option_hooks <- f :: option.option_hooks
let add_option_hook option f = option.option_hooks <- f :: option.option_hooks
let clear_option_hooks option = option.option_hooks <- []

let class_hook option_class f =
  option_class.class_hooks <- f :: option_class.class_hooks

let rec iter_order f list =
  match list with
    [] -> ()
  | v :: tail -> f v; iter_order f tail

let help oc opfile =
  List.iter (fun s ->
      List.iter
        (fun o ->
          Printf.bprintf oc "OPTION \"";
          begin match o.option_name with
              [] -> Printf.bprintf oc "???"
            | [name] -> Printf.bprintf oc "%s" name
            | name :: tail ->
                Printf.bprintf oc "%s" name;
                iter_order (fun name -> Printf.bprintf oc ":%s" name) o.option_name
          end;
          Printf.bprintf oc "\" (TYPE \"%s\"): %s\n   CURRENT: \n"
            o.option_class.class_name
            (String.concat "\n" o.option_long_help);
          begin try
              once_values_counter := 0;
              Hashtbl.clear once_values_rev;
              save_value "" oc (o.option_class.to_value o.option_value)
            with
              _ -> ()
          end;
          Printf.bprintf oc "\n")
      s.section_options;
  ) opfile.file_sections


let tuple2_to_value (c1, c2) (a1, a2) =
  SmallList [to_value c1 a1; to_value c2 a2]

let rec value_to_tuple2 (c1, c2 as cs) v =
  match v with
    List [v1; v2] -> from_value c1 v1, from_value c2 v2
  | SmallList [v1; v2] -> from_value c1 v1, from_value c2 v2
  | OnceValue v -> value_to_tuple2 cs v
  | List l | SmallList l ->
      Printf.fprintf stderr "list of %d\n" (List.length l);
      failwith "Options: not a tuple2 list option"
  | _ -> failwith "Options: not a tuple2 option"

let tuple2_option p =
  define_option_class "tuple2_option" (value_to_tuple2 p) (tuple2_to_value p)

let tuple3_to_value (c1, c2, c3) (a1, a2, a3) =
  SmallList [to_value c1 a1; to_value c2 a2; to_value c3 a3]
let rec value_to_tuple3 (c1, c2, c3 as cs) v =
  match v with
    List [v1; v2; v3] -> from_value c1 v1, from_value c2 v2, from_value c3 v3
  | SmallList [v1; v2; v3] ->
      from_value c1 v1, from_value c2 v2, from_value c3 v3
  | OnceValue v -> value_to_tuple3 cs v
  | _ -> failwith "Options: not a tuple3 option"

let tuple3_option p =
  define_option_class "tuple3_option" (value_to_tuple3 p) (tuple3_to_value p)

let tuple4_to_value (c1, c2, c3, c4) (a1, a2, a3, a4) =
  SmallList [to_value c1 a1; to_value c2 a2; to_value c3 a3; to_value c4 a4]
let rec value_to_tuple4 (c1, c2, c3, c4 as cs) v =
  match v with
    List [v1; v2; v3; v4] | SmallList [v1; v2; v3; v4] ->
      from_value c1 v1, from_value c2 v2, from_value c3 v3, from_value c4 v4
  | OnceValue v -> value_to_tuple4 cs v
  | _ -> failwith "Options: not a tuple4 option"

let tuple4_option p =
  define_option_class "tuple4_option" (value_to_tuple4 p) (tuple4_to_value p)

let tuple5_to_value (c1, c2, c3, c4, c5) (a1, a2, a3, a4, a5) =
  SmallList [
    to_value c1 a1;
    to_value c2 a2;
    to_value c3 a3;
    to_value c4 a4;
    to_value c5 a5;
    ]

let rec value_to_tuple5 ( (c1, c2, c3, c4, c5) as cs) v =
  match v with
  | List [v1; v2; v3; v4; v5]
  | SmallList [v1; v2; v3; v4; v5] ->
      from_value c1 v1,
      from_value c2 v2,
      from_value c3 v3,
      from_value c4 v4,
      from_value c5 v5
  | OnceValue v -> value_to_tuple5 cs v
  | _ -> failwith "Options: not a tuple5 option"

let tuple5_option p =
  define_option_class "tuple5_option" (value_to_tuple5 p) (tuple5_to_value p)


let value_to_filename v =
  File.of_string
    (match v with
       StringValue s -> s
     | _ -> failwith "Options: not a filename option")

let filename_to_value v = StringValue (File.to_string v)

let file_option =
  define_option_class "Filename" value_to_filename filename_to_value

let shortname o = String.concat ":" o.option_name
let get_class o = o.option_class
let get_help o =
  let help = o.option_long_help in
  if help = [] then
    match o.option_short_help with
        None -> "No Help Available"
      | Some help -> help
  else String.concat "\n" help

let advanced o = o.option_level > 0

(*
let simple_options opfile =
  let list = ref [] in
  List.iter (fun s ->
      List.iter
        (fun o ->
          match o.option_name with
            [] | _ :: _ :: _ -> ()
          | [name] ->
              match o.option_class.to_value o.option_value with
                Module _ | SmallList _ | List _ | DelayedValue _ ->
                  begin match o.option_class.string_wrappers with
                      None -> ()
                    | Some (to_string, from_string) ->
                        list := (name, to_string o.option_value) :: !list
                  end
              | v -> list := (name, safe_value_to_string v) :: !list)
      s.section_options)
  opfile.file_sections;
  !list
*)


let get_option opfile name =
(*  Printf.fprintf stderr "get_option [%s]\n" name;*)
  let rec iter name list sections =
    match list with
    | o :: list -> if o.option_name = name then o else
          iter name list sections
    | [] ->
        match sections with
          [] ->
            prerr_endline
              (Printf.sprintf "option [%s] not_found in %s"
                (String.concat ";" name) (File.to_string opfile.file_name));
            raise Not_found
        | s :: tail ->
            iter name s.section_options tail
  in
  iter [name] [] opfile.file_sections


let set_simple_option opfile name v =
  let o = get_option opfile name in
  begin match o.option_class.string_wrappers with
    None -> o.option_value <- o.option_class.from_value (string_to_value v)
  | Some (_, from_string) -> o.option_value <- from_string v
  end;
  exec_class_hooks o;
  exec_option_hooks o

let get_simple_option opfile name =
  let o = get_option opfile name in
  match o.option_class.string_wrappers with
    None -> safe_value_to_string (o.option_class.to_value o.option_value)
  | Some (to_string, _) -> to_string o.option_value

let set_option_hook opfile name hook =
  let o = get_option opfile name in o.option_hooks <- hook :: o.option_hooks

let set_string_wrappers o to_string from_string =
  o.string_wrappers <- Some (to_string, from_string)

let option_type o = (get_class o).class_name

let once_value v = OnceValue v

  (*
let strings_of_option o =
  match o.option_name with
    [] | _ :: _ :: _ -> failwith "Complex option"
  | [name] ->
      name,
      (match o.option_class.string_wrappers with
         None -> safe_value_to_string (o.option_class.to_value o.option_value)
       | Some (to_string, _) -> to_string o.option_value)
      *)


let restore_default o =
  o =:= o.option_default

(*
let set_option_desc o s =
  o.option_desc <- s
*)


let set_volatile o = o.option_volatile <- true

(*
module SimpleOptions : sig

    val name : 'a config_option -> string
    val help : 'a t -> string

    val iter_sections : (config_section -> unit) -> config_file -> unit
    val iter_options : (Obj.t t -> unit) -> config_section -> unit
    val find : config_file -> string -> 'a t
    val to_string : 'a t -> string
    val of_string : 'a t -> string -> unit

  end = struct

    let name o = String.concat ":" o.option_name
    let help o = o.option_long_help

    let iter_sections f file = List.iter f file.file_sections
    let iter_options f sec = List.iter (fun o ->
          if not o.option_volatile then f o) sec.section_options

    let find file name =
      let rec iter sections options name =
        match options with
          [] ->
            (match sections with
                [] -> raise Not_found
              | s :: tail ->
                  iter tail s.section_options name
            )
        | o :: tail ->
            if not o.option_volatile && o.option_name = name then Obj.magic o else
              iter sections tail name
      in
      iter file.file_sections [] (OcpString.split_simplify name ':')

    let of_string o s =
      let st = lexer (Stream.of_string s) in
      let v = parse_option st in
      o.option_value <- o.option_class.from_value v

    let to_string o =
      let v = o.option_class.to_value o.option_value in
      let b = Buffer.create 100 in
      save_value "" b v;
      Buffer.contents b

  end
*)

(*
module M = struct

    type option_info = {
        option_name : string;
        option_shortname : string;
        option_desc : string;
        option_value : string;
        option_help : string;
        option_advanced : bool;
        option_default : string;
        option_type : string;
      }

  end
*)

let string_of_option_value o v =
  match o.option_class.string_wrappers with
    None ->
      value_to_string (o.option_class.to_value v)
  | Some (to_string, _) -> to_string v

let tuple2_to_value f x =
  let (v1, v2) = f x in
  SmallList [v1; v2]

let value_to_tuple2 f x =
  match value_to_list (fun id -> id) x with
    [v1;v2] -> f (v1, v2)
  | _ -> assert false

(*
let info_of_option prefix o =
  match o.option_name with
    [] | _ :: _ :: _ -> failwith "Complex option"
  | [name] ->
      {
        M.option_name = Printf.sprintf "%s%s" prefix name;
        M.option_shortname = name;
        M.option_short_help = o.option_short_help;
        M.option_value = string_of_option_value o o.option_value;
        M.option_default = string_of_option_value o o.option_default;
        M.option_advanced = o.option_advanced;
        M.option_long_help = o.option_long_help;
        M.option_type = o.option_class.class_name;
      }

let simple_options prefix opfile =
  let list = ref [] in
  List.iter (fun s ->
      List.iter
        (fun o ->
          try list := info_of_option prefix o :: !list  with _ -> ())
      s.section_options)
  opfile.file_sections;
  List.rev !list

let simple_args prefix opfile =
  OcpList.tail_map
    (fun oi ->
       "-" ^ oi.M.option_name,
       Arg.String
         (fun s ->
            Printf.fprintf stderr "Setting option %s\n" oi.M.option_name;
            set_simple_option opfile oi.M.option_shortname s),
       Printf.sprintf "<string> : \t%s (current: %s)"
         oi.M.option_help oi.M.option_value)
    (simple_options prefix opfile)

let prefixed_args prefix file =
  List.map
    (fun (s, f, h) ->
       let s = String.sub s 1 (String.length s - 1) in
       Printf.sprintf "-%s:%s" prefix s, f, h)
    (simple_args "" file)

let strings_of_section_options prefix s =
  let list = ref [] in
  List.iter
  (fun o ->
      try list := info_of_option prefix o :: !list  with _ -> ())
  s.section_options;
  List.rev !list

type option_info = M.option_info = {
    option_name : string;
    option_shortname : string;
    option_desc : string;
    option_value : string;
    option_help : string;
    option_advanced : bool;
    option_default : string;
    option_type : string;
  }

let info_of_option o = info_of_option "" o
*)

let sections file = file.file_sections
let section_name s = string_of_string_list s.section_name

let iter_section f s =
  List.iter f s.section_options

let iter_file f file =
  List.iter (iter_section f) file.file_sections

let set_after_load_hook file f =
  file.file_after_load_hooks <- f :: file.file_after_load_hooks

let set_after_save_hook file f =
  file.file_after_save_hooks <- f :: file.file_after_save_hooks

let set_before_save_hook file weight f =
  file.file_before_save_hooks <-
    List.sort (fun (w1,_) (w2,_) -> compare w1 w2) ((weight,f) :: file.file_before_save_hooks)

let option_value_to_string v =
  match v with
    StringValue s -> s
  | IntValue i -> string_of_int i
  | FloatValue f -> string_of_float f
  | OnceValue v -> "OnceValue"
  | Module _ -> "Module"
  | List _ -> "List"
  | SmallList _ -> "SmallList"
  | DelayedValue _ -> "DelayedValue"

let must_field assocs name option =
  try from_value option (List.assoc name assocs)
  with _ -> raise Not_found

let safe_field assocs name option default =
  try from_value option (List.assoc name assocs)
  with _ -> default

let option_field assocs name option  =
  try Some (from_value option (List.assoc name assocs))
  with _ -> None

let if_different  value name option default fields =
  if value <> default then
    (name, to_value option value) :: fields
  else fields




end

include LowLevel


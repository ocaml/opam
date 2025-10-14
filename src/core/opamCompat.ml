(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Gc = struct
  [@@@warning "-32"]

  let ramp_up f = (f (), ())

  include Gc

  let ramp_up f = fst (ramp_up f)
end

module String = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let exists p s =
    let n = String.length s in
    let rec loop i =
      if i = n then false
      else if p (String.unsafe_get s i) then true
      else loop (succ i) in
    loop 0

  (** NOTE: OCaml >= 4.13 *)
  let starts_with ~prefix s =
    let x = String.length prefix in
    let n = String.length s in
    n >= x &&
    let rec chk i = i >= x || prefix.[i] = s.[i] && chk (i+1) in
    chk 0

  (** NOTE: OCaml >= 4.13 *)
  let ends_with ~suffix s =
    let x = String.length suffix in
    let n = String.length s in
    n >= x &&
    let rec chk i = i >= x || suffix.[i] = s.[i+n-x] && chk (i+1) in
    chk 0

  (** NOTE: OCaml >= 4.13 *)
  let for_all f s =
    let len = String.length s in
    let rec aux i = i >= len || f s.[i] && aux (i+1) in
    aux 0

  (** NOTE: OCaml >= 4.13 *)
  let fold_left f acc s =
    let acc = ref acc in
    for i = 0 to String.length s - 1 do acc := f !acc s.[i] done;
    !acc

  include Stdlib.String
end

module Seq = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.14 *)
  let rec find_map f xs =
    match xs() with
    | Seq.Nil ->
      None
    | Seq.Cons (x, xs) ->
      match f x with
      | None ->
          find_map f xs
      | Some _ as result ->
          result

  include Seq
end

module Either = struct
  (** NOTE: OCaml >= 4.12 *)
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
end

module Unix = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let realpath s =
    let getchdir s =
      let p =
        try Sys.getcwd ()
        with Sys_error _ -> Filename.get_temp_dir_name ()
      in
      Unix.chdir s;
      p
    in
    try getchdir (getchdir s) with Unix.Unix_error _ -> s

  include Unix
end

module Lazy = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let map f x =
    lazy (f (Lazy.force x))

  (** NOTE: OCaml >= 4.13 *)
  let map_val f x =
    if Lazy.is_val x
    then Lazy.from_val (f (Lazy.force x))
    else lazy (f (Lazy.force x))

  include Stdlib.Lazy
end

module Filename = struct
  [@@@warning "-32"]

  let quote s =
    let l = String.length s in
    let b = Buffer.create (l + 20) in
    Buffer.add_char b '\"';
    let rec loop i =
      if i = l then Buffer.add_char b '\"' else
      match s.[i] with
      | '\"' -> loop_bs 0 i;
      | '\\' -> loop_bs 0 i;
      | c    -> Buffer.add_char b c; loop (i+1);
    and loop_bs n i =
      if i = l then begin
        Buffer.add_char b '\"';
        add_bs n;
      end else begin
        match s.[i] with
        | '\"' -> add_bs (2*n+1); Buffer.add_char b '\"'; loop (i+1);
        | '\\' -> loop_bs (n+1) (i+1);
        | _    -> add_bs n; loop i
      end
    and add_bs n = for _j = 1 to n do Buffer.add_char b '\\'; done
    in
    loop 0;
    Buffer.contents b

(*
Quoting commands for execution by cmd.exe is difficult.
1- Each argument is first quoted using the "quote" function above, to
   protect it against the processing performed by the C runtime system,
   then cmd.exe's special characters are escaped with '^', using
   the "quote_cmd" function below.  For more details, see
   https://blogs.msdn.microsoft.com/twistylittlepassagesallalike/2011/04/23
2- The command and the redirection files, if any, must be double-quoted
   in case they contain spaces.  This quoting is interpreted by cmd.exe,
   not by the C runtime system, hence the "quote" function above
   cannot be used.  The two characters we don't know how to quote
   inside a double-quoted cmd.exe string are double-quote and percent.
   We just fail if the command name or the redirection file names
   contain a double quote (not allowed in Windows file names, anyway)
   or a percent.  See function "quote_cmd_filename" below.
3- The whole string passed to Sys.command is then enclosed in double
   quotes, which are immediately stripped by cmd.exe.  Otherwise,
   some of the double quotes from step 2 above can be misparsed.
   See e.g. https://stackoverflow.com/a/9965141
*)
  let quote_cmd s =
    let b = Buffer.create (String.length s + 20) in
    String.iter
      (fun c ->
         match c with
         | '(' | ')' | '!' | '^' | '%' | '\"' | '<' | '>' | '&' | '|' ->
           Buffer.add_char b '^'; Buffer.add_char b c
         | _ ->
           Buffer.add_char b c)
      s;
    Buffer.contents b

  let quote_cmd_filename f =
    if String.contains f '\"' || String.contains f '%' then
      failwith ("Filename.quote_command: bad file name " ^ f)
    else if String.contains f ' ' then
      "\"" ^ f ^ "\""
    else
      f
  (* Redirections in cmd.exe: see https://ss64.com/nt/syntax-redirection.html
     and https://docs.microsoft.com/en-us/previous-versions/windows/it-pro/windows-xp/bb490982(v=technet.10)
  *)

  (** NOTE: OCaml >= 4.10 *)
  let quote_command cmd ?stdin ?stdout ?stderr args =
    String.concat "" [
      "\"";
      quote_cmd_filename cmd;
      " ";
      quote_cmd (String.concat " " (List.map quote args));
      (match stdin  with None -> "" | Some f -> " <" ^ quote_cmd_filename f);
      (match stdout with None -> "" | Some f -> " >" ^ quote_cmd_filename f);
      (match stderr with None -> "" | Some f ->
          if stderr = stdout
          then " 2>&1"
          else " 2>" ^ quote_cmd_filename f);
      "\""
    ]

  include Stdlib.Filename
end

module List = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.11 *)
  let fold_left_map f s l =
    let s, l_rev =
      List.fold_left (fun (s, l_rev) x ->
          let s, y = f s x in
          s, y :: l_rev)
        (s, []) l
    in
    s, List.rev l_rev

  (* NOTE: OCaml >= 4.12 *)
  let rec equal eq x y = match x, y with
    | [], [] -> true
    | [], _::_ | _::_, [] -> false
    | x::_, y::_ when eq x y -> true
    | _::xs, _::ys -> equal eq xs ys

  include Stdlib.List
end

module type MAP = sig
  include Stdlib.Map.S

  (** NOTE: OCaml >= 4.11 *)
  val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t

  (** NOTE: OCaml >= 5.1 *)
  val add_to_list: key -> 'a -> 'a list t -> 'a list t
end

module Map(Ord : Stdlib.Map.OrderedType) = struct
  [@@@warning "-32"]

  module M = Stdlib.Map.Make(Ord)

  (** NOTE: OCaml >= 4.11 *)
  let filter_map f map =
    M.fold (fun key value map ->
        match f key value with
        | Some value -> M.add key value map
        | None -> map
      ) map M.empty

  (** NOTE: OCaml >= 5.1 *)
  let add_to_list x data m =
    let add = function None -> Some [data] | Some l -> Some (data :: l) in
    M.update x add m

  include M
end

module Pair = struct
  (** NOTE: OCaml >= 5.4 *)
  let equal eq1 eq2 (x1, y1) (x2, y2) =
    eq1 x1 x2 && eq2 y1 y2
end

module Int = struct
  [@@@warning "-32"]

  (* NOTE: OCaml >= 4.13 *)
  let min : int -> int -> int = Stdlib.min

  include Stdlib.Int
end

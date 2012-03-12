let split s s_reg = 
  let i = Str.search_forward (Str.regexp_string s_reg) s 0 in
  String.sub s 0 i, let i = i + String.length s_reg in String.sub s i (String.length s - i)

let print (BatIO.O oc) = Buffer.add_string oc

let nsplit s s_reg = Str.split (Str.regexp_string s_reg) s

module C_set = Set.Make (struct type t = char let compare = compare end)

let fold f acc s =
  let l = String.length s in
  let rec aux acc n = 
    if n = l then
      acc
    else
      aux (f acc s.[n]) (succ n) in
  aux acc 0

let to_list s = 
  List.rev (fold (fun acc c -> c :: acc) [] s)

let of_list l = 
  List.fold_left (Printf.sprintf "%s%c") "" l

let strip ?(chars="\t\r\n") s =
  let set = 
    fold (fun acc s -> C_set.add s acc) C_set.empty chars in

  let del_first =
    let rec aux = function
      | [] -> []
      | x :: xs -> 
        if C_set.mem x set then
          aux xs
        else
          x :: xs in
    aux in

  of_list (List.rev (del_first (List.rev (del_first (to_list s)))))

let trim = strip ~chars:" \010\013\009\026\012"

(** Suppress the extension [tgz] in the given [s] string.
    [None] is returned in case the [s] does not end with [tgz]. *)
let right_chop s tgz = 
  let l_s, l_tgz = String.length s, String.length tgz in
  if l_s < l_tgz then
    None
  else 
    let dim = l_s - l_tgz in
    if String.sub s dim l_tgz = tgz then
      Some (String.sub s 0 dim)
    else
      None

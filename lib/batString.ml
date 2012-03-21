open ExtString

let print (BatIO.O oc) = Buffer.add_string oc

let trim = String.strip ~chars:" \010\013\009\026\012"

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

let split s sep = try String.split s sep with _ -> raise Not_found

module Exceptionless =
struct
  let split s sep = try Some (String.split s sep) with _ -> None
end

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 David Allsopp Ltd.                                   *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t = int * int

let supported_versions = [(2, 0); (2, 1); (2,2)]

let is_supported v = List.mem v supported_versions

let of_string s =
  match String.index s '.' with
  | i when s.[0] <> '0' && (i >= String.length s - 2 || s.[i + 1] <> '0') ->
    begin
      try Scanf.sscanf s "%u.%u%!" (fun major minor -> (major, minor))
      with Scanf.Scan_failure _ -> failwith "OpamVersion.CLI.of_string"
    end
  | exception Not_found -> failwith "OpamVersion.CLI.of_string"
  | _ -> failwith "OpamVersion.CLI.of_string"

let current = of_string @@ OpamVersion.(to_string current_nopatch)

(* This line is checked on CI to ensure that default cli version
   matches release opam version *)
let default = (2,0)

let of_string_opt s = try Some (of_string s) with Failure _ -> None

let to_string (major, minor) = Printf.sprintf "%d.%d" major minor

let to_json v = `String (to_string v)
let of_json = function
  | `String x -> of_string_opt x
  | _ -> None

let ( >= ) = Stdlib.( >= )
let ( < ) = Stdlib.( < )
let compare (vm, vn) (wm, wn) =
  let major = Int.compare vm wm in
  if major <> 0 then major else
    Int.compare vn wn
let equal v w = compare v w = 0

let previous cli =
  let f previous version =
    if version > previous && cli > version then version else previous
  in
  let zero = (0, 0) in
  let previous = List.fold_left f zero supported_versions in
  if previous = zero then raise Not_found
  else previous

(* CLI version extended with provenance *)
module Sourced = struct
  type nonrec t = t * OpamStateTypes.provenance

  let current = current, `Default

  let env s =
    OpamStd.Option.Op.(s >>= of_string_opt >>| (fun c -> c, `Env))

end

module Op = struct
  let ( @>= ) (c,_) = Stdlib.( >= ) c
  let ( @< ) (c,_) = Stdlib.( < ) c
  let ( @= ) (c,_) = Stdlib.( = ) c
end

module O = struct
  type nonrec t = t
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
  let compare = compare
end

module Set = OpamStd.Set.Make(O)
module Map = OpamStd.Map.Make(O)

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

type version_control = [ `git | `darcs | `hg ]

type backend = [ `http | `rsync | version_control ]

type t = {
  transport: string;
  path: string;
  hash: string option;
  backend: backend;
}

let empty = {
  backend = `http;
  transport = "https";
  path = "";
  hash = None;
}

let split_url =
  let re =
    let (@@) f x = f x in
    Re.(compile @@ seq [
        opt @@ seq [
          opt @@ seq [ group @@ rep @@ diff any (set "+:");
                       alt [ char '+'; str "://"] ];
          group @@ rep @@ diff any (char ':');
          str "://"
        ];
        group @@ seq [
          non_greedy @@ rep @@ diff any (char '#');
          opt @@ seq [ char '.'; group @@ rep1 @@ diff any (set ".#")]
        ];
        opt @@ seq [ char '#'; group @@ rep any ];
        eos;
      ])
  in
  fun u ->
    match Re.get_all (Re.exec re u) with
    | [| _; vc; transport; path; suffix; hash |] ->
      let opt = function "" -> None | s -> Some s in
      opt vc, opt transport, path, opt suffix, opt hash
    | _ -> assert false

let vc_of_string = function
  | "git" -> `git
  | "hg" -> `hg
  | "darcs" -> `darcs
  | x -> failwith (Printf.sprintf "Unsupported version control system %S" x)

let string_of_vc = function
  | `git   -> "git"
  | `darcs -> "darcs"
  | `hg    -> "hg"

let string_of_backend = function
  | `http  -> "http"
  | `rsync -> "rsync"
  | #version_control as vc -> string_of_vc vc

let backend_of_string = function
  | "http" | "https" | "ftp" | "wget" | "curl" -> `http
  | "file" -> `rsync
  | "git" -> `git
  | "darcs" -> `darcs
  | "hg" -> `hg
  | "path" | "local" | "rsync" | "ssh" | "scp" | "sftp" -> `rsync
  | p -> failwith (Printf.sprintf "Unsupported protocol %S" p)


let looks_like_ssh_re =
  (* ':' before any '/' : assume ssh, like git does. Exception for 'x:' with
     single char, because Windows *)
  Re.(compile @@ seq [repn (diff any (char '/')) 2 None; char ':'])

let parse ?backend s =
  let vc, transport, path, suffix, hash = split_url s in
  let backend, transport =
    match backend with
    | Some b -> b, transport
    | None ->
      match vc with
      | Some vc -> vc_of_string vc, transport
      | None -> match transport with
        | Some tr ->
          (try vc_of_string tr, None with Failure _ ->
           match suffix with
           | Some sf ->
             (try vc_of_string sf, transport with Failure _ ->
                backend_of_string tr, transport)
           | None -> backend_of_string tr, transport)
        | None -> `rsync, None
  in
  let transport, path =
    match backend, transport with
    | _, Some tr -> tr, path
    | `http, None -> "http", path
    | _, None ->
      if Re.execp looks_like_ssh_re path then "ssh", path
      else "file", OpamSystem.real_path path
  in
  {
    transport;
    path;
    hash;
    backend;
  }

let of_string url = parse url

let to_string url =
  let hash = match url.hash with Some h -> "#" ^ h | None -> "" in
  match url.backend with
  | #version_control as vc ->
    let vc = string_of_backend vc in
    if url.transport = vc then (* Don't be redundant on e.g git:// protocols *)
      Printf.sprintf "%s://%s%s" vc url.path hash
    else
      Printf.sprintf "%s+%s://%s%s" vc url.transport url.path hash
  | `rsync | `http ->
    Printf.sprintf "%s://%s%s" url.transport url.path hash

let base_url url =
  match url.transport with
  | "" -> url.path
  | tr -> Printf.sprintf "%s://%s" tr url.path

let local_path = function
  | { transport = ("file"|"path"|"local"|"rsync"); path;
      hash = None; backend = (#version_control | `rsync); }
    when not (Re.execp looks_like_ssh_re path) ->
    Some path
  | _ -> None

let local_dir url =
  let open OpamStd.Option.Op in
  local_path url >>|
  OpamFilename.Dir.of_string >>= fun d ->
  if OpamFilename.exists_dir d then Some d
  else None

let local_file url =
  let open OpamStd.Option.Op in
  local_path url >>|
  OpamFilename.of_string >>= fun f ->
  if OpamFilename.exists f then Some f
  else None

let guess_version_control s =
  let vc,transport,path,_,_ = split_url s in
  if vc = None && transport = None && not (Re.execp looks_like_ssh_re path) then
    let open OpamFilename in
    let open Op in
    let dir = Dir.of_string path in
    if exists_dir (dir / ".git") then Some`git else
    if exists_dir (dir / ".hg") then Some `hg else
    if exists_dir (dir / "_darcs") then Some `darcs else
      None
  else
  None

let basename =
  let re =
    Re.(compile @@ seq [
        opt @@ seq [rep any; char '/'];
        group @@ rep1 (diff any (char '/'));
        rep @@ char '/';
      ])
  in
  fun t ->
    try
      Re.get (Re.exec re t.path) 1
    with Not_found -> ""

let has_trailing_slash url =
  OpamStd.String.ends_with ~suffix:"/" url.path

let to_json url = `String (to_string url)

type url = t

module O = struct
  type t = url
  let to_string = to_string
  let to_json = to_json
  let compare = compare
end

module Set = OpamStd.Set.Make(O)

module Map = OpamStd.Map.Make(O)

module Op = struct

  (** appending to an url path *)
  let ( / ) url dir =
    let path =
      if has_trailing_slash url then url.path ^ dir
      else String.concat "/" [url.path; dir]
    in
    {url with path }

end

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Op

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
        bos;
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


let looks_like_ssh_path =
  (* ':' before any '/' : assume ssh, like git does. Exception for 'x:' with
     single char, because Windows *)
  let re =
    Re.(compile @@ seq [
        group @@ repn (diff any (set "/:")) 2 None;
        char ':';
        opt @@ group @@ seq [
          alt [
            diff any digit;
            seq [rep digit; compl [digit; char '/']]
          ];
          rep any;
        ];
        eos;
      ])
  in
  fun path ->
    try
      let sub = Re.exec re path in
      Some (Re.get sub 1 ^ try "/" ^ Re.get sub 2 with Not_found -> "")
    with Not_found -> None

let parse ?backend ?(handle_suffix=true) s =
  let vc, transport, path, suffix, hash = split_url s in
  let backend =
    match backend with
    | Some b -> b
    | None ->
      match vc with
      | Some vc -> vc_of_string vc
      | None ->
        let of_suffix ~default =
          if not handle_suffix then default else
          match suffix with
          | Some sf -> (try vc_of_string sf with Failure _ -> default)
          | None ->
            match OpamStd.String.cut_at path '@' with
            | Some (user, _) ->
              (try vc_of_string user with Failure _ -> default)
            | None -> default
        in
        match transport with
        | None -> of_suffix ~default:`rsync
        | Some tr ->
          try vc_of_string tr with Failure _ ->
            of_suffix ~default:(backend_of_string tr)
  in
  let transport, path =
    match backend, transport, looks_like_ssh_path path with
    | `http, None, _ ->
      "http", path
    | _, (None | Some ("git"|"hg"|"darcs")), Some path ->
      "ssh", path
    | _, (None | Some ("hg"|"darcs")), None ->
      "file", OpamSystem.real_path path
    | _, Some tr, _ ->
      tr, path
  in
  {
    transport;
    path;
    hash;
    backend;
  }

let of_string url = parse ~handle_suffix:false url

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
      hash = _; backend = (#version_control | `rsync); }
    when looks_like_ssh_path path = None ->
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
  if vc = None && transport = None && looks_like_ssh_path path = None then
    let open OpamFilename in
    let open Op in
    let dir = Dir.of_string path in
    if exists_dir (dir / ".git") || exists (dir // ".git")
    then Some`git else
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

let root =
  let re = Re.(compile @@ seq [char '/'; rep any]) in
  fun t ->
    { t with path = Re.replace_string re ~by:"" t.path }

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
    let url =
      if OpamStd.String.starts_with ~prefix:"/" dir then
        root url
      else url
    in
    let path =
      if has_trailing_slash url then url.path ^ dir
      else String.concat "/" [url.path; dir]
    in
    {url with path }

end

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamFilename.Op

let root root name = root / "repo" / OpamRepositoryName.to_string name

let tar root name = root / "repo" // (OpamRepositoryName.to_string name ^ ".tar.gz")

let download_cache root = root / "download-cache"

let pin_cache_dir =
  let dir =
    lazy (OpamSystem.mk_temp_dir ~prefix:"opam-pin-cache" ()
          |> OpamFilename.Dir.of_string )
  in
  fun () -> Lazy.force dir

let pin_cache u =
  pin_cache_dir () /
  String.sub
    (OpamHash.contents @@
     OpamHash.compute_from_string ~kind:`SHA512 @@
     OpamUrl.to_string u)
    0 16

let repo repo_root = repo_root // "repo" |> OpamFile.make

let packages_dir repo_root = repo_root / "packages"

let packages repo_root prefix nv =
  match prefix with
  | None   -> packages_dir repo_root / OpamPackage.to_string nv
  | Some p -> packages_dir repo_root / p / OpamPackage.to_string nv

let opam repo_root prefix nv =
  packages repo_root prefix nv // "opam" |> OpamFile.make

let descr repo_root prefix nv =
  packages repo_root prefix nv // "descr" |> OpamFile.make

let url repo_root prefix nv =
  packages repo_root prefix nv // "url" |> OpamFile.make

let files repo_root prefix nv =
  packages repo_root prefix nv / "files"

let extrafile_nv_dir filename =
  let rec find_files prefix_files tail =
    match tail with
    | "files"::_ -> Some prefix_files
    | h::t -> find_files (h::prefix_files) t
    | [] -> None
  in
  let rec aux (pre, rest) =
    match rest with
    | "packages"::packages ->
      (* We don't check packages/name/name.version layer because repo loading
         is more permissive *)
      (match find_files [] packages with
       | Some (nv::prefix_files) ->
         (match OpamPackage.of_string_opt nv with
          | Some pkg ->
            let dir =
              List.rev (nv :: prefix_files @ ["packages"])
              |> OpamFilename.Dir.of_list
              |> OpamFilename.Unix.Dir.of_dir
            in
            Some (pkg, dir)
          | None -> None)
       | None | Some [] -> None)
    | p::r -> aux (p::pre, r)
    | [] -> None
  in
  aux ([], OpamFilename.Unix.to_list filename)

module Remote = struct
  (** URL, not FS paths *)
  open OpamUrl.Op

  let repo root_url =
    root_url / "repo"

  let packages_url root_url =
    root_url / "packages"

  let archive root_url nv =
    root_url / "archives" / (OpamPackage.to_string nv ^ "+opam.tar.gz")
end

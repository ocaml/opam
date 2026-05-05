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

let get_pkg_dir file =
  let rec find_pkg_rev prefix_files = function
    | x::xs ->
      begin match OpamPackage.of_string_opt x with
      | Some nv -> Some (nv, x::prefix_files)
      | None -> find_pkg_rev (x::prefix_files) xs
      end
    | [] -> None
  in
  match OpamFilename.Unix.to_relative_canonical_list file with
  | "packages"::xs ->
    begin match find_pkg_rev [] xs with
    | Some (nv, full_rev_prefix) ->
      let dir =
        List.rev full_rev_prefix
        |> List.fold_left OpamFilename.Unix.Op.(/)
          (OpamFilename.Unix.Dir.of_string "packages")
      in
      Some (nv, dir)
    | None -> None
    end
  | _ -> None

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

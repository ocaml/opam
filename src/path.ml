(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open Types
open Utils

let log fmt = Globals.log "PATH" fmt

let available_packages dir =
  log "available_packages %s" (Dirname.to_string dir);
  if Dirname.exists dir then (
    let files = Filename.list dir in
    let files = List.filter (fun f -> Filename.check_suffix f ".opam") files in
    List.fold_left (fun set file ->
      match NV.of_filename file with
      | None    ->
          log "%s is not a valid package filename!" (Filename.to_string file);
          set
      | Some nv -> NV.Set.add nv set
    ) NV.Set.empty files
  ) else
    NV.Set.empty

let available_compilers dir =
  log "available_compilers %s" (Dirname.to_string dir);
  if Dirname.exists dir then (
    let files = Filename.list dir in
    let files = List.filter (fun f -> Filename.check_suffix f ".comp") files in
    let l =
      List.map
        (Filename.chop_extension
         |> Filename.basename
         |> Basename.to_string
         |> OCaml_V.of_string)
        files in
    OCaml_V.Set.of_list l
  ) else
    OCaml_V.Set.empty

let versions nvset =
  NV.Set.fold (fun nv vset -> V.Set.add (NV.version nv) vset) nvset V.Set.empty

module C = struct

  type t = dirname (* [$opam/$oversion/ *)

  let create oversion =
    Dirname.of_string !Globals.root_path / Alias.to_string oversion

  let root x = x

  let lib_dir t = t / "lib"

  let lib t n = lib_dir t / N.to_string n

  let stublibs t = lib_dir t / "stublibs"

  let toplevel t = lib_dir t / "toplevel"

  let doc_dir t = t / "doc"

  let man_dir t = t / "man"

  let doc t n = doc_dir t / N.to_string n

  let bin t = t / "bin"

  let installed t = t // "installed"

  let build_dir t = t / "build"

  let build t nv = build_dir t / NV.to_string nv

  let build_env t nv = build t nv // (N.to_string (NV.name nv) ^ ".env")

  let build_old_env t nv = build t nv // (N.to_string (NV.name nv) ^ ".old.env")

  let build_ocaml t = build_dir t / "ocaml"

  let build_install t nv = build t nv // (N.to_string (NV.name nv) ^ ".install")

  let build_config t nv = build t nv // (N.to_string (NV.name nv) ^ ".config")

  let install_dir t = t / "install"

  let install t n = install_dir t // (N.to_string n ^ ".install")

  let reinstall t = t // "reinstall"

  let config_dir t = t / "config"

  let config t n = config_dir t // (N.to_string n ^ ".config")

  let pinned t = t // "pinned"

end

module G = struct

  type t = dirname (* [$opam/] *)

  let create () = Dirname.of_string !Globals.root_path

  let root opam = opam

  let dirname_of_nv nv = Dirname.of_string (NV.to_string nv)

  let config t = t // "config"

  let opam_dir t = t / "opam"

  let aliases t = t // "aliases"

  let opam t nv = opam_dir t // (NV.to_string nv ^ ".opam")

  let compilers_dir t = t / "compilers"

  let compiler t ov = compilers_dir t // (OCaml_V.to_string ov ^ ".comp")

  let available_compilers t = available_compilers (compilers_dir t)

  let available_packages t = available_packages (opam_dir t)

  let available_versions t n =
    versions (NV.Set.filter (fun nv -> NV.name nv = n) (available_packages t))

  let descr_dir t = t / "descr"

  let descr t nv = descr_dir t // NV.to_string nv

  let archives_dir t = t / "archives"

  let archive t nv = archives_dir t // (NV.to_string nv ^ "+opam.tar.gz")

  let repo_index t = t / "repo" // "index"

  let available_aliases t =
    let l = List.map fst (File.Aliases.safe_read (aliases t)) in
    Alias.Set.of_list l
end

module R = struct

  type t = dirname (* [$opam/repo/$repo/] *)

  let create r =
    Dirname.of_string !Globals.root_path / "repo" / Repository.name r

  let of_dirname path = path

  let cwd () = Dirname.cwd ()

  let root t = t

  let config t = t // "config"

  let packages_dir t = t / "packages"

  let available_packages t =
    log "available_packages %s" (Dirname.to_string t);
    let dir = packages_dir t in
    if Dirname.exists dir then (
      let all = Dirname.list dir in
      let basenames = List.map Dirname.basename all in
      NV.Set.of_list (List.map (Basename.to_string |> NV.of_string) basenames)
    ) else
      NV.Set.empty

  let available_versions t n =
    versions (NV.Set.filter (fun nv -> NV.name nv = n) (available_packages t))

  let package t nv = packages_dir t / NV.to_string nv

  let opam t nv = package t nv // "opam"

  let descr t nv = package t nv // "descr"

  let archives_dir t = t / "archives"

  let archive t nv = archives_dir t // (NV.to_string nv ^ "+opam.tar.gz")

  let available_archives t =
    let d = archives_dir t in
    if Dirname.exists d then
      Filename.Set.of_list (Filename.list d)
    else
      Filename.Set.empty

  let updated t = t // "updated"

  let upload_dir t = t / "upload"

  let compilers_dir t = t / "compilers"

  let compiler t ov = compilers_dir t // (OCaml_V.to_string ov ^ ".comp")

  let available_compilers t = available_compilers (compilers_dir t)

  let url t nv = package t nv // "url"

  let files t nv = package t nv / "files"

  let tmp t = t / "tmp"

  let tmp_dir t nv = tmp t / NV.to_string nv

  let available_tmp t =
    let d = tmp t in
    let files = if Dirname.exists d then
        Filename.Set.of_list (Filename.list d)
      else
        Filename.Set.empty in
    NV.Set.of_list (Utils.filter_map NV.of_filename (Filename.Set.elements files))

  let available_files t nv =
    if Dirname.exists (files t nv) then
      Filename.rec_list (files t nv)
    else
      []

end

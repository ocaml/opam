(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

open OpamTypes
open OpamFilename.OP

type t = dirname

let root () =
  OpamFilename.Dir.of_string !OpamGlobals.root_dir

let config t = t // "config"

let state_cache t = t // "state.cache"

let update_cache t = t // "update.cache"

let lock t = t // "lock"

let aliases t = t // "aliases"

let packages_dir t = t / "packages"

let packages t nv =
  packages_dir t
  / OpamPackage.Name.to_string (OpamPackage.name nv)
  / OpamPackage.to_string nv

let opam t nv = packages t nv // "opam"

let url t nv = packages t nv // "url"

let descr t nv = packages t nv // "descr"

let archives_dir t = t / "archives"

let archive t nv = archives_dir t // (OpamPackage.to_string nv ^ "+opam.tar.gz")

let files t nv = packages t nv / "files"

let compilers_dir t = t / "compilers"

let compilers t c =
  compilers_dir t
  / OpamCompiler.Version.to_string (OpamCompiler.version c)
  / OpamCompiler.to_string c

let compiler_comp t c =
  compilers t c // (OpamCompiler.to_string c ^ ".comp")

let compiler_descr t c =
  compilers t c // (OpamCompiler.to_string c ^ ".descr")

let repo_index t = t / "repo" // "index"

let init  t = t / "opam-init"

let log t = t / "log"

let dev_packages_dir t = t / "packages.dev"

let dev_package t nv = dev_packages_dir t / OpamPackage.to_string nv

module Switch = struct

  let root t a = t / OpamSwitch.to_string a

  let lock t a = root t a // "lock"

  let lib_dir t a = root t a / "lib"

  let lib t a n = lib_dir t a / OpamPackage.Name.to_string n

  let stublibs t a = lib_dir t a / "stublibs"

  let toplevel t a = lib_dir t a / "toplevel"

  let doc_dir t a = root t a / "doc"

  let man_dir ?num t a =
    match num with
    | None -> root t a / "man"
    | Some n -> root t a / "man" / ("man" ^ n)

  let share_dir t a = root t a / "share"

  let share t a n = share_dir t a / OpamPackage.Name.to_string n

  let doc t a n = doc_dir t a / OpamPackage.Name.to_string n

  let bin t a = root t a / "bin"

  let installed t a = root t a // "installed"

  let installed_roots t a = root t a // "installed.roots"

  let build_dir t a = root t a / "build"

  let build t a nv = build_dir t a / OpamPackage.to_string nv

  let build_ocaml t a = build_dir t a / "ocaml"

  let build_install t a nv =
    build t a nv // (OpamPackage.Name.to_string (OpamPackage.name nv) ^ ".install")

  let build_config t a nv =
    build t a nv // (OpamPackage.Name.to_string (OpamPackage.name nv) ^ ".config")

  let install_dir t a = root t a / "install"

  let install t a n = install_dir t a // (OpamPackage.Name.to_string n ^ ".install")

  let reinstall t a = root t a // "reinstall"

  let config_dir t a = root t a / "config"

  let config t a n = config_dir t a // (OpamPackage.Name.to_string n ^ ".config")

  let pinned t a = root t a // "pinned"

  let dev_packages_dir t a = root t a / "packages.dev"

  let dev_package t a nv = dev_packages_dir t a / OpamPackage.to_string nv

  let overlay_dir t a = root t a / "overlay"

  let overlay t a nv = overlay_dir t a / OpamPackage.to_string nv

  let opam t a nv = overlay t a nv // "opam"

  let url t a nv = overlay t a nv // "url"

  let descr t a nv = overlay t a nv // "descr"

  let files t a nv = overlay t a nv / "files"

end

module Repository = struct

  let root t = t.repo_root

  let update_cache t = root t // "update.cache"

  let create root name = root / "repo" / OpamRepositoryName.to_string name

  let version t = root t // "version"

  let remote_version t =
    OpamFilename.raw_dir (fst t.repo_address) // "version"

  let raw_config root name =
    root / "repo" / OpamRepositoryName.to_string name // "config"

  let config t = root t // "config"

  let packages_dir t = root t / "packages"

  let remote_packages_dir t =
    OpamFilename.raw_dir (fst t.repo_address) / "packages"

  let packages t prefix nv =
    match prefix with
    | None   -> packages_dir t / OpamPackage.to_string nv
    | Some p -> packages_dir t / p / OpamPackage.to_string nv

  let opam t prefix nv = packages t prefix nv // "opam"

  let descr t prefix nv = packages t prefix nv // "descr"

  let url t prefix nv = packages t prefix nv // "url"

  let files t prefix nv = packages t prefix nv / "files"

  let archives_dir t = root t / "archives"

  let archive t nv = archives_dir t // (OpamPackage.to_string nv ^ "+opam.tar.gz")

  let remote_archive t nv =
    OpamFilename.raw_dir (fst t.repo_address)
    / "archives"
    // (OpamPackage.to_string nv ^ "+opam.tar.gz")

  let upload_dir t = root t / "upload"

  let compilers_dir t = root t / "compilers"

  let remote_compilers_dir t =
    OpamFilename.raw_dir (fst t.repo_address) / "compilers"

  let compiler_comp t prefix c =
    match prefix with
    | None   -> compilers_dir t // (OpamCompiler.to_string c ^ ".comp")
    | Some p -> compilers_dir t / p // (OpamCompiler.to_string c ^ ".comp")

  let compiler_descr t prefix c =
    match prefix with
    | None   -> compilers_dir t // (OpamCompiler.to_string c ^ ".descr")
    | Some p -> compilers_dir t / p // (OpamCompiler.to_string c ^ ".descr")

end

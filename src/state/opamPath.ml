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

open OpamTypes
open OpamFilename.Op

type t = dirname

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

let package_index t = t / "repo" // "package-index"

let compiler_index t = t / "repo" // "compiler-index"

let init  t = t / "opam-init"

let log t = t / "log"

let dev_packages_dir t = t / "packages.dev"

let dev_package t nv = dev_packages_dir t / OpamPackage.to_string nv

let backup_file =
  let file = lazy Unix.(
      let tm = gmtime (Unix.gettimeofday ()) in
      Printf.sprintf "state-%04d%02d%02d%02d%02d%02d.export"
        (tm.tm_year+1900) tm.tm_mon tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    ) in
  fun () -> Lazy.force file

let backup_dir t = t / "backup"

let backup t = backup_dir t // backup_file ()

module Switch = struct

  let root t a = t / OpamSwitch.to_string a

  (** Internal files and dirs with static location *)

  let lock t a = root t a // "lock"

  let backup_dir t a = root t a / "backup"

  let backup t a = backup_dir t a // backup_file ()

  let state t a = root t a // "state"

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

  let global_config t a = config_dir t a // "global-config.config"

  let dev_packages_dir t a = root t a / "packages.dev"

  let dev_package t a name = dev_packages_dir t a / OpamPackage.Name.to_string name

  let environment t a = root t a // "environment"

  module Default = struct

    (** Visible files that can be redirected using
        [config/global-config.config] *)

    let lib_dir t a = root t a / "lib"

    let lib t a n = lib_dir t a / OpamPackage.Name.to_string n

    let config t a n = lib t a n // "opam.config"

    let stublibs t a = lib_dir t a / "stublibs"

    let toplevel t a = lib_dir t a / "toplevel"

    let doc_dir t a = root t a / "doc"

    let man_dir ?num t a =
      match num with
      | None -> root t a / "man"
      | Some n -> root t a / "man" / ("man" ^ n)

    let share_dir t a = root t a / "share"

    let share t a n = share_dir t a / OpamPackage.Name.to_string n

    let etc_dir t a = root t a / "etc"

    let etc t a n = etc_dir t a / OpamPackage.Name.to_string n

    let doc t a n = doc_dir t a / OpamPackage.Name.to_string n

    let bin t a = root t a / "bin"

    let sbin t a = root t a / "sbin"

  end

  let lookup c var dft =
    match OpamFile.Dot_config.variable c (OpamVariable.of_string var) with
    | Some (S f) -> OpamFilename.Dir.of_string f
    | None | Some (B _) -> dft

  let prefix t a c =
    lookup c "prefix" (root t a)

  let lib_dir t a c =
    lookup c "lib" (prefix t a c / "lib")

  let lib t a c n =
    lib_dir t a c / OpamPackage.Name.to_string n

  let config t a c n =
    lib t a c n // "opam.config"

  let stublibs t a c =
    lookup c "stublibs" (lib_dir t a c / "stublibs")

  let toplevel t a c =
    lookup c "toplevel" (lib_dir t a c / "toplevel")

  let doc_dir t a c =
    lookup c "doc" (prefix t a c / "doc")

  let man_dir ?num t a c =
    let base = lookup c "man" (prefix t a c / "man") in
    match num with
    | None -> base
    | Some n -> base / ("man" ^ n)

  let share_dir t a c =
    lookup c "share" (prefix t a c / "share")

  let share t a c n =
    share_dir t a c / OpamPackage.Name.to_string n

  let etc_dir t a c =
    lookup c "etc" (prefix t a c / "etc")

  let etc t a c n =
    etc_dir t a c / OpamPackage.Name.to_string n

  let doc t a c n =
    doc_dir t a c / OpamPackage.Name.to_string n

  let bin t a c =
    lookup c "bin" (prefix t a c / "bin")

  let sbin t a c =
    lookup c "sbin" (prefix t a c / "sbin")

  module Overlay = struct

    let dir t a = root t a / "overlay"

    let package t a n = dir t a / OpamPackage.Name.to_string n

    let opam t a n = package t a n // "opam"

    let tmp_opam t a n = package t a n // "opam_"

    let url t a n = package t a n // "url"

    let descr t a n = package t a n // "descr"

    let files t a n = package t a n / "files"

  end
end

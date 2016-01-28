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

let root () = OpamStateConfig.(!r.root_dir)

let config () = root () // "config"

let state_cache () = root () // "state.cache"

let update_cache () = root () // "update.cache"

let lock () = root () // "lock"

let aliases () = root () // "aliases"

let packages_dir () = root () / "packages"

let archives_dir () = root () / "archives"

let compilers_dir () = root () / "compilers"

let package_index () = root () / "repo" // "package-index"

let compiler_index () = root () / "repo" // "compiler-index"

let init () = root () / "opam-init"

let log () = root () / "log"

let dev_packages_dir () = root () / "packages.dev"

let backup_dir () = root () / "backup"

let packages nv =
  packages_dir ()
  / OpamPackage.Name.to_string (OpamPackage.name nv)
  / OpamPackage.to_string nv

let opam nv = packages nv // "opam"

let url nv = packages nv // "url"

let descr nv = packages nv // "descr"

let archive nv = archives_dir () // (OpamPackage.to_string nv ^ "+opam.tar.gz")

let files nv = packages nv / "files"

let compilers c =
  compilers_dir ()
  / OpamCompiler.Version.to_string (OpamCompiler.version c)
  / OpamCompiler.to_string c

let compiler_comp c =
  compilers c // (OpamCompiler.to_string c ^ ".comp")

let compiler_descr c =
  compilers c // (OpamCompiler.to_string c ^ ".descr")

let dev_package nv = dev_packages_dir () / OpamPackage.to_string nv

let backup_file =
  let file = lazy Unix.(
      let tm = gmtime (Unix.gettimeofday ()) in
      Printf.sprintf "state-%04d%02d%02d%02d%02d%02d.export"
        (tm.tm_year+1900) tm.tm_mon tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    ) in
  fun () -> Lazy.force file

let backup () = backup_dir () // backup_file ()

module Switch = struct

  let root a = OpamStateConfig.(!r.root_dir) / OpamSwitch.to_string a

  (** Internal files and dirs with static location *)

  let lock a = root a // "lock"

  let backup_dir a = root a / "backup"

  let backup a = backup_dir a // backup_file ()

  let state a = root a // "state"

  let build_dir a = root a / "build"

  let build a nv = build_dir a / OpamPackage.to_string nv

  let build_ocaml a = build_dir a / "ocaml"

  let build_install a nv =
    build a nv // (OpamPackage.Name.to_string (OpamPackage.name nv) ^ ".install")

  let build_config a nv =
    build a nv // (OpamPackage.Name.to_string (OpamPackage.name nv) ^ ".config")

  let install_dir a = root a / "install"

  let install a n = install_dir a // (OpamPackage.Name.to_string n ^ ".install")

  let reinstall a = root a // "reinstall"

  let config_dir a = root a / "config"

  let global_config a = config_dir a // "global-config.config"

  let config a n =
    config_dir a // (OpamPackage.Name.to_string n ^ ".config")

  let dev_packages_dir a = root a / "packages.dev"

  let dev_package a name = dev_packages_dir a / OpamPackage.Name.to_string name

  let environment a = root a // "environment"

  module Default = struct

    (** Visible files that can be redirected using
        [config/global-config.config] *)

    let lib_dir a = root a / "lib"

    let lib a n = lib_dir a / OpamPackage.Name.to_string n

    let stublibs a = lib_dir a / "stublibs"

    let toplevel a = lib_dir a / "toplevel"

    let doc_dir a = root a / "doc"

    let man_dir ?num a =
      match num with
      | None -> root a / "man"
      | Some n -> root a / "man" / ("man" ^ n)

    let share_dir a = root a / "share"

    let share a n = share_dir a / OpamPackage.Name.to_string n

    let etc_dir a = root a / "etc"

    let etc a n = etc_dir a / OpamPackage.Name.to_string n

    let doc a n = doc_dir a / OpamPackage.Name.to_string n

    let bin a = root a / "bin"

    let sbin a = root a / "sbin"

  end

  let lookup c var dft =
    match OpamFile.Dot_config.variable c (OpamVariable.of_string var) with
    | Some (S f) -> OpamFilename.Dir.of_string f
    | None | Some (B _) -> dft

  let prefix a c =
    lookup c "prefix" (root a)

  let lib_dir a c =
    lookup c "lib" (prefix a c / "lib")

  let lib a c n =
    lib_dir a c / OpamPackage.Name.to_string n

  let stublibs a c =
    lookup c "stublibs" (lib_dir a c / "stublibs")

  let toplevel a c =
    lookup c "toplevel" (lib_dir a c / "toplevel")

  let doc_dir a c =
    lookup c "doc" (prefix a c / "doc")

  let man_dir ?num a c =
    let base = lookup c "man" (prefix a c / "man") in
    match num with
    | None -> base
    | Some n -> base / ("man" ^ n)

  let share_dir a c =
    lookup c "share" (prefix a c / "share")

  let share a c n =
    share_dir a c / OpamPackage.Name.to_string n

  let etc_dir a c =
    lookup c "etc" (prefix a c / "etc")

  let etc a c n =
    etc_dir a c / OpamPackage.Name.to_string n

  let doc a c n =
    doc_dir a c / OpamPackage.Name.to_string n

  let bin a c =
    lookup c "bin" (prefix a c / "bin")

  let sbin a c =
    lookup c "sbin" (prefix a c / "sbin")

  module Overlay = struct

    let dir a = root a / "overlay"

    let package a n = dir a / OpamPackage.Name.to_string n

    let opam a n = package a n // "opam"

    let tmp_opam a n = package a n // "opam_"

    let url a n = package a n // "url"

    let descr a n = package a n // "descr"

    let files a n = package a n / "files"

  end
end

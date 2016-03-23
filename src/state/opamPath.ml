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

(* Returns a generic file, coerced by the .mli *)
let ( /- ) dir f = OpamFile.make (dir // f)

let config t = t /- "config"

let state_cache t = t / "repo" // "state.cache"

let lock t = t // "lock"

let config_lock t = t // "config.lock"

let archives_dir t = t / "archives"

let archive t nv = archives_dir t // (OpamPackage.to_string nv ^ "+opam.tar.gz")

let repos_lock t = t / "repo" // "lock"

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

let backup t = backup_dir t /- backup_file ()

module Switch = struct

  let root t a = t / OpamSwitch.to_string a

  (** Internal files and dirs with static location *)

  let meta t a = root t a / ".opam-switch"

  let lock t a = meta t a // "lock"

  let backup_dir t a = meta t a / "backup"

  let backup t a = backup_dir t a /- backup_file ()

  let selections t a = meta t a /- "switch-state"

  let build_dir t a = meta t a / "build"

  let build t a nv = build_dir t a / OpamPackage.to_string nv

  let build_ocaml t a = build_dir t a / "ocaml"

  let build_install t a nv =
    build t a nv /- (OpamPackage.Name.to_string nv.name ^ ".install")

  let build_config t a nv =
    build t a nv /- (OpamPackage.Name.to_string nv.name ^ ".config")

  let install_dir t a = meta t a / "install"

  let install t a n = install_dir t a /- (OpamPackage.Name.to_string n ^ ".install")

  let reinstall t a = meta t a /- "reinstall"

  let config_dir t a = meta t a / "config"

  let global_config t a = config_dir t a /- "global-config.config"

  let config t a n =
    config_dir t a /- (OpamPackage.Name.to_string n ^ ".config")

  let dev_packages_dir t a = meta t a / "packages.dev"

  let dev_package t a name = dev_packages_dir t a / OpamPackage.Name.to_string name

  let environment t a = meta t a /- "environment"

  let installed_opams t a = meta t a / "packages"

  let installed_opam t a nv =
    installed_opams t a /- (OpamPackage.to_string nv ^ ".opam")

  module Default = struct

    (** Visible files that can be redirected using
        [config/global-config.config] *)

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

    let dir t a = meta t a / "overlay"

    let package t a n = dir t a / OpamPackage.Name.to_string n

    let opam t a n = package t a n /- "opam"

    let tmp_opam t a n = package t a n /- "opam_"

    let url t a n = package t a n /- "url"

    let descr t a n = package t a n /- "descr"

    let files t a n = package t a n / "files"

  end
end

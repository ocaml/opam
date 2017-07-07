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

open OpamTypes
open OpamFilename.Op

type t = dirname

(* Returns a generic file, coerced by the .mli *)
let ( /- ) dir f = OpamFile.make (dir // f)

let config t = t /- "config"

let init_config_files () =
  List.map OpamFile.make [
    OpamFilename.Dir.of_string (OpamStd.Sys.etc ()) // "opamrc";
    OpamFilename.Dir.of_string (OpamStd.Sys.home ()) // ".opamrc";
  ]

let state_cache t = t / "repo" // "state.cache"

let lock t = t // "lock"

let config_lock t = t // "config.lock"

let archives_dir t = t / "archives"

let archive t nv = archives_dir t // (OpamPackage.to_string nv ^ "+opam.tar.gz")

let repos_lock t = t / "repo" // "lock"

let repos_config t = t / "repo" /- "repos-config"

let init  t = t / "opam-init"

let log t = t / "log"

let backup_file =
  let file = lazy Unix.(
      let tm = gmtime (Unix.gettimeofday ()) in
      Printf.sprintf "state-%04d%02d%02d%02d%02d%02d.export"
        (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    ) in
  fun () -> Lazy.force file

let backup_dir t = t / "backup"

let backup t = backup_dir t /- backup_file ()

module Switch = struct

  let root t a = OpamSwitch.get_root t a

  (** Internal files and dirs with static location *)

  let meta_dirname = ".opam-switch"

  let meta t a = root t a / meta_dirname

  let lock t a = meta t a // "lock"

  let backup_dir t a = meta t a / "backup"

  let backup t a = backup_dir t a /- backup_file ()

  let selections t a = meta t a /- "switch-state"

  let build_dir t a = meta t a / "build"

  let build t a nv = build_dir t a / OpamPackage.to_string nv

  let remove_dir t a = meta t a / "remove"

  let remove t a nv = remove_dir t a / OpamPackage.to_string nv

  let install_dir t a = meta t a / "install"

  let install t a n = install_dir t a /- (OpamPackage.Name.to_string n ^ ".install")

  let changes t a n = install_dir t a /- (OpamPackage.Name.to_string n ^ ".changes")

  let reinstall t a = meta t a /- "reinstall"

  let switch_config t a = meta t a /- "switch-config"

  let config_dir t a = meta t a / "config"

  let config t a n =
    config_dir t a /- (OpamPackage.Name.to_string n ^ ".config")

  let sources_dir t a = meta t a / "sources"

  let sources t a nv = sources_dir t a / OpamPackage.to_string nv

  let pinned_package t a name = sources_dir t a / OpamPackage.Name.to_string name

  let env_filename = "environment"

  let environment t a = meta t a /- env_filename

  let env_relative_to_prefix pfx = pfx / meta_dirname /- env_filename

  let installed_opams t a = meta t a / "packages"

  let installed_package_dir t a nv =
    installed_opams t a / OpamPackage.to_string nv

  let installed_opam t a nv =
    installed_package_dir t a nv /- "opam"

  let installed_opam_files_dir t a nv =
    installed_package_dir t a nv / "files"

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

  let lookup stdpath relative_to default config =
    let dir =
      OpamStd.Option.default default
        (OpamFile.Switch_config.path config stdpath)
    in
    if Filename.is_relative dir then
      if dir = "" then relative_to else relative_to / dir
    else OpamFilename.Dir.of_string dir

  let prefix t a c = lookup Prefix (root t a) "" c

  let lib_dir t a c = lookup Lib (prefix t a c) "lib" c

  let lib t a c n = lib_dir t a c / OpamPackage.Name.to_string n

  let stublibs t a c = lookup Stublibs (lib_dir t a c) "stublibs" c

  let toplevel t a c = lookup Toplevel (lib_dir t a c) "toplevel" c

  let doc_dir t a c = lookup Doc (prefix t a c) "doc" c

  let doc t a c n = doc_dir t a c / OpamPackage.Name.to_string n

  let man_dir ?num t a c =
    let base = lookup Man (prefix t a c) "man" c in
    match num with
    | None -> base
    | Some n -> base / ("man" ^ n)

  let share_dir t a c = lookup Share (prefix t a c) "share" c

  let share t a c n = share_dir t a c / OpamPackage.Name.to_string n

  let etc_dir t a c = lookup Etc (prefix t a c) "etc" c

  let etc t a c n = etc_dir t a c / OpamPackage.Name.to_string n

  let bin t a c = lookup Bin (prefix t a c) "bin" c

  let sbin t a c = lookup Sbin (prefix t a c) "sbin" c

  let get_stdpath t a c = function
    | Prefix -> prefix t a c
    | Lib -> lib_dir t a c
    | Bin -> bin t a c
    | Sbin -> sbin t a c
    | Share -> share_dir t a c
    | Doc -> doc_dir t a c
    | Etc -> etc_dir t a c
    | Man -> man_dir t a c
    | Toplevel -> toplevel t a c
    | Stublibs -> stublibs t a c

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

module Builddir = struct

  let install builddir nv =
    builddir /- (OpamPackage.Name.to_string nv.name ^ ".install")

  let config builddir nv =
    builddir /- (OpamPackage.Name.to_string nv.name ^ ".config")

end

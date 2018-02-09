(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t = (string * string list)

let unpack x = x
let custom x = x

let has_space (cmd, _) = String.contains cmd ' '
let cmd_to_string (cmd, _) = cmd

let to_string (cmd, args) =
  (* TODO: Add backslashs to quotes for each args *)
  let quote x = if String.contains x ' ' then "'" ^ x ^ "'" else x in
  let args = List.map quote args in
  String.concat " " (cmd::args)

module Unzip = struct
  let extract ~file ~dir = ("unzip", [file; "-d"; dir])
end

module Tar = struct
  let extract_gzip ~file ~dir = ("tar", ["xfz"; file; "-C"; dir])
  let extract_bzip2 ~file ~dir = ("tar", ["xfj"; file; "-C"; dir])
  let extract_xz ~file ~dir = ("tar", ["xfJ"; file; "-C"; dir])
  let extract_lzma ~file ~dir = ("tar", ["xfY"; file; "-C"; dir])

  let create_gzip ~output ~inputs = ("tar", ("czhf" :: output :: inputs))
end

module Patch = struct
  let apply ~file = ("patch", ["-p1"; "-i"; file])
end

module Diff = struct
  let dirs ~dir1 ~dir2 = ("diff", ["-ruaN"; dir1; dir2])
end

module Rm = struct
  let recursive ~dir = ("rm", ["-rf"; dir])
end

module Cmd = struct
  let rm_recursive ~dir = ("cmd", ["/d"; "/v:off"; "/c"; "rd"; "/s"; "/q"; dir])
  let get_os_version = ("cmd", ["/C"; "ver"])
end

module Sw_vers = struct
  let get_os_version = ("sw_vers", ["-productVersion"])
end

module Getprop = struct
  let get_os_version = ("getprop", ["ro.build.version.release"])
end

module Sleep = struct
  let one_second = ("sleep", ["1"])
end

module Cp = struct
  let cp ~src ~dst = ("cp", [src; dst])
  let recursive ~srcs ~dst = ("cp", ("-PRp" :: srcs @ [dst]))
end

module Mv = struct
  let mv ~src ~dst = ("mv", [src; dst])
end

module Install = struct
  let exec ~src ~dst = ("install", ["-m"; "0755"; src; dst])
  let file ~src ~dst = ("install", ["-m"; "0644"; src; dst])
end

module Sysctl = struct
  let get_hw_ncpu = ("sysctl", ["-n"; "hw.ncpu"])
end

module Getconf = struct
  let get_nproc = ("getconf", ["_NPROCESSORS_ONLN"])
end

module Lsb_release = struct
  let get_id = ("lsb_release", ["-i"; "-s"])
  let get_release = ("lsb_release", ["-s"; "-r"])
end

module Tput = struct
  let cols = ("tput", ["cols"])
end

module Stty = struct
  let size = ("stty", ["size"])
end

module Uname = struct
  let kern_name = ("uname", ["-s"])
  let kern_version = ("uname", ["-r"])
  let arch = ("uname", ["-m"])
  let freebsd_version = ("uname", ["-U"])
end

module Openssl = struct
  let sha256 ~file = ("openssl", ["sha256"; file])
  let sha512 ~file = ("openssl", ["sha512"; file])
end

module Git = struct
  let get_user_name = ("git", ["config"; "--get"; "user.name"])
  let get_user_email = ("git", ["config"; "--get"; "user.email"])
end

module OCaml = struct
  let vnum = ("ocaml", ["-vnum"])
end

module Command = struct
  let lookup ~cmd = ("/bin/sh", ["-c"; "command -v "^cmd])
end

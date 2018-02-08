(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Unzip = struct
  let extract cmd ~file ~dir = cmd "unzip" [file; "-d"; dir]
end

module Tar = struct
  let extract_gzip cmd ~file ~dir = cmd "tar" ["xfz"; file; "-C"; dir]
  let extract_bzip2 cmd ~file ~dir = cmd "tar" ["xfj"; file; "-C"; dir]
  let extract_xz cmd ~file ~dir = cmd "tar" ["xfJ"; file; "-C"; dir]
  let extract_lzma cmd ~file ~dir = cmd "tar" ["xfY"; file; "-C"; dir]

  let create_gzip cmd ~output ~inputs = cmd "tar" ("czhf" :: output :: inputs)
end

module Patch = struct
  let apply cmd ~file = cmd "patch" ["-p1"; "-i"; file]
end

module Rm = struct
  let recursive cmd ~dir = cmd "rm" ["-rf"; dir]
end

module Cmd = struct
  let rm_recursive cmd ~dir = cmd "cmd" ["/d"; "/v:off"; "/c"; "rd"; "/s"; "/q"; dir]
  let get_os_version cmd = cmd "cmd" ["/C"; "ver"]
end

module Sw_vers = struct
  let get_os_version cmd = cmd "sw_vers" ["-productVersion"]
end

module Getprop = struct
  let get_os_version cmd = cmd "getprop" ["ro.build.version.release"]
end

module Sleep = struct
  let one_second cmd = cmd "sleep" ["1"]
end

module Cp = struct
  let cp cmd ~src ~dst = cmd "cp" [src; dst]
  let recursive cmd ~srcs ~dst = cmd "cp" ("-PRp" :: srcs @ [dst])
end

module Mv = struct
  let mv cmd ~src ~dst = cmd "mv" [src; dst]
end

module Install = struct
  let exec cmd ~src ~dst = cmd "install" ["-m"; "0755"; src; dst]
  let file cmd ~src ~dst = cmd "install" ["-m"; "0644"; src; dst]
end

module Sysctl = struct
  let get_hw_ncpu cmd = cmd "sysctl" ["-n"; "hw.ncpu"]
end

module Getconf = struct
  let get_nproc cmd = cmd "getconf" ["_NPROCESSORS_ONLN"]
end

module Lsb_release = struct
  let get_id cmd = cmd "lsb_release" ["-i"; "-s"]
  let get_release cmd = cmd "lsb_release" ["-s"; "-r"]
end

module Tput = struct
  let cols cmd = cmd "tput" ["cols"]
end

module Stty = struct
  let size cmd = cmd "stty" ["size"]
end

module Uname = struct
  let kern_name cmd = cmd "uname" ["-s"]
  let kern_version cmd = cmd "uname" ["-r"]
  let arch cmd = cmd "uname" ["-m"]
  let freebsd_version cmd = cmd "uname" ["-U"]
end

module Openssl = struct
  let sha256 cmd ~file = cmd "openssl" ["sha256"; file]
  let sha512 cmd ~file = cmd "openssl" ["sha512"; file]
end

module Git = struct
  let get_user_name cmd = cmd "git" ["config"; "--get"; "user.name"]
  let get_user_email cmd = cmd "git" ["config"; "--get"; "user.email"]
end

module OCaml = struct
  let vnum cmd = cmd "ocaml" ["-vnum"]
end

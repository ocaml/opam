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

include OpamStd.AbstractString

let unset = of_string "#unset#"

let not_installed s =
  OpamConsole.error_and_exit
    "The selected compiler switch %s is not installed. Please choose a \
     different one using the 'opam switch' command."
    (to_string s)

let is_external s =
  OpamStd.String.starts_with ~prefix:"." s ||
  OpamStd.String.contains ~sub:Filename.dir_sep s

let external_dirname = "_opam"

let of_string s =
  if is_external s then OpamFilename.Dir.(to_string (of_string s))
  else s

let get_root root s =
  if is_external s
  then OpamFilename.Op.(OpamFilename.Dir.of_string s / external_dirname)
  else OpamFilename.Op.(root / s)

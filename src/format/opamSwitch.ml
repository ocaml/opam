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

let is_external s =
  OpamStd.String.starts_with ~prefix:"." s ||
  OpamStd.String.contains ~sub:Filename.dir_sep s

let external_dirname = "_opam"

let of_string s =
  if is_external s then OpamFilename.Dir.(to_string (of_string s))
  else s

let of_dirname d =
  let s = OpamFilename.Dir.to_string d in
  try
    let swdir = Filename.concat s external_dirname in
    let r = OpamSystem.real_path Filename.(concat s (Unix.readlink swdir)) in
    if Filename.basename r = external_dirname then Filename.dirname r else s
  with Unix.Unix_error _ -> s

let get_root root s =
  if is_external s
  then OpamFilename.Dir.of_string (Filename.concat s external_dirname)
  else OpamFilename.Op.(root / s)

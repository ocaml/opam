(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2018 OCamlPro                                        *)
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
  OpamFilename.is_rel_seg s ||
  OpamCompat.String.exists OpamFilename.is_dir_sep s

let external_dirname = "_opam"

let check s =
  if s = "" ||
     let re =
       Re.(compile @@
           seq [
             bol;
             opt @@ seq [ wordc ; char ':'; set "/\\" ];
             rep @@ diff any @@ set "<>!`$():";
             eol
           ])
     in
     (try
        let _ : Re.Group.t = Re.exec re s in
        false
      with Not_found -> true)
  then
    failwith (Printf.sprintf "Invalid character in switch name %S" s);
  s

let of_string s =
  check @@
  if is_external s then OpamFilename.Dir.(to_string (of_string s))
  else s

let of_dirname d =
  let s = OpamFilename.Dir.to_string d in
  check @@
  try
    let swdir = Unix.readlink (Filename.concat s external_dirname) in
    let swdir =
      if Filename.is_relative swdir then Filename.concat s swdir else swdir
    in
    let r = OpamSystem.real_path swdir in
    if Filename.basename r = external_dirname then Filename.dirname r else s
  with Unix.Unix_error _ -> s

let get_root root s =
  if is_external s
  then OpamFilename.Dir.of_string (Filename.concat s external_dirname)
  else OpamFilename.Op.(root / s)

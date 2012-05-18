(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(* Small helper to create .install.in files *)

let version () =
  Printf.printf "\
%s version %s

Copyright (C) 2012 OCamlPro - INRIA

This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
    Sys.argv.(0) Globals.version;
  exit 0

let usage =
  Printf.sprintf "%s -package <name> [-lib <name>|-syntax <name>|-bin <name>]+" Sys.argv.(0)

let p = ref ""

let sections = ref []
let add x = sections := x :: !sections
let add_l x = add ("lib", x)
let add_s x = add ("lib", x)
let add_b x = add ("bin", x)

let specs = Arg.align [
  ("-version", Arg.Unit version, " display version information");
  ("-bin"    , Arg.String add_b, "<name> add a library");
  ("-lib"    , Arg.String add_l, "<name> add a library");
  ("-syntax" , Arg.String add_s, "<name> add a syntax extension");
  ("-package", Arg.Set_string p, "<name> set the package name");
]

let ano x =
  Printf.eprintf "Don't know what to do with %s\n" x;
  exit 1

let _ =
  Arg.parse specs ano usage

let package =
  if !p <> "" then (
    !p
  ) else (
    Printf.eprintf "Missing package name\n";
    exit 1
  )

let sections = List.rev !sections

let () =
  let oc = open_out (package ^ ".install") in
  let libs = List.filter (fun (s,_) -> s = "lib") sections in
  let bins = List.filter (fun (s,_) -> s = "bin") sections in
  if libs <> [] then (
    Printf.fprintf oc "lib: [";
    List.iter (fun (_, name) -> Printf.fprintf oc "%S " name) libs;
    Printf.fprintf oc "]\n"
  );
  if bins <> [] then (
    Printf.fprintf oc "bin: [";
    List.iter (fun (_, name) -> Printf.fprintf oc "%s " name) libs;
    Printf.fprintf oc "]\n"
  )

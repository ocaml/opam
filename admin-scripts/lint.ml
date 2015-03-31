#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;
open Opam_admin_top;;

let includes = ref []
let excludes = ref []
let short = ref false
let list = ref false

let usage =
  "Arguments:\n\
  \  -s\tshort format, don't print explanations\n\
  \  -l\tlist format, only print package names\n\
  \  [N]\tshow only the listed warnings\n\
  \  -[N]\tskip any packages that trigger any of these warnings\n\
  "

let () =
  let args = match Array.to_list Sys.argv with
    | _::args -> args
    | [] -> []
  in
  List.iter (function
      | "-s" -> short := true
      | "-l" -> list := true
      | a ->
        try
          if String.length a > 0 && a.[0] = '-' then
            excludes := 0 - int_of_string a :: !excludes
          else
            includes := int_of_string a :: !includes
        with Failure _ ->
          OpamGlobals.msg "%s" usage;
          OpamGlobals.exit 2)
    args

let () =
  let quiet = !short || !list in
  iter_packages ~quiet ~opam:(fun nv opam ->
      let w = OpamFile.OPAM.validate opam in
      if List.exists (fun (n,_,_) -> List.mem n !excludes) w then opam else
      let w =
        if !includes = [] then w
        else List.filter (fun (n,_,_) -> List.mem n !includes) w
      in
      if w <> [] then
        if !list then
          print_endline (OpamPackage.to_string nv)
        else if !short then
          OpamGlobals.msg "%s %s\n" (OpamPackage.to_string nv)
            (OpamMisc.List.concat_map " " (fun (n,k,_) ->
                 OpamGlobals.colorise
                   (match k with `Warning -> `yellow | `Error -> `red)
                   (string_of_int n))
                w)
        else
          OpamGlobals.msg "\r\027[KIn %s:\n%s\n"
            (OpamPackage.to_string nv)
            (OpamFile.OPAM.warns_to_string w);
      opam
    ) ()

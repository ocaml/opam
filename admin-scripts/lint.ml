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
          OpamConsole.msg "%s" usage;
          OpamStd.Sys.exit 2)
    args

let () =
  OpamPackage.Map.iter (fun nv prefix ->
      let opam_file = OpamRepositoryPath.opam repo prefix nv in
      let w, _ = OpamFileTools.lint_file opam_file in
      if List.exists (fun (n,_,_) -> List.mem n !excludes) w then () else
      let w =
        if !includes = [] then w
        else List.filter (fun (n,_,_) -> List.mem n !includes) w
      in
      if w <> [] then
        if !list then
          print_endline (OpamPackage.to_string nv)
        else if !short then
          OpamConsole.msg "%s %s\n" (OpamPackage.to_string nv)
            (OpamStd.List.concat_map " " (fun (n,k,_) ->
                 OpamConsole.colorise
                   (match k with `Warning -> `yellow | `Error -> `red)
                   (string_of_int n))
                w)
        else
          OpamConsole.msg "\r\027[KIn %s:\n%s\n"
            (OpamPackage.to_string nv)
            (OpamFileTools.warns_to_string w))
    (OpamRepository.packages_with_prefixes repo)

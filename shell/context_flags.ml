#directory "+compiler-libs";;
#load "ocamlcommon.cma";;

let p = String.index Sys.ocaml_version '.' in
let _ocaml_major = String.sub Sys.ocaml_version 0 p |> int_of_string in
let p = succ p in
let _ocaml_minor = String.sub Sys.ocaml_version p (String.index_from Sys.ocaml_version p '.' - p) |> int_of_string in
match Sys.argv.(1) with
| "flags" ->
    print_string "()"
| "mingw-arch" ->
    if Config.system = "mingw64" then
      print_string "x86_64"
    else
      print_string "i686"
| "clibs" ->
    if Sys.win32 then
      let common =
        "-ladvapi32 -lgdi32 -luser32 -lshell32 -lole32 -luuid -luserenv"
      in
      if Config.system = "mingw" then
        (* This appears to be a packaging bug in i686-w64-mingw32, as
           GetFileVersionInfoEx and friends are include in the x86_64 copy of
           libversion.a *)
        Printf.printf "(%s -lwindowsapp)" common
      else
        Printf.printf "(%s)" common
    else
      print_string "()"
| _ ->
    Printf.eprintf "Unrecognised context instruction: %s\n" Sys.argv.(1);
    exit 1

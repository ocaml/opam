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
      print_string "(-ladvapi32 -lgdi32 -luser32 -lshell32 -lpdh)"
    else
      print_string "()"
| _ ->
    Printf.eprintf "Unrecognised context instruction: %s\n" Sys.argv.(1);
    exit 1

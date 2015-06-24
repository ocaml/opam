(* Taken from ocurl - https://github.com/ygrek/ocurl/blob/b537a2b7259d2230b66d6aa3e104cb0e54beddc5/print_ext.ml *)
#use "topfind";;
#require "compiler-libs.common";;
set_binary_mode_out stdout true;;
match List.tl (Array.to_list Sys.argv) with
| ["dll"] -> print_endline Config.ext_dll
| ["obj"] -> print_endline Config.ext_obj
| ["lib"] -> print_endline Config.ext_lib
| ["arch"] -> print_endline Config.architecture
| ["ccomp_type"] -> print_endline Config.ccomp_type
| ["os_type"] -> print_endline Sys.os_type
| _ -> prerr_endline "print_config.ml: wrong usage"; exit 2

#directory "+compiler-libs";;
#load "ocamlcommon.cma";;
set_binary_mode_out stdout true;;
match List.tl (Array.to_list Sys.argv) with
| ["dll"] -> print_endline Config.ext_dll
| ["obj"] -> print_endline Config.ext_obj
| ["lib"] -> print_endline Config.ext_lib
| ["arch"] -> print_endline Config.architecture
| ["ccomp_type"] -> print_endline Config.ccomp_type
| ["system"] -> print_endline Config.system
| ["os_type"] -> print_endline Sys.os_type
| _ -> prerr_endline "print_config.ml: wrong usage"; exit 2

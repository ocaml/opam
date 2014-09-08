#!/usr/bin/env opam-admin.top

#directory "+../cudf";;
#directory "+../dose3";;
#directory "+../opam-lib";;
open Opam_admin_top;;

let cudf_pp cpkg =
  (try Cudf.lookup_package_property cpkg OpamCudf.s_source
   with Not_found -> Common.CudfAdd.decode cpkg.Cudf.package),
  (try Cudf.lookup_package_property cpkg OpamCudf.s_source_number
   with Not_found -> Printf.sprintf "#cudf%d" cpkg.Cudf.version),
  []

let _ =
  match Cudf_parser.load_from_file Sys.argv.(1) with
  | Some preamble, univ, Some req ->
    begin match Algo.Depsolver.check_request ~explain:true (preamble, univ, req) with
      | Algo.Depsolver.Unsat (Some f) -> 
        OpamGlobals.msg "== DOSE MESSAGE ==\n";
        flush stdout;
        Algo.Diagnostic.fprintf_human
          ~pp:cudf_pp
          Format.err_formatter
          f;
        flush stderr;
        begin match OpamCudf.make_conflicts univ f with
          | OpamTypes.Conflicts cs ->
            OpamGlobals.msg "== OPAM MESSAGE ==\n%s\n"
              (OpamCudf.string_of_conflict (fun a -> Printf.sprintf "%s unavailable" (OpamFormula.string_of_atom a)) cs)
          | _ -> prerr_endline "unhandled case"
        end
      | _ -> ()
    end
  | _ -> OpamGlobals.error_and_exit "unsupported cudf file"

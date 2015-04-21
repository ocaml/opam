#!/usr/bin/env opam-admin.top

#directory "+../cudf";;
#directory "+../dose3";;
#directory "+../opam-lib";;
open Opam_admin_top;;

let cudf2opam_name cpkg =
  OpamPackage.Name.of_string
    (try Cudf.lookup_package_property cpkg OpamCudf.s_source
     with Not_found -> Common.CudfAdd.decode cpkg.Cudf.package)

let cudf2opam_version cpkg =
  OpamPackage.Version.of_string
    (try Cudf.lookup_package_property cpkg OpamCudf.s_source_number
     with Not_found -> Printf.sprintf "#cudf%d" cpkg.Cudf.version)

let cudf_pp cpkg =
  OpamPackage.Name.to_string (cudf2opam_name cpkg),
  OpamPackage.Version.to_string (cudf2opam_version cpkg),
  []

let rebuild_version_map univ =
  Cudf.fold_packages (fun acc cpkg ->
      let nv =
        OpamPackage.create (cudf2opam_name cpkg) (cudf2opam_version cpkg)
      in
      OpamPackage.Map.add nv cpkg.Cudf.version acc
    )
    OpamPackage.Map.empty univ

let _ =
  match Cudf_parser.load_from_file Sys.argv.(1) with
  | Some preamble, univ, Some req ->
    begin match Algo.Depsolver.check_request ~explain:true (preamble, univ, req) with
      | Algo.Depsolver.Unsat (Some f) -> 
        OpamConsole.msg "== DOSE MESSAGE ==\n";
        flush stdout;
        Algo.Diagnostic.fprintf_human
          ~pp:cudf_pp
          Format.err_formatter
          f;
        flush stderr;
        let version_map = rebuild_version_map univ in
        begin match OpamCudf.make_conflicts ~version_map univ f with
          | OpamTypes.Conflicts cs ->
            OpamConsole.msg "== OPAM MESSAGE ==\n%s\n"
              (OpamCudf.string_of_conflict (fun a -> Printf.sprintf "%s unavailable" (OpamFormula.string_of_atom a)) cs)
          | _ -> prerr_endline "unhandled case"
        end
      | _ -> ()
    end
  | _ -> OpamConsole.error_and_exit "unsupported cudf file"

(* OPAM - PEF interface *)

open OpamTypes
open OpamTypesBase

let dose_solver_callback ~criteria (_,universe,_ as cudf) =
  let solver_in =
    OpamFilename.of_string (OpamSystem.temp_file "solver-in") in
  let solver_out =
    OpamFilename.of_string (OpamSystem.temp_file "solver-out") in
  try
    let _ =
      let oc = OpamFilename.open_out solver_in in
      Cudf_printer.pp_cudf oc cudf;
      close_out oc
    in
    let solver_command =
      match
        OpamSolverConfig.external_solver_command
          ~input:(OpamFilename.to_string solver_in)
          ~output:(OpamFilename.to_string solver_out)
          ~criteria
      with
      | Some c -> c
      | None -> raise (Common.CudfSolver.Error "External solver misconfigured")
    in
    OpamSystem.command
      ~verbose:(OpamCoreConfig.(!r.debug_level >= 2))
      solver_command;
    OpamFilename.remove solver_in;
    if not (OpamFilename.exists solver_out) then
      raise (Common.CudfSolver.Error "no output")
    else if
      (let ic = OpamFilename.open_in solver_out in
       try
         let i = input_line ic in close_in ic;
         i = "FAIL"
       with End_of_file -> close_in ic; false)
    then
      raise Common.CudfSolver.Unsat
    else
    let r =
      Cudf_parser.load_solution_from_file
        (OpamFilename.to_string solver_out) universe in
    OpamFilename.remove solver_out;
    r
  with e ->
    OpamFilename.remove solver_in;
    OpamFilename.remove solver_out;
    raise e

let pp_pkg fmt (s,univ) =
  try
    let p = Common.CudfAdd.Cudf_set.choose s in
    let pkg = Hashtbl.find univ (p.Cudf.package,p.Cudf.version) in
    pkg#pp fmt 
  with Not_found -> failwith "internal error"
;;

let call_external_solver request opampkglist =
  let tables = Pef.Pefcudf.init_tables Versioning.Debian.compare opampkglist in
  let from_cudf (p,i) = (p, Pef.Pefcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p, Pef.Pefcudf.get_cudf_version tables (p,v)) in
  let univ = Hashtbl.create (2*(List.length opampkglist)-1) in
  let options = { Opam.Opamcudf.switch = request.Opam.Packages.switch; switches = []; profiles = []; depopts = false} in

  let cudfpkglist =
      List.map (fun pkg ->
        let pl = Opam.Opamcudf.tocudf tables ~options pkg in
        List.fold_left (fun acc1 p ->
          if not(Hashtbl.mem univ (p.Cudf.package,p.Cudf.version)) then begin
            Hashtbl.add univ (p.Cudf.package,p.Cudf.version) pkg;
            p :: acc1
          end else begin
            Printf.eprintf "Duplicated package (same version, name) : (%s,%s)"
              pkg#name pkg#version;
            acc1
          end
        ) [] pl
      ) opampkglist
  in
  let universe = Cudf.load_universe (List.flatten cudfpkglist) in

  let cudf_request = Opam.Opamcudf.requesttocudf tables universe request in
  let cudf = (Opam.Opamcudf.preamble,universe,cudf_request) in

  let criteria = request.Opam.Packages.preferences in

  let cudfdump = Filename.temp_file "opam-universe" ".cudf" in
  begin
    let oc = open_out cudfdump in
    Printf.fprintf oc "#criteria: %s\n" criteria;
    Cudf_printer.pp_preamble oc Opam.Opamcudf.preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_universe oc universe;
    Cudf_printer.pp_request oc cudf_request;
    close_out oc
  end;

  let solpre,soluniv =
    match
      Algo.Depsolver.check_request_using 
        ~call_solver:(dose_solver_callback ~criteria) 
        ~criteria 
        ~explain:true cudf 
    with
    |Algo.Depsolver.Error s -> failwith s
    |Algo.Depsolver.Unsat None ->
      failwith "(UNSAT) No Solutions according to the given preferences"
    |Algo.Depsolver.Unsat Some d -> begin
      let pp = Common.CudfAdd.pp from_cudf in
      Format.printf "Error: (UNSAT) No Solutions according to the given preferences\n";
      Format.printf "%a\n" (Algo.Diagnostic.fprintf_human ~prefix:"Message: " ~pp) d;
      exit 1
    end
    |Algo.Depsolver.Sat s -> s
  in

  let diff = Common.CudfDiff.diff universe soluniv in
  let empty = ref true in
  let cache = Common.CudfAdd.Cudf_hashtbl.create 1023 in
  Hashtbl.iter (fun pkgname s ->
    let inst = s.Common.CudfDiff.installed in
    let rem = s.Common.CudfDiff.removed in
    match Common.CudfAdd.Cudf_set.is_empty inst, Common.CudfAdd.Cudf_set.is_empty rem with
    |false,_ -> begin
      empty := false;
      List.iter (fun pkg -> Common.CudfAdd.Cudf_hashtbl.add cache pkg ()) (Common.CudfAdd.Cudf_set.elements inst);
      Printf.printf "Install:\n%a\n" pp_pkg (inst,univ)
    end
    |true,false -> begin
      empty := false;
      Printf.printf "Remove:\n%a\n" pp_pkg (rem,univ)
    end
    |true,true -> ()
  ) diff;

  (* Print also all packages that are were requested, but don't show up in the 
   * diff because already installed *)
  List.iter (fun (n,c) ->
    try
      let s = Hashtbl.find diff n in
      if (Common.CudfAdd.Cudf_set.is_empty s.Common.CudfDiff.installed) then begin
        List.iter (fun pkg ->
          empty := false;
          if Common.CudfAdd.Cudf_hashtbl.mem cache pkg then ()
          else begin
            Common.CudfAdd.Cudf_hashtbl.add cache pkg ();
            Printf.printf "Install:\n%a" pp_pkg ((Common.CudfAdd.Cudf_set.singleton pkg),univ);
          end
        ) (Common.CudfAdd.who_provides soluniv (n,c))
      end
    with Not_found -> ()
  ) cudf_request.Cudf.install;

  Printf.printf "%!"
;;

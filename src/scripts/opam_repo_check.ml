(* Script to check that a given repository is well-typed (or well-parsed) *)
open OpamTypes
open OpamFilename.OP

let () =
  let usage = Printf.sprintf "Usage: %s" Sys.argv.(0) in
  let specs = [
    ("--version", Arg.Unit OpamGlobals.version_msg, " Display version information")
  ] in
  let ano x =
    Printf.eprintf "%s: invalid argument" x in
  Arg.parse specs ano usage

let () =
  let t = OpamPath.Repository.raw (OpamFilename.cwd ()) in
  OpamPackage.Set.iter (fun nv ->
    OpamGlobals.msg "Processing (package) %s\n" (OpamPackage.to_string nv);

    (** Descr *)
    let descr = OpamPath.Repository.descr t nv in
    OpamFile.Descr.write descr (OpamFile.Descr.read descr);

    (** OPAM *)
    let opam = OpamPath.Repository.opam t nv in
    OpamFile.OPAM.write opam (OpamFile.OPAM.read opam);

    (** URL *)
    let url = OpamPath.Repository.url t nv in
    if OpamFilename.exists url then (
      OpamFile.URL.write url (OpamFile.URL.read url);
    );

    (** Dot_install *)
    let dot_install = OpamPath.Repository.files t nv // (OpamPackage.Name.to_string (OpamPackage.name nv) ^ ".install") in
    if OpamFilename.exists dot_install then (
      OpamFile.Dot_install.Raw.write dot_install (OpamFile.Dot_install.Raw.read dot_install);
    );
  ) (OpamRepository.packages t);

  (** Comp *)
  let comps = OpamFilename.list_files (OpamPath.Repository.compilers_dir t) in
  List.iter (fun comp ->
    let comp_ = OpamFile.Comp.read comp in
    OpamGlobals.msg "Processing (compiler) %s\n" (OpamVersion.Compiler.to_string (OpamFile.Comp.name comp_));
    OpamFile.Comp.write comp comp_;
  ) comps

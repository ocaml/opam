(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 Thomas Gazagnaire <thomas@gazagnaire.org>            *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(* Script to add findlib info *)
open OpamTypes
open OpamFilename.OP
open OpamMisc.OP

module StringSet = OpamMisc.StringSet

let () =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir

type args = {
  opam_pkgs: string list;
  findlib_pkgs: string list;
  infer: bool;
  install: bool;
  force: bool;
  orphans: bool;
}

let args =
  let open Cmdliner in
  let infer =
    let doc = "Infer the `findlib' file by looking at the contents of the \
               `remove` field." in
    Arg.(value & flag & info ~doc ["infer"]) in
  let install =
    let doc = "Try to install the package if is not already installed and no \
               previous information is known about that package." in
    Arg.(value & flag & info ~doc ["install"]) in
  let force =
    let doc = "Used in conjunction with `--infer` or `--install` to force \
               the installation of packages, even if previous information \
               about `findlib' libraries are already known for that package."
    in
    Arg.(value & flag & info ~doc ["force"]) in
  let orphans =
    let doc = "Display the orphans findlib packages" in
    Arg.(value & flag & info ~doc ["orphans"]) in
  let findlib_pkgs =
    let doc = "Findlib package name" in
    Arg.(value & opt (list string) [] & info ["pkg"] ~doc
           ~docv:"FINDLIB-PKGS")
  in
  let opam_pkgs =
    let doc = "OPAM package pattern" in
    Arg.(value & pos_all string [] & info [] ~doc
           ~docv:"OPAM-PKG")
  in
  Term.(pure (fun infer install force orphans findlib_pkgs opam_pkgs ->
      { infer; install; force; orphans; findlib_pkgs; opam_pkgs }
    ) $ infer $ install $ force $ orphans $ findlib_pkgs $ opam_pkgs)

let state = lazy (OpamState.load_state "opam-admin-findlib")

let installed_findlibs () =
  let { OpamState.Types.root; switch; _ } = Lazy.force state in
  let libdir = OpamPath.Switch.lib_dir root switch in
  let dirs = OpamFilename.dirs libdir in
  let libs = List.fold_left (fun acc dir ->
      let file = dir // "META" in
      if OpamFilename.exists file then
        let lib = Filename.basename (OpamFilename.Dir.to_string dir) in
        StringSet.add lib acc
      else
        acc
    ) StringSet.empty dirs in
  let files = OpamFilename.files libdir in
  let libs = List.fold_left (fun acc file ->
      let raw = Filename.basename (OpamFilename.to_string file) in
      let prefix = "META." in
      if OpamMisc.starts_with ~prefix raw then
        let lib = OpamMisc.remove_prefix ~prefix raw in
        StringSet.add lib acc
      else
        acc
    ) libs files in
  libs

let declared_findlibs repo packages =
  let aux package acc =
    try
      let prefix = OpamPackage.Map.find package packages in
      let findlib_f =
        OpamPath.Repository.packages repo prefix package // "findlib"
      in
      let findlib = OpamFile.Lines.safe_read findlib_f in
      let findlib = StringSet.of_list (List.flatten findlib) in
      StringSet.union acc findlib
    with Not_found ->
      acc
  in
  let { OpamState.Types.installed } = Lazy.force state in
  OpamPackage.Set.fold aux installed StringSet.empty

let infer_from_remove_command opam =
  let cmds = OpamFile.OPAM.remove opam in
  List.fold_left (fun acc (cmd: OpamTypes.command) ->
      match fst cmd with
      | (CString "ocamlfind",_) :: l ->
        let pkgs = OpamMisc.filter_map (function
            | CString s, _ -> Some s
            | _ -> None
          ) (List.tl l) in
        StringSet.union acc (StringSet.of_list pkgs);
      | _ -> acc
    ) StringSet.empty cmds

let infer_from_name args package =
  let { OpamState.Types.root; switch; installed } = Lazy.force state in
  if args.install && not (OpamPackage.Set.mem package installed) then (
    let atom =
      OpamPackage.name package, Some (`Eq, OpamPackage.version package)
    in
    let yes = !OpamGlobals.yes in
    OpamGlobals.yes := true;
    (* XXX: we could just install the dependencies, snapshot
      the build filsystem, install the package and look at the
      filesystem diff. We don't do this because I think this is
      not very useful in practice: the current heuristic will work
      well enough. *)
    begin
      try OpamClient.SafeAPI.install [atom] None false
      with OpamGlobals.Exit _ -> ()
    end;
    OpamGlobals.yes := yes;
  );
  let name = OpamPackage.name package in
  let name_str = OpamPackage.Name.to_string name in
  let meta = OpamPath.Switch.lib root switch name // "META" in
  let meta_dot = OpamPath.Switch.lib_dir root switch // ("META." ^ name_str) in
  if OpamFilename.exists meta || OpamFilename.exists meta_dot then
    StringSet.singleton name_str
  else
  StringSet.empty

let process args =
  let repo = OpamRepository.local (OpamFilename.cwd ()) in
  let packages = OpamRepository.packages_with_prefixes repo in
  if args.orphans then
    let orphans =
      StringSet.diff (installed_findlibs ()) (declared_findlibs repo packages)
    in
    match StringSet.elements orphans with
    | []      -> ()
    | orphans -> OpamGlobals.msg "%s\n" (String.concat "\n" orphans)
  else if args.infer || args.opam_pkgs <> [] || args.findlib_pkgs <> [] then
  let regexps =
    List.map (fun pattern ->
        if OpamPackage.Map.exists (fun pkg _ ->
            OpamPackage.Name.to_string (OpamPackage.name pkg) = pattern
          ) packages
        then pattern ^ ".*"
        else pattern
      ) args.opam_pkgs
    |> List.map (fun pattern -> Re.compile (Re_glob.globx pattern))
  in
  let should_process package = match regexps with
    | [] -> true
    | _  ->
      let str = OpamPackage.to_string package in
      List.exists (fun re -> OpamMisc.exact_match re str) regexps
  in
  OpamPackage.Map.iter (fun package prefix ->
      if should_process package then (
        let opam_f = OpamPath.Repository.opam repo prefix package in
        let filename = OpamFilename.dirname opam_f // "findlib" in
        if OpamFilename.exists filename && not args.force then
          ()
        else (
          OpamGlobals.msg "Processing %s\n" (OpamPackage.to_string package);
          let opam = OpamFile.OPAM.read opam_f in
          let pkgs0 =
            OpamFile.Lines.safe_read filename
            |> List.flatten
            |> StringSet.of_list
          in
          let pkgs1 =
            if args.infer then (
              StringSet.union
                (infer_from_name args package)
                (infer_from_remove_command opam)
            ) else StringSet.of_list args.findlib_pkgs
          in
          let pkgs = StringSet.union pkgs0 pkgs1 in
          let contents = List.map (fun x -> [x]) (StringSet.elements pkgs) in
          if contents <> [] then OpamFile.Lines.write filename contents)
      )
    ) packages
  else
    OpamGlobals.msg "Nothing to do.\n"

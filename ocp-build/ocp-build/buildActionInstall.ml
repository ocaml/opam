(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

(* ocp-build install [OPTIONS]

  Set the options of the user preference file.

*)

(* open BuildBase *)
open BuildArgs
open BuildOptions
(* open Stdlib2 *)
open SimpleConfig

open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions
open BuildArgs
open BuildTerm
open BuildActions


let do_install bc install_where install_what projects =

  let already_installed =
    List.map (fun pj -> pj.lib_name)
      (List.filter
         (fun pj -> pj.lib_install &&
                    BuildOCamlInstall.is_installed install_where pj.lib_name)
         projects)
  in
  let bold s =
    if term.esc_ansi then Printf.sprintf "\027[1m%s\027[m" s else s
  in
  if already_installed <> [] then
    if !BuildArgs.auto_uninstall then begin
      Printf.printf "Packages %s are already installed, removing first...\n"
        (String.concat ", " (List.map bold already_installed));
      let uninstall_state = BuildOCamlInstall.uninstall_init install_where in
      List.iter
        (BuildOCamlInstall.uninstall_by_name uninstall_state)
        already_installed;
      BuildOCamlInstall.uninstall_finish uninstall_state
    end else begin
      Printf.eprintf "Error: Packages %s are already installed."
        (String.concat ", " (List.map bold already_installed));
      BuildMisc.clean_exit 2
    end;

  let projects_to_install = ref StringMap.empty in
  let rec add_to_install pj =
    if pj.lib_install &&
       not (StringMap.mem pj.lib_name !projects_to_install) then begin
      projects_to_install :=
        StringMap.add pj.lib_name pj !projects_to_install;
      let bundle =
        BuildOCPVariable.get_strings_with_default [pj.lib_options]
          "bundle" [] in
      List.iter (fun name ->
        try
          let pj2 = StringMap.find name bc.packages_by_name in
          pj2.lib_bundles <- pj :: pj2.lib_bundles
        with Not_found ->
          Printf.eprintf
            "Error: package %S bundled in package %S, not found\n%!"
            pj.lib_name name;
          BuildMisc.clean_exit 2
      ) bundle
    end
  in

  List.iter add_to_install projects;
  let install_errors = ref 0 in
  let install_ok = ref 0 in
  StringMap.iter (fun _ pj ->
    if pj.lib_install then
      match       BuildOCamlInstall.find_installdir
          install_where install_what
          pj.lib_name with
        None -> incr install_errors
      | Some installdir ->
        BuildOCamlInstall.install
          install_where install_what
          pj installdir;
        incr install_ok
  )
    !projects_to_install;
  if !install_errors > 0 then begin
    if !install_ok = 0 then
      Printf.eprintf "Install completely failed\n%!"
    else
      Printf.eprintf
        "Install partially failed: %d/%d packages not installed"
        !install_errors (!install_errors + !install_ok);
    BuildMisc.clean_exit 2
  end


let arg_list =
  BuildOptions.merge
    [
      [
  "-install-bundle", Arg.String (fun s ->
    Printf.eprintf "Warning: option -install-bundle is obsolete\n%!"
    ),
  "BUNDLE Install a bundle packages to uninstall all\n  packages at once";

      ];
      BuildActionBuild.arg_list
    ]



let action () =
  let p = BuildActions.load_project () in
  let (bc, projects) = BuildActionBuild.do_build p in

  let install_what =

    let open BuildOCamlInstall in
    {
      install_asm_bin = true;
      install_byte_bin = true;
      install_asm_lib = true;
      install_byte_lib = true;
    }
  in



  do_install bc (install_where p) install_what projects;
  ()



let subcommand = {
  sub_name = "install";
  sub_help =  "Install the project.";
  sub_arg_list = arg_list;
  sub_arg_anon = Some arg_anon;
  sub_arg_usage = [ "Install the project."; ];
  sub_action = action;
}


(**************************************************************************)
(*                                                                        *)
(*    Copyright 2013 OCamlPro                                             *)
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

open OpamTypes
open Cmdliner

type options = {
  file: OpamFilename.t;
  pkgname: OpamPackage.Name.t;
  prefix: OpamFilename.Dir.t;
  script: bool;
  mandir: OpamFilename.Dir.t option;
  libdir: OpamFilename.Dir.t option;
  stubsdir: OpamFilename.Dir.t option;
  topdir: OpamFilename.Dir.t option;
  docdir: OpamFilename.Dir.t option;
}

(* A wrapper on top of commands to either proceed, or output a script *)
type commands = {
  mkdir: OpamFilename.Dir.t -> unit;
  rmdir: opt:bool -> OpamFilename.Dir.t -> unit;
  cp: ?exec:bool -> opt:bool -> src:OpamFilename.t -> dst:OpamFilename.t -> unit -> unit;
  rm: opt:bool -> OpamFilename.t -> unit;
  confirm: string -> (unit -> unit) -> unit;
}

let do_commands project_root =
  let mkdir d =
    if not (OpamFilename.exists_dir d) then
      (OpamGlobals.msg "Creating directory %s\n%!" (OpamFilename.Dir.to_string d);
       OpamFilename.mkdir d)
  in
  let rec rmdir ~opt d =
    if not (OpamFilename.exists_dir d) then ()
    else if Sys.readdir (OpamFilename.Dir.to_string d) = [||] then
      (OpamGlobals.msg "Removing empty dir %S\n" (OpamFilename.Dir.to_string d);
       OpamFilename.rmdir d;
       let parent = OpamFilename.dirname_dir d in
       if parent <> d then rmdir ~opt:true parent)
    else if not opt then
      OpamGlobals.warning "Directory %S is not empty\n" (OpamFilename.Dir.to_string d)
  in
  let cp ?exec ~opt ~src ~dst () =
    let print src dst =
      OpamGlobals.msg "%-32s => %s\n"
        (OpamFilename.remove_prefix project_root src)
        (OpamFilename.to_string dst)
    in
    if OpamFilename.exists src then
      (mkdir (OpamFilename.dirname dst);
       print src dst;
       OpamFilename.install ?exec ~src ~dst ();
       if exec <> Some true &&
          OpamFilename.ends_with ".cmi" src &&
          OpamFilename.ends_with ".cmi" dst then
         let install_ext e =
           let replace_ext f =
             OpamFilename.add_extension (OpamFilename.chop_extension f)
           in
           let src = replace_ext src e in
           if OpamFilename.exists src then
             let dst = replace_ext dst e in
             print src dst;
             OpamFilename.install ~src ~dst ()
         in
         install_ext "cmti";
         install_ext "cmt")
    else if not opt then
      OpamGlobals.error "Could not find %S" (OpamFilename.to_string src)
  in
  let rm ~opt f =
    if OpamFilename.exists f then
      (OpamGlobals.msg "Removing %s\n" (OpamFilename.to_string f);
       OpamFilename.remove f)
    else if not opt then
      OpamGlobals.warning "%S doesn't exist" (OpamFilename.to_string f)
  in
  let confirm s f =
    if OpamGlobals.confirm "%s" s then f ()
  in
  { mkdir; rmdir; cp; rm; confirm }

let script_commands project_root ochan =
  let made_dirs = ref [] in
  Printf.fprintf ochan "#!/bin/bash\n";
  let mkdir d =
    if not (List.mem d !made_dirs) then (
      Printf.fprintf ochan "mkdir -p %S\n" (OpamFilename.Dir.to_string d);
      made_dirs := d :: !made_dirs
    ) in
  let rmdir ~opt d =
    let f = OpamFilename.Dir.to_string d in
    Printf.fprintf ochan "if [ -d %S ]\n" f;
    Printf.fprintf ochan "then rmdir -p %S 2>/dev/null" f;
    if not opt then
      Printf.fprintf ochan " ||\n  echo \"Warning: could not remove directory %s\"" f;
    Printf.fprintf ochan "\nfi\n"
  in
  let cp ?exec ~opt ~src ~dst () =
    mkdir (OpamFilename.dirname dst);
    let mode = match exec with
      | Some true -> "-m 0755"
      | Some false -> "-m 0644"
      | None -> "" in
    let src = OpamFilename.remove_prefix project_root src in
    let dst = OpamFilename.to_string dst in
    Printf.fprintf ochan "if [ -e %S ]\n" src;
    Printf.fprintf ochan "then install %s %S %S\n" mode src dst;
    if exec <> Some true &&
       OpamMisc.ends_with ~suffix:".cmi" src &&
       OpamMisc.ends_with ~suffix:".cmi" dst
    then
      (let chext f e = OpamMisc.remove_suffix ~suffix:"cmi" f ^ e in
       Printf.fprintf ochan "if [ -e %S ]; then install %s %S %S; fi"
         (chext src "cmt") mode (chext src "cmt") (chext dst "cmt");
       Printf.fprintf ochan "if [ -e %S ]; then install %s %S %S; fi"
         (chext src "cmti") mode (chext src "cmti") (chext dst "cmti"));
    if not opt then
      Printf.fprintf ochan "else echo \"Error: %s doesn't exist\"\n" src;
    Printf.fprintf ochan "fi\n"
  in
  let rm ~opt file =
    let f = OpamFilename.to_string file in
    Printf.fprintf ochan "if [ -e %S ]; then rm -f %S\n" f f;
    if not opt then
      Printf.fprintf ochan "else echo \"Warning: %s doesn't exist\"\n" f;
    Printf.fprintf ochan "fi\n"
  in
  let confirm msg f =
    Printf.fprintf ochan
      "read -p %S' [y/n] ' -n 1 -r; echo; if [ \"$REPLY\" = 'y' ]; then\n" msg;
    f ();
    Printf.fprintf ochan "fi\n";
  in
  { mkdir; rmdir; cp; rm; confirm }

(* [f (dest, file_list, is_exec)] should take care of the processing,
   where [dest src dst] returns the destination of a file with a
  ["src" {"dst"}] line in the .install *)
let iter_install f instfile o =
  let module D = OpamPath.Switch in
  let module S = OpamFile.Dot_install in
  let dest ?fix dir =
    let dir = OpamMisc.Option.default dir fix in
    fun src dst ->
      OpamFilename.create dir
        (OpamMisc.Option.default (OpamFilename.basename src) dst)
  in
  let dest_global ?fix instdir_f =
    dest ?fix (instdir_f o.prefix (OpamSwitch.of_string ""))
  in
  let dest_pkg ?fix instdir_f =
    let fix =
      OpamMisc.Option.map
        OpamFilename.OP.(fun d ->
            d / OpamPackage.Name.to_string o.pkgname)
        fix
    in
    dest ?fix
      (instdir_f o.prefix (OpamSwitch.of_string "") o.pkgname)
  in
  List.iter f
    [ dest_global                 D.bin,      S.bin instfile,        true;
      dest_global                 D.sbin,     S.sbin instfile,       true;
      dest_pkg    ?fix:o.libdir   D.lib,      S.lib instfile,        false;
      dest_global ?fix:o.topdir   D.toplevel, S.toplevel instfile,   false;
      dest_global ?fix:o.stubsdir D.stublibs, S.stublibs instfile,   true;
      dest_global ?fix:o.mandir   D.man_dir,  S.man instfile,        false;
      dest_pkg                    D.share,    S.share instfile,      false;
      dest_global                 D.share_dir,S.share_root instfile, false;
      dest_pkg                    D.etc,      S.etc instfile,        false;
      dest_pkg    ?fix:o.docdir   D.doc,      S.doc instfile,        false; ]

let install options =
  let instfile = OpamFile.Dot_install.safe_read options.file in
  let project_root = OpamFilename.cwd () in
  let cmd =
    if options.script then script_commands project_root stdout
    else do_commands project_root
  in
  let install_files (dest, files, exec) =
    List.iter
      (fun (base, dst) ->
         let src_file = OpamFilename.create project_root base.c in
         let dst_file = dest src_file dst in
         cmd.cp ~exec ~opt:base.optional ~src:src_file ~dst:dst_file ())
      files
  in
  iter_install install_files instfile options;
  List.iter
    (fun (src, dst) ->
       let src_file = OpamFilename.create (OpamFilename.cwd ()) src.c in
       cmd.confirm
         (Printf.sprintf "Do you want to install %s to %s ?"
            (OpamFilename.Base.to_string src.c) (OpamFilename.to_string dst))
         (fun () -> cmd.cp ~opt:false ~src:src_file ~dst ())
    ) (OpamFile.Dot_install.misc instfile)

let uninstall options =
  let instfile = OpamFile.Dot_install.safe_read options.file in
  let project_root = OpamFilename.cwd () in
  let cmd =
    if options.script then script_commands project_root stdout
    else do_commands project_root
  in
  let dirs_to_remove = ref OpamFilename.Dir.Set.empty in
  let remove_files (dest, files, _) =
    List.iter (fun (base, dst) ->
        let src_file = OpamFilename.create project_root base.c in
        let dst_file = dest src_file dst in
        cmd.rm ~opt:base.optional dst_file;
        dirs_to_remove := OpamFilename.Dir.Set.add
          (OpamFilename.dirname dst_file) !dirs_to_remove)
      files
  in
  iter_install remove_files instfile options;
  List.iter (cmd.rmdir ~opt:true)
    (List.rev (OpamFilename.Dir.Set.elements !dirs_to_remove));
  List.iter (fun df ->
      cmd.rmdir ~opt:false
        (df options.prefix (OpamSwitch.of_string "") options.pkgname))
    OpamPath.Switch.([ lib; share; etc; doc ]);
  List.iter
    (fun (_src, dst) ->
       cmd.confirm
         (Printf.sprintf "Remove %s ?" (OpamFilename.to_string dst))
         (fun () -> cmd.rm ~opt:false dst))
    (OpamFile.Dot_install.misc instfile)

let options =
  let file =
    let doc = "The OPAM .install file to read for installation instructions" in
    Arg.(value & pos 0 (some string) None & info ~docv:"PKG.install" ~doc [])
  in
  let prefix =
    let doc = "The prefix to install to. You can use \
               eg '$$$$PREFIX' to output a relocatable script" in
    Arg.(value & opt string "/usr/local" &  info ~docv:"PREFIX" ~doc ["prefix"])
  in
  let script =
    let doc = "Don't execute the commands, but output a shell-script \
               (experimental)" in
    Arg.(value & flag & info ~doc ["script"])
  in
  let pkgname =
    let doc = "Specify the package name. Used to set install directory under \
               `share/', etc. \
               By default, basename of the .install file" in
    Arg.(value & opt (some string) None & info ~docv:"NAME" ~doc ["name"])
  in
  let mandir =
    let doc = "Manpages dir. Relative to $(b,prefix) or absolute. \
               By default $(i,$$prefix/man)." in
    Arg.(value & opt (some string) None & info ~docv:"PATH" ~doc ["mandir"])
  in
  let libdir =
    let doc = "OCaml lib dir. Relative to $(b,prefix) or absolute. \
               By default $(i,$$prefix/lib) ; sometimes setting this to \
               $(i,$$(ocamlc -where)) is preferable." in
    Arg.(value & opt (some string) None & info ~docv:"PATH" ~doc ["libdir"])
  in
  let stubsdir =
    let doc = "Stubs installation dir. Relative to $(b,prefix) or absolute. \
               By default $(i,$$libdir/stublibs)." in
    Arg.(value & opt (some string) None & info ~docv:"PATH" ~doc ["stubsdir"])
  in
  let topdir =
    let doc = "Toplevel install dir. Relative to $(b,prefix) or absolute. \
               By default $(i,$$libdir/toplevel)." in
    Arg.(value & opt (some string) None & info ~docv:"PATH" ~doc ["topdir"])
  in
  let docdir =
    let doc = "Documentation dir. Relative to $(b,prefix) or absolute. \
               By default $(i,$$prefix/doc)." in
    Arg.(value & opt (some string) None & info ~docv:"PATH" ~doc ["docdir"])
  in
  let make_options file prefix script name mandir libdir stubsdir topdir docdir
    =
    let file =
      match file with
      | Some file ->
        let f = OpamFilename.of_string (file ^ ".install") in
        if OpamFilename.exists f then f else
        let f = OpamFilename.of_string file in
        if OpamFilename.exists f then f else
          raise (Invalid_argument ("File not found: " ^ file))
      | None ->
        let candidates = OpamFilename.files (OpamFilename.cwd ()) in
        match
          List.filter (fun f -> OpamFilename.check_suffix f ".install")
            candidates
        with
        | [f] -> f
        | [] ->
          raise (Invalid_argument "No .install file found")
        | files ->
          let msg =
            Printf.sprintf
              "Please specify a .install file, %s found in current dir"
              (OpamMisc.pretty_list
                 (List.map
                    (fun f -> OpamFilename.(Base.to_string (basename f)))
                    files))
          in
          raise (Invalid_argument msg)
    in
    let prefix = OpamFilename.Dir.of_string prefix in
    let pkgname = match name with
      | Some n -> OpamPackage.Name.of_string n
      | None when OpamFilename.check_suffix file ".install" ->
        OpamPackage.Name.of_string
          (OpamFilename.Base.to_string
             (OpamFilename.basename (OpamFilename.chop_extension file)))
      | None ->
        raise (Invalid_argument
                 "Could not guess the package name, please specify `--name'")
    in
    let mk_dir = function
      | None -> None
      | Some d when Filename.is_relative d ->
        Some OpamFilename.OP.(prefix / d)
      | Some d ->
        Some (OpamFilename.Dir.of_string d)
    in
    let mandir = mk_dir mandir in
    let libdir = mk_dir libdir in
    let stubsdir = match mk_dir stubsdir, libdir with
      | None, Some d -> Some OpamFilename.OP.(d / "stubslibs")
      | d, None | (Some _ as d), _ -> d
    in
    let topdir = match mk_dir topdir, libdir with
      | None, Some d -> Some OpamFilename.OP.(d / "toplevel")
      | d, None | (Some _ as d), _ -> d
    in
    let docdir = mk_dir docdir in
    { file; prefix; script; pkgname; mandir; libdir; stubsdir; topdir; docdir }
  in
  Term.(pure make_options $ file $ prefix $ script $ pkgname
        $ mandir $ libdir $ stubsdir $ topdir $ docdir)

let command =
  let remove =
    Arg.(value & vflag false &
         [ false, Arg.info ["i";"install"] ~doc:"Install the package (the default)";
           true, Arg.info ["u";"uninstall";"remove"] ~doc:"Remove the package"; ])
  in
  Term.(
    pure
      (fun options remove ->
         if remove then uninstall options else install options)
    $ options $ remove)

let info =
  let doc = "Handles (un)installation of package files following instructions from \
             OPAM *.install files." in
  Term.info "opam-installer" ~version:OpamVersion.(to_string current) ~doc

let () =
  try
    match
      Term.eval ~catch:false (command,info)
    with
    | `Error _ -> exit 2
    | _ -> exit 0
  with
  | Invalid_argument s ->
    prerr_string "ERROR: "; prerr_endline s; exit 2
  | OpamGlobals.Exit i -> exit i
  | e ->
    OpamGlobals.error "Failure during install";
    prerr_string (Printexc.to_string e); exit 1

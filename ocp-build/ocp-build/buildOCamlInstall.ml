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


(* open BuildBase *)
(* open Stdlib2 *)
open MetaTypes
open BuildTypes
open BuildEngineTypes
open BuildEngineGlobals
open BuildOCPTypes
open BuildOCPVariable

(* TODO:
  When installing, we should accept the option -sanitize, to clean everything
  before, i.e. remove META files that would be refer to the same package.
  We should check that there is nothing remaining before installing, and
  ask the user to use -sanitize if necessary to remove conflicting files.
*)


(*
   When to generate META files ?
   - 1st easy: when we install files into one of the ocamlfind directories
        The META file is in the directory, without "directory"
   - 2nd easy: when we install files into OCAMLLIB subdirectories
        The META. file is in one of the ocamlfind directories, and
        the "directory" starts with "^"
   - 3rd : when we install files into somewhere else
        The META. file is in one of the ocamlfind directory, and
        the "directory" is absolute.
*)

type install_where = {
  install_destdir : string option;
  install_libdirs : string list;
  install_bindir : string;
  install_datadir : string option;

  install_ocamlfind : string list;
  install_ocamllib : string;
}

type install_what = {
  install_byte_bin : bool;
  install_asm_bin : bool;
  install_byte_lib : bool;
  install_asm_lib : bool;
}

type package_uninstaller = {
  mutable un_nfiles : int;
  mutable un_ndirs : int;
  mutable un_version : string;
  mutable un_name : string;
  mutable un_descr : string;
  mutable un_warning : string option;
  mutable un_directory : string;
  mutable un_type : string;
  mutable un_packages : string list;
}

type kind = DIR | FILE | VERSION | WARNING | DESCR | TYPE | PACK

type log = (kind * string) list


module List = struct
  include List

  let rec split_after l1 l2 =
    match l1, l2 with
      _, [] -> Some l1
    | [], _ -> None
    | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then
        split_after t1 t2
      else
        None

  let starts_with l1 l2 =
    (split_after l1 l2) <> None

end

let split_dir dir =
  let rec iter pos pos0 path dir len =
    if pos = len then begin
      if pos = pos0 then List.rev path else
        List.rev (String.sub dir pos0 (pos-pos0) :: path)
    end else
      match dir.[pos] with
        '/' | '\\' ->
          let path =
            if pos = pos0 then path
            else
              String.sub dir pos0 (pos-pos0) :: path
          in
          let pos = pos+1 in
          iter pos pos path dir len
      | _ -> iter (pos+1) pos0 path dir len
  in
  iter 0 0 [] dir (String.length dir)

let string_of_kind = function
| CMI -> "CMI"
| CMO -> "CMO"
| CMX -> "CMX"
| CMXS -> "CMXS"
| CMA -> "CMA"
| CMXA -> "CMXA"
| CMXA_A -> "CMXA_A"
| C_A -> "C_A"
| RUN_BYTE -> "RUN_BYTE"
| RUN_ASM -> "RUN_ASM"


let add_log log kind name =
  log := (kind, name) :: !log

let in_destdir where file =
  match where.install_destdir with
    None -> file
  | Some destdir -> Filename.concat destdir file

let rec safe_mkdir where log filename =
  let filename_d = in_destdir where filename in
  try
    let st = MinUnix.stat filename_d in
(*    Printf.eprintf "safe_mkdir: %S exists\n%!" filename_d; *)
    match st.MinUnix.st_kind with
      MinUnix.S_DIR -> ()
    | _ ->
      failwith (Printf.sprintf
                  "File.safe_mkdir: %S exists, but is not a directory"
                  filename)
  with MinUnix.Unix_error (MinUnix.ENOENT, _, _) ->
(*    Printf.eprintf "safe_mkdir: %S doesnot exist\n%!" filename_d; *)
    let dirname = Filename.dirname filename in
    safe_mkdir where log dirname;
    let basename = Filename.basename filename in
    match basename with
    | "." | ".." -> ()
    | _ ->
      MinUnix.mkdir filename_d 0o755;
      add_log log DIR filename

(* [dst] must be the target file name, not the name of its directory *)
let rec copy_rec where log src dst =
    (*    Printf.eprintf "copy_rec: %S -> %S\n%!" src dst; *)
  let dst_d = in_destdir where dst in
  let st = MinUnix.stat src in
  match st.MinUnix.st_kind with
  | MinUnix.S_DIR ->
    safe_mkdir where log dst;
    File.RawIO.iter_dir (fun basename ->
      copy_rec where log (Filename.concat src basename)
        (Filename.concat dst basename)) src
  | MinUnix.S_REG ->
    add_log log FILE dst;
    File.RawIO.copy_file src dst_d;
    MinUnix.chmod dst_d st.MinUnix.st_perm
  | _ ->
    failwith (Printf.sprintf
                "File.copy_rec: cannot copy unknown kind file %S"
                src)

let copy_file where log src_file dst_file =
  Printf.eprintf " %s%!" (Filename.basename dst_file);
  copy_rec where log src_file dst_file


(* TODO: we should do an analysis on the packages that are going to be
  installed, to check that all libraries have also their dependencies
  loaded. *)

let save_uninstall_log uninstall_file log =
  let oc = open_out uninstall_file in
  Printf.fprintf oc "OCP 1\n";
  List.iter (fun (kind, file) ->
    Printf.fprintf oc "%s %s\n" (match kind with
      FILE -> "REG"
    | DIR -> "DIR"
    | VERSION -> "VER"
    | WARNING -> "WAR"
    | DESCR -> "LOG"
    | TYPE -> "TYP"
    | PACK -> "PCK"
    ) (String.escaped file);
  ) log;
  close_out oc



let install where what lib installdir =
  let log = ref [] in
  let uninstall_file = Filename.concat installdir
    (Printf.sprintf "%s.uninstall" lib.lib_name) in
  let save_uninstall warning =
      let log = !log in
      let log =
        (VERSION, lib.lib_version) ::
          (TYPE, BuildOCPTree.string_of_package_type lib.lib_type) ::
          log in
      let log = match warning with
          None -> log
        | Some warning -> (WARNING, warning) :: log
    in
    let uninstall_file_d = in_destdir where uninstall_file in
    save_uninstall_log uninstall_file_d log
  in
  try
    let installbin = where.install_bindir in
    let installdir_d = in_destdir where installdir in
    if not (Sys.file_exists installdir_d) then
      safe_mkdir where log installdir;
    add_log log FILE uninstall_file;

    let bundle = lib.lib_bundles in
    List.iter (fun pk -> add_log log PACK pk.lib_name) bundle;

  (* Do the installation *)
    let meta = MetaFile.empty () in

    meta.meta_version <- Some lib.lib_version;
    meta.meta_description <- Some
      (get_string_with_default [lib.lib_options] "description" lib.lib_name);
    List.iter (fun dep ->
      if dep.dep_link then
        MetaFile.add_requires meta [] [dep.dep_project.lib_name]
    ) lib.lib_requires;

    let install_file file kind =
      let dst_file =
        match kind with

        | CMI when
            what.install_asm_lib || what.install_byte_lib ->
          Some (Filename.concat installdir file.file_basename)
        | C_A when
            what.install_asm_lib || what.install_byte_lib ->
          Some (Filename.concat installdir file.file_basename)
        | CMO when
            what.install_byte_lib ->
          Some (Filename.concat installdir file.file_basename)
        | CMX
        | CMXA_A when
            what.install_asm_lib ->
          Some (Filename.concat installdir file.file_basename)
        | CMA when
            what.install_byte_lib ->
          MetaFile.add_archive meta [ "byte", true ] [ file.file_basename ];
              meta.meta_exists_if <- file.file_basename::
                meta.meta_exists_if;
              Some (Filename.concat installdir file.file_basename)
        | CMXA when
            what.install_asm_lib ->
          MetaFile.add_archive meta [ "native", true ] [ file.file_basename ];
              Some (Filename.concat installdir file.file_basename)
        | CMXS when
            what.install_asm_lib ->
          Some (Filename.concat installdir file.file_basename)
        | RUN_ASM when
            what.install_asm_bin ->
          Some (Filename.concat installbin
                  (Filename.chop_suffix file.file_basename ".asm"))
        | RUN_BYTE when
            what.install_byte_bin ->
          Some (Filename.concat installbin file.file_basename)

        | RUN_BYTE
        | RUN_ASM
        | CMI
        | CMO
        | CMX
        | CMXS
        | CMA
        | CMXA
        | CMXA_A
        | C_A
          -> None

      in
      match dst_file with
        None -> ()
      | Some dst_file ->

        let dirname = Filename.dirname dst_file in
        let dirname_d = in_destdir where dirname in
        if not (Sys.file_exists dirname_d) then
          safe_mkdir where log dirname;

      (*            Printf.fprintf stderr "\tto %S : %!" dst_file; *)
        let src_file = file_filename file in
        copy_file where log src_file dst_file
    in
    Printf.eprintf "\tfiles: %!";
    List.iter (fun (file, kind) ->
      install_file file kind
    ) lib.lib_byte_targets;
    List.iter (fun (file, kind) ->
      install_file file kind
    ) lib.lib_asm_targets;

    begin match  where.install_datadir with
      None -> ()
    | Some datadir ->
      let datadir = Filename.concat datadir lib.lib_name in
      List.iter (fun file ->
        safe_mkdir where log datadir;
        let basename = Filename.basename file in
        let dst_file = Filename.concat datadir basename in
        let src_file = Filename.concat (File.to_string lib.lib_dirname) file in
        copy_file where log src_file dst_file
      )
        (get_strings_with_default [lib.lib_options] "data_files" []);

    end;


    List.iter (fun file ->
      safe_mkdir where log installdir;
      let basename = Filename.basename file in
      let dst_file = Filename.concat installdir basename in
      let src_file = Filename.concat (File.to_string lib.lib_dirname) file in
      copy_file where log src_file dst_file
    )
      (get_strings_with_default [lib.lib_options] "lib_files" []);

    List.iter (fun file ->
      safe_mkdir where log installbin;
      let basename = Filename.basename file in
      let dst_file = Filename.concat installbin basename in
      let src_file = Filename.concat (File.to_string lib.lib_dirname) file in
      copy_file where log src_file dst_file
    )
      (get_strings_with_default [lib.lib_options] "bin_files" []);

  (* What kind of META file do we create ? *)
    let topdir_list = split_dir (Filename.dirname installdir) in
    let ocamlfind_path = List.map split_dir where.install_ocamlfind in

    Printf.fprintf stderr "\n%!";
    let meta_files =
      if List.mem topdir_list ocamlfind_path then
        [Filename.concat installdir "META"]
      else
        let ocamllib = split_dir where.install_ocamllib in
        let installdir_list = split_dir installdir in
        match List.split_after installdir_list ocamllib with
        | None ->
          meta.meta_directory <- Some installdir;
          []
        | Some subdir ->
          meta.meta_directory <- Some ("^" ^ String.concat "/" subdir);
          []
    in
    let rec iter meta_files =
      match meta_files with
        [] ->
          Printf.eprintf
            "Warning: could not find a correct location to write\n";
          Printf.eprintf "\tthe META file\n%!"

      | meta_file :: meta_files ->
        try
(*          Printf.eprintf "CHECK %S\n%!" meta_file; *)
          let meta_file_d = in_destdir where meta_file in
          safe_mkdir where log (Filename.dirname meta_file);
          MetaFile.create_meta_file meta_file_d meta;
          add_log log FILE meta_file;
          Printf.eprintf "Generated META file %s\n%!" meta_file;
        with _ -> iter meta_files
    in
    iter (if meta_files = [] then
        let meta_basename = Printf.sprintf "META.%s" lib.lib_name in
        List.map (fun dirname ->
          Filename.concat dirname meta_basename
        ) (where.install_ocamlfind @ [ where.install_ocamllib ])
      else meta_files);

    save_uninstall None;

  with e ->
    save_uninstall (Some "Install partially failed");
    raise e

(* TODO: we might install the same package several times in different
   directories, no ? *)

let find_installdir where what lib_name =
  (match where.install_destdir with
      None -> ()
    | Some destdir ->
      try
        File.RawIO.safe_mkdir destdir
      with e ->
        Printf.eprintf "Error: install DESTDIR %S can be created\n%!"
          destdir;
        BuildMisc.clean_exit 2
  );


    (* Check whether it is already installed : *)
  let rec iter possible libdirs =
    match libdirs with
      [] ->
        begin
          match possible with
            None ->
              Printf.eprintf "Error: no directory where to install files\n%!";
              None
          | Some installdir ->
            Some installdir
        end

    | libdir :: libdirs ->
      let installdir = Filename.concat libdir lib_name in
      let installdir_d = in_destdir where installdir in
      if Sys.file_exists installdir_d then (* Found ! *)
        begin
(*
          TODO: we should copy all files from this directory to a
          sub-directory "_attic". Can we disable .ocp files within them ?
*)
            (* TODO: shouldn't we check for an .uninstall file ? *)
          Printf.eprintf "Error: package %S seems already installed in\n"
            lib_name;
          Printf.eprintf "\t%S\n%!" installdir;
          None
        end
      else
        begin
          match possible with
          | None ->
            let testlog = ref [] in
            if
              (try
                 safe_mkdir where testlog installdir;
                 true
               with _ -> false)
            then begin
              List.iter (function
              | (DIR, dir) ->
                let dir_d = in_destdir where dir in
                MinUnix.rmdir dir_d
              | _ -> assert false) !testlog;
              iter (Some installdir) libdirs
            end else begin
              Printf.eprintf
                "Warning: skipping install dir %S, not writable.\n%!"
                libdir;
              iter None libdirs
            end
          | Some _ ->
            iter possible libdirs
        end
  in
  iter None where.install_libdirs

let uninstallers = Hashtbl.create 13

let rec scan_for_uninstallers where files libdirs =
  match libdirs with
    [] -> files
  | libdir :: libdirs ->
    let files = ref files in
    let libdir_d = in_destdir where libdir in
    BuildScanner.scan_directory_for_suffix libdir_d ".uninstall"
      (fun filename ->
(*        Printf.eprintf "UINSTALLER %S\n%!" filename; *)
        let name = Filename.chop_suffix (Filename.basename filename)
          ".uninstall" in
        files := StringMap.add name filename !files);
    scan_for_uninstallers where !files libdirs

let load_uninstallers where =
  try
    Hashtbl.find uninstallers where.install_libdirs
  with Not_found ->
    let files = scan_for_uninstallers where StringMap.empty
      where.install_libdirs in
    Hashtbl.add uninstallers where.install_libdirs files;
    files

let find_uninstaller where lib_name =
  let uninstallers = load_uninstallers where in
  try
    Some (StringMap.find lib_name uninstallers)
  with Not_found -> None

type uninstall_state = {
  uninstall_where : install_where;
  mutable uninstall_done : StringSet.t;
  mutable uninstall_dirs : string list;
  mutable uninstall_errors : int;
}

let uninstall_init where =
  {
    uninstall_where = where;
    uninstall_done = StringSet.empty;
    uninstall_dirs = [];
    uninstall_errors = 0;
  }

let rec uninstall_by_uninstaller state uninstall_file_d =
  let where = state.uninstall_where in
  let lib_name = Filename.chop_suffix (Filename.basename uninstall_file_d)
    ".uninstall"
  in
  if not (Sys.file_exists uninstall_file_d) then begin
    Printf.eprintf "Warning: uninstaller file %S not found\n%!"
      uninstall_file_d;
    state.uninstall_errors <- state.uninstall_errors + 1
  end else
    let list = File.lines_of_file uninstall_file_d in
    List.iter (fun line ->
      match OcpString.cut_at line ' ' with
      | "OCP", _ -> ()
      | "REG", file ->
        let file_d = in_destdir where file in
        if Sys.file_exists file_d then begin try
          Sys.remove file_d
        with e ->
          Printf.eprintf
            "Warning: exception %S while removing regular file %S\n%!"
            (Printexc.to_string e) file_d
        end
      | "DIR", file ->
        let file_d = in_destdir where file in
        state.uninstall_dirs <- file_d :: state.uninstall_dirs
      | "VER", version -> ()
      | "WAR", warning -> ()
      | "LOG", log -> ()
      | "TYP", kind -> ()
      | "PCK", name ->
        let name =
          if Filename.check_suffix name ".uninstall" then
            Filename.chop_suffix (Filename.basename name) ".uninstall"
          else name
        in
        schedule_uninstall state name
      | _ ->
        Printf.eprintf "Bad line [%S] in file %S\n%!" line uninstall_file_d;
    ) list;
    Printf.printf "Package %s uninstalled\n%!" lib_name

and schedule_uninstall state lib_name =
  if not (StringSet.mem lib_name state.uninstall_done) then begin
    state.uninstall_done <- StringSet.add lib_name state.uninstall_done;
      match find_uninstaller state.uninstall_where lib_name with
        None ->
        Printf.eprintf
          "Warning, uninstall of %S failed:\n" lib_name;
        Printf.eprintf "   could not find uninstaller file %S\n%!"
          (lib_name ^ ".uninstall");
        state.uninstall_errors <- state.uninstall_errors + 1
    | Some uninstall_file ->
      uninstall_by_uninstaller state uninstall_file
  end

let uninstall_by_name state lib_name =
  schedule_uninstall state lib_name

let uninstall state lib =
  if lib.lib_install then uninstall_by_name state lib.lib_name

let uninstall_finish state =
  let dirs = state.uninstall_dirs in
  let dirs = List.sort compare dirs in
  let dirs = List.rev dirs in
  List.iter (fun file_d ->
    if Sys.file_exists file_d then begin try
      MinUnix.rmdir file_d
    with e ->
      Printf.eprintf
        "Warning: exception %S while removing directory %S\n%!"
        (Printexc.to_string e) file_d
    end
  ) dirs

let load_uninstaller filename =
    let list = File.lines_of_file filename in
    let name = Filename.chop_suffix (Filename.basename filename) ".uninstall" in
    let un = {
      un_nfiles = 0;
      un_ndirs = 0;
      un_version = "n/a";
      un_name = name;
      un_descr = name;
      un_warning = None;
      un_directory = Filename.dirname filename;
      un_type = "n/a";
      un_packages = [];
    } in
    List.iter (fun line ->
      match OcpString.cut_at line ' ' with
      | "OCP", _ -> ()
      | "REG", file -> un.un_nfiles <- 1 + un.un_nfiles
      | "DIR", file -> un.un_ndirs <- 1 + un.un_ndirs
      | "VER", v -> un.un_version <- v
      | "WAR", w -> un.un_warning <- Some w
      | "LOG", d -> un.un_descr <- d
      | "TYP", t -> un.un_type <- t
      | "PCK", pck -> un.un_packages <- pck :: un.un_packages
      | _ ->
        Printf.eprintf "Bad line [%S] in file %S\n%!" line filename;
    ) list;
    un

let list_installed where =
  let uninstallers = load_uninstallers where in
  let list = ref [] in
  StringMap.iter (fun _ filename ->
    list := load_uninstaller filename :: !list) uninstallers;
  !list

let is_installed where lib_name =
  (find_uninstaller where lib_name) <> None

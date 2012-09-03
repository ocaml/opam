(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(* A script helper to initialize an OPAM repo.
   It takes as input a directory where:
   * opam/                 contains some OPAM files
   * descr/                contains some description files
   * archives/             might contain some archive
   * url/$name.$version    contains archive address
   * files/$name.$version  contains some files to include in
                           the archives (typically .config.in
                           and .install)

   After the script is run, archives/ contains all the package archives
   for the available descr and OPAM files *)

module F = Filename

open Types

let tmp_dir0 = Run.mk_temp_dir "mk-repo"

let root = Path.R.of_path (Dirname.cwd ())

let opams = Path.R.available root

module StringSet = Set.Make (struct type t = string let compare = compare end)

module Checking = struct
  module Checking = struct
      
    type t = 
        { name : string 
        ; set : StringSet.t }

    (** returns a set containing filenames of [s] *)
    let init s = 
      { set = 
          Utils.set_of_list StringSet.empty StringSet.add
            (Array.to_list
               (*(fun f -> Basename.to_string (Dirname.basename f)) *)
               (Sys.readdir (Dirname.to_string (Path.R.root root / s))))
      ; name = s }
        
    let remove nv t = { t with set = StringSet.remove (NV.to_string nv) t.set }
    let not_empty t = not (StringSet.is_empty t.set)

    let warning t = 
      if StringSet.is_empty t.set then
        ()
      else
        Globals.error "should not exist : %s" (String.concat " " (List.rev (StringSet.fold (fun s l -> Printf.sprintf "%s/%s" t.name s :: l) t.set [])))
  end

  let init () = List.map Checking.init [ "url" ; "files" (*; "descr"*) ]
  let remove nv = List.map (Checking.remove nv)
  let not_empty = List.exists Checking.not_empty

  let warning = List.iter Checking.warning
end

let l_check = ref (Checking.init ())

let () =
  Globals.msg "Available:\n";
  NV.Set.iter (fun nv ->
    l_check := Checking.remove nv !l_check;
    Globals.msg " - %s\n" (NV.to_string nv)
  ) opams

let opams =
  NV.Set.filter (fun nv ->
    not (Filename.exists (Path.R.archive root nv))
  ) opams

let () =
  Globals.msg "To update:\n";
  NV.Set.iter (fun nv ->
    Globals.msg " - %s\n" (NV.to_string nv)
  ) opams

let url nv =
  let f = Path.R.root root / "url" // NV.to_string nv in
  if Filename.exists f then
    Some (Utils.string_strip (Raw.to_string (Filename.read f)))
  else
    None

let files nv =
  let d = Path.R.root root / "files" / NV.to_string nv in
  if Dirname.exists d then
    Filename.list d
  else
    []

let tmp nv =
  Path.R.root root / "tmp" / NV.to_string nv

let tmp_dir nv =
  Dirname.of_string tmp_dir0 / NV.to_string nv

let wget src =
  let open Globals in
  match os with
  | Darwin | FreeBSD | OpenBSD -> [ "curl"; "--insecure" ; "-OL"; src ]
  | _ -> [ "wget"; "--no-check-certificate"; src ]

let archive_name src =
  let name = F.basename src in
  if F.check_suffix name ".tar.gz"
  || F.check_suffix name ".tar.bz2"
  || F.check_suffix name ".tgz"
  || F.check_suffix name ".tbz" then
    name
  else    
    Printf.sprintf "%s.tar.gz" name

let mv src =
  let name = archive_name src in
  if (F.basename src) = name then
    []
  else
    [ "mv" ; F.basename src ; name ]

let () =
  let has_error = ref false in
  Dirname.mkdir (Path.R.archive_dir root);
  NV.Set.iter (fun nv ->
    Globals.msg "Processing %-40s ." (NV.to_string nv);
    let tmp_dir = tmp_dir nv in
    Dirname.rmdir tmp_dir;
    Dirname.mkdir tmp_dir;
    begin match url nv with
    | None     -> let _ = Globals.error "no url found for %s" (NV.to_string nv) in has_error := true
    | Some url ->
        Filename.remove (tmp nv // archive_name url);
        Dirname.mkdir (tmp nv);
        let err = Dirname.exec (tmp nv) [
          wget url;
          mv url;
        ] in
        if err <> 0 then
          Globals.error_and_exit "Cannot get %s" url;
        Filename.extract (tmp nv // archive_name url) tmp_dir;
        Globals.msg ".";
        List.iter (fun f ->
          Filename.copy_in f tmp_dir
        ) (files nv);
        Globals.msg ".";
        let err = Dirname.exec (Dirname.of_string tmp_dir0) [
          [ "tar" ; "czf" ; Filename.to_string (Path.R.archive root nv) ; NV.to_string nv ]
        ] in
        if err <> 0 then
          Globals.error_and_exit "Cannot compress %s" (Dirname.to_string tmp_dir);
        Globals.msg " OK\n";
    end;
  ) opams;

  Checking.warning !l_check;
  if !has_error || Checking.not_empty !l_check then
    Globals.exit 66

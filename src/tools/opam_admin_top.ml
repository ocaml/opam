(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 OCamlPro                                             *)
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

(* To be used for quick repo scripts using the toplevel *)
open OpamFilename.Op
open OpamStd.Op

let identity _ x = x
let true_ _ = true

let repo = OpamRepositoryBackend.local (OpamFilename.cwd ())
let packages = OpamRepository.packages repo
let compilers = OpamRepository.compilers repo

let wopt w f = function
  | None -> OpamFilename.remove f
  | Some contents -> w f contents

let apply f x prefix y =
  match f with
  | None   -> ()
  | Some f -> f x prefix y

type 'a action = [`Update of 'a | `Remove  | `Keep]

let to_action f x y =
  match f with
  | None   -> `Keep
  | Some f -> match y with
    | None   -> `Keep
    | Some y -> `Update (f x y)

let of_action o = function
  | `Keep     -> o
  | `Update x -> Some x
  | `Remove   -> None

let iter_packages_gen ?(quiet=false) f =
  let packages = OpamRepository.packages_with_prefixes repo in
  let changed_pkgs = ref 0 in
  let changed_files = ref 0 in
  (* packages *)
  OpamPackage.Map.iter (fun package prefix ->
      if not quiet then
        OpamConsole.msg "Processing package %s... "
          (OpamPackage.to_string package);
      let opam_file = OpamRepositoryPath.opam repo prefix package in
      let opam = OpamFile.OPAM.read opam_file in
      let descr_file = OpamRepositoryPath.descr repo prefix package in
      let descr =
        if OpamFilename.exists descr_file then
          Some (OpamFile.Descr.read descr_file)
        else None
      in
      let url_file = OpamRepositoryPath.url repo prefix package in
      let url =
        if OpamFilename.exists url_file then
          Some (OpamFile.URL.read url_file)
        else None
      in
      let dot_install_file =
        OpamRepositoryPath.files repo prefix package
        // (OpamPackage.Name.to_string (OpamPackage.name package) ^ ".install") in
      let dot_install =
        if OpamFilename.exists dot_install_file then
          Some (OpamFile.Dot_install.read dot_install_file)
        else None
      in
      let opam2, descr2, url2, dot_install2 =
        f package ~prefix ~opam ~descr ~url ~dot_install
      in
      let descr2 = of_action descr descr2 in
      let url2 = of_action url url2 in
      let dot_install2 = of_action dot_install dot_install2 in
      let changed = ref false in
      let upd () = changed := true; incr changed_files in
      if opam <> opam2 then
        (upd ();
         let s = OpamFile.OPAM.to_string_with_preserved_format opam_file opam2 in
         OpamFilename.write opam_file s);
      if descr <> descr2 then
        (upd (); wopt OpamFile.Descr.write descr_file descr2);
      if url <> url2 then
        (upd (); wopt OpamFile.URL.write url_file url2);
      if dot_install <> dot_install2 then
        (upd (); wopt OpamFile.Dot_install.write dot_install_file dot_install2);
      if !changed then
        (incr changed_pkgs;
         if not quiet then
           OpamConsole.msg "\r\027[KUpdated %s\n" (OpamPackage.to_string package))
      else if not quiet then
        OpamConsole.msg "\r\027[K";
    ) packages;
  if not quiet then
    OpamConsole.msg "Done. Updated %d files in %d packages.\n"
      !changed_files !changed_pkgs

let iter_packages ?quiet
    ?(filter=true_) ?f ?(opam=identity) ?descr ?url ?dot_install
    () =
  iter_packages_gen ?quiet
    (fun p ~prefix ~opam:o ~descr:d ~url:u ~dot_install:i ->
      if filter p then (
        apply f p prefix o;
        opam p o, to_action descr p d , to_action url p u,
        to_action dot_install p i
      ) else
        o, `Keep, `Keep, `Keep)

let iter_compilers_gen ?(quiet=false) f =
  let compilers = OpamRepository.compilers_with_prefixes repo in
  let changed_comps = ref 0 in
  let changed_files = ref 0 in
  OpamCompiler.Map.iter (fun c prefix ->
      if not quiet then
        OpamConsole.msg "Processing compiler %s... " (OpamCompiler.to_string c);
      let comp_file = OpamRepositoryPath.compiler_comp repo prefix c in
      let comp = OpamFile.Comp.read comp_file in
      let descr_file = OpamRepositoryPath.compiler_descr repo prefix c in
      let descr =
        if OpamFilename.exists descr_file then
          Some (OpamFile.Descr.read descr_file)
        else None
      in
      let comp2, descr2 = f c ~prefix ~comp ~descr in
      let descr2 = of_action descr descr2 in
      let changed = ref false in
      let upd () = changed := true; incr changed_files in
      if comp <> comp2 then
        (upd (); OpamFile.Comp.write comp_file comp2);
      if descr <> descr2 then
        (upd (); wopt OpamFile.Descr.write descr_file descr2);
      if !changed then
        (incr changed_comps;
         if not quiet then
           OpamConsole.msg "\r\027[KUpdated %s\n" (OpamCompiler.to_string c))
      else if not quiet then
        OpamConsole.msg "\r\027[K";
    ) compilers;
  if not quiet then
    OpamConsole.msg "Done. Updated %d files in %d compiler descriptions.\n"
      !changed_files !changed_comps

let iter_compilers ?quiet ?(filter=true_) ?f ?(comp=identity) ?descr () =
  iter_compilers_gen ?quiet (fun x ~prefix ~comp:c ~descr:d ->
      if filter x then (
        apply f x prefix c;
        comp x c, to_action descr x d
      ) else
        c, `Keep)

let regexps_of_patterns patterns =
  let contains_dot str =
    let len = String.length str in
    let rec aux = function
      | -1 -> false
      | i  -> str.[i] = '.' || aux (i-1)
    in
    aux (len-1)
  in
  List.map (fun pattern ->
      if contains_dot pattern then pattern
      else pattern ^ ".*"
    ) patterns
  |> List.map (fun pattern -> Re.compile (Re_glob.globx pattern))

let filter fn patterns =
  let regexps = regexps_of_patterns patterns in
  fun t ->
    match regexps with
    | [] -> true
    | _  ->
      let str = fn t in
      List.exists (fun re -> OpamStd.String.exact_match re str) regexps

let filter_packages = filter OpamPackage.to_string
let filter_compilers = filter OpamCompiler.to_string

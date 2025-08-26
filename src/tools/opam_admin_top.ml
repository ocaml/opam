(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* To be used for quick repo scripts using the toplevel *)
open OpamFilename.Op
open OpamStd.Op

let identity _ x = x
let true_ _ = true

let repo = OpamFilename.cwd ()
let packages = OpamRepository.packages repo

let wopt w f = function
  | None -> OpamFilename.remove (OpamFile.filename f)
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
      let dot_install_file : OpamFile.Dot_install.t OpamFile.t =
        OpamFile.make
          (OpamRepositoryPath.files repo prefix package
           // (OpamPackage.Name.to_string (OpamPackage.name package) ^ ".install"))
      in
      let dot_install = OpamFile.Dot_install.read_opt dot_install_file in
      let opam2, dot_install2 =
        f package ~prefix ~opam ~dot_install
      in
      let dot_install2 = of_action dot_install dot_install2 in
      let changed = ref false in
      let upd () = changed := true; incr changed_files in
      if opam <> opam2 then
        (upd (); OpamFile.OPAM.write_with_preserved_format opam_file opam2);
      if dot_install <> dot_install2 then
        (upd (); wopt OpamFile.Dot_install.write dot_install_file dot_install2);
      if !changed then
        (incr changed_pkgs;
         if not quiet then begin
           OpamConsole.carriage_delete ();
           OpamConsole.msg "Updated %s\n" (OpamPackage.to_string package)
         end)
      else if not quiet then
        OpamConsole.carriage_delete ();
    ) packages;
  if not quiet then
    OpamConsole.msg "Done. Updated %d files in %d packages.\n"
      !changed_files !changed_pkgs

let iter_packages ?quiet
    ?(filter=true_) ?f ?(opam=identity) ?dot_install
    () =
  iter_packages_gen ?quiet
    (fun p ~prefix ~opam:o ~dot_install:i ->
      if filter p then (
        apply f p prefix o;
        opam p o, to_action dot_install p i
      ) else
        o, `Keep)

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
  |> List.map (fun pattern -> Re.compile (Re.Glob.globx pattern))

let filter fn patterns =
  let regexps = regexps_of_patterns patterns in
  fun t ->
    match regexps with
    | [] -> true
    | _  ->
      let str = fn t in
      List.exists (fun re -> OpamStd.String.exact_match re str) regexps

let filter_packages = filter OpamPackage.to_string

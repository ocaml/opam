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

let ocamlfind_command =
  if Filename.basename Sys.executable_name = "ocamlfind"
  then "ocamlfind.real"
  else "ocamlfind"

let pass_on argv =
  prerr_endline " [go on]";
  Unix.execvp ocamlfind_command
    (Array.concat [ [|ocamlfind_command|]; argv ])

let handle_install args =
  match args with
  | [] -> pass_on [||]
  | package :: args ->
    let dot_install = package ^ ".install" in
    if Sys.file_exists dot_install then
      (Printf.eprintf " %s exists, skipping" dot_install;
       pass_on (Array.of_list (package::args)))
    else
    let isoption s = try s.[0] = '-' with Invalid_argument _ -> false in
    let rec parse_params forcedll optional dlls files options = function
      | ("-destdir" | "-metadir" | "-ldconf"
        | "-patch-version" | "-patch-rmpkg") as arg :: param :: l ->
        parse_params forcedll optional dlls files ((arg,param)::options) l
      | "-optional" :: l -> parse_params forcedll true dlls files options l
      | "-dll" :: l -> parse_params (Some true) optional dlls files options l
      | "-nodll" :: l -> parse_params (Some false) optional dlls files options l
      | option :: l when isoption option ->
        parse_params forcedll optional dlls files ((option,"")::options) l
      | file :: l ->
        let isdll = match forcedll with
          | Some d -> d
          | None -> Filename.check_suffix file "dll" ||
                    Filename.check_suffix file "so"
        in
        let dlls, files =
          if isdll then (file,optional)::dlls, files
          else dlls, (file,optional)::files
        in
        parse_params forcedll optional dlls files options l
      | [] -> List.rev dlls, List.rev files, List.rev options
    in
    let dlls, files, _options = parse_params None false [] [] [] args in
    (* if try List.assoc "-destdir" rev_options <> opam-lib-dir
          with Not_found -> false
       then... *)
    let oc = open_out dot_install in
    let printl oc =
      List.iter (fun (src,opt) ->
          Printf.fprintf oc "  %S\n" (if opt then "?" ^src else src))
    in
    if files <> [] then
      Printf.fprintf oc "lib: [\n%a]\n" printl files;
    if dlls <> [] then
      Printf.fprintf oc "stublibs: [\n%a]\n" printl dlls;
    close_out oc;
    Printf.eprintf " %s%s%s generated"
      (Sys.getcwd ()) Filename.dir_sep dot_install;
    Printf.eprintf " [skip]\n"

let () =
  prerr_string "[OPAM ocamlfind wrapper]";
  if Array.length Sys.argv = 0 then pass_on Sys.argv else
  match Sys.argv.(0) with
  | "install" ->
    let args = List.tl (Array.to_list Sys.argv) in
    handle_install args
  | _ -> pass_on Sys.argv

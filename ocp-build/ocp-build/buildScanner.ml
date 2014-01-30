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
(* open SafeCaml *)
(* open OcpSystem *)

exception LocalNotFound
let not_found = LocalNotFound

exception IgnoreDirectory

let ignore_directory = IgnoreDirectory
let ignore_file_or_directory () = raise ignore_directory

let scan_directory f directory =
  let queue = Stack.create () in
  Stack.push directory queue;
  while not (Stack.is_empty queue) do
    try
      let dirname = Stack.pop queue in
      let files = Sys.readdir dirname in
      Array.sort compare files;
      Array.iter (fun basename ->
	let filename = Filename.concat dirname basename in
        if (try
          f dirname basename filename;
          true
        with
        | IgnoreDirectory -> false
        | _ -> true) then
	  Stack.push filename queue;
      ) files;
    with _ -> ()
  done;
  ()

let scan_directory_for_suffix directory extension f =
  scan_directory (fun dirname basename filename ->
    if Filename.check_suffix basename extension then
	f filename) directory

let scan_directory_for_files directory extensions =
  scan_directory (fun dirname basename filename ->
    let f = StringMap.find basename extensions in
    f filename
  ) directory

let scan_directory_for_extensions directory extensions =
  scan_directory (fun dirname basename filename ->
    let (_, last_ext) = File.cut_last_extension basename in
    let last_ext = String.lowercase last_ext in
    let f = StringMap.find last_ext extensions in
    f filename
  ) directory

(*
let scan_directory_for_extensions2 directory extensions =
  let rec iter dirname subdir =
    let files = Sys.readdir dirname in
    Array.iter (fun file ->
      let filename = Filename.concat dirname file in
      let subdir = Filename.concat subdir file in
      let (_, last_ext) = cut_last_extension file in
      let last_ext = String.lowercase last_ext in
      try
	let f = try StringMap.find last_ext extensions
	  with Not_found -> raise not_found in
	f subdir filename
      with LocalNotFound ->
	try
	  iter filename subdir
	with _ -> ()
    ) files
  in
  iter directory ""
*)

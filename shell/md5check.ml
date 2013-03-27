#load "unix.cma"

let command, file, md5 =
  if Array.length Sys.argv <> 4 then (
    Printf.eprintf "usage: ocaml %s <md5-command> <file> <md5>\n" Sys.argv.(0);
    exit 1
  ) else
    Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)

let input_line fmt =
  Printf.kprintf (fun cmd ->
    try
      let ic = Unix.open_process_in cmd in
      let r = input_line ic in
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> r
      | _ -> failwith "cmd_input_line"
    with
    | End_of_file
    | Unix.Unix_error _ -> failwith "cmd_input_line"
  ) fmt

let md5_of_file =
  match command with
  | "md5" -> input_line "md5 -q %s" file
  | "md5sum" ->
    let line = input_line "md5sum %s" file in
    begin try
      let i = String.index line ' ' in
      String.sub line 0 i
    with _ ->
      ""
    end
  | x ->
    Printf.eprintf "%S is not a valid md5 command name." x;
    exit 2

let () =
  if md5 <> md5_of_file then (
    Printf.eprintf
      "MD5 for %s differ:\n\
       \  expected: %s\n\
       \    actual: %s\n"
      file md5 md5_of_file;
    Unix.unlink file
  ) else
    Printf.printf "%s has the expected MD5.\n" file

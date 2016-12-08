let file, md5 =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "usage: ocaml %s <file> <md5>\n" Sys.argv.(0);
    exit 1
  ) else
    Sys.argv.(1), Sys.argv.(2)

let md5_of_file =
  Digest.to_hex (Digest.file file)

let () =
  if md5 <> md5_of_file then (
    Printf.eprintf
      "MD5 for %s differ:\n\
      \  expected: %s\n\
      \    actual: %s\n"
      file md5 md5_of_file;
    Sys.remove file;
    exit 1
  ) else
    Printf.printf "%s has the expected MD5.\n" file

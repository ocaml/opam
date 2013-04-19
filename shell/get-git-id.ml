let file =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "usage: ocaml %s <file>\n" Sys.argv.(0);
    exit 1
  ) else
    Sys.argv.(1)

let read file =
  let ic = open_in_bin file in
  input_line ic

let write file contents =
  let write () =
    let oc = open_out file in
    output_string oc contents;
    output_char oc '\n';
    close_out oc in
  if Sys.file_exists file then (
    let actual = read file in
    if actual <> contents then write ()
  ) else
    write ()

let (/) = Filename.concat

let git file = ".git" / file

let () =
  let opamGitVersion = "src" / "core" / "opamGitVersion.ml" in
  if Sys.file_exists (git "HEAD") then (
    let reference =
      let s = read (git "HEAD") in
      try
        let c = String.rindex s ' ' in
        String.sub s (c+1) (String.length s -c-1)
      with Not_found ->
        s in
    let sha1 = read (git reference) in
    write opamGitVersion (Printf.sprintf "let version = Some %S" sha1)
  ) else
    write opamGitVersion "let version = None"

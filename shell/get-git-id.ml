let file =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "usage: ocaml %s <file>\n" Sys.argv.(0);
    exit 1
  ) else
    Sys.argv.(1)

let read file =
  if Sys.file_exists file then
    let ic = open_in_bin file in
    Some (input_line ic)
  else
    None

let write file contents =
  let write () =
    let oc = open_out file in
    output_string oc contents;
    output_char oc '\n';
    close_out oc in
  match read file with
  | None        -> write ()
  | Some actual -> if actual <> contents then write ()

let (/) = Filename.concat

let git file = ".git" / file

let () =
  let opamGitVersion = "src" / "core" / "opamGitVersion.ml" in
  let version_none () =
    write opamGitVersion "let version = None" in
  match read (git "HEAD") with
  | None   -> version_none ()
  | Some s ->
    let reference =
      try
        let c = String.rindex s ' ' in
        String.sub s (c+1) (String.length s -c-1)
      with Not_found ->
        s in
    match read (git reference) with
    | None      -> version_none ()
    | Some sha1 -> write opamGitVersion (Printf.sprintf "let version = Some %S" sha1)

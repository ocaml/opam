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
  let version_none () =
    write file "let version = None" in
  match read (git "HEAD") with
  | None   -> version_none ()
  | Some s ->
    let reference =
      try (* look for "ref: refs/heads/..." *)
        let c = String.rindex s ' ' in
	let namedref = String.sub s (c+1) (String.length s -c-1) in
	read (git namedref)
      with Not_found -> (* detached state, .git/HEAD contains sha1 *)
        Some s in
    match reference with
    | None      -> version_none ()
    | Some sha1 -> write file (Printf.sprintf "let version = Some %S" sha1)

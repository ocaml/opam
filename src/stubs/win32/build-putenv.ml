#load "unix.cma"

let () =
  let (prefix, suffix) =
    let c = open_in Sys.argv.(3) in
    let prefix = input_line c in
    let suffix = input_line c in
    let rec f () =
      match input_line c with
      | env -> g env
      | exception End_of_file -> close_in c
    and g env =
      let elt = input_line c in
      let elt = if elt.[String.length elt - 1] <> ';' then elt ^ ";" else elt in
      let elt =
        (* See https://support.microsoft.com/en-us/help/830473 *)
        let l = String.length elt in
        if l > 8191 then begin
          Printf.eprintf "Variable %s has length %d which exceeds the maximum of 8191\n%!" env l;
          exit 1
        end else
          let current = Unix.getenv env in
          if String.length elt + String.length current > 8191 then begin
            Printf.eprintf "Warning: replacing, rather than prepending %s\n%!" env;
            elt
          end else
            elt ^ current
      in
      let () = Unix.putenv env elt in
      f ()
    in
    f ();
    (prefix, suffix)
  in
  exit (Sys.command (Printf.sprintf "%s%s%s-DUNICODE -D_UNICODE %s" prefix Sys.argv.(1) suffix Sys.argv.(2)))

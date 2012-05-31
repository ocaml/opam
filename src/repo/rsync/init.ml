(* Init scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

let remote_address = Sys.argv.(1)

let () =
  Run.mkdir "opam";
  Run.mkdir "descr";
  Run.mkdir "archives";
  Run.mkdir "compilers"

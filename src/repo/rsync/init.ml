(* Init scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

let () =
  let state = Repo_helpers.make_state () in
  Rsync.Init.make state

let f =
  try P3_bar.f (); P1.x ()
  with _ -> P3.z ()

let () =
  let t =
    try Sys.getenv "TEST"
    with _ -> "<not found>" in
  Printf.printf "TEST=%s\n%!" t

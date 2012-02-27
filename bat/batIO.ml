type 'a input = I of 'a list
type 'a output = O of Buffer.t

let read_all (I l) = List.fold_left (Printf.sprintf "%s%c") "" l

let to_string f x = 
  let b = Buffer.create 1024 in
  let () = f (O b) x in
  Buffer.contents b

type 'a input = I of 'a list
type 'a output = 
  | O of Buffer.t
  | OC of out_channel

let read_all (I l) = List.fold_left (Printf.sprintf "%s%c") "" l

let to_string f x = 
  let b = Buffer.create 1024 in
  let () = f (O b) x in
  Buffer.contents b

let write_string = 
  function 
    | OC b -> fun s ->
      output_string b s
    | _ -> failwith "to complete !"

let close_out = function
  | OC b -> ignore (Unix.close_process_out b)
  | _ -> failwith "to complete !"

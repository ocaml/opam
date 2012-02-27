let with_file_in fic f =
  let ic = open_in_bin fic in
  let rec aux l = 
    match try Some (input_char ic) with End_of_file -> None with
      | None -> List.rev l
      | Some x -> aux (x :: l) in
  let l = aux [] in
  let () = close_in ic in
  f (BatIO.I l)

let with_file_out fic f =
  let b = Buffer.create 1024 in
  let _ = f (BatIO.O b) in
  let oc = open_out fic in
  begin
    Buffer.output_buffer oc b;
    close_out oc;
  end

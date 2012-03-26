let with_file_out fic f =
  let b = Buffer.create 1024 in
  let _ = f (BatIO.O b) in
  let oc = open_out fic in
  begin
    Buffer.output_buffer oc b;
    close_out oc;
  end

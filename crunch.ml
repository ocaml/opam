let add_stdin buf =
  try
    while true do
      let line = input_line stdin in
      Buffer.add_string buf line;
      Buffer.add_char   buf '\n'
    done
  with End_of_file ->
    ()

let () =
  let name = Sys.argv.(1) in
  let buf = Buffer.create 1024 in
  add_stdin buf;
  let contents = Buffer.contents buf in
  Printf.printf "let %s =\n\"%s\"\n\n"
    name
    (String.escaped contents)

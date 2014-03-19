try
  let x = input_char stdin in
  print_char(Char.uppercase x);
  while true do 
    match input_char stdin with
   | '.' -> raise End_of_file
   | c -> print_char c
  done
with End_of_file -> print_newline()

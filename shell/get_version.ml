let c = open_in Sys.argv.(1) in
try while true do
  let line = input_line c in
  if String.length line > 8 && String.sub line 0 8 = "AC_INIT(" then
    let idx1 = String.index line ',' in
    let idx2 = String.index line ')' in
    print_string @@ String.sub line (idx1 + 1) (idx2 - idx1 - 1);
    raise Exit
done with Exit -> close_in c; exit 0
        | End_of_file
        | Sys_error _ ->
            close_in c; exit 1

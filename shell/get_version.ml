let c = open_in Sys.argv.(1) in
try while true do
  let line = input_line c in
  begin try
    Scanf.sscanf line "AC_INIT([opam],[%s@])" print_endline;
    raise Exit
  with Scanf.Scan_failure _ -> () end
done with Exit -> close_in c; exit 0
        | End_of_file
        | Sys_error _ ->
            close_in c; exit 1

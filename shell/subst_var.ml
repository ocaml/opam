let name = Printf.sprintf "@%s@" Sys.argv.(1) in
let value =
  let file, magic =
    let root =
      let rec process acc dir =
        let dir' = Filename.dirname dir in
        let base = Filename.basename dir in
        if dir' = dir then
          failwith "Invalid invocation - couldn't locate build root"
        else
          let acc = Filename.concat acc Filename.parent_dir_name in
          if base = "_build" then
            acc
          else
            process acc dir'
      in
      process "" (Sys.getcwd ())
    in
    let file = Filename.concat root "config.status" in
    if Sys.file_exists file then
      file, Printf.sprintf "S[\"%s\"]=\"" Sys.argv.(1)
    else if Sys.argv.(1) = "PACKAGE_VERSION" then
      Filename.concat root "configure.ac", "AC_INIT(opam,"
    else
      "", ""
  in
  if file <> "" then
    let c = open_in file in
    let magic_l = String.length magic in
    (* End_of_file is permitted to leak as the failure of this build step *)
    let rec process () =
      let line = input_line c in
      let line_l = String.length line in
      if line_l > magic_l then
        if String.sub line 0 magic_l = magic then begin
          close_in c;
          Scanf.unescaped @@ String.sub line magic_l (line_l - magic_l - 1)
        end else
          process ()
      else
        process ()
    in
    process ()
  else
    Sys.argv.(2)
in
let cin = open_in Sys.argv.(3) in
let name_l = String.length name in
let rec process () =
  match input_line cin with
  | exception End_of_file ->
    close_in cin
  | line ->
    begin
      try
        let idx = String.index line '@' in
        let line_l = String.length line in
        if line_l > idx + name_l - 1 && String.sub line idx name_l = name then begin
          if idx > 0 then
            print_string (String.sub line 0 idx);
          print_string value;
          print_endline (String.sub line (idx + name_l) (line_l - idx - name_l));
        end else
          print_endline line
      with Not_found ->
        print_endline line
    end;
    process ()
in
process ()

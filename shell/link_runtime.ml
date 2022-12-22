#directory "+compiler-libs";;
#load "ocamlcommon.cma";;
#directory "+unix";;
#load "unix.cma";;

let tool_arch =
  if Config.system = "mingw64" then "x86_64" else "i686"

let runtime =
  let c = Unix.open_process_in @@ Printf.sprintf "%s-w64-mingw32-gcc -print-sysroot | cygpath -f - -w" tool_arch in
  let sysroot = input_line c in
  if Unix.close_process_in c <> Unix.WEXITED 0 then
    exit 1
  else
    Filename.concat sysroot @@ Filename.concat "mingw" "bin"

let copy_file source dest =
  let {Unix.st_size} = Unix.stat source in
  let buffer = Bytes.create st_size in
  let c = open_in_bin source in
  really_input c buffer 0 st_size;
  close_in c;
  let c = open_out_bin dest in
  output c buffer 0 st_size;
  close_out c

let link_item item =
  let runtime = Filename.concat runtime item in
  if not (Sys.file_exists runtime) then begin
    Printf.eprintf "%s not found!\n" runtime;
    exit 1
  end;
  if Sys.file_exists item then
    Unix.unlink item;
  try
    Unix.symlink runtime item
  with Unix.Unix_error _ ->
    copy_file runtime item

let () = Array.iteri (fun idx item -> if idx > 0 then link_item item) Sys.argv

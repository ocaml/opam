let valid_chars = [|
  'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';
  'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';
  '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
  '+';'-';'_';'~';'.';
|]
let valid_chars_len = Array.length valid_chars

let create_timer () =
  let time1 = Posix_time2.clock_gettime `Realtime in
  fun () ->
    let time2 = Posix_time2.clock_gettime `Realtime in
    Posix_time2.Timespec.sub time2 time1

let gen_random_version () =
  let len = Random.int 5 + 1 in
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    let c = valid_chars.(Random.int valid_chars_len) in
    Bytes.set buf i c;
  done;
  Bytes.to_string buf

(** NOTE: This unit-test checks the time OpamPackage.Version.compare takes.
    The commented part below can also check new implementation against
    the old one for validity and speedup. *)
let () =
  Random.self_init ();
(*  let old_time = ref (Posix_time2.Timespec.create 0L 0L) in *)
  let new_time = ref (Posix_time2.Timespec.create 0L 0L) in
  let new_parse_x_time = ref (Posix_time2.Timespec.create 0L 0L) in
  let new_parse_y_time = ref (Posix_time2.Timespec.create 0L 0L) in
  for _ = 0 to 10_000_000 do
    let x = gen_random_version () in
    let y = gen_random_version () in
(*    let timer = create_timer () in
    let old_cmp = OpamVersionCompare.equal x y in
    old_time := Posix_time2.Timespec.add !old_time (timer ()); *)
    let timer = create_timer () in
    let vx = OpamPackage.Version.of_string x in
    new_parse_x_time := Posix_time2.Timespec.add !new_parse_x_time (timer ());
    let timer = create_timer () in
    let vy = OpamPackage.Version.of_string y in
    new_parse_y_time := Posix_time2.Timespec.add !new_parse_y_time (timer ());
    let timer = create_timer () in
    let _new_cmp = OpamPackage.Version.compare vx vy in
    new_time := Posix_time2.Timespec.add !new_time (timer ());
(*    if not (Int.equal old_cmp new_cmp) then begin
      Printf.printf "[ERROR] old_cmp %S %S = %d\n\
                             new_cmp %S %S = %d\n"
        x y old_cmp x y new_cmp;
      exit 1;
    end; *)
  done;
  Printf.printf "parse_x_time = %s, parse_y_time = %s, cmp_time = %s, total = %s\n"
    (Posix_time2.Timespec.to_string !new_parse_x_time) (Posix_time2.Timespec.to_string !new_parse_y_time) (Posix_time2.Timespec.to_string !new_time) (Posix_time2.Timespec.to_string Posix_time2.Timespec.(add !new_parse_x_time (add !new_parse_y_time !new_time)))

type content =
  | File of string (* file *)
  | Dir of (string * string) list (* directory with list filename * content *)
  | Symlink (* Soft Link *)
  | Hardlink (* Hard link *)
  | V (* void *)

type arborescence = {
  name: string;
  first : content;
  second : content;
}

(** Contents *)

let foo = "foo\n"
let bar = "bar\n"
let foobar = "foo\nbar\n"
let same_file = {
  name = "same-file";
  first = File foo;
  second = File foo;
}
let diff_file = {
  name = "diff-file";
  first = File foo;
  second = File bar;
}
let diff_file_plus_fst = {
  name = "diff-file-plus-fst";
  first = File foobar;
  second = File foo;
}
let diff_file_plus_snd = {
  name = "diff-file-plus-snd";
  first = File foo;
  second = File foobar;
}

let content_working_diff = [
  same_file;
  diff_file;
  diff_file_plus_fst;
  diff_file_plus_snd;
  { name = "diff-file";
    first = File foo;
    second = File bar;
  };
  { name = "diff-file-plus-fst";
    first = File foobar;
    second = File foo;
  };
  { name = "diff-file-plus-snd";
    first = File foo;
    second = File foobar;
  };
  { name = "file-only-fst";
    first = File foo;
    second = V;
  };
  { name = "file-only-snd";
    first = V;
    second = File foo;
  };
  { name = "same-dir";
    first = Dir [];
    second = Dir [];
  };
  { name = "diff-dir-plus-fst";
    first = Dir [ "fst", foo ];
    second = Dir [ "fst", foobar ] ;
  };
  { name = "diff-dir-plus-snd";
    first = Dir [ "fst", foobar ];
    second = Dir [ "fst", foo ];
  };
  { name = "dir-only-fst";
    first = Dir [ "fst", foo ];
    second = V;
  };
]

let content_dir_file = [
  same_file;
  { name = "file-fst-dir-snd";
    first = File foo;
    second = Dir [ "fst", foo];
  };
]

let content_file_dir = [
  same_file;
  { name = "dir-fst-file-snd";
    first = Dir [ "fst", foo ];
    second = File foo;
  };
]

let content_symlink_fst = [
  same_file;
  { name = "linked-file-fst";
    first = Symlink;
    second = File foo;
  };
]

let content_symlink_snd = [
  same_file;
  { name = "linked-file-snd";
    first = File foo;
    second = Symlink;
  };
]

let content_hardlink_fst = [
  same_file;
  { name = "hardlinked-file-fst";
    first = Hardlink;
    second = File foo;
  };
]

let content_hardlink_snd = [
  same_file;
  { name = "hardlinked-file-snd";
    first = File foo;
    second = Hardlink;
  };
]


let content_patch_failure_garbage = [
  same_file;
  diff_file;
]
let diff_patch_failure_garbage =
  "something in\n" ^
  "the file\n" ^
  "that is not\n" ^
  "patch format\n"

let content_patch_failure_truncated = [
  same_file;
  diff_file;
  diff_file_plus_fst;
]
let diff_patch_failure_truncated =
  "--- first/diff-file\n" ^
  "+++ second/diff-file\n" ^
  "@@ -1,1 +1,1 @@\n" ^
  "-foo\n" ^
  "+bar\n" ^
  "--- first/diff-fi\n"

let _good_diff =
  "\n" ^
  "--- first/diff-file\n" ^
  "+++ second/diff-file\n" ^
  "@@ -1,1 +1,1 @@\n" ^
  "-foo\n" ^
  "+bar\n" ^
  "--- first/diff-file-plus-fst\n" ^
  "+++ second/diff-file-plus-fst\n" ^
  "@@ -2,1 +2,0 @@\n" ^
  "-bar\n"

(** Utils *)

let print = Printf.printf

open OpamFilename.Op
let read_dir root names =
  let lst =
    List.map (fun name ->
        let dir = root / name in
        (name^"/", []) ::
        List.map (fun f ->
            OpamFilename.remove_prefix root f,
            OpamStd.String.split (OpamFilename.read f) '\n')
          (OpamFilename.rec_files dir)
        @
        List.map (fun d -> OpamFilename.remove_prefix_dir root d, [])
          (OpamFilename.rec_dirs dir))
      names
    |> List.flatten
    |> List.map (fun (file, content) ->
        (OpamSystem.back_to_forward file, content))
  in

  let lst = List.sort (fun (f,_) (f',_) -> String.compare f f') lst in
  OpamStd.Format.itemize ~bullet:"+ "
    (function
      | d, [] -> d
      | d, c ->
        Printf.sprintf "%s\n%s"
          d
          (OpamStd.List.concat_map ~left:"" ~right:"" "\n"
             (Printf.sprintf "> %s") c))
    lst

let first = "first"
let second = "second"

let write_setup dir content =
  let first_root = dir / first in
  let second_root = dir / second in
  List.iter (fun d ->
      OpamFilename.cleandir d;
      OpamFilename.mkdir d)
    [ first_root; second_root; ] ;
  let link_f =
    let link = lazy (
      let f = dir // "linked_file" in
      if not (OpamFilename.exists f) then
        OpamFilename.write f bar;
      f
    ) in
    fun () -> Lazy.force link
  in
  let create inner_dir name = function
    | File content ->
      OpamFilename.write (inner_dir // name) content
    | Dir lst ->
      let inner_dir = inner_dir / name in
      List.iter (fun (n,c) -> OpamFilename.write (inner_dir // n) c) lst
    | Symlink ->
      OpamFilename.link ~relative:false ~target:(link_f ())
        ~link:(inner_dir // name)
    | Hardlink ->
      let target = OpamFilename.to_string (link_f ()) in
      let link = OpamFilename.to_string (inner_dir // name) in
      Unix.link target link
    | V -> ()
  in
  List.iter (fun {name; first; second} ->
      create first_root name first;
      create second_root name second)
    content

type diff_patch =
  | DiffPatch
  | Patch of string

let print_dirs dir =
  print "%s\n" (read_dir dir [ first; second ])

let diff_patch dir content kind =
  write_setup dir content;
  print "*** SETUP ***\n";
  print_dirs dir;
  let diff =
    match kind with
    | Patch patch ->
      print "*** GIVEN DIFF ***\n";
      let fpatch = dir // "patch" in
      OpamFilename.write fpatch patch;
      Some fpatch
    | DiffPatch ->
      print "*** DIFF ***\n";
      match
        OpamRepositoryBackend.get_diff dir
          (OpamFilename.Base.of_string first)
          (OpamFilename.Base.of_string second)
      with
      | exception Failure s -> print "ERROR: %s\n" s; None
      | None -> print "No diff\n"; None
      | some -> some
  in
  match diff with
  | None -> ()
  | Some diff ->
    print "%s\n" (OpamFilename.read diff);
    let result =
      OpamFilename.patch ~allow_unclean:false diff
        (dir / first)
    in
    match result with
    | None ->
      print "*** PATCHED ***\n";
      print_dirs dir
    | Some exn ->
      print "*** PATCH ERROR ***\n";
      print "ERROR: %s\n" (Printexc.to_string exn)

(** The tests *)

let tests = [
  "normal", content_working_diff, DiffPatch;
  "diff file/dir error", content_dir_file, DiffPatch;
  "diff dir/file error", content_file_dir, DiffPatch;
  "symlink fst", content_symlink_fst, DiffPatch;
  "symlink snd", content_symlink_snd, DiffPatch;
  "hardlink fst", content_hardlink_fst, DiffPatch;
  "hardlink snd", content_hardlink_snd, DiffPatch;
  "patch error garbage", content_patch_failure_garbage,
  Patch diff_patch_failure_garbage;
  "patch truncated", content_patch_failure_truncated,
  Patch diff_patch_failure_truncated;
]

let () =
  (* This causes Windows to use LF endings instead of CRLF, which simplifies the comparison with the reference file *)
  Unix.putenv "LC_ALL" "C";
  set_binary_mode_out stdout true;
  Unix.dup2 Unix.stdout Unix.stderr;
  OpamFilename.with_tmp_dir @@ fun dir ->
  List.iteri (fun i (label, content, kind) ->
      print "\n----------------------\n";
      print " Test %d: %s\n" (i+1) label;
      print "----------------------\n\n";
      diff_patch dir content kind)
    tests

type content =
  | File of string (* file *)
  | Dir of (string * string) list (* directory with list filename * content *)
  | NamedDir of string * (string * string) list (* directory with list filename * content *)
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

let content_empty_file_snd = [
  same_file;
  { name = "im-empty";
    first = V;
    second = File "";
  };
]

let content_empty_file_fst = [
  same_file;
  { name = "im-empty";
    first = File "";
    second = V;
  };
]

let content_file_fst_to_file_in_dir_snd = [
  same_file;
  let name = "move-me" in
  { name;
    first = File bar;
    second = NamedDir ("inner", [name, bar]);
  };
]

let content_single_file_in_dir_snd = [
  same_file;
  { name = "im-here";
    first = Dir [ "delete-me", bar];
    second = Dir [];
  };
  { name = "im-not-here";
    first = Dir [ "delete-me", "baz"];
    second = V;
  };
]

(** Utils *)

let print = Printf.printf
let rm_hex =
  let re =
    Str.regexp {|[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]?|}
  in
  let by = "c0ffee" in
  fun s -> Str.global_replace re by s

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
    |> List.filter (fun (name, _) ->
        not (OpamStd.String.contains ~sub:".git" name))
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

let write_setup ?(only_fst=false) dir content =
  let first_root = dir / first in
  let second_root = dir / second in
  List.iter (fun d ->
      OpamFilename.cleandir d;
      OpamFilename.mkdir d)
    (if only_fst then [first_root] else [ first_root; second_root; ]);
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
    | (Dir _ | NamedDir _) as cdir ->
      let name, lst =
        match cdir with
        | Dir lst -> name, lst
        | NamedDir (name, lst) -> name, lst
        | _ -> assert false
      in
      let inner_dir = inner_dir / name in
      OpamFilename.mkdir inner_dir;
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
      if not only_fst then create second_root name second)
    content

(* --Git-- *)
let git_cmds repo_root commands error_msg =
  let commands =
    List.map (fun args ->
        OpamSystem.make_command "git"
          ("-C"::(OpamFilename.Dir.to_string repo_root)::args))
      commands
  in
  try
    List.iter (fun command ->
        match OpamProcess.run command with
        | {OpamProcess.r_code = 0; _ } -> ()
        | _ -> failwith (OpamProcess.string_of_command command))
      commands
  with Failure e ->
    print "ERROR:%s: %s\n" error_msg (rm_hex e)

let make_git_repo dir =
  let first_root = dir / first in
  let commands = [
    [ "init"];
    [ "add"; "--all" ];
    [ "commit"; "-qm"; "first" ];
  ] in
  git_cmds first_root commands "Git init"

let generate_git_diff dir =
  let first_root = dir / first in
  let name = dir // "diff-git" in
  OpamFilename.remove name;
  OpamFilename.touch name;
  let commands = [
    [ "add"; "--all" ];
    [ "commit"; "-qm"; "second" ];
    [ "status" ];
    [ "-c"; "diff.noprefix=false"; "diff"; "--text"; "--no-ext-diff"; "-R"; "-p";
      "HEAD..HEAD^"; "--output="^(OpamFilename.to_string name) ]
  ] in
  git_cmds first_root commands "Git generate diff";
  print "*** GIT DIFF ***\n";
  print "%s\n" (rm_hex @@ OpamFilename.read name);
  name
(* --Git-- *)

type diff_patch =
  | DiffPatch
  | Patch of string

type setup = {
  label: string; (* setup label *)
  content: arborescence list; (* the content of directory, first and second one *)
  kind: diff_patch; (* what test to run *)
  git: bool; (* add a test where the first directory is a git directory or not *)
}

let print_dirs dir =
  print "%s\n" (read_dir dir [ first; second ])

let diff_patch dir setup =
  let { content; kind; git; _ } = setup in
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
      | exception Failure s -> print "ERROR: %s\n" (rm_hex s); None
      | exception e ->
        print "ERROR: %s\n" (rm_hex @@ Printexc.to_string e);
        None
      | None -> print "No diff\n"; None
      | Some (f,_) -> Some f
  in
  match diff with
  | None -> ()
  | Some diff ->
    if git then make_git_repo dir;
    print "%s\n" (OpamFilename.read diff);
    let apply ~git diff =
      let git = if git then "GIT " else "" in
      let result =
        OpamFilename.patch ~allow_unclean:false (`Patch_file diff)
          (dir / first)
      in
      match result with
      | Ok _ ->
        print "*** %sPATCHED ***\n" git;
        print_dirs dir;
        true
      | Error exn ->
        print "*** %sPATCH ERROR ***\n" git;
        print "ERROR: %s\n" (rm_hex @@ Printexc.to_string exn);
        false
    in
    let patched = apply ~git:false diff in
    if patched && git then
      (let diff = generate_git_diff dir in
       write_setup ~only_fst:true dir content;
       let _ : bool = apply ~git:true diff in ())

(** The tests *)

let tests = [
  { label = "normal";
    content = content_working_diff;
    kind = DiffPatch;
    git = true;
  };
  { label = "diff file/dir error";
    content = content_dir_file;
    kind = DiffPatch;
    git = true;
  };
  { label = "diff dir/file error";
    content = content_file_dir;
    kind = DiffPatch;
    git = true;
  };
  { label = "symlink fst";
    content = content_symlink_fst;
    kind = DiffPatch;
    git = false;
  };
  { label = "symlink snd";
    content = content_symlink_snd;
    kind = DiffPatch;
    git = false;
  };
  { label = "hardlink fst";
    content = content_hardlink_fst;
    kind = DiffPatch;
    git = false;
  };
  { label = "hardlink snd";
    content = content_hardlink_snd;
    kind = DiffPatch;
    git = false;
  };
  { label = "patch error garbage";
    content = content_patch_failure_garbage;
    kind = Patch diff_patch_failure_garbage;
    git = false;
  };
  { label = "patch truncated";
    content = content_patch_failure_truncated;
    kind = Patch diff_patch_failure_truncated;
    git = false;
  };
  { label = "add empty file";
    content = content_empty_file_snd;
    kind = DiffPatch;
    git = true;
  };
  { label = "remove empty file";
    content = content_empty_file_fst;
    kind = DiffPatch;
    git = true;
  };
  { label = "move file into a new directory";
    content = content_file_fst_to_file_in_dir_snd;
    kind = DiffPatch;
    git = true;
  };
  { label = "delete file that deletes the directory";
    content = content_single_file_in_dir_snd;
    kind = DiffPatch;
    git = true;
  };
]

let () =
  (* This causes Windows to use LF endings instead of CRLF, which simplifies the comparison with the reference file *)
  Unix.putenv "LC_ALL" "C";
  set_binary_mode_out stdout true;
  Unix.dup2 Unix.stdout Unix.stderr;
  OpamFilename.with_tmp_dir @@ fun dir ->
  List.iteri (fun i setup ->
      print "\n----------------------\n";
      print " Test %d: %s\n" (i+1) setup.label;
      print "----------------------\n\n";
      diff_patch dir setup)
    tests

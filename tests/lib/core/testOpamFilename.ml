(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let test_starts_with ~ctxt () =
  let fun_name = "OpamFilename.starts_with" in
  let test ~on ~prefix_dir ~file ~expected =
    let test_name = Printf.sprintf "(%s, %s)" prefix_dir file in
    if List.exists (String.equal Sys.os_type) on then
      (let prefix_dir' = OpamFilename.raw_dir prefix_dir in
       let file' = OpamFilename.raw file in
       let got = OpamFilename.starts_with prefix_dir' file' in
       if Bool.equal expected got then
         OpamUnit.success ~ctxt ~fun_name ~test_name ()
       else
         let expected = Bool.to_string expected in
         let got = Bool.to_string got in
         OpamUnit.failure ~ctxt ~fun_name ~test_name ~expected ~got ())
    else
      let reason = Printf.sprintf "Test does not run on %s" Sys.os_type in
      OpamUnit.skip ~ctxt ~fun_name ~test_name ~reason ()
  in
  List.iter
    (fun (on, prefix_dir, file, expected) ->
       test ~on ~prefix_dir ~file ~expected)
    [
      (* absolute path without parent dirname *)
      ["Unix"; "Cygwin"], "/no/match", "/foo/bar/file", false;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar/file", true;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar/baz/file", true;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar-baz/file", false;
      ["Unix"; "Cygwin"], "/foo/bar/", "/foo/bar-baz/file", false;
      ["Unix"; "Cygwin"], "/foo/bar/file", "/foo/bar/file", false;
      ["Cygwin"], "/cygdrive/c/foo/bar", "/cygdrive/c/foo/bar/file", true;
      ["Cygwin"], "/cygdrive/c/foo/bar", "/cygdrive/d/foo/bar/file", false;
      ["Win32"], {|C:\no\match|}, {|C:\foo\bar\file|}, false;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar\file|}, true;
      ["Win32"], {|C:\foo\bar|}, {|D:\foo\bar\file|}, false;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar\baz\file|}, true;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar-baz\file|}, false;
      ["Win32"], {|C:\foo\bar\|}, {|C:\foo\bar-baz\file|}, false;
      ["Win32"], {|C:\foo\bar\file|}, {|C:\foo\bar\file|}, false;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar/file|}, true;
      (* from current directory *)
      (* Note that those tests exhibit how starts_with behaves with [.]
         and [..] when constructed with OpamFilename.raw rather than specify
         what the ideal behaviour is. *)
      ["Unix"; "Cygwin"], "/no/match", "./foo/bar/file", false;
      ["Unix"; "Cygwin"], "/foo/bar", "./foo/bar/file", false;
      ["Unix"; "Cygwin"], "./foo/bar", "./foo/bar/file", true;
      ["Win32"], {|C:\no\match|}, {|.\foo\bar\file|}, false;
      ["Win32"], {|C:\foo\bar|}, {|.\foo\bar\file|}, false;
      ["Win32"], {|.\foo\bar|}, {|.\foo\bar\file|}, true;
      (* current directory in the middle *)
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/./bar/file", false;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar/./file", true;
      ["Unix"; "Cygwin"], "/foo/./bar", "/foo/./bar/file", true;
      ["Unix"; "Cygwin"], "/foo/./bar", "/foo/./bar/./file", true;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\.\bar\file|}, false;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar\.\file|}, true;
      ["Win32"], {|C:\foo\.\bar|}, {|C:\foo\.\bar\file|}, true;
      ["Win32"], {|C:\foo\.\bar|}, {|C:\foo\.\bar\.\file|}, true;
      (* absolute path with parent dirname *)
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/../bar/file", false;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar/../file", true;
      ["Unix"; "Cygwin"], "/foo/../bar", "/foo/../bar/file", true;
      ["Unix"; "Cygwin"], "/foo/../bar", "/foo/../bar/../file", true;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\..\bar\file|}, false;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar\..\file|}, true;
      ["Win32"], {|C:\foo\..\bar|}, {|C:\foo\..\bar\file|}, true;
      ["Win32"], {|C:\foo\..\bar|}, {|C:\foo\..\bar\..\file|}, true;
    ]

let test_dir_starts_with ~ctxt () =
  let fun_name = "OpamFilename.dir_starts_with" in
  let test ~on ~prefix_dir ~dir ~expected =
    let test_name = Printf.sprintf "(%s, %s)" prefix_dir dir in
    if List.exists (String.equal Sys.os_type) on then
      (let prefix_dir' = OpamFilename.raw_dir prefix_dir in
       let dir' = OpamFilename.raw_dir dir in
       let got = OpamFilename.dir_starts_with prefix_dir' dir' in
       if Bool.equal expected got then
         OpamUnit.success ~ctxt ~fun_name ~test_name ()
       else
         let expected = Bool.to_string expected in
         let got = Bool.to_string got in
         OpamUnit.failure ~ctxt ~fun_name ~test_name ~expected ~got ())
    else
      let reason = Printf.sprintf "Test does not run on %s" Sys.os_type in
      OpamUnit.skip ~ctxt ~fun_name ~test_name ~reason ()
  in
  List.iter
    (fun (on, prefix_dir, dir, expected) ->
       test ~on ~prefix_dir ~dir ~expected)
    [
      (* absolute path without parent dirname *)
      ["Unix"; "Cygwin"], "/no/match", "/foo/bar/dir", false;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar/dir", true;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar/baz/dir", true;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar-baz/dir", false;
      ["Unix"; "Cygwin"], "/foo/bar/", "/foo/bar-baz/dir", false;
      ["Unix"; "Cygwin"], "/foo/bar/dir", "/foo/bar/dir", true;
      ["Cygwin"], "/cygdrive/c/foo/bar", "/cygdrive/c/foo/bar/dir", true;
      ["Cygwin"], "/cygdrive/c/foo/bar", "/cygdrive/d/foo/bar/dir", false;
      ["Win32"], {|C:\no\match|}, {|C:\foo\bar\dir|}, false;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar\dir|}, true;
      ["Win32"], {|C:\foo\bar|}, {|D:\foo\bar\dir|}, false;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar\baz\dir|}, true;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar-baz\dir|}, false;
      ["Win32"], {|C:\foo\bar\|}, {|C:\foo\bar-baz\dir|}, false;
      ["Win32"], {|C:\foo\bar\dir|}, {|C:\foo\bar\dir|}, true;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar/dir|}, true;
      (* from current directory *)
      (* Note that those tests exhibit how dir_starts_with behaves with [.]
         and [..] when constructed with OpamFilename.raw rather than specify
         what the ideal behaviour is. *)
      ["Unix"; "Cygwin"], "/no/match", "./foo/bar/dir", false;
      ["Unix"; "Cygwin"], "/foo/bar", "./foo/bar/dir", false;
      ["Unix"; "Cygwin"], "./foo/bar", "./foo/bar/dir", true;
      ["Win32"], {|C:\no\match|}, {|.\foo\bar\dir|}, false;
      ["Win32"], {|C:\foo\bar|}, {|.\foo\bar\dir|}, false;
      ["Win32"], {|.\foo\bar|}, {|.\foo\bar\dir|}, true;
      (* current directory in the middle *)
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/./bar/dir", false;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar/./dir", true;
      ["Unix"; "Cygwin"], "/foo/./bar", "/foo/./bar/dir", true;
      ["Unix"; "Cygwin"], "/foo/./bar", "/foo/./bar/./dir", true;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\.\bar\dir|}, false;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar\.\dir|}, true;
      ["Win32"], {|C:\foo\.\bar|}, {|C:\foo\.\bar\dir|}, true;
      ["Win32"], {|C:\foo\.\bar|}, {|C:\foo\.\bar\.\dir|}, true;
      (* absolute path with parent dirname *)
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/../bar/dir", false;
      ["Unix"; "Cygwin"], "/foo/bar", "/foo/bar/../dir", true;
      ["Unix"; "Cygwin"], "/foo/../bar", "/foo/../bar/dir", true;
      ["Unix"; "Cygwin"], "/foo/../bar", "/foo/../bar/../dir", true;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\..\bar\dir|}, false;
      ["Win32"], {|C:\foo\bar|}, {|C:\foo\bar\..\dir|}, true;
      ["Win32"], {|C:\foo\..\bar|}, {|C:\foo\..\bar\dir|}, true;
      ["Win32"], {|C:\foo\..\bar|}, {|C:\foo\..\bar\..\dir|}, true;
    ]

let test ~ctxt () =
  test_starts_with ~ctxt ();
  test_dir_starts_with ~ctxt ()

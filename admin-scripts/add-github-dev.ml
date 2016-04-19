#!/usr/bin/env opam-admin.top
#directory "+../opam-lib";;
open Opam_admin_top;;

#use "topfind";;
#require "re";;

let github_re =
  Re.compile (Re_perl.re "([^/]*github.com/.*)/archive/.*");;

iter_packages_gen @@ fun nv ~prefix:_ ~opam ~descr:_ ~url ~dot_install:_ ->
let opam =
  if OpamFile.OPAM.dev_repo opam <> None then opam else
    match url with
    | None -> opam
    | Some u ->
      let url = OpamFile.URL.url u in
      if url.OpamUrl.backend = `http &&
         Re.execp github_re url.OpamUrl.path then
        let substrings = Re.exec github_re url.OpamUrl.path in
        let dev_url =
          { OpamUrl.transport = "git";
            path = Re.get substrings 1;
            hash = None;
            backend = `git }
        in
        let opam = OpamFile.OPAM.with_dev_repo opam dev_url in
        OpamFile.OPAM.with_opam_version opam (OpamVersion.of_string "1.2")
      else opam
in
opam, `Keep, `Keep, `Keep

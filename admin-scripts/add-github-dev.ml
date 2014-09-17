#!/usr/bin/env opam-admin.top
#directory "+../opam-lib";;
open Opam_admin_top;;

#use "topfind";;
#require "re";;

let github_re =
  Re.compile (Re_perl.re "https?://([^/]*github.com/.*)/archive/.*");;

iter_packages_gen @@ fun nv ~prefix:_ ~opam ~descr:_ ~url ~dot_install:_ ->
let opam =
  if OpamFile.OPAM.dev_repo opam <> None then opam else
    match url with
    | None -> opam
    | Some u ->
        match OpamFile.URL.(kind u, url u) with
        | `http, (addr,None) when Re.execp github_re addr ->
            let substrings = Re.exec github_re addr in
            let git = Printf.sprintf "git://%s" (Re.get substrings 1) in
            let opam =
              OpamFile.OPAM.with_dev_repo opam (Some (OpamTypes.Git (git,None)))
            in
            OpamFile.OPAM.with_opam_version opam (OpamVersion.of_string "1.2")
        | _ -> opam
in
opam, `Keep, `Keep, `Keep

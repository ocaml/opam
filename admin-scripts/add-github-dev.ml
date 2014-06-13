#!/usr/bin/env opam-admin.top
#directory "+../opam";;
open Opam_admin_top;;

#use "topfind";;
#require "re";;

let github_re =
  Re.compile (Re_perl.re "https?://([^/]*github.com/.*)/archive/.*");;

map_packages_gen @@ fun nv ~opam ~descr ~url ~dot_install ->
let opam =
  if OpamFile.OPAM.dev_repo opam <> None then opam else
    match url with
    | None -> opam
    | Some u ->
        match OpamFile.URL.(kind u, url u) with
        | `http, (addr,None) when Re.execp github_re addr ->
            let substrings = Re.exec github_re addr in
            let git = Printf.sprintf "git://%s" (Re.get substrings 1) in
            OpamFile.OPAM.with_dev_repo opam (Some (OpamTypes.Git (git,None)))
        | _ -> opam
in
opam, descr, url, dot_install

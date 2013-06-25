(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamFilename.OP

let log fmt = OpamGlobals.log "Darcs" fmt

module Darcs = struct

  let exists repo =
    OpamFilename.exists_dir (repo.repo_root / "_darcs")

  let init repo =
    let repo = OpamFilename.Dir.to_string repo.repo_address in
    OpamSystem.commands [
      (* Initialize a new darcs repository, and set a default source
         repository.  The dummy tag prevents the patches from being
         actually fetched. We use the 'fetch' command because there is
         no dedicated command with darcs to set a remote repository, as
         'git remote'. *)
      [ "darcs" ; "initialize" ];
      [ "darcs" ; "fetch" ; "--tags=ThisIsADummyTag#00"; "--set-default" ; repo ];
    ]

  let fetch repo =
    OpamGlobals.msg "%-10s Fetching %s\n"
      (OpamRepositoryName.to_string repo.repo_name)
      (OpamFilename.prettify_dir repo.repo_address);
    OpamFilename.in_dir repo.repo_root (fun () ->
      (* Fetch the changes and save them to a temporary patch bundle *)
      OpamSystem.command [ "darcs" ; "fetch"; "--all"; "--output=opam_update.bundle"]
    )

  let merge repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
      let patches_bundle = OpamFilename.of_string "opam_update.bundle" in
      if OpamFilename.exists patches_bundle then
        OpamSystem.command [ "darcs" ; "apply"; "opam_update.bundle" ];
      OpamFilename.remove patches_bundle
    )

  (* Look for file pathes {packages,compilers}/* in a set of XML lines. *)
  let files_of_xmlchanges lines =
    let rex = Re_perl.compile_pat "((packages|compilers)((/[\\.\\w]+))+)" in
    let rec aux acc = function
      | [] -> acc
      | h :: t ->
        try
          let subs = Re.exec rex h in
          let file = Re.get subs 1 in
          aux (file :: acc) t
        with Not_found ->
          aux acc t
    in
    aux [] lines

  (* Return the list of modified files of the darcs repository located
     at [dirname].  There is no simple way to get a diff of files with
     differences between the local and a remote repository, with darcs,
     as 'git diff --name-only'. We use the following workaround: 1. Tag
     the current state of the repo 'opam_update' 2. Pull (fetch and
     apply) all new patches 3. Get the changes made to the repo since
     the 'opam_update' tag, in a XML format 4. Back to initial state:
     obliterate the 'opam_update' tag and all subsequent patches *)
  let diff repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
      OpamSystem.commands [
        [ "darcs" ; "tag" ; "--author=opam@ocamlpro.com" ; "opam_update" ] ;
        [ "darcs" ; "pull"; "--all" ]
      ];
      let xml_changes = OpamSystem.read_command_output
          [ "darcs" ; "changes" ; "--xml-output" ; "--summary" ;
            "--from-tag=opam_update" ] in
      let files = files_of_xmlchanges xml_changes in
      OpamSystem.command
        [ "darcs" ; "obliterate" ; "--all" ; "--from-tag=opam_update" ];
      files <> []
    )

end

module B = OpamVCS.Make(Darcs)

let register () =
  OpamRepository.register_backend `darcs (module B: OpamRepository.BACKEND)

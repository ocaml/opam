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

let log fmt = OpamGlobals.log "GIT" fmt

module Git = struct

  type address = {
    address: dirname;
    commit : string option;
  }

  (* A git address could be of the form: git://path/to/the/repository/#SHA *)
  let address repo =
    let address = OpamFilename.Dir.to_string repo.repo_address in
    let address, commit = OpamMisc.git_of_string address in
    { address = OpamFilename.raw_dir address; commit }

  let exists repo =
    OpamFilename.exists_dir (repo.repo_root / ".git")

  let init repo =
    let address = address repo in
    let repo = OpamFilename.Dir.to_string address.address in
    OpamSystem.commands [
      [ "git" ; "init" ] ;
      [ "git" ; "remote" ; "add" ; "origin" ; repo ] ;
    ]

  let fetch repo =
    let address = address repo in
    OpamGlobals.msg "%-10s Fetching %s%s\n"
      (OpamRepositoryName.to_string repo.repo_name)
      (OpamFilename.prettify_dir address.address)
      (match address.commit with
      | None   -> ""
      | Some c -> Printf.sprintf " [%s]" c);
    OpamFilename.in_dir repo.repo_root (fun () ->
      OpamSystem.command [ "git" ; "fetch" ; "origin" ]
    )

  let merge repo =
    let address = address repo in
    let merge commit =
      try OpamSystem.command [ "git" ; "merge" ; commit ]; true
      with _ -> false in
    let commit = match address.commit with
      | None   -> "origin/master"
      | Some c -> c in
    OpamFilename.in_dir repo.repo_root (fun () ->
      if not (merge commit) then
        if not (merge ("origin/"^commit)) then
          OpamSystem.internal_error "Unknown revision: %s." commit
    )

  let diff repo =
    let address = address repo in
    let diff commit =
      try Some (
        OpamSystem.read_command_output ["git" ; "diff" ; commit ; "--name-only"])
      with _ -> None in
    let commit = match address.commit with
      | None   -> "origin/master"
      | Some c -> c in
    OpamFilename.in_dir repo.repo_root (fun () ->
      match diff commit with
      | Some [] -> false
      | Some _  -> true
      | None       ->
        match diff ("origin/"^commit) with
        | Some [] -> false
        | Some _  -> true
        | None    -> OpamSystem.internal_error "Unknown revision: %s." commit
    )

end

module B = OpamVCS.Make(Git)

let register () =
  OpamRepository.register_backend `git (module B: OpamRepository.BACKEND)

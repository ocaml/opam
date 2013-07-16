(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

let log fmt = OpamGlobals.log "VCS" fmt

open OpamTypes

module type VCS = sig
  val exists: repository -> bool
  val init: repository -> unit
  val fetch: repository -> unit
  val merge: repository -> unit
  val diff: repository -> bool
  val revision: repository -> string
end


module Make (VCS: VCS) = struct

  let init repo =
    VCS.init repo

  let pull_file_quiet dirname filename =
    let local_file = OpamFilename.create dirname (OpamFilename.basename filename) in
    if OpamFilename.exists local_file then
      Up_to_date local_file
    else
      Not_available

  let pull repo =
    VCS.fetch repo;
    let diff = VCS.diff repo in
    VCS.merge repo;
    diff

  let check_updates repo =
    if VCS.exists repo then begin
      Some (pull repo)
    end else
      None

  let rec pull_repo repo =
    match check_updates repo with
    | None ->
      OpamFilename.mkdir repo.repo_root;
      OpamFilename.in_dir repo.repo_root (fun () -> VCS.init repo);
      pull_repo repo
    | Some updated ->
      if updated then Result repo.repo_root
      else Up_to_date repo.repo_root

  let pull_file name dirname filename =
    OpamGlobals.msg "%-10s Fetching %s\n"
      (OpamPackage.Name.to_string name)
      (OpamFilename.prettify filename);
    pull_file_quiet dirname filename

  let repo dirname address =
    let repo = OpamRepository.default () in
    {
      repo with
      repo_root    = dirname;
      repo_address = address;
    }

  let pull_dir name dirname address =
    OpamGlobals.msg "%-10s Fetching %s\n"
      (OpamPackage.Name.to_string name)
      (OpamFilename.prettify_dir address);
    pull_repo (repo dirname address)

  let pull_repo repo =
    OpamGlobals.msg "%-10s Fetching %s\n"
      (OpamRepositoryName.to_string repo.repo_name)
      (OpamFilename.prettify_dir repo.repo_address);
    ignore (pull_repo repo)

  let pull_archive repo filename =
    OpamGlobals.msg "%-10s Fetching %s\n"
      (OpamRepositoryName.to_string repo.repo_name)
      (OpamFilename.prettify filename);
    pull_file_quiet (OpamPath.Repository.archives_dir repo) filename

  let revision repo =
    Some (OpamPackage.Version.of_string (VCS.revision repo))

end

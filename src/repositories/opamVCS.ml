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
  val reset: repository -> unit
  val diff: repository -> bool
  val revision: repository -> string
end


module Make (VCS: VCS) = struct

  let init repo =
    VCS.init repo

  let pull repo =
    VCS.fetch repo;
    let diff = VCS.diff repo in
    VCS.reset repo;
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

  let repo dirname address =
    let repo = OpamRepository.default () in
    {
      repo with
      repo_root    = dirname;
      repo_address = address;
    }

  let pull_url package dirname remote_url =
    let repo = repo dirname remote_url in
    OpamGlobals.msg "%-10s Fetching %s\n"
      (OpamPackage.to_string package)
      (string_of_address remote_url);
    download_dir (pull_repo repo)

  let pull_repo repo =
    OpamGlobals.msg "%-10s Fetching %s\n"
      (OpamRepositoryName.to_string repo.repo_name)
      (string_of_address repo.repo_address);
    ignore (pull_repo repo)

  let pull_archive repo filename =
    let dirname = OpamPath.Repository.archives_dir repo in
    let basename = OpamFilename.basename filename in
    let local_file = OpamFilename.create dirname basename in
    if OpamFilename.exists local_file then (
      OpamGlobals.msg "%-10s Using %s\n"
        (OpamRepositoryName.to_string repo.repo_name)
        (OpamFilename.prettify local_file);
      Up_to_date local_file
    ) else
      Not_available (OpamFilename.to_string filename)

  let revision repo =
    Some (OpamPackage.Version.of_string (VCS.revision repo))

end

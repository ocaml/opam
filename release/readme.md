# Steps to follow for each release

## Finalise opam code for release
* update version in all the opam files and in configure.ac
* run `make configure` to regenerate `./configure` [checked by github actions]
* update copyright headers
* if you're releasing the first final release of a new branch (e.g. 2.2.0) and the `root_version` has changed since the previous stable version (e.g. 2.1.6): make sure `root_version` in OpamFile.ml is set to the final release number (e.g. for 2.2.0, `root_version` should be 2.2). Make sure that `opamFormatUpgrade.ml` also contains an upgrade function from the previous version (that function will most likely be empty), and that `opamroot-versions.test` is updated accordingly too.
* run `make tests`, `opam-rt` [checked by github actions]
* update the CHANGE file: take `master_changes.md` content to fill it

## Github release

[ once bump version & changes PRs merged ]
* tag the release (git tag -am 2.2.0 2.2.0; git push origin 2.2.0)
* on an alpha1, create a branch `x.y` and push it
* /!\ Once the tag pushed, it can be updated [different commit] only in case of severe issue
* create a release (or prerelease if intermediate release) draft on github based on your tag (https://github.com/ocaml/opam/releases/new)
* add releases notes (content of `master_changes.md`) in the release draft

## Prepare blog posts

* propose the release draft for review
* prepare the blog entry in opam.ocaml.org and propose it for review
* prepare the announcement on discuss.ocaml.org and propose it for review

## Pre-built binaries creation

* If you do not have `Windows-10-x86_64.qcow2` already, read the instruction in the `windows.md` file
* Make sure your macOS system, Docker installation and brew packages are up-to-date
* Make sure the repository is in the correct state: `git switch --detach <tag>`
* launch docker using the Docker GUI macOS app
* generate opam artifacts, using `release/release.sh <tag>` from a macOS/arm64 machine, it requires to have Docker and QEMU installed (see below device requirements)
* generate the signatures using `release/sign.sh <tag>`
* upload everything from `release/out/<tag>` in the release draft

## Publish the release

* finalise the release (publish)
* add hashes in `install.sh` and `install.ps1` (and check signatures)
* bring the changes to the changelog (CHANGES) from the branch of the release to the `master` branch
* publish opam packages in opam-repository (use `opam publish --pre-release` if this is not a stable version)
* update versions (and messages, if necessary) in https://github.com/ocaml/opam-repository/blob/master/repo

## Announce!

* publish the blog entry in opam.ocaml.org
* wait until the blog post is online
* publish the announcement in discuss.ocaml.org
* update the link to the discuss post in the blog post
* update the link to the blog post in the release note
* copy the blog entry from opam.ocaml.org for https://github.com/ocaml/ocaml.org/tree/main/data/changelog/opam
* announce the release on the OCaml Discord server

## After release

* Remove the milestone that has just been released and create a new milestone for the next version
* Bump the version with a `~dev` at the end (e.g. `2.2.0~alpha~dev`)
* Check if reftests needs an update

---

## Device requirements
* Mac M1 or above with Rosetta2
* >=70GB of disk space free
* brew dependencies: git, git-lfs, gpg, qemu>=8.1.0 (avoid qemu 9.1.x, see https://gitlab.com/qemu-project/qemu/-/issues/2581), docker>=24.0.0, sshpass
* opam repo with the tag fetched
* Have the secret key available

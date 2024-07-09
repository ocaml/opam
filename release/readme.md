# Steps to follow for each release

## Finalise opam code for release
* update version in all the opam files and in configure.ac
* run `make configure` to regenerate `./configure` [checked by github actions]
* update copyright headers
* if you're releasing the first final release of a new branch (e.g. 2.2.0): make sure `root_version` in OpamFile.ml is set to the final release number (e.g. for 2.2.0, root_version should be 2.2). Make sure that opamFormatUpgrade.ml also contains an upgrade function from the previous version (that function will most likely be empty)
* run `make tests`, `opam-rt` [checked by github actions]
* update the CHANGE file: take `master_changes.md` content to fill it

## Github release

[ once bump version & changes PRs merged ]
* tag the release (git tag -am 2.2.0 2.2.0; git push origin 2.2.0)
* /!\ Once the tag pushed, it can be updated [different commit] only in case of severe issue
* create a release (or prerelease if intermediate release) draft on github based on your tag (https://github.com/ocaml/opam/releases/new)
* Make sure the repository is in the correct state: `git switch --detach <tag>`
* launch docker using the Docker GUI macOS app
* generate opam artifacts, using `release/release.sh <tag>` from a macOS/arm64 machine, it requires to have Docker and QEMU installed (see below device requirements)
* generate the signatures using `release/sign.sh <tag>`
* add releases notes (content of `master_changes.md`) in the release draft
* upload everything from `release/out/<tag>`
* finalise the release (publish)

## Publish the release

* add hashes in `install.sh` (and check signatures)
* publish opam packages in opam-repository (and add `flags: avoid-version` and `available: opam-version >= "2.1.0"` to each packages if this is not a stable version)
* update versions (and messages, if necessary) in https://github.com/ocaml/opam-repository/blob/master/repo

## Announce!

* a blog entry in opam.ocaml.org
* a announcement in discuss.ocaml.org
* copy the blog entry from opam.ocaml.org for https://github.com/ocaml/ocaml.org/tree/main/data/changelog/opam
* announce the release on the OCaml Discord server


## After release

* Bump the version with a `~dev` at the end (e.g. `2.2.0~alpha~dev`)
* Check if reftests needs an update
* Bring the changes to the changelog (CHANGES) from the branch of the release to the `master` branch

### On a release candidate
* create a branch to a `x.y` for rc's and the final release

---

## Device requirements
* Mac M1 or above with Rosetta2
* brew dependencies: git, gpg, qemu>=8.1.0, docker>=24.0.0, md5sha1sum
* opam repo with the tag fetched
* Have the secret key available

# Steps to follow for each release

## Finalise opam code for release
* update version in opam files, configure.ac
* run `make configure` to regenerate `./configure` [checked by github actions]
* update copyright headers
* run `make tests`, `opam-rt` [checked by github actions & appveyor]
* update the CHANGE file: take `master_changes.md` content to fil it

## Github release

[ once bump version & changes PRs merged ]
* tag the release (git tag -a 2.2.0; git push origin 2.2.0)
* /!\ Once the tag pushed, it can be updated only in case of severe issue
* create a release (or prerelease if intermediate release) draft on github based on your tag (https://github.com/ocaml/opam/releases/new)
* generate opam artifacts, using `shell/release.sh`, it requires to have Docker install with several remotes, the different arches
* add releases notes (content of `master_changes.md`) in the release
* upload signature of artefacts
* finalise the release (publish)

## Publish the release

* add hashes in `install.sh` (and check signatures)
* publish opam packages in opam-repository

## Announce!

* a blog entry in opam.ocaml.org
* a announcement in discuss.ocaml.org

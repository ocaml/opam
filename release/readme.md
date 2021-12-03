# Steps to follow for each release

## Finalise opam code for release
* update version in opam files, configure.ac
* run `make configure` to regenerate `./configure` [checked by github actions]
* update copyright headers
* run `make tests`, `opam-rt` [checked by github actions]
* update the CHANGE file: take `master_changes.md` content to fil it

## Github release

[ once bump version & changes PRs merged ]
* tag the release (git tag -a 2.2.0; git push origin 2.2.0)
* /!\ Once the tag pushed, it can be updated [different commit] only in case of severe issue
* create a release (or prerelease if intermediate release) draft on github based on your tag (https://github.com/ocaml/opam/releases/new)
* Install the github-unix package from opam (needed to get git-upload-release)
* Create a new token at https://github.com/settings/tokens with the following rights: read:user, repo, user:email, write:packages
* Create a new file at ~/.github/jar/infra with: {"scopes":[],"token":"ghp_XXX","app":{"name":"infra","url":"https://developer.github.com/v3/oauth_authorizations/"},"url":"https://api.github.com/authorizations/YYY","id":"YYY","note":"infra"}
  * Replace XXX by the token
  * Replace YYY by its ID
* generate opam artifacts, using `release/release.sh`, it requires to have Docker install with several remotes, the different arches
* add releases notes (content of `master_changes.md`) in the release
* upload signature of artefacts
* finalise the release (publish)

## Publish the release

* add hashes in `install.sh` (and check signatures)
* publish opam packages in opam-repository

## Announce!

* a blog entry in opam.ocaml.org
* a announcement in discuss.ocaml.org


## After release

* Bump the version with a `~dev` at the end (e.g. `2.2.0~alpha~dev`)
* Check if reftests needs an update

### On a release candidate
* create a branch to a `x.y` for rc's and the final release
* remove `shell/install.sh`
